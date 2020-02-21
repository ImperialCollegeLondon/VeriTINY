module Connectioniser

open System
open SharedTypes
////////////////////////////////////////////////testing,get rid of it l8r
let  l1 ={  
    Name = "Test1"
    Inputs=["A";"B";"C";"D"]
    Outputs = ["G"]
    Wires= ["E";"F"]
    ExpressionList = [(And,["A";"B"],["E"]);(And,["C";"D"],["F"]);(And,["E";"F"],["G"])]
}
let  l2 ={  
    Name = "Test2"
    Inputs=["A";"B";"C";"D"]
    Outputs = ["G"]
    Wires= ["E";"F"]
    ExpressionList = [(And,["A";"B"],["E"]);(And,["C";"D"],["F"]);(And,["E";"F"],["G"])]
}
let  DFF ={  
    Name = "DFF"
    Inputs=["A"]
    Outputs = ["B"]
    Wires= []
    ExpressionList = []
}
let avaliableBlocks = [DFF;l1;l2]
//////////////////////////////////////////////////////////////////////
/// 3-tuple helper functions
let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c
///search helper functions
let searchBlocks name (block:TLogic)=
    match block.Name with
    |n when n =name-> true
    |_->false

let searchInNets str (conn:Connection)=
    List.contains (str) (List.map (fun x->(fst(snd x))) (second conn))

let searchOutNets str (conn:Connection)=
    List.contains (str) (List.map (fun x->(fst(snd x))) (third conn))

/// net type helper funcitons
let netLen (net:GeneralNet): int =
    match  snd (snd net) with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map fst |> List.length

let rec genGenNets (blist:string list)=
    List.map (fun str ->false, (str, Wire (Map [0, Low]))) blist  //only works for unclocked wires

let genConnections name blist=
    let mBlock = (List.filter (searchBlocks name) blist).Head
    (Name name,genGenNets mBlock.Inputs,genGenNets mBlock.Outputs)

///// recursive input functions
let rec addMegaBlock ()=
    match Console.ReadLine() with
    |"end" ->[]
    |str when List.exists (searchBlocks str) avaliableBlocks ->printf "New Megablock added"
                                                               (genConnections str avaliableBlocks)::(addMegaBlock () )
    |str -> printf "NANI?! match failed when adding megablocks, no block exists with name %s" str
            addMegaBlock ()

let rec refactor (blist: Connection list) =
    match blist.Head with
    |connection when (blist.Tail).Length=0 -> [connection]
    |connection -> ((first connection),(List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (second connection)),(List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (third connection)))::(refactor blist.Tail)
    |_->[]               

let rec makeLinks ()=
    printf "Enter input node"
    match Console.ReadLine() with
    |"end" ->[]
    |str -> printf "enter output node"
            [(str, Console.ReadLine())]::makeLinks()



let rec UserIn() =
    let blockLst = addMegaBlock ()
    let refLst = refactor (List.sort blockLst)
    printf"Current list %A" refLst
    let connections = makeLinks ()
    printf "Final list: %A" connections
    

