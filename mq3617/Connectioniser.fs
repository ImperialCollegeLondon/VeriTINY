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
let avaliableBlocks = [l1;l2]
//////////////////////////////////////////////////////////////////////
let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

let MakeConnection (net1:GeneralNet)(net2:GeneralNet) =
    ((fst net1 || fst net2) , snd net1)

let rec genGenNets (blist:string list)=
    List.map (fun str ->false, (str, Wire (Map [0, Low]))) blist  //only works for unclocked wires

let searchBlocks name (block:TLogic)=
    match block.Name with
    |n when n =name-> true
    |_->false

let searchConnections name (conn:Connection)=
    match conn with
    |n when n = name-> true
    |_->false

let genConnections name blist=
    let mBlock = (List.filter (searchBlocks name) blist).Head
    (Name name,genGenNets mBlock.Inputs,genGenNets mBlock.Outputs)

let rec AddMegaBlock []=
    match Console.ReadLine() with
    |"end" ->[]
    |str when List.exists (searchBlocks str) avaliableBlocks ->(genConnections str avaliableBlocks)::(AddMegaBlock [] )
    |str -> printf "NANI?! match failed when adding megablocks, no block exists with name %s" str
            AddMegaBlock []

let rec refactor (blist: Connection list) =
    printf "ref starts"
    match blist.Head with
    |connection when (blist.Tail).Length=0 -> [connection]
    |connection -> ((first connection),(List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (second connection)),(List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (third connection)))::(refactor blist.Tail)
    |_->[]                                                                   
let rec UserIn =
    let blockLst = AddMegaBlock []
    let conLst = refactor (List.sort blockLst)
    printf "%A" conLst
    

