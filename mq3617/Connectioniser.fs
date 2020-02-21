module Connectioniser

open System
open SharedTypes
////////////////////////////////////////////////testing,get rid of it l8r
let a1 = {
    Name="Test1"
    ExpressionList =[] //doesnt matter 4 me 
    Inputs = [{Name="a";SliceIndices =Some(2,Some 0)};{Name="b";SliceIndices=Some(2,Some 0)}]
    Outputs =[{Name="out";SliceIndices=Some(2,Some 0)}]
    Wires =[] //also dont matter 4 me
}
let avaliableBlocks =[a1]
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
let makeBusBits int=
    match int with
    |0 -> Map [0,Low]
    |int -> (List.map (fun x-> (x,Low))[0..int])|>Map.ofList

let interpretNetId netId =
    match netId.SliceIndices with 
    |Some (0,_)-> Wire (Map [0,Low])
    |Some (int1,Some int2) -> Bus (makeBusBits (int1-int2))
    |_->printfn "NANI!? netId could not be interpreted either because Mark is stupid or Tuck didn't describe it well enough"
        Wire (Map [0,Low])

let rec genGenNets (blist:NetIdentifier list)=
    List.map (fun (x:NetIdentifier) ->false, (x.Name,interpretNetId x )) blist  //only works for unclocked nets

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

let finaliseConnections conlist =
    let conns = makeLinks()
    []

let rec UserIn() =
    addMegaBlock ()
    |> List.sort
    |> refactor
    |> printf"Current list %A"
    |> finaliseConnections 
    |> printf "final output list%A" 
    

