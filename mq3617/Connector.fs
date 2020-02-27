module Connector

open System
open SharedTypes


/// Placeholder Megqblocks
open Blocks

/// 3-tuple helper functions
let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

// /search helper functions
let searchBlocks name (block:TLogic)=
    match block.Name with
    |n when n =name-> true
    |_->false

let searchInNets str (conn:Connection)=
    List.contains (str) (List.map (fun x->(fst(snd x))) (second conn))

let searchOutNets str (conn:Connection)=
    List.contains (str) (List.map (fun x->(fst(snd x))) (third conn))

/// net type helper funcitons
let netLen genNet =
    match  snd (snd genNet) with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map fst |> List.length

let makeBusBits int=
    match int with
    |0 -> Map [0,Low]
    |int -> (List.map (fun x-> (x,Low))[0..int])|>Map.ofList

let interpretNetId netId =
    match netId.SliceIndices with 
    |Some (0,_)-> Bus (Map [0,Low])
    |Some (int1,Some int2) -> Bus (makeBusBits (int1-int2))
    |None -> Wire (Map [0,Low])
    |_->printfn "NANI!? netId could not be interpreted either because Mark is stupid or Tuck didn't describe it well enough \n"
        Wire (Map [0,Low])

let rec genGenNets (blist:NetIdentifier list)=
    List.map (fun (x:NetIdentifier) ->false, (x.Name,interpretNetId x )) blist  //only works for unclocked nets

let genConnections name blist=
    let mBlock = (List.filter (searchBlocks name) blist).Head
    (Name name,genGenNets mBlock.Inputs,genGenNets mBlock.Outputs)

let getNamefromNet refName (genNet:GeneralNet) =
    let name = fst (snd genNet)
    match name with
    |name when name=refName -> true
    |_ -> false

let getNetfromName (name:string) (netList:GeneralNet list)=
    List.find (getNamefromNet name) (netList)

/// main functions
let rec addMegaBlock ()=
    match Console.ReadLine() with
    |"end" ->[]
    |"DFF" -> printf "specify DFF size \n"
              match Int32.TryParse(Console.ReadLine()) with
                  |(true,int)->((Name "DFF"),
                                            [(false,("a",Bus( (List.map (fun n->(n,Low)) [0..int])|>Map.ofList)))],
                                            [(true,("out",Bus( (List.map (fun n->(n,Low)) [0..int])|>Map.ofList)))])::(addMegaBlock())
                  |_->printf "input invalid,number required \n"
                      addMegaBlock()
    |str when List.exists (searchBlocks str) avaliableTBlocks ->printf "New Megablock added \n"
                                                                (genConnections str avaliableTBlocks)::(addMegaBlock () )
    |str -> printf "NANI?! match failed when adding megablocks, no block exists with name %s \n" str
            addMegaBlock ()

let rec refactor (blist: Connection list) =
    match blist.Head with
    |connection when (blist.Tail).Length=0 -> [ ((first connection),
                                                    (List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (second connection)),
                                                    (List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (third connection)))]
    |connection -> ((first connection),(List.map (fun (x:GeneralNet) -> (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (second connection)),
                                                                        (List.map (fun (x:GeneralNet) -> 
                                                                        (fst x),(((fst (snd x))+((blist.Length).ToString())),(snd (snd x)))) (third connection)))::(refactor blist.Tail)
    |_->[]               

let checkValidConnection inName outName (conlist:Connection list)=
    let inNet =getNetfromName inName (List.collect (second) conlist)
    let outNet =getNetfromName outName (List.collect (third) conlist)
    match inNet,outNet with
    |a,b when (netLen a <> netLen b) -> printf "nets cannot be connected as they are of differnet sizes\n"
                                        false
    |a,b when (inName.[inName.Length - 1]=outName.[outName.Length - 1]) && not(fst a) && not(fst b) -> printf "nets cannot be connected as they would form an unclocked loop\n"
                                                                                                       false
    |_ -> printf "connection made between %s %s, if this is impossible tell Mark\n" inName outName
          true

let rec makeLinks conlist=
    printf "Enter block input node \n"
    match Console.ReadLine() with
    |"end" ->[]
    |str when List.contains (true) (List.map (searchInNets str)  conlist) -> printf "enter block output node \n"
                                                                             match Console.ReadLine () with  
                                                                             |st2 when not(List.contains (true) (List.map (searchOutNets st2)  conlist))-> printf "NANI!? could not find output node: %A\n" st2    
                                                                                                                                                           makeLinks conlist
                                                                             |st2 when (checkValidConnection str st2 conlist)->
                                                                             (str, st2,((fst (getNetfromName str (List.collect (second) conlist)))||(fst (getNetfromName st2 (List.collect (third) conlist))),
                                                                                (snd (getNetfromName str (List.collect (second) conlist)))))::makeLinks conlist
                                                                             |_->makeLinks conlist
    |str -> printf "NANI!? input node: %A was not found \n" str
            makeLinks conlist

let updateNets conn links=
    let matchNames name link =
        match first link,second link with
        |a,b when a=name || b=name ->true
        |_->false
    List.map (fun x->match fst (snd x) with 
                        |name when List.contains name (List.map first links)||List.contains name (List.map second links)->third (List.find (matchNames name) links) 
                        |_->x) conn

let finaliseConnections conlist =
    printf"Current list %A\n" conlist
    let links = makeLinks conlist
    printf "links: %A\n" links
    List.map (fun x -> (first x,updateNets (second x) links,updateNets (third x) links)) conlist
    
// outward functions
let UserIn() =
    addMegaBlock ()
    |> List.sort
    |> refactor
    |> finaliseConnections 
