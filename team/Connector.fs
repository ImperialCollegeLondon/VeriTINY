module Connector

open System
open SharedTypes
open Helper

// tuple helper function
let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

// /search helper functions
let searchBlocks name (block:TLogic)=
    match block.Name with
    | n when n = name -> 
        true
    | _ -> 
        false

// extract name from GeneralNet
let getName (gNet: GeneralNet) =
    gNet 
    |> snd 
    |> fst

// get Net from netList
let getGNet (name:string) (gLst:GeneralNet list): GeneralNet=
    let checkNetName refName (gNet:GeneralNet) =
        let name = getName gNet
        match name with
            |name when name = refName -> 
                true
            |_ -> 
                false
    List.find (checkNetName name) gLst


// /// net type helper funcitons
// let netLen genNet =
//     match  snd (snd genNet) with
//     | Wire netMap
//     | Bus netMap ->
//     netMap |> Map.toList |> List.map fst |> List.length



 //only works for unclocked nets
 // previously genConnections
let addTLogic (str:string) (tLogLst:TLogic list): Connection=
    let mBlock = (List.filter (searchBlocks str) tLogLst).Head

    let interpretNetId netId =
        match netId.SliceIndices with 
        |Some (0, _)-> 
            Map [0,Low]
            |> Bus    
        |Some (num2, Some num1) -> 
            num2 - num1
            |> createNewMap 
            |> Bus
        |None -> 
            Map [0,Low]
            |> Wire 
        | _ -> 
            printfn "NANI!? netId could not be interpreted either because Mark is stupid or Tuck didn't describe it well enough \n"
            Map [0,Low]
            |> Wire 

    let createGenNets (netIdLst :NetIdentifier list): GeneralNet list=
        List.map (fun (x:NetIdentifier) -> false, (x.Name,interpretNetId x)) netIdLst

    Name str, createGenNets mBlock.Inputs, createGenNets mBlock.Outputs



let addDFF (size:int): Connection =
    let net = createNewMap size |> Bus
    Name "DFF", [false, ("a", net)], [true,("out", net)]


// let rec addMegaBlock (tLogLst: TLogic list)=
//     match Console.ReadLine() with
//     |"end" ->[]
//     |"DFF" -> 
//         printf "specify DFF size \n"
//         match Int32.TryParse(Console.ReadLine()) with
//             | (true,int) ->
//                 ((Name "DFF"),
//                     [(false,("a",Bus( (List.map (fun n->(n,Low)) [0..int])|>Map.ofList)))],
//                     [(true,("out",Bus( (List.map (fun n->(n,Low)) [0..int])|>Map.ofList)))])::(addMegaBlock tLogLst)
//             | _ -> printf "input invalid,number required \n"
//                    addMegaBlock tLogLst
//     |str when List.exists (searchBlocks str) tLogLst ->
//         printf "New Megablock added \n"
//         (genConnections str avaliableTBlocks)::(addMegaBlock tLogLst )
//     |str -> printf "NANI?! match failed when adding megablocks, no block exists with name %s \n" str
//             addMegaBlock tLogLst


// gives nets unique names in Connection list
let rec refactor (cLst: Connection list) =
    
    let renameGNet (gNet: GeneralNet) =
        let addToStr = cLst.Length.ToString()

        let addToName str (gNet: GeneralNet) =
            match gNet with
            | sync, (oldStr, net) ->
                sync, (oldStr+str, net)
        addToName addToStr gNet

    match cLst.Head with
    | conn when (cLst.Tail).Length = 0 -> 
        [first conn,
            List.map renameGNet (second conn),
            List.map renameGNet (third conn)]
    | conn -> 
        (first conn,
            List.map renameGNet (second conn),
            List.map renameGNet (third conn)) :: (refactor cLst.Tail)
    | _ -> []       


let checkValidConnection inName outName (cLst:Connection list) : bool=
    let inNet = getGNet inName (List.collect second cLst)
    let outNet = getGNet outName (List.collect third cLst)

    let netLen (gNet: GeneralNet) =
        gNet
        |> extractNet
        |> netSize

    match inNet,outNet with
    | a,b when (netLen a <> netLen b) -> 
        printf "nets cannot be connected as they are of differnet sizes\n"
        false
    | a,b when (inName.[inName.Length - 1]=outName.[outName.Length - 1]) && not(fst a) && not(fst b) -> 
        printf "nets cannot be connected as they would form an unclocked loop\n"
        false
    |_ -> 
        printf "connection made between %s %s, if this is impossible tell Mark\n" inName outName
        true


let rec makeLinks (cLst: Connection list) : (string * string * GeneralNet) list=

    let searchGNetLst str (gLst: GeneralNet list) =
        List.contains str (List.map getName gLst)

    let searchInNets str (conn:Connection) : bool=
        searchGNetLst str (second conn)

    let searchOutNets str (conn:Connection) : bool=
        searchGNetLst str (third conn)

    printf "Enter block input node \n"
    match Console.ReadLine() with
    | "end" ->
        []
    | inName when List.contains true (List.map (searchInNets inName) cLst) -> 
        printf "enter block output node \n"
        match Console.ReadLine() with  
            | outName when not(List.contains (true) (List.map (searchOutNets outName)  cLst))-> 
                printf "NANI!? could not find output node: %A\n" outName    
                makeLinks cLst
            | outName when (checkValidConnection inName outName cLst) ->
                let gLstIn = List.collect second cLst
                let gLstOut = List.collect third cLst

                let sync = fst (getGNet inName gLstIn) || fst (getGNet outName gLstOut)
                let net = getGNet inName gLstIn |> snd

                (inName, outName, (sync, net)) :: makeLinks cLst
            | _ -> makeLinks cLst
    | str -> 
        printf "NANI!? input node: %A was not found \n" str
        makeLinks cLst


let finaliseConnections (cLst: Connection list): Connection list =
    printf "Current list %A\n" cLst
    let links = makeLinks cLst
    printf "links: %A\n" links

    let updateNets (gLst:GeneralNet list) links =
        let matchNames name link =
            match first link,second link with
            | a,b when a = name || b = name ->
                true
            | _ ->
                false

        let lstA = List.map first links
        let lstB = List.map second links

        let returnNewGNet (gNet:GeneralNet) =
            match getName gNet with 
                |name when List.contains name lstA || List.contains name lstB ->
                    let newGNet = third (List.find (matchNames name) links) 
                    newGNet
                | _ -> 
                    gNet

        List.map returnNewGNet gLst

    let updateConnection (conn: Connection): Connection =
        let gLstIn = second conn
        let gLstOut = third conn
        first conn, updateNets gLstIn links, updateNets gLstOut links

    List.map updateConnection cLst
    
// outward functions
// let UserIn() =
//     addMegaBlock ()
//     |> List.sort
//     |> refactor
//     |> finaliseConnections 
