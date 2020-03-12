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


/// return a bool list 
let searchGNetLst str (gLst: GeneralNet list) =
    List.contains str (List.map getName gLst)

let searchInNets str (conn:Connection) : bool=
    searchGNetLst str (second conn)

let searchOutNets str (conn:Connection) : bool=
    searchGNetLst str (third conn)

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


// acc: tLst, newCLst
let rec giveUniqueNames (tLst: string list, newCLst: Connection list) (cLst: Connection list): Connection list =

    let renameGNet str (gNet: GeneralNet) =
        match gNet with
            | sync, (oldStr, net) ->
                sync, (str+oldStr, net)

    let rec countInstances (acc:int) (refStr:string) (lst:string list) =
        match lst with
        | hd::tl when refStr = hd -> 
            countInstances (acc+1) refStr tl
        | _ ::tl ->
            countInstances acc refStr tl
        | _ -> 
            acc

    let renameConnection (str:string) (prefix:string) (cIn:Connection): Connection =
        let (lstIn, lstOut) = extractGenNetLsts cIn
        let lstIn'= List.map (renameGNet prefix) lstIn
        let lstOut' = List.map (renameGNet prefix) lstOut
        (Name str, lstIn', lstOut')

    match cLst with
    | hd::tl ->
        let (Name str) = first hd
        // if megablock appeared before
        if List.contains str tLst then
            let tLst' = tLst @ [str]
            let num = countInstances 0 str tLst'
            let prefix = str + num.ToString() + "_"

            let newCLst' = renameConnection str prefix hd 
            giveUniqueNames (tLst', newCLst @ [newCLst']) tl 
        // if megablock not appeared before
        else
            let prefix = str + "0_" 
            let newCLst' = renameConnection str prefix hd
            giveUniqueNames (tLst @ [str], newCLst @ [newCLst']) tl 
    | _ -> newCLst


let checkValidConnection outName inName  (cLst:Connection list)=
    let inGNet = getGNet inName (List.collect second cLst)
    let outGNet = getGNet outName (List.collect third cLst)

    let netLen (gNet: GeneralNet) =
        gNet
        |> extractNet
        |> netSize

    match inGNet,outGNet with
    | a,b when (netLen a <> netLen b) -> 
        printf "nets cannot be connected as they are of differnet sizes\n"
        false
    | a,b when (inName.[inName.Length - 1]=outName.[outName.Length - 1]) && not(fst a) && not(fst b) -> 
        printf "nets cannot be connected as they would form an unclocked loop\n"
        false
    |_ -> 
        printf "connection made between %s %s, if this is impossible tell Mark\n" inName outName
        true


/// given input string and output string, make a link between them in connection list
let makeLinks outName inName (cLst: Connection list) : (string * string * GeneralNet) =
    let gLstIn = List.collect second cLst
    let gLstOut = List.collect third cLst

    let sync = fst (getGNet inName gLstIn) || fst (getGNet outName gLstOut)
    let net = getGNet inName gLstIn |> snd

    inName, outName, (sync, net)


// take link list as an argument
// take in connection list and return updated connection list
type Link = string * string * GeneralNet
let finaliseConnections (linkLst:Link List) (cLst: Connection list): Connection list =
 
    // lst of input strings
    let inputNames = List.map first linkLst
    // lst of output strings
    let outputNames = List.map second linkLst

    let updateGNetLst (gLst:GeneralNet list)  =
        let matchNames name (link:Link) =
            match first link,second link with
            | a,b when a = name || b = name ->
                true
            | _ ->
                false        

        let returnNewGNet (gNet:GeneralNet) =
            match getName gNet with 
                | name when List.contains name inputNames || List.contains name outputNames ->
                    let newGNet = third (List.find (matchNames name) linkLst) 
                    newGNet
                | _ -> 
                    gNet

        List.map returnNewGNet gLst

    let updateConnection (conn: Connection): Connection =
        let gLstIn = second conn
        let gLstOut = third conn
        first conn, updateGNetLst gLstIn, updateGNetLst gLstOut

    List.map updateConnection cLst


/// example of changing cLst
/// where "jeff" is output of a tLog and "b0" is input of a different tLog
// let a = checkValidConnection "jeff" "b0" c4CLst
// let linkA = makeLinks "jeff" "b0" c4CLst
// let linkLst = [linkA]
