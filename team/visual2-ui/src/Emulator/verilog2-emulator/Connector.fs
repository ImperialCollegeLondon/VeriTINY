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




let addConnection (connLst: Connection list) (cIn: Connection) =
    let renameGNet str (gNet: GeneralNet) =
        match gNet with
            | sync, (oldStr, net) ->
                sync, (str+oldStr, net)

    let (Name newConnName) = first cIn

    let connCount = List.filter (fun (Name nameStr, _, _) -> nameStr = newConnName) connLst |> List.length

    let prefixGNet = List.map (fun gNet -> renameGNet (sprintf "%s_%i" newConnName connCount) gNet)
    let prefixedInpGNets =  prefixGNet (second cIn)
    let prefixedOutGNets = prefixGNet (third cIn)

    let prefixedConnection = (Name newConnName, prefixedInpGNets, prefixedOutGNets)
    connLst @ [prefixedConnection]

let checkValidConnection outName inName  (cLst:Connection list)=
    let inGNet = getGNet inName (List.collect second cLst)
    let outGNet = getGNet outName (List.collect third cLst)

    let netLen (gNet: GeneralNet) =
        gNet
        |> extractNet
        |> netSize

    match inGNet,outGNet with
    | a,b when (netLen a <> netLen b) -> 
        Error (sprintf "nets cannot be connected as they are of differnet sizes")
    |a,b when List.exists (searchOutNets inName) cLst ->
        Error (sprintf "two output nets cannot be connected together")
    |a,b when List.contains ((List.filter(searchOutNets outName)cLst).Head)(List.filter(searchInNets inName)cLst) && not(fst a) && not(fst b)->
        Error (sprintf "two output nets cannot be connected together")
    |_ -> 
        printf "connection made between %s %s, if this is impossible tell Mark\n" inName outName
        Ok ()


type Link = string * string * GeneralNet
/// given input string and output string, make a link between them in connection list
let makeLink outName inName (cLst: Connection list) : Link =
    let gLstIn = List.collect second cLst
    let gLstOut = List.collect third cLst

    let sync = fst (getGNet inName gLstIn) || fst (getGNet outName gLstOut)
    let net = getGNet inName gLstIn |> snd

    inName, outName, (sync, net)


// take link list as an argument
// take in connection list and return updated connection list

let applyLinks (linkLst:Link List) (cLst: Connection list): Connection list =
 
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
