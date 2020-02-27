module Evaluator
open SharedTypes
open Helper


let extractGenNetLsts (cIn: Connection) =
    let _, lstIn, lstOut = cIn
    lstIn, lstOut

let extractNet (gNet: GeneralNet): Net =
        match gNet with
        | (_, (_, Wire netMap)) -> 
            Wire netMap
        | (_, (_, Bus netMap)) ->
            Bus netMap

let netSize (netIn: Net): int =
        match netIn with
        | Wire netMap
        | Bus netMap ->
        netMap |> Map.toList |> List.map fst |> List.length

let updateGenNet (gNet: GeneralNet) newMap =
        match gNet with
        | (sync, (str, Wire _)) -> 
            sync, (str, Wire newMap) 
        | (sync, (str, Bus _)) -> 
            sync, (str, Bus newMap)

// let updateGenLst gNetLst newMaps =
//     lstOpParallel [] updateGenNet gNetLst newMaps

let extractLogLevel (netIn: Net) =
        match netIn with
        | Wire netMap
        | Bus netMap ->
        netMap |> Map.toList |> List.map snd

let updateDFF (mapIn: Map<NetIdentifier,Net>) (mapOut: Map<NetIdentifier,Net>): Map<NetIdentifier,Net> =
    let netID = mapOut |> Map.toList |> List.head |> fst
    let newNet = mapIn |> Map.toList |> List.head |> snd
    Map.add netID newNet mapOut


// let updateDFF gNetLstIn gNetLstOut =

//     // generate list of lists of LogicLevel of appropriate size for output
//     let rec groupLogic acc lstOfLog lstOfLengths =
//         match lstOfLengths with
//         | hd::tl ->
//             let acc', lstOfLog' = List.splitAt hd lstOfLog
//             groupLogic (acc @ [acc']) lstOfLog' tl
//         | [] -> 
//             acc

//     let newLogic = List.collect (extractNet >> extractLogLevel) gNetLstIn 
//     let lstOfLengths = List.map (extractNet >> netSize) gNetLstOut

//     let grpsOfLogic = groupLogic [] newLogic lstOfLengths
//     let grpsOfNum = List.map generateList lstOfLengths

//     let newMapLst = (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip |> List.map Map.ofList
//     (gNetLstOut, newMapLst) ||> lstOpParallel [] updateGenNet 

