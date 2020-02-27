module Evaluator
open SharedTypes
open ExampleTypes
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

let extractLogLevel (netIn: Net) =
        match netIn with
        | Wire netMap
        | Bus netMap ->
        netMap |> Map.toList |> List.map snd

let updateDFF gNetLstIn gNetLstOut =

    // generate list of lists of LogicLevel of appropriate size for output
    let rec groupLogic acc lstOfLog lstOfLengths =
        match lstOfLengths with
        | hd::tl ->
            let acc', lstOfLog' = List.splitAt hd lstOfLog
            groupLogic (acc @ [acc']) lstOfLog' tl
        | [] -> 
            acc

    let newLogic = List.collect (extractNet >> extractLogLevel) gNetLstIn 
    let lstOfLengths = List.map (extractNet >> netSize) gNetLstOut

    let grpsOfLogic = groupLogic [] newLogic lstOfLengths
    let grpsOfNum = List.map generateList lstOfLengths

    let newMapLst = (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip |> List.map Map.ofList
    (gNetLstOut, newMapLst) ||> lstOpParallel [] updateGenNet 


//let evaluateModuleWithInputs (combModule: TLogic) (inputMap: Map<NetIdentifier, GraphEndPoint>) : Map<NetIdentifier, Net> =
// let evaluateModuleWithInputs tLog inMap : Map<NetIdentifier, Net> =
//     evaluateOutEx
  
// let updateGenLst gNetLst newMaps =
//     lstOpParallel [] updateGenNet gNetLst newMaps

//let extractGenNetLsts (cIn: Connection) =
//     let _, lstIn, lstOut = cIn
//     lstIn, lstOut

// let extractNetName (gNet: GeneralNet) =
//     match gNet with
//     | (_, (str, _)) -> str