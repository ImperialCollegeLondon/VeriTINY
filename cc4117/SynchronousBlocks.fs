module SynchronousBlocks
open SharedTypes
open Helper


let updateDFF (mapIn: Map<NetIdentifier,Net>) (mapOut: Map<NetIdentifier,Net>): Map<NetIdentifier,Net> =
    let netID = mapOut |> Map.toList |> List.head |> fst
    let newNet = mapIn |> Map.toList |> List.head |> snd
    Map.add netID newNet mapOut


// given inputs and outputs, evaluate general function "func"
let updateGeneral func gNetLstIn gNetLstOut =

    // generate list of lists of LogicLevel of appropriate size for output
    let rec groupLogic acc lstOfLog lstOfLengths =
        match lstOfLengths with
        | hd::tl ->
            let acc', lstOfLog' = List.splitAt hd lstOfLog
            groupLogic (acc @ [acc']) lstOfLog' tl
        | [] -> 
            acc

    let newLogic = func gNetLstIn gNetLstOut
    let lstOfLengths = List.map (extractNet >> netSize) gNetLstOut

    let grpsOfLogic = groupLogic [] newLogic lstOfLengths
    let grpsOfNum = List.map generateList lstOfLengths

    let newMapLst = (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip |> List.map Map.ofList
    (gNetLstOut, newMapLst) ||> lstOpParallel [] updateGenNet 

