module SynchronousBlocks
open SharedTypes
open Helper

// list of implemented Synchronous Megablocks
let syncMegaLst: Megablock list = [Name "DFF"]

let evaluateDFF (mapIn: Map<NetIdentifier,Net>) (mapOut: Map<NetIdentifier,Net>): Map<NetIdentifier,Net> =
    let netID = mapOut |> Map.toList |> List.head |> fst
    let newNet = mapIn |> Map.toList |> List.head |> snd
    Map.add netID newNet mapOut

// given megablock name, return the appropriate function for synchronous evaluation
let evaluateSyncBlock (mBlock: Megablock) =
    match mBlock with
    | Name "DFF" -> 
        evaluateDFF
    | _ -> 
        failwithf "This synchronous megablock is not supported yet"



// given function that evaluates a gNetLst and returns a list of new logic levels, group outputs corresponding to 
// the sizes of the output bus/wires 
let updateGeneral func gNetLstIn gNetLstOut =

    // generate list of lists of LogicLevel of appropriate size for output
    let rec groupLogic acc lstOfLog lstOfLengths =
        match lstOfLengths with
        | hd::tl ->
            let acc', lstOfLog' = List.splitAt hd lstOfLog
            groupLogic (acc @ [acc']) lstOfLog' tl
        | [] -> 
            acc

    let newLogic = func gNetLstIn
    let lstOfLengths = List.map (extractNet >> netSize) gNetLstOut

    let grpsOfLogic = groupLogic [] newLogic lstOfLengths
    let grpsOfNum = List.map generateList lstOfLengths

    let newMapLst = (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip |> List.map Map.ofList
    (gNetLstOut, newMapLst) ||> lstOpParallel [] updateGenNet 

