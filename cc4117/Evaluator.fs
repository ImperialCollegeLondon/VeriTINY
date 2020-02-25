module Evaluator
open SharedTypes
open ExampleTypes
open Helper

let extractGenNetLsts (cIn: Connection) =
    let _, lstIn, lstOut = cIn
    lstIn, lstOut

let extractNetName (gNet: GeneralNet) =
    match gNet with
    | (_, (str, _)) -> str

let extractNet (gNet: GeneralNet): Net =
    match gNet with
    | (_, (_, Wire netMap)) -> 
        Wire netMap
    | (_, (_, Bus netMap)) ->
        Bus netMap
    
let extractLogLevel (netIn: Net) =
    match netIn with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map snd

let netSize (netIn: Net): int =
    match netIn with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map fst |> List.length

let findLengths (lst: GeneralNet list) =
    List.map (extractNet >> netSize) lst

// generate list of lists of LogicLevel of appropriate size for output
let rec groupLogic acc lstOfLog lstOfLengths =
    match lstOfLengths with
    | hd::tl ->
        let acc', lstOfLog' = List.splitAt hd lstOfLog
        groupLogic (acc @ [acc']) lstOfLog' tl
    | [] -> 
        acc

let updateGenNet (gNet: GeneralNet) newMap =
    match gNet with
    | (sync, (str, Wire _)) -> 
        sync, (str, Wire newMap) 
    | (sync, (str, Bus _)) -> 
        sync, (str, Bus newMap) 

let updateGenLst gNetLst newMaps =
    lstOpParallel [] updateGenNet gNetLst newMaps    

let takePrevInputs gNetLstIn gNetLstOut= List.collect (extractNet >> extractLogLevel) gNetLstIn 

let updateOutputs func gNetLstIn gNetLstOut =
    let newLogic = func gNetLstIn gNetLstOut
    let lstOfLengths = findLengths gNetLstOut

    let grpsOfLogic = groupLogic [] newLogic lstOfLengths
    let grpsOfNum = List.map generateList lstOfLengths

    let newMapLst = (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip |> List.map Map.ofList
    (gNetLstOut, newMapLst) ||> lstOpParallel [] updateGenNet 

let updateDFF gNetLstIn gNetLstOut =
    updateOutputs takePrevInputs gNetLstIn gNetLstOut

let oddCheck gNetLstIn gNetLstOut = 
    let logic = if List.length gNetLstIn % 2 = 0 then Low else High
    let lstOfLengths = findLengths gNetLstOut
    let totalNum = List.sum lstOfLengths
    [1..totalNum] |> List.map (fun _ -> logic)

let oddUpdate gNetLstIn gNetLstOut =
    updateOutputs oddCheck gNetLstIn gNetLstOut


//let evaluateModuleWithInputs (combModule: TLogic) (inputMap: Map<NetIdentifier, GraphEndPoint>) : Map<NetIdentifier, Net> =
let evaluateModuleWithInputs: Map<NetIdentifier, Net> =
    evaluateOutEx

let reconstructNet (netIDMap: Map<NetIdentifier, Net>) =
    Map.toList netIDMap |> List.map (fun ((a:NetIdentifier), b) -> false, (a.Name, b))

let getTLogic (mBlock: Megablock) (tLst: TLogic list) =
    let (Name str) = mBlock
    let checker s (tLog: TLogic): bool = 
        if s = tLog.Name then true else false
    List.tryFind (checker str) tLst 






    

  
