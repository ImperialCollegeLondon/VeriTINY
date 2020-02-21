module Evaluator
open SharedTypes
open ExampleTypes
open Helper

let extractGenNetLsts (cIn: Connection) =
    let _, lstIn, lstOut = cIn
    lstIn, lstOut

let extractNet (genNetIn: GeneralNet): Net =
    match genNetIn with
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

let updateGenNet (genNetIn: GeneralNet) newMap =
    match genNetIn with
    | (sync, (str, Wire _)) -> 
        sync, (str, Wire newMap)
    | (sync, (str, Bus _)) ->
        sync, (str, Bus newMap)

let updateGenLst genLst newMaps =
    lstOpParallel [] updateGenNet genLst newMaps    

let generateNewMapLst func genNetLstIn genNetLstOut =
    let newLogic = func genNetLstIn 
    let lstOfLengths = findLengths genNetLstOut

    let grpsOfLogic = groupLogic [] newLogic lstOfLengths
    let grpsOfNum = List.map generateList lstOfLengths

    (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip 
    |> List.map Map.ofList

let updateDFF genNetLstIn genNetLstOut =
    let takePrevInputs genNetLstIn = List.collect (extractNet >> extractLogLevel) genNetLstIn 

    let newMapLst = generateNewMapLst takePrevInputs genNetLstIn genNetLstOut
    (genNetLstOut, newMapLst) ||> lstOpParallel [] updateGenNet 


let syncCheck (genNet:GeneralNet) = 
    match genNet with
    | false, _ ->
        false
    | true, _ ->
        true


let initializeSync cLst =

    let updateIfSync (genNet:GeneralNet) = 
        if syncCheck genNet 
        then
            let newMapLen = genNet |> extractNet |> netSize 
            updateGenNet genNet (createNewMap newMapLen)
        else
            genNet

    let setToLow (cIn: Connection) =
        cIn 
        |> extractGenNetLsts 
        |> opOnTuple (List.map updateIfSync)
    List.map setToLow cLst



// how to use memoisation:
// let memoise fn =
//     let mutable cache = Map []
//     fun x -> ...
// let square x = 
//     printfn "square called with x = %A" x
//     x*x
// let mSquare = memoise square



// netLst = list of Nets   do i need this?
// cLst = connection List 
// megaLst = megablock list    type megablock = Name of String   don't need this?
// let advanceState netLst cLst megaLst =