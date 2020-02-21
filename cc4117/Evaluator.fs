module Evaluator
open SharedTypes
open ExampleTypes
open Helper

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

let extractGenNetLstIn (cIn: Connection) =
    let _, genNetLstIn, _ = cIn
    genNetLstIn
  
let extractGenNetLstOut (cIn: Connection) =
    let _, _, genNetLstOut = cIn
    genNetLstOut 

let netSize (netIn: Net): int =
    match netIn with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map fst |> List.length

let findLengths (lst: GeneralNet list) =
    List.map (extractNet >> netSize) lst

// generate list of lists of LogicLevel of appropriate size for output
let rec groupLogic acc (logLst:LogicLevel list) (lstOfLengths: int list) =
    match lstOfLengths with
    | hd::tl ->
        let acc', logLst' = List.splitAt hd logLst
        groupLogic (acc @ [acc']) logLst' tl
    | [] -> 
        acc

let updateNet (genNetIn: GeneralNet) newMap =
    match genNetIn with
    | (sync, (str, Wire _)) -> 
        sync, (str, Wire newMap)
    | (sync, (str, Bus _)) ->
        sync, (str, Bus newMap)

let updateGenLst genLst newMaps =
    lstOpParallel [] updateNet genLst newMaps    

let takePrevInputs genNetLstIn =
    List.collect (extractNet >> extractLogLevel) genNetLstIn 

let generateNewMapLst f genNetLstIn genNetLstOut =
    let newLogic = f genNetLstIn 
    let lstOfLengths = findLengths genNetLstOut

    let grpsOfLogic = groupLogic [] newLogic lstOfLengths
    let generateList n = [0..n-1]
    let grpsOfNum = List.map generateList lstOfLengths

    (grpsOfNum,grpsOfLogic) ||> lstOpParallel [] List.zip 
    |> List.map Map.ofList

let updateDFF genNetLstIn genNetLstOut =
    let newMapLst = generateNewMapLst takePrevInputs genNetLstIn genNetLstOut
    (genNetLstOut, newMapLst) ||> lstOpParallel [] updateNet 


let setToLow (genNetLst:GeneralNet list) =
    let lstOfLengths = findLengths genNetLst
    let newMaps = List.map createNewMap lstOfLengths
    updateGenLst genNetLst newMaps
    

//given a genNet, check if synchronous or not
let syncCheck (genNet:GeneralNet) = 
    match genNet with
    | false, _ ->
        false
    | true, _ ->
        true

// given a list of GenNets, filter out the GenNets that are asynchronous
let removeNotSync (genNetLst:GeneralNet list) =
    List.filter syncCheck genNetLst



let filterSync (cInLst:Connection list) =
    List.map (extractGenNetLstIn >> removeNotSync) cInLst
        
// genNet
// |> extractNet
// |> netSize
// |> createNewMap
/// initialize synchronous nets (outputs of DFFs) to 0 
let initializeSync (connectionLst:Connection List) =
    connectionLst |> List.map extractGenNetLstIn 
    // need to use updateNet, which requires newMap ... 

    

    // search for all synchronous nets then update them to 0
    
    // then set all of the nets to 0 using logicLst of "LOW" length N 
    
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