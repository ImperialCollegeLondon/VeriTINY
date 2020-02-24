module Evaluator
open SharedTypes
open ExampleTypes
open Helper

let extractGenNetLsts (cIn: Connection) =
    let _, lstIn, lstOut = cIn
    lstIn, lstOut

let reformatNet (gNet: GeneralNet) =
    match gNet with
    | (sync, (str, net)) -> str, (sync, net)

let extractNetName (genNetIn: GeneralNet) =
    match genNetIn with
    | (_, (str, _)) -> str

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

let updateGenLst genNetLst newMaps =
    lstOpParallel [] updateGenNet genNetLst newMaps    

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

let syncGenNets (cIn:Connection) =
    let pickIf genNet =
        if syncCheck genNet then extractNetName genNet else "hehe"
    cIn |> extractGenNetLsts |> opOnTuple (List.map pickIf)


let getInitMap currentInputs cLst =
    let findAllSync (cLst:Connection List) =
        let rec findSync acc gLst =
            match gLst with
            | hd::tl -> 
                if syncCheck hd then 
                    findSync (acc @ [hd]) tl
                else 
                    findSync acc tl
            | [] -> List.distinct acc
        let c (cIn: Connection) =
            cIn |> extractGenNetLsts |> opOnTuple (findSync []) |> appendTuple
        List.collect c cLst

    currentInputs @ findAllSync cLst |> List.map reformatNet |> Map



// let advanceState (cLst: Connection List) =
//     let memoise fn initMap =
//        let mutable cache = initMap
//        fun x ->
//           match Map.containsKey x cache with
//           | true -> cache.[x] // return cached value
//           | false -> let res = fn x // compute function
//                      cache <- Map.add x res cache //store result in cache
//                      res //   
    


