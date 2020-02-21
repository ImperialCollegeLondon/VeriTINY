module Evaluator
open SharedTypes
open ExampleTypes


// GeneralNet = bool * NamedNet
// NamedNet = string * Net
// Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>

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

/// find length of output nets from genNetOut list 
let findLength (lst: GeneralNet list) =
    List.map (extractNet >> netSize) lst

// tail recursion
// generate list of lists of LogicLevel of appropriate size for output
let rec splitLst acc (logLst:LogicLevel list) (lstOfLengths: int list) =
    match lstOfLengths with
    | hd::tl ->
        let acc', logLst' = List.splitAt hd logLst
        splitLst (acc @ [acc']) logLst' tl
    | [] -> 
        acc

let splitTest = splitLst [] [High;Low;High;Low;High;Low;High;Low] [1;3;3;1]

// rebuild GeneralNet with newMap
let updateNet (genNetIn: GeneralNet) newMap =
    match genNetIn with
    | (sync, (str, Wire _)) -> 
        sync, (str, Wire newMap)
    | (sync, (str, Bus _)) ->
        sync, (str, Bus newMap)

/// operate on two lists 
let rec lstOpParallel acc f lstA lstB =
        match lstA, lstB with
        | h1::t1, h2::t2 ->
            let acc' = f h1 h2
            lstOpParallel (acc @ [acc']) f t1 t2
        | _ -> 
            acc

// will need to take megablock, function to evaluate outputs
// also need genNetLstIn to evaluate, and genNetLstOut to know what map to create
// currently just takes input and passes to output (DFF functionality)
let generateNewMap genNetLstIn genNetLstOut =
    let newLog = List.collect (extractNet >> extractLogLevel) genNetLstIn 
    let lstOfLengths = findLength genNetLstOut

    let grpLog = splitLst [] newLog lstOfLengths
    let generateList n = [0..n-1]
    let grpNum = List.map generateList lstOfLengths

    let zipOut = lstOpParallel [] List.zip grpNum grpLog
    zipOut |> List.map Map.ofList

let updateDFF genNetLstIn genNetLstOut =
    // generateNewMap will have an input argument that takes in "DFF"
    let newMapLst = generateNewMap genNetLstIn genNetLstOut
    lstOpParallel [] updateNet genNetLstOut newMapLst

let updateDFFTest = updateDFF dffMixedIn dffMixedOut

//given a genNetlist, check if synchronous or not
let syncCheck (genNet:GeneralNet) = 
    match genNet with
    | false, _ ->
        false
    | true, _ ->
        true

let filterNotSync (genNetLst:GeneralNet list) =
    List.filter syncCheck genNetLst

/// initialize synchronous nets (outputs of DFFs) to 0 
let initializeSync (cIn:Connection) =
    // search for all synchronous nets then update them to 0
    let _, genLstIn, genLstOut = cIn
    filterNotSync genLstOut
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


//given Connection list, find all synchronous elements
// let updateDFF (input:Connection) : Connection = 
//     let Name block, genNetIn, genNetOut = input
//     let sync, (str, Bus )
//     if block = "DFF" then
//     let indexList = m |> Map.toList |> List.map fst
//     else
//     input