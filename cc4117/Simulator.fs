module Simulator
open Evaluator
open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper




// let initializeSync cLst =
//         let updateIfSync (gNet:GeneralNet) = 
//             if fst gNet 
//             then
//                 let newMapLen = gNet |> extractNet |> netSize 
//                 updateGenNet gNet (createNewMap newMapLen)
//             else
//                 gNet
//         let setToLow (cIn: Connection) =
//             cIn 
//             |> extractGenNetLsts  
//             |> opOnTuple (List.map updateIfSync)
//         List.map setToLow cLst 

// let findAllSync (cLst:Connection List) =
//         let rec findSync acc gLst =
//             match gLst with
//             | hd::tl -> 
//                 if fst hd then 
//                     findSync (acc @ [hd]) tl
//                 else 
//                     findSync acc tl
//             | [] -> List.distinct acc
//         let c (cIn: Connection) =
//             cIn |> extractGenNetLsts |> opOnTuple (findSync []) |> (fun (a, b) -> a @ b)
//         List.collect c cLst


// needed when users define inputs with Map<NetIdentifier, Net>
let mapToGLst (netIDMap: Map<NetIdentifier, Net>) : GeneralNet List=
        Map.toList netIDMap |> List.map (fun ((netID:NetIdentifier), net) -> false, (netID.Name, net))


let getOutputs (cLst: Connection List) = 
    let output (_,_,c) = c
    List.map output cLst


// first take cLst and extract all syncNets as a list and initialize them to Low
let returnSyncNets (cLst: Connection List) = 
    let findAllSync (cLst:Connection List) =
        let rec findSync acc gLst =
            match gLst with
            | hd::tl -> 
                if fst hd then 
                    findSync (acc @ [hd]) tl
                else 
                    findSync acc tl
            | [] -> 
                acc
        let getSyncFromConnection (cIn: Connection) =
            cIn |> extractGenNetLsts |> opOnTuple (findSync []) |> (fun (a, b) -> a @ b)
        List.collect getSyncFromConnection cLst
    
    let initializeSync (gNet: GeneralNet) = 
        let newMapLen = gNet |> extractNet |> netSize 
        updateGenNet gNet (createNewMap newMapLen)
        
    cLst |> findAllSync |> List.distinct |> List.map initializeSync 

// need this function!!!
let gLstToMap (gLst: GeneralNet List) : Map<NetIdentifier, Net> =
    let getSliceIndices netIn: (int * int option) option =
        match netIn with
        | Wire _ 
            -> None 
        | Bus _ 
            -> let sliceWidth = netSize netIn
               if sliceWidth = 1 then Some (0, None) 
               else Some(sliceWidth-1, Some 0)
    let getNetIds gNet = 
        match gNet with
        | _, (str, net) -> 
            {Name=str; SliceIndices=getSliceIndices net}, net
    List.map getNetIds gLst |> Map.ofList


// then use the currentInputs and syncNet list to create an initial map of known values
let getInitMap (currentInputs:GeneralNet list) (syncNets: GeneralNet list) =
    currentInputs @ syncNets |> gLstToMap


let cLstToBlockLst (cLst: Connection List) : Block list =
    List.map (fun (megaBlock, a, b) -> (megaBlock, gLstToMap a, gLstToMap b)) cLst


// seperate sync/async megablocks (DFFCLst, asyncClst)
// all DFFS can be updated by my function, the rest use evaluateWithOutputs
let seperateDFF (lst: (Megablock * _ * _) list) =
    let checkIfDFF (cIn: Megablock * _ * _) =
        let (Name str), _, _ = cIn
        str = "DFF"
    let rec seperate lstA lstB lst =
        match lst with
        | hd::tl ->
            if checkIfDFF hd then
                seperate (lstA @ [hd]) lstB tl
            else
                seperate lstA (lstB @ [hd]) tl
        | [] -> lstA, lstB
    seperate [] [] lst // -> (syncCLst, asyncCLst)


// seperateDFF cLst or bLst -> (syncCLst, asyncCLst) or bLSt 
// plug into advance state... 
let advanceState (currentInputs: GeneralNet list) (syncNets: GeneralNet List) (bLst: Block list) (tLst: TLogic List)  = 
    let knownNets = getInitMap currentInputs syncNets // map<NetIdentifier, Net> -> all info of sync or not is removed

    // check if otherMap is a subset of reference map
    let checkIfInMap (refMap: Map<'T,'b>) (otherMap: Map<'T,'b>) =
        let rec listCompare m lst =
            match lst with 
            | hd::tl when Map.containsKey hd m->
                listCompare m tl
            | [] -> true
            | _ -> false
        otherMap |> Map.toList |> List.map fst |> listCompare refMap
        
    // update map with "otherMap"
    // takes two maps and merges them (map1 will be overwritten by map2 if there is the same key)
    let updateMap (origMap: Map<NetIdentifier,Net>) (otherMap: Map<NetIdentifier,Net>) =
            Seq.fold (fun m (KeyValue(k,v)) -> Map.add k v m) origMap otherMap

    // returns option type 
    let getTLogic (mBlock: Megablock) =
        let (Name str) = mBlock
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        //List.tryFind (checker str) tLst 
        List.find (checker str) tLst
    
    // output of this function will return "mapOfVals"
    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (bLst: Block list) =
        match bLst with 
        | (n, mapIn, _)::tl when checkIfInMap acc mapIn ->
            let outputMap = n |> getTLogic |> evaluateModuleWithInputs mapIn
            let acc' = updateMap acc outputMap
            simulateAsync acc' tl
        | hd::tl ->             
            // if not known, simulate tl @ [hd] else if known simulate tl
            simulateAsync acc (tl @ [hd])
        | [] -> acc
        | _ -> failwithf "nani? how did that happen"

    let mapOfVals = simulateAsync knownNets bLst
    // List.map (printfn "%A") syncNets 
    let rec simulateSync (acc:Map<NetIdentifier,Net>) (bLst: Block list) =
        match bLst with
        | (n, mapIn, mapOut)::tl ->
            let acc' = updateDFF (mapToGLst mapIn) (mapToGLst mapOut) |> gLstToMap
            simulateSync acc' tl
        | [] -> acc 
    simulateSync mapOfVals bLst
