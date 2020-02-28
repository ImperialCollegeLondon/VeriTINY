module TestFunctions
open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open CombEval
open Simulator
open SynchronousBlocks

// this module contains key functions in the simulation workflow


let evaluateTLogicTest (mapIn:Map<NetIdentifier,Net>) (origNames:Map<NetIdentifier,Net>) (tLog:TLogic) =
    // temporary names of nets for evaluation as they appear in tLogic
    let renameForEval (mapIn:Map<NetIdentifier,Net>)  =
        let nets = mapIn |> Map.toList |> List.map snd
        let tempKeys = tLog.Inputs 
        List.zip tempKeys nets |> Map.ofList
    // rename output to user-defined name after evaluation
    let renameEvalOut (origNames:Map<NetIdentifier,Net>) (evalMap:Map<NetIdentifier,Net>)   =
        let nets = evalMap |> Map.toList |> List.map snd
        let realKeys = origNames |> Map.toList |> List.map fst
        List.zip realKeys nets |> Map.ofList

    mapIn |> renameForEval |> evaluateModuleWithInputs tLog |> renameEvalOut origNames

let seperateMegaBlocksTest (bLst: Block list) =
        let checkIfSyncBlock (bIn: Block) =
            let (megaBlock), _, _ = bIn
            List.contains megaBlock syncMegaLst
        let rec seperate lstA lstB lst =
            match lst with
            | hd::tl ->
                if checkIfSyncBlock hd then
                    seperate (lstA @ [hd]) lstB tl
                else
                    seperate lstA (lstB @ [hd]) tl
            | [] -> lstA, lstB
        seperate [] [] bLst // -> (syncBLst, asyncBLst)


/// evaluate asynchronous circuit
let evaluateAsyncTest (initMap: Map<NetIdentifier,Net>) (asyncBLst: Block list) (tLst: TLogic List) = // map<NetIdentifier, Net> -> all info of sync or not is removed

    // Check if otherMap is a subset of reference map
    let checkIfInMap (refMap: Map<'a,'b>) (otherMap: Map<'a,'b>) =
        let rec listCompare m lst =
            match lst with 
            | hd::tl when Map.containsKey hd m->
                listCompare m tl
            | [] -> true
            | _ -> false
        otherMap 
        |> Map.toList 
        |> List.map fst 
        |> listCompare refMap

    // Take values from refMap given keys of mapIn
    let takeFromMap (refMap:Map<NetIdentifier,Net>) (mapIn:Map<NetIdentifier,Net>) =
        let netIds = mapIn |> Map.toList |> List.map fst
        let netList= List.map (fun x -> refMap.[x]) netIds
        List.zip netIds netList |> Map.ofList
    
    let getTLogicByName (tLst:TLogic List) (mBlock: Megablock) =
        let (Name str) = mBlock
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        //List.tryFind (checker str) tLst 
        List.find (checker str) tLst

    let evaluateTLogic (mapIn:Map<NetIdentifier,Net>) (origNames:Map<NetIdentifier,Net>) (tLog:TLogic) =
        // temporary names of nets for evaluation as they appear in tLogic
        let renameForEval (mapIn:Map<NetIdentifier,Net>)  =
            let nets = mapIn |> Map.toList |> List.map snd
            let tempKeys = tLog.Inputs 
            List.zip tempKeys nets |> Map.ofList
        // rename output to user-defined name after evaluation
        let renameEvalOut (origNames:Map<NetIdentifier,Net>) (evalMap:Map<NetIdentifier,Net>)   =
            let nets = evalMap |> Map.toList |> List.map snd
            let realKeys = origNames |> Map.toList |> List.map fst
            List.zip realKeys nets |> Map.ofList

        mapIn |> renameForEval |> evaluateModuleWithInputs tLog |> renameEvalOut origNames
    

    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (asyncBLst: Block list) =
        match asyncBLst with 
        | (mBlock, mapIn, mapOut)::rest when checkIfInMap acc mapIn ->
            // take values from acc using keys of mapIn
            let mapIn' = takeFromMap acc mapIn
            let acc' = mBlock |> getTLogicByName tLst |> evaluateTLogic mapIn' mapOut |> updateMap acc
            simulateAsync acc' rest
        | hd::rest ->             
            simulateAsync acc (rest @ [hd])
        | [] -> 
            acc
        | _ -> failwithf "Shouldn't happen"


    let mapOfVals = simulateAsync initMap asyncBLst
    //printfn "States after asynchronous evaluation: \n %A" mapOfVals
    mapOfVals


let evaluateSyncTest (initMap: Map<NetIdentifier,Net>) (syncBLst: Block list) = // map<NetIdentifier, Net> -> all info of sync or not is removed

    // Check if otherMap is a subset of reference map
    let checkIfInMap (refMap: Map<'a,'b>) (otherMap: Map<'a,'b>) =
        let rec listCompare m lst =
            match lst with 
            | hd::tl when Map.containsKey hd m->
                listCompare m tl
            | [] -> true
            | _ -> false
        otherMap 
        |> Map.toList 
        |> List.map fst 
        |> listCompare refMap

    // Take values from refMap given keys of mapIn
    let takeFromMap (refMap:Map<NetIdentifier,Net>) (mapIn:Map<NetIdentifier,Net>) =
        let netIds = mapIn |> Map.toList |> List.map fst
        let netList= List.map (fun x -> refMap.[x]) netIds
        List.zip netIds netList |> Map.ofList

    let rec simulateSync (acc:Map<NetIdentifier,Net>) (refMap:Map<NetIdentifier,Net>) (syncBLst: Block list) =
        match syncBLst with
        | (mBlock, mapIn, mapOut)::rest when List.contains mBlock syncMegaLst->
            // takes values from refMap using keys of mapIn
            let mapIn' = takeFromMap refMap mapIn
            let acc' = (mapIn', mapOut) ||> evaluateSyncBlock mBlock  |> updateMap acc
            simulateSync acc' refMap rest
        | [] -> acc 
        | _ -> failwithf "other megablocks not supported yet"

    let nextState = simulateSync (Map []) initMap syncBLst
    // printfn "Synchronous states after synchronous evaluation: \n %A" nextState
    nextState


// returns state after asynchronous evaluation and state of DFFs after synchronous evaluation in a tuple
let advanceStateTest (initMap: Map<NetIdentifier,Net>) (asyncBLst: Block list) (syncBLst: Block list) (tLst: TLogic List) = // map<NetIdentifier, Net> -> all info of sync or not is removed

    // Check if otherMap is a subset of reference map
    let checkIfInMap (refMap: Map<'a,'b>) (otherMap: Map<'a,'b>) =
        let rec listCompare m lst =
            match lst with 
            | hd::tl when Map.containsKey hd m->
                listCompare m tl
            | [] -> true
            | _ -> false
        otherMap 
        |> Map.toList 
        |> List.map fst 
        |> listCompare refMap

    // Take values from refMap given keys of mapIn
    let takeFromMap (refMap:Map<NetIdentifier,Net>) (mapIn:Map<NetIdentifier,Net>) =
        let netIds = mapIn |> Map.toList |> List.map fst
        let netList= List.map (fun x -> refMap.[x]) netIds
        List.zip netIds netList |> Map.ofList
    
    let getTLogicByName (tLst:TLogic List) (mBlock: Megablock) =
        let (Name str) = mBlock
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        //List.tryFind (checker str) tLst 
        List.find (checker str) tLst

    let evaluateTLogic (mapIn:Map<NetIdentifier,Net>) (origNames:Map<NetIdentifier,Net>) (tLog:TLogic) =
        // temporary names of nets for evaluation as they appear in tLogic
        let renameForEval (mapIn:Map<NetIdentifier,Net>)  =
            let nets = mapIn |> Map.toList |> List.map snd
            let tempKeys = tLog.Inputs 
            List.zip tempKeys nets |> Map.ofList
        // rename output to user-defined name after evaluation
        let renameEvalOut (origNames:Map<NetIdentifier,Net>) (evalMap:Map<NetIdentifier,Net>)   =
            let nets = evalMap |> Map.toList |> List.map snd
            let realKeys = origNames |> Map.toList |> List.map fst
            List.zip realKeys nets |> Map.ofList

        mapIn |> renameForEval |> evaluateModuleWithInputs tLog |> renameEvalOut origNames
    

    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (asyncBLst: Block list) =
        match asyncBLst with 
        | (mBlock, mapIn, mapOut)::rest when checkIfInMap acc mapIn ->
            // take values from acc using keys of mapIn
            let mapIn' = takeFromMap acc mapIn
            let acc' = mBlock |> getTLogicByName tLst |> evaluateTLogic mapIn' mapOut |> updateMap acc
            simulateAsync acc' rest
        | hd::rest ->             
            simulateAsync acc (rest @ [hd])
        | [] -> 
            acc
        | _ -> failwithf "Shouldn't happen"

    let rec simulateSync (acc:Map<NetIdentifier,Net>) (refMap:Map<NetIdentifier,Net>) (syncBLst: Block list) =
        match syncBLst with
        | (mBlock, mapIn, mapOut)::rest when List.contains mBlock syncMegaLst->
            // takes values from refMap using keys of mapIn
            let mapIn' = takeFromMap refMap mapIn
            let acc' = (mapIn', mapOut) ||> evaluateSyncBlock mBlock  |> updateMap acc
            simulateSync acc' refMap rest
        | [] -> acc 
        | _ -> failwithf "other megablocks not supported yet"

    let mapOfVals = simulateAsync initMap asyncBLst
    //printfn "States after asynchronous evaluation: \n %A" mapOfVals
    let nextState = simulateSync (Map []) mapOfVals syncBLst
    //printfn "Synchronous states after synchronous evaluation: \n %A" nextState
    mapOfVals, nextState

    