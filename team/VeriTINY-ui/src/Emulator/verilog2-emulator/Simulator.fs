module Simulator
open SharedTypes
open EvalNetHelper
open Helper
open SynchronousBlocks
open CombEval


/// Advance to next clock cycle, return synchronous states
let advanceState (initMap: Map<NetIdentifier,Net>) (asyncCLst: SimBlock list) (syncCLst: SimBlock list) (tLst: TLogic List) = 

    let rec checkIfNetsAreKnown (refMap: Map<NetIdentifier, Net>) (lst: SimNetInfo List) =
        match lst with
        | hd::tl when Map.containsKey hd.ID refMap ->
            checkIfNetsAreKnown refMap tl
        | [] -> true
        | _ -> false

    // return map with keys according to input list and values from refmap 
    let takeFromMap (refMap:Map<NetIdentifier,Net>) (lst: SimNetInfo list) =
        let netIds = List.map extractNetIDFromInfo lst
        let netList = List.map (fun x -> refMap.[x]) netIds
        List.zip netIds netList |> Map.ofList

    // take nets from map retaining order from gNetLst
    let takeNetsFromMap (refMap:Map<NetIdentifier,Net>) (lst:SimNetInfo list): Net list =
        let netIds = List.map extractNetIDFromInfo lst
        let netList = List.map (fun x -> refMap.[x]) netIds
        netList 
    
    let getTLogicByName (tLst:TLogic List) (mBlock: MegablockType) =
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        List.find (checker mBlock) tLst

    let evaluateTLogic (refMap:Map<NetIdentifier,Net>) (simBlock:SimBlock) (tLog:TLogic): Map<NetIdentifier, Net> =
        
        let reorderOutputs (outputMap: Map<NetIdentifier,Net>) (orderedOutputNames: NetIdentifier list): Net list = 
            List.map (fun x -> outputMap.[x]) orderedOutputNames

        // names ordered according to tLogic

        // nets ordered from SimBlock 
        let inputNetLst = takeNetsFromMap refMap simBlock.inNets
        let outputNetLst = List.map makeNetFromInfo simBlock.outNets //only works because TLogics are purely combininatorial right now
       
        // make maps for evaluation
        let mapForEval = List.zip tLog.Inputs inputNetLst |> Map
        let defaultOutput = List.zip tLog.Outputs outputNetLst |> Map

        let result = evaluateModuleWithInputs tLog mapForEval defaultOutput
        let outputNets = reorderOutputs result tLog.Outputs

        List.zip (List.map extractNetIDFromInfo simBlock.outNets) outputNets |> Map

        

    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (asyncBLst: SimBlock list) =
        match asyncBLst with 
        | hd::rest when checkIfNetsAreKnown acc hd.inNets ->
            let acc' = hd.MegablockType |> getTLogicByName tLst |> evaluateTLogic acc hd |> updateMap acc
            simulateAsync acc' rest
        | hd::rest ->             
            simulateAsync acc (rest @ [hd])
        | [] -> 
            acc
        | _ -> failwithf "Shouldn't happen"

    let rec simulateSync (acc:Map<NetIdentifier,Net>) (refMap:Map<NetIdentifier,Net>) (syncBLst: SimBlock list) =
        match syncBLst with
        | hd::rest when List.contains hd.MegablockType syncMegaBlockLst->
            // takes values from refMap using keys of mapIn
            let mapIn' = takeFromMap refMap hd.inNets
            let acc' = (mapIn', hd.outNets) ||> evaluateSyncBlock hd.MegablockType  |> updateMap acc
            simulateSync acc' refMap rest
        | [] -> acc 
        | _ -> failwithf "other megablocks not supported yet"

    let mapOfVals = simulateAsync initMap asyncCLst
    //printfn "States after asynchronous evaluation: \n %A" mapOfVals
    let nextState = simulateSync (Map []) mapOfVals syncCLst

    //printfn "Synchronous states after synchronous evaluation: \n %A" nextState
    mapOfVals, nextState

/// Extract synchronous nets from cLst and initialize them to Low
let returnSyncNets (bLst: SimBlock List) = 
    
    let findAllSync (bLst:SimBlock List) =
        List.fold (fun clockedNetInfos block ->
            let getClockedNetInfos netInfos = List.filter (fun netInfo -> netInfo.isClocked) netInfos
            let clockedInps = getClockedNetInfos block.inNets
            let clockedOuts = getClockedNetInfos block.outNets
            clockedNetInfos @ clockedInps @ clockedOuts
            ) [] bLst

    bLst 
    |> findAllSync 
    |> List.distinct 

// Seperate sync/async megablocks 
let seperateMegaBlocks (bLst) =
    let checkIfSyncBlock (bIn: SimBlock) =
        List.contains bIn.MegablockType syncMegaBlockLst
    let rec seperate lstA lstB lst =
        match lst with
        | hd::tl ->
            if checkIfSyncBlock hd then
                seperate (lstA @ [hd]) lstB tl
            else
                seperate lstA (lstB @ [hd]) tl
        | [] -> lstA, lstB
    seperate [] [] bLst // -> (syncBLst, asyncBLst)


let setupSimulation (bLst:SimBlock list) =
    let initialSyncNetInfoLst = returnSyncNets bLst
    let initialSyncNetMap = 
        List.map (fun netInfo -> 
        let netMap = makeNetFromInfo netInfo
        let netID = extractNetIDFromInfo netInfo
        (netID, netMap)) initialSyncNetInfoLst |> Map
    
    initialSyncNetMap, seperateMegaBlocks bLst


//additional top level function for iterating through inputs one by one
let iterateState syncState (inpMap: Map<NetIdentifier, Net>) (asyncCLst: SimBlock list) (syncCLst: SimBlock list)  (tLst: TLogic list) =
    let initMap = updateMap inpMap syncState
    advanceState initMap asyncCLst syncCLst tLst

let simulateInputList (lstOfInputs: Map<NetIdentifier, Net> list) (cLst:SimBlock list) (tLst: TLogic list)=
   
    let initialSyncNetMap, (syncCLst, asyncCLst) = setupSimulation cLst
    // Keep advancing state until the lst of inputs are exhausted
    let rec advanceMore currState syncState (lstOfInputs: Map<NetIdentifier, Net> list) =
        match lstOfInputs with
        | currentInputs::rest -> 
            let currState', syncState' = iterateState syncState currentInputs asyncCLst syncCLst tLst
            advanceMore currState' syncState' rest
        | [] -> currState, syncState

    advanceMore (Map []) initialSyncNetMap lstOfInputs