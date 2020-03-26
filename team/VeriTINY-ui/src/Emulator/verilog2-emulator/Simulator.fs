module Simulator
open SharedTypes
open EvalNetHelper
open Helper
open SynchronousBlocks
open CombEval


/// Advance to next clock cycle, return synchronous states
let advanceState (initMap: Map<NetIdentifier,Net>) (asyncCLst: Connection list) (syncCLst: Connection list) (tLst: TLogic List) = 

    let rec checkIfNetsAreKnown (refMap: Map<NetIdentifier, Net>) (lst: GeneralNet List) =
        match lst with
        | hd::tl when Map.containsKey (gNetToNetID hd |> fst) refMap ->
            checkIfNetsAreKnown refMap tl
        | [] -> true
        | _ -> false

    // return map with keys according to input list and values from refmap 
    let takeFromMap (refMap:Map<NetIdentifier,Net>) (lst: GeneralNet list) =
        let netIds = List.map (gNetToNetID >> fst) lst
        let netList = List.map (fun x -> refMap.[x]) netIds
        List.zip netIds netList |> Map.ofList

    // take nets from map retaining order from gNetLst
    let takeNetsFromMap (refMap:Map<NetIdentifier,Net>) (lst:GeneralNet list): Net list =
        let netIds = List.map (gNetToNetID >> fst) lst
        let netList = List.map (fun x -> refMap.[x]) netIds
        netList 
    
    let getTLogicByName (tLst:TLogic List) (mBlock: Megablock) =
        let (Name str) = mBlock
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        List.find (checker str) tLst

    let evaluateTLogic (refMap:Map<NetIdentifier,Net>) (lstIn: GeneralNet list) (lstOut: GeneralNet list) (tLog:TLogic) =
        
        let newGNet (name:string) (net:Net) =
            false, (name, net) 

        let rec makeGNetLst acc (nameLst: string list) (netLst: Net list): GeneralNet list =
            match nameLst, netLst with
            | nameHd::nameTl, netHd::netTl ->
                let gNet = newGNet nameHd netHd
                makeGNetLst ([gNet] @ acc) nameTl netTl
            | [], [] -> 
                acc
            | _ -> 
                failwithf "Length of name list did not match length of net list"

        let reorderOutputs (outputMap: Map<NetIdentifier,Net>) (orderedOutputNames: string list): Net list = 
            List.map (fun x -> 
                               let netId = getNetByName x outputMap
                               outputMap.[netId]) orderedOutputNames


        // names ordered according to tLogic
        let inputNamesFromTLog = tLog.Inputs |> List.map (fun x -> x.Name)
        let outputNamesFromTLog = tLog.Outputs |> List.map (fun x -> x.Name)

        // nets ordered from connection 
        let inputNetLst = takeNetsFromMap refMap lstIn
        let outputNetLst = List.map extractNet lstOut
        let originalOutputNames = List.map gNetName lstOut

        // make maps for evaluation
        let mapForEval = makeGNetLst [] inputNamesFromTLog inputNetLst |> gLstToMap
        let defaultOutput = makeGNetLst [] outputNamesFromTLog outputNetLst |> gLstToMap

        let result = evaluateModuleWithInputs tLog mapForEval defaultOutput
        let outputNets = reorderOutputs result outputNamesFromTLog
       
        makeGNetLst [] originalOutputNames outputNets |> gLstToMap

    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (asyncCLst: Connection list) =
        match asyncCLst with 
        | (mBlock, lstIn, lstOut)::rest when checkIfNetsAreKnown acc lstIn ->
            let acc' = mBlock |> getTLogicByName tLst |> evaluateTLogic acc lstIn lstOut |> updateMap acc
            simulateAsync acc' rest
        | hd::rest ->             
            simulateAsync acc (rest @ [hd])
        | [] -> 
            acc
        | _ -> failwithf "Shouldn't happen"

    let rec simulateSync (acc:Map<NetIdentifier,Net>) (refMap:Map<NetIdentifier,Net>) (syncCLst: Connection list) =
        match syncCLst with
        | (mBlock, lstIn, lstOut)::rest when List.contains mBlock syncMegaBlockLst->
            // takes values from refMap using keys of mapIn
            let mapIn' = takeFromMap refMap lstIn
            let acc' = (mapIn', gLstToMap lstOut) ||> evaluateSyncBlock mBlock  |> updateMap acc
            simulateSync acc' refMap rest
        | [] -> acc 
        | _ -> failwithf "other megablocks not supported yet"

    let mapOfVals = simulateAsync initMap asyncCLst
    //printfn "States after asynchronous evaluation: \n %A" mapOfVals
    let nextState = simulateSync (Map []) mapOfVals syncCLst

    //printfn "Synchronous states after synchronous evaluation: \n %A" nextState
    mapOfVals, nextState

/// Extract synchronous nets from cLst and initialize them to Low
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
            cIn 
            |> extractGenNetLsts 
            |> opOnTuple (findSync []) 
            |> (fun (a, b) -> a @ b)
        List.collect getSyncFromConnection cLst
    
    let initializeSync (gNet: GeneralNet) = 
        let newMapLen = gNet |> extractNet |> netSize 
        updateGenNet gNet (createNewMap newMapLen)
        
    cLst 
    |> findAllSync 
    |> List.distinct 
    |> List.map initializeSync 

// Seperate sync/async megablocks 
let seperateMegaBlocks (cLst) =
    let checkIfSyncBlock (cIn: Connection) =
        let (megaBlock), _, _ = cIn
        List.contains megaBlock syncMegaBlockLst
    let rec seperate lstA lstB lst =
        match lst with
        | hd::tl ->
            if checkIfSyncBlock hd then
                seperate (lstA @ [hd]) lstB tl
            else
                seperate lstA (lstB @ [hd]) tl
        | [] -> lstA, lstB
    seperate [] [] cLst // -> (syncCLst, asyncCLst)


let setupSimulation (cLst:Connection list) =
    let initialSyncNetMap = returnSyncNets cLst |> gLstToMap
    initialSyncNetMap, seperateMegaBlocks cLst


//additional top level function for iterating through inputs one by one
let iterateState syncState (inpLst: GeneralNet list) (asyncCLst: Connection list) (syncCLst: Connection list)  (tLst: TLogic list) =
    let initMap = updateMap (gLstToMap inpLst) syncState
    advanceState initMap asyncCLst syncCLst tLst

let simulateInputList (lstOfInputs: GeneralNet List list) (cLst:Connection list) (tLst: TLogic list)=
   
    let initialSyncNetMap, (syncCLst, asyncCLst) = setupSimulation cLst
    // Keep advancing state until the lst of inputs are exhausted
    let rec advanceMore currState syncState (lstOfInputs: GeneralNet list list) =
        match lstOfInputs with
        | currentInputs::rest -> 
            let currState', syncState' = iterateState syncState currentInputs asyncCLst syncCLst tLst
            advanceMore currState' syncState' rest
        | [] -> currState, syncState

    advanceMore (Map []) initialSyncNetMap lstOfInputs