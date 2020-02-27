module Simulator
open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open Evaluator
open CombEval



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


// then use the currentInputs and syncNet list to create an initial map of known values
let getInitMap (currentInputs:Map<NetIdentifier,Net>) (syncNetMap:Map<NetIdentifier,Net>) =
    updateMap currentInputs syncNetMap


let cLstToBlockLst (cLst: Connection List) : Block list =
    List.map (fun (megaBlock, a, b) -> (megaBlock, gLstToMap a, gLstToMap b)) cLst


// seperate sync/async megablocks (DFFCLst, asyncClst)
// all DFFS can be updated by my function, the rest use evaluateWithOutputs
let seperateMegaBlocks (lst: (Megablock * _ * _) list) =
    let checkIfSyncBlock (cIn: Megablock * _ * _) =
        let (megaBlock), _, _ = cIn
        List.contains megaBlock syncMegaLst
    let rec seperate lstA lstB lst =
        match lst with
        | hd::tl ->
            if checkIfSyncBlock hd then
                seperate (lstA @ [hd]) lstB tl
            else
                seperate lstA (lstB @ [hd]) tl
        | [] -> lstA, lstB
    seperate [] [] lst // -> (syncBLst, asyncBLst)


// seperateDFF cLst or bLst -> (syncCLst, asyncCLst) or bLSt 
// plug into advance state... 
let advanceState (initMap: Map<NetIdentifier,Net>) (asyncBLst: Block list) (syncBLst: Block list) (tLst: TLogic List) = // map<NetIdentifier, Net> -> all info of sync or not is removed

    // check if otherMap is a subset of reference map
    let checkIfInMap (refMap: Map<'T,'b>) (otherMap: Map<'T,'b>) =
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

    let takeFromMap (acc:Map<NetIdentifier,Net>) (mapIn:Map<NetIdentifier,Net>) =
        let netIds = mapIn |> Map.toList |> List.map fst
        let netList= List.map (fun x -> acc.[x]) netIds
        List.zip netIds netList |> Map.ofList


    // returns option type 
    let getTLogic (mBlock: Megablock) (tLst:TLogic List)=
        let (Name str) = mBlock
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        //List.tryFind (checker str) tLst 
        List.find (checker str) tLst

    let getOutput (mapIn:Map<NetIdentifier,Net>) (origNames:Map<NetIdentifier,Net>) (tLog:TLogic) =
        let renameForEval (mapIn:Map<NetIdentifier,Net>)  =
        //type Expression = (Operator * NetIdentifier list * NetIdentifier list)
            let nets = mapIn |> Map.toList |> List.map snd
            let tempKeys = tLog.Inputs 
            List.zip tempKeys nets |> Map.ofList

        let renameEvalOut (origNames:Map<NetIdentifier,Net>) (evalMap:Map<NetIdentifier,Net>)   =
            let nets = evalMap |> Map.toList |> List.map snd
            let realKeys = origNames |> Map.toList |> List.map fst
            List.zip realKeys nets |> Map.ofList

        mapIn |> renameForEval |> evaluateModuleWithInputs tLog |> renameEvalOut origNames
    
    // output of this function will return "mapOfVals"
    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (asyncBLst: Block list) =
        match asyncBLst with 
        | (n, mapIn, mapOut)::tl when checkIfInMap acc mapIn ->
            let tLog = getTLogic n tLst
            let mapIn' = takeFromMap acc mapIn
            let outputMap = getOutput mapIn' mapOut tLog
            let acc' = updateMap acc outputMap
            printfn "yeaaah i found it man, what's next"
            simulateAsync acc' tl
        | hd::tl ->             
            // if not known, simulate tl @ [hd] else if known simulate tl
            printfn "huh? not here brahkihgu"
            simulateAsync acc (tl @ [hd])
        | [] -> 
            printfn "omg... it worked im so emotional"
            acc
        | _ -> failwithf "nani? how did that happen"

    let rec simulateSync (acc:Map<NetIdentifier,Net>) (refMap:Map<NetIdentifier,Net>) (syncBLst: Block list) =
        match syncBLst with
        | (n, mapIn, mapOut)::tl when List.contains n syncMegaLst->
            //let acc' = updateDFF mapIn mapOut
            let mapIn' = takeFromMap refMap mapIn
            let outputMap = updateDFF mapIn' mapOut
            let acc' = updateMap acc outputMap
            simulateSync acc' refMap tl
        | [] -> acc 
        | _ -> failwithf "other megablocks not supported yet"

    let mapOfVals = simulateAsync initMap asyncBLst
    let finalVals = simulateSync (Map []) mapOfVals syncBLst
    finalVals

let simulate (lstOfInputs: GeneralNet List list) (cLst:Connection list) (tLst: TLogic list)=
    // initialize/setup
    let initialSyncNetMap = returnSyncNets cLst |> gLstToMap
    let bLst = cLstToBlockLst cLst
    let (syncBLst, asyncBLst) = seperateMegaBlocks bLst

    // keep advancing state until the lst of known inputs are exhausted
    let rec advanceMore prevState (lstOfInputs: GeneralNet list list) =
        match lstOfInputs with
        | currentInputs::tl -> 
            let initMap = getInitMap (gLstToMap currentInputs) prevState
            let nextState = advanceState initMap asyncBLst syncBLst tLst
            advanceMore nextState tl 
        | [] -> prevState

    advanceMore initialSyncNetMap lstOfInputs
    
        


