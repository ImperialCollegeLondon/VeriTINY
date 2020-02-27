module Simulator
open Evaluator
open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open CombEval


// needed when users define inputs with Map<NetIdentifier, Net>
let mapToGLst (netIDMap: Map<NetIdentifier, Net>) : GeneralNet List=
        Map.toList netIDMap 
        |> List.map (fun ((netID:NetIdentifier), net) -> false, (netID.Name, net))


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


let gLstToMap (gLst: GeneralNet List) : Map<NetIdentifier, Net> =
    let gNetToNetID (gNet: GeneralNet) =
        let _, (netName, net) = gNet
    
        let sliceIndices = 
            match net with
            |Bus busMap ->
                let indexList =
                    busMap
                    |> Map.toList
                    |> List.map fst
                match List.length indexList with
                |0 -> failwith "Expecting net logic level map, got empty map"
                |1 -> Some(List.head indexList, None)
                |_ -> Some(List.max indexList, Some (List.min indexList))            
            |Wire _ -> None
        {Name = netName; SliceIndices = sliceIndices}, net
    List.map gNetToNetID gLst |> Map.ofList


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
        otherMap 
        |> Map.toList 
        |> List.map fst 
        |> listCompare refMap

    let takeFromMap (acc:Map<NetIdentifier,Net>) (mapIn:Map<NetIdentifier,Net>) =
        let netIds = mapIn |> Map.toList |> List.map fst
        let netList= List.map (fun x -> acc.[x]) netIds
        List.zip netIds netList |> Map.ofList

    // update map with "otherMap"
    // takes two maps and merges them (map1 will be overwritten by map2 if there is the same key)
    let updateMap (origMap: Map<NetIdentifier,Net>) (otherMap: Map<NetIdentifier,Net>) =
            Seq.fold (fun m (KeyValue(k,v)) -> Map.add k v m) origMap otherMap

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
    let rec simulateAsync (acc:Map<NetIdentifier,Net>) (bLst: Block list) =
        match bLst with 
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

    // List.map (printfn "%A") syncNets 
    let rec simulateSync (acc:Map<NetIdentifier,Net>) (bLst: Block list) =
        match bLst with
        | (n, mapIn, mapOut)::tl ->
            let acc' = updateDFF (mapToGLst mapIn) (mapToGLst mapOut) |> gLstToMap
            simulateSync acc' tl
        | [] -> acc 
    //simulateSync mapOfVals bLst
    let mapOfVals = simulateAsync knownNets bLst
    mapOfVals


    


