module LogicOperations
open SharedTypes
open EvalTypes
open EvalNetHelper

let ANDOp logicLvl1 logicLvl2 = 
    if logicLvl1 = High then logicLvl2 else Low

let OROp logicLvl1 logicLvl2 =
    if logicLvl1 = High then High else logicLvl2

let NOTOp logicLvl = 
    if logicLvl = High then Low else High


//assumes little endian - bus1 determines output bus size, therefore when calling any derivatives of this function, use bus1 to give output size
let apply2OpToEvalNets op (net1: EvalNet, net1StartIndex: int) (net2: EvalNet, net2StartIndex : int) = 
    let LLMap1 = extractLLMap net1
    let LLMap2 = extractLLMap net2
    let getCorrespondingNet2Index net1Index = net2StartIndex + net1Index - net1StartIndex
    Map.toList LLMap1
    |> List.map(fun (mapIndex, logicLvlOpt) ->
        let logicLvl1 = extractLogicLevel logicLvlOpt
        let logicLvl2 = extractLogicLevel LLMap2.[getCorrespondingNet2Index mapIndex]
        mapIndex - net1StartIndex, Some (op logicLvl1 logicLvl2))
    |> Map

let ANDOpNet (net1: EvalNet, net1StartIndex: int) (net2: EvalNet, net2StartIndex : int)  =
    apply2OpToEvalNets ANDOp (net1, net1StartIndex) (net2, net2StartIndex)

let OROpNet(net1: EvalNet, net1StartIndex: int) (net2: EvalNet, net2StartIndex : int)  =
    apply2OpToEvalNets OROp (net1, net1StartIndex) (net2, net2StartIndex)


let NOTOpNet (net:EvalNet) (netStartIndex: int) (outputBusLength: int) = 
    let applyNotOpToLLOpt logicLevelOpt = 
        logicLevelOpt
        |> extractLogicLevel
        |> NOTOp
        |> Some

    let LLMap = extractLLMap net
    [0.. (outputBusLength-1)]
    |> List.map (fun x -> x, applyNotOpToLLOpt LLMap.[netStartIndex + x])
    |> Map

let PassOpNet (net:EvalNet) (netStartIndex: int) (outputBusLength: int) = 
    let LLMap = extractLLMap net 
    [0..(outputBusLength-1)]
    |> List.map (fun x -> x, LLMap.[netStartIndex + x])
    |> Map


let getConcatOutputSize concatNetIDLst inputNetIDLst =
    List.fold (fun totalSize netID -> 
    let currNetSize = 
        match netID.SliceIndices with
        |Some (x, Some y) -> abs (x-y) + 1
        |Some (x, None) -> 1
        |None -> getBusSize (List.find ((=) netID) inputNetIDLst)
    
    totalSize + currNetSize) 0 concatNetIDLst


let concatOp (evalNetMap: Map<NetIdentifier, EvalNet>) (inpIDLst: NetIdentifier list) = 
    inpIDLst
    |> List.rev
    |> List.fold (fun (concatenatedLLLst: LogicLevel list) (concatInpID : NetIdentifier)-> 
        let fullNet = evalNetMap.[getNetByName concatInpID.Name evalNetMap]
        let sliceLst = 
           match concatInpID.SliceIndices with
            |Some (x, Some y) -> getNetSliceAsList (Some(min x y, max x y)) fullNet
            |Some (x, None) -> getNetSliceAsList (Some(x, x)) fullNet
            |None -> getNetSliceAsList None fullNet
        List.append concatenatedLLLst sliceLst
        ) [] 
    |> List.map (fun logicLvl -> Some logicLvl)
    |> List.mapi (fun i logicLvlOpt -> (i,logicLvlOpt))
    |> Map


let reverseConcat (concatenatedNet: EvalNet) (concatenationInputs: NetIdentifier list) (evalNetMap: Map<NetIdentifier, EvalNet>) =
    let concatenatedLLLst = 
        extractLLMap concatenatedNet
        |> Map.toList
        |> List.sortBy fst
        |> List.map (snd >> extractLogicLevel)

    let revConcatInputs = List.rev concatenationInputs 

    let _, updatedEvalNetMap = 
        List.fold (fun (remainingLLLst, modifiedEvalNetMap) (concatInpID: NetIdentifier) ->
            let inpNetID = getNetByName concatInpID.Name evalNetMap
            let inpLLMap = extractLLMap evalNetMap.[inpNetID]
            let updatedInpLLMap, remainder =
                match concatInpID.SliceIndices with
                |Some (x, Some y) -> updateBus inpLLMap (Some(min x y, max x y)) (List.take (getBusSize concatInpID) remainingLLLst), List.skip (getBusSize concatInpID) remainingLLLst
                |Some (x, None) -> updateBus inpLLMap (Some (x,x)) [List.head remainingLLLst], List.tail remainingLLLst 
                |None -> updateBus inpLLMap None (List.take (getBusSize inpNetID) remainingLLLst), List.skip (getBusSize inpNetID) remainingLLLst
            
            let updatedEvalNet = 
                match evalNetMap.[inpNetID] with
                |EvalBus _ -> EvalBus updatedInpLLMap
                |EvalWire _ -> EvalWire updatedInpLLMap

            remainder, Map.add inpNetID updatedEvalNet modifiedEvalNetMap   
            ) (concatenatedLLLst, evalNetMap) revConcatInputs

    updatedEvalNetMap
        
