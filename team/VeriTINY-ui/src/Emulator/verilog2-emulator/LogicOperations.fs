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

let XOROp logicLvl1 logicLvl2 =
    if (logicLvl1 = High && logicLvl2 = Low) || (logicLvl1 = Low && logicLvl2 = High) then High else Low

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

let XOROpNet (net1: EvalNet, net1StartIndex: int) (net2: EvalNet, net2StartIndex : int)  =
    apply2OpToEvalNets XOROp (net1, net1StartIndex) (net2, net2StartIndex)

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

let ConcatOpNet (inpLst: NetIdentifier list) (allNets: Map<NetIdentifier, EvalNet>) = 
    let revInpLst = List.rev inpLst

    let concatenateInpSlice result (inpNetID: NetIdentifier) =
        let fullNet = allNets.[getNetByName inpNetID.Name allNets]
        let sliceLst = 
            match inpNetID.SliceIndices with
            |Some (x, Some y) -> getSliceAsLst fullNet (min x y, max x y)
            |Some (x, None) -> getSliceAsLst fullNet (x, x)
            |None -> getSliceAsLst fullNet (getNetEdgeIndices (extractLLMap fullNet))
        List.append result sliceLst

    let concatenatedLst = List.fold concatenateInpSlice  [] revInpLst
    
    concatenatedLst |> List.mapi (fun i el -> (i, Some el)) |> Map

let reverseConcat (concatenatedNet: Map<int, LogicLevel option>) (cocatenationInputs: NetIdentifier list) (allNets: Map<NetIdentifier, EvalNet>) =
    let concatenatedLLLst = concatenatedNet |> LLOptMapToLLList

    let revInpLst = List.rev cocatenationInputs

    let applyConcatenationToAllNets (updatedAllNets, remainingConcatenation) (inpID: NetIdentifier) =
        let fullNetID = getNetByName inpID.Name allNets
        let fullNet = extractLLMap allNets.[fullNetID]
        let sliceSize, updatedEvalNet = 
            match inpID.SliceIndices with
            |Some (x, Some y) -> 
                let sliceSize = getBusSize inpID
                let updatedEvalNet = updateBus fullNet (Some(min x y, max x y)) (List.take sliceSize remainingConcatenation) |> EvalBus
                sliceSize, updatedEvalNet
            |Some (x, None) -> 
                let sliceSize = getBusSize inpID
                let updatedEvalNet = updateBus fullNet (Some(x, x)) (List.take sliceSize remainingConcatenation) |> EvalBus
                sliceSize, updatedEvalNet
            |None -> 
                let sliceSize = getBusSize fullNetID
                let updatedNetMap = updateBus fullNet None (List.take sliceSize remainingConcatenation) 
                let updatedEvalNet =
                    match fullNetID.SliceIndices with
                    |Some(_, Some _) -> EvalBus updatedNetMap
                    |None -> EvalWire updatedNetMap
                    |_ -> failwithf "Expected bus definition slice indices, got %A" fullNetID.SliceIndices      
                sliceSize, updatedEvalNet          
        Map.add fullNetID updatedEvalNet updatedAllNets, List.skip sliceSize remainingConcatenation

    fst (List.fold applyConcatenationToAllNets (allNets, concatenatedLLLst) revInpLst)