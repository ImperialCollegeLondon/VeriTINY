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

let ConcatOpNet (inpLst: NetIdentifier list) (allNets: Map<NetIdentifier, EvalNet>) = 
    let revInpLst = List.rev inpLst

    let concatenatedLst = 
        List.fold (fun concatenatedLLs (inpNetID: NetIdentifier) ->
            let fullNet = allNets.[getNetByName inpNetID.Name allNets]
            let sliceLst = 
                match inpNetID.SliceIndices with
                |Some (x, Some y) -> getSliceAsLst fullNet (min x y, max x y)
                |Some (x, None) -> getSliceAsLst fullNet (x, x)
                |None -> getSliceAsLst fullNet (getNetEdgeIndices (extractLLMap fullNet))
            List.append concatenatedLLs sliceLst
                ) [] revInpLst
    
    concatenatedLst |> List.mapi (fun i el -> (i, Some el)) |> Map

let reverseConcat (concatenatedNet: EvalNet) (cocatenationInputs: NetIdentifier list) (allNets: Map<NetIdentifier, EvalNet>) =
    let concatenatedLLLst = concatenatedNet |> extractLLMap |> LLOptMapToLLList

    let revInpLst = List.rev cocatenationInputs

    fst (List.fold (fun (updatedAllNets, remainingConcatenation) (inpID: NetIdentifier) ->
            let fullNetID = getNetByName inpID.Name allNets
            let fullNet = extractLLMap allNets.[fullNetID]
            match inpID.SliceIndices with
            |Some (x, Some y) -> 
                let sliceSize = getBusSize inpID
                let updatedEvalNet = updateBus fullNet (Some(min x y, max x y)) (List.take sliceSize remainingConcatenation) |> EvalBus
                Map.add fullNetID updatedEvalNet updatedAllNets, List.skip sliceSize remainingConcatenation
            |Some (x, None) -> 
                let sliceSize = getBusSize inpID
                let updatedEvalNet = updateBus fullNet (Some(x, x)) (List.take sliceSize remainingConcatenation) |> EvalBus
                Map.add fullNetID updatedEvalNet updatedAllNets, List.skip sliceSize remainingConcatenation
            |None -> 
                let sliceSize = getBusSize fullNetID
                let updatedNetMap = updateBus fullNet None (List.take sliceSize remainingConcatenation) 
                let updatedEvalNet =
                    match fullNetID.SliceIndices with
                    |Some(_, Some _) -> EvalBus updatedNetMap
                    |None -> EvalWire updatedNetMap
                    |_ -> failwithf "Expected bus definition slice indices, got %A" fullNetID.SliceIndices
                
                Map.add fullNetID updatedEvalNet updatedAllNets, (List.skip sliceSize remainingConcatenation)) (allNets, concatenatedLLLst) revInpLst)