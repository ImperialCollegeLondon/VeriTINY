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

    List.fold (fun concatenatedLLs inpNetID ->
        let fullNet = allNets.[getNetByName inpNetID.Name allNets]
        let sliceLst = 
            match inpNetID.SliceIndices with
            |Some (x, Some y) -> getSlice fullNet (min x y, max x y)
            |Some (x, None) -> getSlice fullNet (x, x)
            |None -> extractLLMap fullNet |> Map.toList |> List.sortBy fst |> List.map (snd >> extractLogicLevel)
            )