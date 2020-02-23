module LogicOperations
open SharedTypes

let ANDOp logicLvl1 logicLvl2 = 
    if logicLvl1 = High then logicLvl2 else Low

let OROp logicLvl1 logicLvl2 =
    if logicLvl1 = High then High else logicLvl2

let NOTOp logicLvl = 
    if logicLvl = High then Low else High

let extractLogicLevel logicLvlOpt = 
     match logicLvlOpt with
        |Some logicLvl -> logicLvl
        |None -> failwith "Logic Level has not been assigned yet"

//assumes little endian - bus1 determines output bus size, therefore when calling any derivatives of this function, use bus1 to give output size
let apply2OpToBus op (bus1: Map<int,LogicLevel option>, bus1StartIndex: int) (bus2: Map<int,LogicLevel option>, bus2StartIndex : int) = 
    let getCorrespondingNet2Index net1Index = bus2StartIndex + net1Index - bus1StartIndex
    Map.toList bus1
    |> List.map(fun (mapIndex, logicLvlOpt) ->
        let logicLvl1 = extractLogicLevel logicLvlOpt
        let logicLvl2 = extractLogicLevel bus2.[getCorrespondingNet2Index mapIndex]
        mapIndex - bus1StartIndex, Some (op logicLvl1 logicLvl2))
    |> Map

let ANDOpBus (bus1: Map<int,LogicLevel option>, bus1StartIndex: int) (bus2: Map<int,LogicLevel option>, bus2StartIndex : int) =
    apply2OpToBus ANDOp (bus1, bus1StartIndex) (bus2, bus2StartIndex)

let OROpBus (bus1: Map<int,LogicLevel option>, bus1StartIndex: int) (bus2: Map<int,LogicLevel option>, bus2StartIndex : int) =
    apply2OpToBus OROp (bus1, bus1StartIndex) (bus2, bus2StartIndex)


let NOTOpBus (bus: Map<int, LogicLevel option>) (busStartIndex: int) (outputBusLength: int) = 
    let applyNotOpToLLOpt logicLevelOpt = 
        logicLevelOpt
        |> extractLogicLevel
        |> NOTOp
        |> Some

    [0.. (outputBusLength-1)]
    |> List.map (fun x -> x, applyNotOpToLLOpt bus.[busStartIndex + x])
    |> Map

let PassOpBus (bus: Map<int, LogicLevel option>) (busStartIndex: int) (outputBusLength: int) = 
    [0..(outputBusLength-1)]
    |> List.map (fun x -> x, bus.[busStartIndex + x])
    |> Map
