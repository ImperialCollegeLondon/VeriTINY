module EvalNetHelper
open SharedTypes
open EvalTypes

let createNewBus (a, b) = 
        [a..b]
        |> List.map (fun x -> (x, None)) 
        |> Map
        |> EvalBus

let padLogicLvlListToLength logicLvlLst fullLength =

    if List.length logicLvlLst >= fullLength
    then logicLvlLst
    else
        [1..(fullLength - (List.length logicLvlLst))]
        |> List.map (fun _ -> Low)
        |> List.append logicLvlLst


//caller can supply None for slice indicies to update whole bus
let updateBus bus sliceIndices newLogicLevels = 
    let (a,b) = 
        match sliceIndices with
        |Some (x,y) -> (x,y)
        |None -> 
            let wireIndices = 
                Map.toList bus
                |> List.map (fst)
            (List.min wireIndices, List.max wireIndices)

    if abs(b - a + 1) <> (List.length newLogicLevels) || not (Map.containsKey b bus) || not (Map.containsKey a bus)
    then failwith "Cannot update bus, error in given bus slice"
    else
        bus
        |> Map.map (fun key oldVal ->
            if key >= a && key <= b
            then Some newLogicLevels.[key - a]
            else oldVal
            )

let updateWire wire newLogicLevel = Map.add 0 (Some newLogicLevel) wire

let rec uintToLogicLevelList uint logicLvlLst =
    if uint = 0
    then logicLvlLst
    else
        if (uint % 2) = 1
        then uintToLogicLevelList (uint / 2) (logicLvlLst @ [High])
        else uintToLogicLevelList (uint / 2) (logicLvlLst @ [Low])

let logicLevelsToUint logicLvlLst = 
    let getDecimalValue bitPos bit = 
        if bit = High
        then int ((float 2) ** (float bitPos))
        else 0
    List.mapi getDecimalValue logicLvlLst
    |> List.reduce (+)