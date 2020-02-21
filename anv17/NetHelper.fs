module NetHelper
open SharedTypes

let createNewBus (a, b) = 
        [a..b]
        |> List.map (fun x -> (x, Low)) 
        |> Map
        |> Bus

let padLogicLvlListToLength logicLvlLst fullLength =

    if List.length logicLvlLst >= fullLength
    then logicLvlLst
    else
        [1..(fullLength - (List.length logicLvlLst))]
        |> List.map (fun _ -> Low)
        |> List.append logicLvlLst


let updateBus bus (a,b) newLogicLevels = 
 
    if (b - a + 1) <> (List.length newLogicLevels) || not (Map.containsKey b bus) || not (Map.containsKey a bus)
    then failwith "Cannot update bus, error in given bus slice"
    else
        bus
        |> Map.map (fun key oldVal ->
            if key >= a && key <= b
            then newLogicLevels.[key - a]
            else oldVal
            )

let updateWire wire newLogicLevel = Map.add 0 newLogicLevel wire

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