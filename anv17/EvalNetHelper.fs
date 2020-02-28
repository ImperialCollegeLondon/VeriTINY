module EvalNetHelper
open SharedTypes
open EvalTypes

let extractLogicLevel logicLvlOpt = 
     match logicLvlOpt with
        |Some logicLvl -> logicLvl
        |None -> failwith "Logic Level has not been assigned yet"

let extractLLMap evalNet =
    match evalNet with
    |EvalBus y
    |EvalWire y -> y

let createNewBusMap (a,b) initVal = 
    [a..b]
    |> List.map (fun x -> (x, initVal)) 
    |> Map

let createNewBus (a, b) initVal = 
        createNewBusMap (a,b) initVal |> EvalBus

let padLogicLvlListToLength fullLength logicLvlLst =

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

let rec intToLogicLevelList uint logicLvlLst =
    if uint = 0
    then logicLvlLst
    else
        if (uint % 2) = 1
        then intToLogicLevelList (uint / 2) (logicLvlLst @ [High])
        else intToLogicLevelList (uint / 2) (logicLvlLst @ [Low])

let logicLevelsToint logicLvlLst = 
    let getDecimalValue bitPos bit = 
        if bit = High
        then int ((float 2) ** (float bitPos))
        else 0
    List.mapi getDecimalValue logicLvlLst
    |> List.reduce (+)


let getSlice (net:EvalNet) (a,b) =
    extractLLMap net
    |> Map.filter (fun index _ -> index >= a && index <= b)
    |> Map.toList
    |> List.sortBy fst
    |> List.map (snd >> extractLogicLevel) 


let isNetEvaluatedAtIndices evalNet sliceIndices = 
    let LLMap = extractLLMap evalNet

    let slicedLLMap =
        match sliceIndices with
        |Some (x, Some y) -> Map.filter (fun key _ -> key >= (min x y) && key <= (max x y)) LLMap
        |Some (x, None) -> Map.filter (fun key _ -> key = x) LLMap
        |None -> LLMap
       
    Map.fold (fun netEvaluated _ logicLevelOpt ->
        if not netEvaluated
        then false
        else
            match logicLevelOpt with
            |Some logicLvl -> true
            |None -> false) true slicedLLMap

let getBusSize netID = 
    match netID.SliceIndices with
    |Some(x, Some y) -> abs (x - y) + 1
    |Some (_, None)
    |None -> 1

let getStartIndex (netID: NetIdentifier) = 
    match netID.SliceIndices with
    |Some (x, Some y) -> min x y
    |Some (x, None) -> x
    |None -> 0

let evalNetToNet evalNet (defaults:Net) = 

    let defaultMapping =
        match defaults with
        |Bus x
        |Wire x -> x

    let noOptLLMap = 
        extractLLMap evalNet
        |> Map.map (fun i logicLvlOpt  -> Option.defaultValue defaultMapping.[i] logicLvlOpt)

    match evalNet with
    |EvalWire _ -> Wire noOptLLMap
    |EvalBus _ -> Bus noOptLLMap

let netToEvalNet net =
    let LLMapToOptLLMap map =
        Map.map (fun _ value -> Some value) map
    match net with
    |Wire wireMap -> EvalWire (LLMapToOptLLMap wireMap)
    |Bus busMap -> EvalBus(LLMapToOptLLMap busMap)

let LLOptMapToLLList logicLvlOptMap =
    logicLvlOptMap
    |> Map.toList
    |> List.sortBy fst
    |> List.map (snd >> extractLogicLevel)

let getNetByName name allNets = 
        match Map.tryFindKey (fun (netID: NetIdentifier) _-> netID.Name = name) allNets with
        |Some key -> key
        |None -> failwithf "Could not find net with name %s in netmap %A" name allNets