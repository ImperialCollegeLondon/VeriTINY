module Helper
open SharedTypes
open EvalNetHelper

// module containing helper functions

let extractNetIDFromInfo netInfo = netInfo.ID

let makeNetFromInfo (netInfo: SimNetInfo) = 
            let ID = netInfo.ID
            match ID.SliceIndices with
            |Some(x, Some y) -> createNewBusMap (min x y, max x y) Low |> Bus
            |Some(x, None)  -> createNewBusMap (x, x) Low |> Bus
            |None -> createNewBusMap (0, 0) Low |> Wire

let netSize (netIn: Net): int =
    match netIn with
    | Wire netMap
    | Bus netMap ->
        netMap 
        |> Map.toList 
        |> List.map fst 
        |> List.length

// Evaluation uses Map<NetIdentifier,Net> type, functions to convert below
// update map with "otherMap" where origMap will be overwritten if given same key
let updateMap (origMap: Map<NetIdentifier,Net>) (otherMap: Map<NetIdentifier,Net>) =
        Seq.fold (fun m (KeyValue(k,v)) -> Map.add k v m) origMap otherMap

