module Helper
open SharedTypes

// module containing helper functions

let rec lstOpParallel acc f lstA lstB =
    match lstA, lstB with
        | h1::t1, h2::t2 ->
            let acc' = f h1 h2
            lstOpParallel (acc @ [acc']) f t1 t2
        | _ -> 
            acc

// initialized to low
let createNewMap len = 
    [0..len-1]
    |> List.map (fun x -> (x, Low)) 
    |> Map

let generateList n = [0..n-1]

let opOnTuple f (a,b) = f a, f b

// General Net helper functions
let extractGenNetLsts (cIn: Connection) =
    let _, lstIn, lstOut = cIn
    lstIn, lstOut

let extractNet (gNet: GeneralNet): Net =
    match gNet with
    | (_, (_, Wire netMap)) -> 
        Wire netMap
    | (_, (_, Bus netMap)) ->
        Bus netMap

let netSize (netIn: Net): int =
    match netIn with
    | Wire netMap
    | Bus netMap ->
        netMap 
        |> Map.toList 
        |> List.map fst 
        |> List.length

let gNetName (gNet: GeneralNet) =
    gNet |> snd |> fst

let updateGenNet (gNet: GeneralNet) newMap =
    match gNet with
    | (sync, (str, Wire _)) -> 
        sync, (str, Wire newMap) 
    | (sync, (str, Bus _)) -> 
        sync, (str, Bus newMap)

let updateGenLst gNetLst newMaps =
    lstOpParallel [] updateGenNet gNetLst newMaps

let extractLogLevel (gNet: GeneralNet) =
    match gNet |> snd |> snd with
    | Wire netMap
    | Bus netMap ->
        netMap 
        |> Map.toList 
        |> List.map snd

// Evaluation uses Map<NetIdentifier,Net> type, functions to convert below
// update map with "otherMap" where origMap will be overwritten if given same key
let updateMap (origMap: Map<NetIdentifier,Net>) (otherMap: Map<NetIdentifier,Net>) =
        Seq.fold (fun m (KeyValue(k,v)) -> Map.add k v m) origMap otherMap


let mapToGLst (netIDMap: Map<NetIdentifier, Net>) : GeneralNet List=
        Map.toList netIDMap 
        |> List.map (fun ((netID:NetIdentifier), net) -> false, (netID.Name, net))

// gNetLst to Map<NetID, Net>
let gLstToMap (gLst: GeneralNet List) : Map<NetIdentifier, Net> =
    let gNetToNetID (gNet: GeneralNet) =
        let _, (netName, net) = gNet
    
        let sliceIndices = 
            match net with
            |Bus netMap ->
                let indexList =
                    netMap
                    |> Map.toList
                    |> List.map fst
                match List.length indexList with
                |0 -> failwith "Expecting net logic level map, got empty map"
                |1 -> Some(List.head indexList, None)
                |_ -> Some(List.max indexList, Some (List.min indexList))            
            |Wire _ -> None
        {Name = netName; SliceIndices = sliceIndices}, net
    List.map gNetToNetID gLst |> Map.ofList