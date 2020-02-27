module Helper
open SharedTypes

let indexFromMap m =
    m |> Map.toList |> List.map fst

let rec lstOpParallel acc f lstA lstB =
    match lstA, lstB with
        | h1::t1, h2::t2 ->
            let acc' = f h1 h2
            lstOpParallel (acc @ [acc']) f t1 t2
        | _ -> 
            acc

let createNewMap len = 
        [0..len-1]
        |> List.map (fun x -> (x, Low)) 
        |> Map

let generateList n = [0..n-1]

let opOnTuple f (a,b) = f a, f b


// needed when users define inputs with Map<NetIdentifier, Net>
let mapToGLst (netIDMap: Map<NetIdentifier, Net>) : GeneralNet List=
        Map.toList netIDMap 
        |> List.map (fun ((netID:NetIdentifier), net) -> false, (netID.Name, net))

// gNetLst to Map<NetID, Net>
let gLstToMap (gLst: GeneralNet List) : Map<NetIdentifier, Net> =
    let gNetToNetID (gNet: GeneralNet) =
        let _, (netName, net) = gNet
    
        let sliceIndices = 
            match net with
            |Bus busMap ->
                let indexList =
                    busMap
                    |> Map.toList
                    |> List.map fst
                match List.length indexList with
                |0 -> failwith "Expecting net logic level map, got empty map"
                |1 -> Some(List.head indexList, None)
                |_ -> Some(List.max indexList, Some (List.min indexList))            
            |Wire _ -> None
        {Name = netName; SliceIndices = sliceIndices}, net
    List.map gNetToNetID gLst |> Map.ofList