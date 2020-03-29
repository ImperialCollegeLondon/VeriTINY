module ConnectionTools

open SharedTypes

let findUnconnectedIn (blockList: SimBlock list) =
   
    let inNetInfos =  List.collect (fun block -> block.inNets) blockList  
    let outNetInfos = List.collect (fun block -> block.outNets) blockList 

    inNetInfos @ outNetInfos
    |> List.countBy (id) 
    |> List.filter (fun x -> 
        match (snd x) with 
        | 1 -> true 
        | _ -> false)
    |> List.map fst
    |> List.filter (fun x -> List.contains x inNetInfos) 

let findUnconnectedOut (blockList: SimBlock list) =
    let inNetInfos =  List.collect (fun block -> block.inNets) blockList  
    let outNetInfos = List.collect (fun block -> block.outNets) blockList 

    inNetInfos @ outNetInfos
    |> List.countBy (id) 
    |> List.filter (fun x -> 
        match (snd x) with 
        | 1 -> true 
        | _ -> false)
    |> List.map fst
    |> List.filter (fun x ->List.contains x outNetInfos) 
