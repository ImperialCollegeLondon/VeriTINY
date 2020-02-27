module ConnectionTools

open SharedTypes

/// 3-tuple helper functions
let first (a, _, _) = a
let second (_, b, _) = b
let third (_, _, c) = c

let findUnconnectedIn (connlist: Connection list) =
    let inlist = List.collect second connlist
    let totlist = inlist@(List.collect third connlist)
    List.countBy (id) totlist
    |> List.filter (fun x-> match (snd x) with |1->true |_->false)
    |> List.map fst
    |> List.filter (fun x ->List.contains x inlist) 

let findUnconnectedOut (connlist: Connection list) =
    let outlist = List.collect third connlist
    let totlist = outlist@(List.collect second connlist)
    List.countBy (id) totlist
    |> List.filter (fun x-> match (snd x) with |1->true |_->false)
    |> List.map fst
    |> List.filter (fun x ->List.contains x outlist) 

//let synchNetInit (connList:Connection List) (logic:LogicLevel)=
