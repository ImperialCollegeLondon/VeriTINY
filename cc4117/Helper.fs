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

let andOp a b = if a && b then High else Low

let mapKeys (m: Map<'a,'b>) = m |> Map.toList |> List.map fst
let mapVals (m: Map<'a,'b>) = m |> Map.toList |> List.map snd