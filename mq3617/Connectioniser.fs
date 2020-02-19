module Connectioniser

open System
open SharedTypes

let MakeConnection (net1:GeneralNet)(net2:GeneralNet) =
    ((fst net1 || fst net2) , snd net1)

let rec AddMegaBlock []=
    match Console.ReadLine() with
    |"end" ->[]
    |str->str::(AddMegaBlock [] )
    |_ -> failwith "NANI?! match failed when adding megablocks"
    // List.append snets (findNest str) do findnets l8r

let rec UserIn o =
    let blockLst = AddMegaBlock []
    printf "%A" blockLst
    

