module Connectioniser

open System
open SharedTypes
////////////////////////////////////////////////testing,get rid of it l8r
let  l1 ={  
    Name = "Test1"
    Inputs=["A";"B";"C";"D"]
    Outputs = ["G"]
    Wires= ["E";"F"]
    ExpressionList = [(And,["A";"B"],["E"]);(And,["C";"D"],["F"]);(And,["E";"F"],["G"])]
}
let avaliableBlocks = [l1]
//////////////////////////////////////////////////////////////////////
let MakeConnection (net1:GeneralNet)(net2:GeneralNet) =
    ((fst net1 || fst net2) , snd net1)
 
let searchBlocks name (block:TLogic)=
    match block.Name with
    |n when n =name-> true
    |_->false

let genConnections name blist=
    let mBlock = (List.filter (searchBlocks name) blist).Head
    [(name,mBlock.Inputs,mBlock.Outputs)]

let rec AddMegaBlock []=
    match Console.ReadLine() with
    |"end" ->[]
    |str when List.exists (searchBlocks str) avaliableBlocks ->(genConnections str avaliableBlocks)::(AddMegaBlock [] )
    |str -> printf "NANI?! match failed when adding megablocks, no block exists with name %s" str
            AddMegaBlock []

let rec UserIn o =
    let blockLst = AddMegaBlock []
    printf "%A" blockLst
    

