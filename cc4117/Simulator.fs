module Simulator
open Evaluator
open SharedTypes
open Helper

let initializeSync cLst =
    let updateIfSync (gNet:GeneralNet) = 
        if fst gNet 
        then
            let newMapLen = gNet |> extractNet |> netSize 
            updateGenNet gNet (createNewMap newMapLen)
        else
            gNet
    let setToLow (cIn: Connection) =
        cIn 
        |> extractGenNetLsts  
        |> opOnTuple (List.map updateIfSync)
    List.map setToLow cLst 

let getOutputs (cLst: Connection List) = 
    let output (_,_,c) = c
    List.map output cLst

let getInitMap currentInputs cLst =
    let findAllSync (cLst:Connection List) =
        let rec findSync acc gLst =
            match gLst with
            | hd::tl -> 
                if fst hd then 
                    findSync (acc @ [hd]) tl
                else 
                    findSync acc tl
            | [] -> List.distinct acc
        let c (cIn: Connection) =
            cIn |> extractGenNetLsts |> opOnTuple (findSync []) |> appendTuple
        List.collect c cLst
    let reformatGNet (gNet: GeneralNet) =
        match gNet with
        | (sync, (str, net)) -> str, (sync, net)

    currentInputs @ findAllSync cLst |> List.map reformatGNet |> Map

//let advanceState (inputs: GeneralNet list) (cLst: Connection List) (tLst: TLogic List)  = 
    

// let checkIfKnown lstRef lst =


// let advanceState currentInputs (cLst: Connection List) =
//     let m = getInitMap currentInputs cLst
//     let outputs = getOutputs cLst

//     let rec op (cLst: Connection List) =
//         match cLst with 
//         | hd::tl ->
            
//         | [] ->
