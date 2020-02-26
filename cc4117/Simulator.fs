module Simulator
open Evaluator
open SharedTypes
open ExampleTypes
open Helper



// let initializeSync cLst =
//         let updateIfSync (gNet:GeneralNet) = 
//             if fst gNet 
//             then
//                 let newMapLen = gNet |> extractNet |> netSize 
//                 updateGenNet gNet (createNewMap newMapLen)
//             else
//                 gNet
//         let setToLow (cIn: Connection) =
//             cIn 
//             |> extractGenNetLsts  
//             |> opOnTuple (List.map updateIfSync)
//         List.map setToLow cLst 

// let findAllSync (cLst:Connection List) =
//         let rec findSync acc gLst =
//             match gLst with
//             | hd::tl -> 
//                 if fst hd then 
//                     findSync (acc @ [hd]) tl
//                 else 
//                     findSync acc tl
//             | [] -> List.distinct acc
//         let c (cIn: Connection) =
//             cIn |> extractGenNetLsts |> opOnTuple (findSync []) |> (fun (a, b) -> a @ b)
//         List.collect c cLst


// needed when users define inputs with Map<NetIdentifier, Net>
let mapToGLst (netIDMap: Map<NetIdentifier, Net>) : GeneralNet List=
        Map.toList netIDMap |> List.map (fun ((a:NetIdentifier), b) -> false, (a.Name, b))


let getOutputs (cLst: Connection List) = 
    let output (_,_,c) = c
    List.map output cLst


// first take cLst and extract all syncNets as a list and initialize them to Low
let returnSyncNets (cLst: Connection List) = 
    let findAllSync (cLst:Connection List) =
        let rec findSync acc gLst =
            match gLst with
            | hd::tl -> 
                if fst hd then 
                    findSync (acc @ [hd]) tl
                else 
                    findSync acc tl
            | [] -> 
                acc
        let getSyncFromConnection (cIn: Connection) =
            cIn |> extractGenNetLsts |> opOnTuple (findSync []) |> (fun (a, b) -> a @ b)
        List.collect getSyncFromConnection cLst
    
    let initializeSync (gNet: GeneralNet) = 
        let newMapLen = gNet |> extractNet |> netSize 
        updateGenNet gNet (createNewMap newMapLen)
        
    cLst |> findAllSync |> List.distinct |> List.map initializeSync 

// then use the currentInputs and syncNet list to create an initial map of known values
let getInitMap (currentInputs:GeneralNet list) (syncNets: GeneralNet list) =
    let gLstToMap (gLst: GeneralNet List) : Map<NetIdentifier, Net> =
        let getSliceIndices netIn: (int * int option) option =
            match netIn with
            | Wire _ 
                -> None 
            | Bus _ 
                -> let sliceWidth = netSize netIn
                   if sliceWidth = 1 then Some (0, None) 
                   else Some(sliceWidth-1, Some 0)
        let getNetIds gNet = 
            match gNet with
            | _, (str, net) -> 
                {Name=str; SliceIndices=getSliceIndices net}, net
        List.map getNetIds gLst |> Map.ofList
    currentInputs @ syncNets |> gLstToMap

let advanceState (currentInputs: GeneralNet list) (syncNets: GeneralNet List) (cLst: Connection List) (tLst: TLogic List)  = 
    let knownNets = getInitMap currentInputs syncNets // map<NetIdentifier, Net> -> all info of sync or not is removed
    let outputs = getOutputs cLst // gNetLst 
    let checkIfKnown netName lstRef = List.tryFind netName lstRef

    let getTLogic (mBlock: Megablock) =
        let (Name str) = mBlock
        let checker s (tLog: TLogic): bool = 
            s = tLog.Name
        List.tryFind (checker str) tLst 
    
    let seperateDFF (cLst:Connection List) =
        let checkIfDFF (cIn: Connection) =
            let (Name str), _, _ = cIn
            str = "DFF"
        let rec seperate lstA lstB cLst =
            match cLst with
            | hd::tl ->
                if checkIfDFF hd then
                    seperate (lstA @ [hd]) lstB tl
                else
                    seperate lstA (lstB @ [hd]) tl
            | [] -> lstA, lstB
        seperate [] [] cLst // -> (syncCLst, asyncCLst)

    // let rec simulate (cLst: Connection List) =
    //     match cLst with 
    //     | (n, lstIn, lstOut)::tl ->
    //         // do optionOrELse thing to check if everything in lstIn is known 
    //         // if not known, simulate tl @ [hd] else if known simulate tl
    //         let tLog = getTLogic n 
    //         let output = lstIn |> gLstToMap |> evaluateModuleWithInputs tLog |> mapToGLst
            

    //     | [] -> 
    printfn "not done yet"
