module Connector

open System
open SharedTypes
open Helper
open EvalNetHelper


type Link = {
    inNet: string;
    outNet: string;
    netInfo: SimNetInfo
}

// /search helper functions
let searchBlocks name (block:TLogic)=
    match block.Name with
    | n when n = name -> 
        true
    | _ -> 
        false

let getNetInfoByName (name: string) (netInfoLst: SimNetInfo list): SimNetInfo =
    List.find (fun netInfo -> 
        let netID = netInfo.ID
        name = netID.Name) netInfoLst

/// return a bool list 
let searchNetInfoLst name (netInfoLst: SimNetInfo list): bool =
    List.exists (fun netInfo -> 
        let netID = extractNetIDFromInfo netInfo
        name = netID.Name) netInfoLst

 //only works for unclocked nets
 // previously genConnections
let addTLogic (str:MegablockType) (tLogLst:TLogic list): SimBlock=
    let mBlock = List.find (searchBlocks str) tLogLst

    let interpretNetId netId =
        match netId.SliceIndices with 
        |Some (0, _)-> 
            Map [0,Low]
            |> Bus    
        |Some (num2, Some num1) -> createNewBusMap (0 , abs(num2 - num1) + 1) Low |> Bus
        |None -> 
            Map [0,Low]
            |> Wire 
        | _ -> 
            printfn "NANI!? netId could not be interpreted either because Mark is stupid or Tuck didn't describe it well enough \n"
            Map [0,Low]
            |> Wire 

    let createSimNetInfos (netIdLst :NetIdentifier list): SimNetInfo list=
        List.map (fun x -> {
            ID = x
            isClocked = false;
        }) netIdLst

    {
        MegablockType = str
        inNets = createSimNetInfos mBlock.Inputs
        outNets = createSimNetInfos mBlock.Outputs
    }



let addDFF (size:int): SimBlock =
    let makeNetIDWithName name = 
        {
            Name = name
            SliceIndices = 
                match size with
                |1 -> None
                |x -> Some (0, Some (x - 1))

        }
        
    {
       MegablockType = "DFF";
       inNets = [
           {
               ID = makeNetIDWithName "D"
               isClocked = false
           }
        ];

        outNets = [
            {
               ID = makeNetIDWithName "Q"
               isClocked = true
            }
        ]  
    }

 




let addConnection (blockLst: SimBlock list) (bIn: SimBlock) =

    let blockCount = List.length blockLst 

    let prefixNetIDsInInfo = 
        List.map (fun (netInfo:SimNetInfo) -> 
            let netID = netInfo.ID
            let prefixedName =  (sprintf "%s.%i" bIn.MegablockType blockCount) + netID.Name 
            let prefixedNetID = {netID with Name = prefixedName}
            {netInfo with ID = prefixedNetID}
        )
    let prefixedInpNetInfos =  prefixNetIDsInInfo bIn.inNets
    let prefixedOutNetInfos = prefixNetIDsInInfo bIn.outNets

    let prefixedBlock = {bIn with inNets = prefixedInpNetInfos; outNets = prefixedOutNetInfos }    
    blockLst @ [prefixedBlock]

let checkValidConnection outName inName (bLst:SimBlock list) =
    let allOutNetInfos = (List.collect (fun block -> block.outNets) bLst)
    let allInNetInfos = (List.collect (fun block -> block.inNets) bLst)
    let inNetInfo = getNetInfoByName inName allInNetInfos
    let outNetInfo = getNetInfoByName outName allOutNetInfos

    let netLen netInfo =
        netInfo
        |> extractNetIDFromInfo
        |> getBusSize

    match inNetInfo,outNetInfo with
    | a,b when (netLen a <> netLen b) -> 
        Error (sprintf "nets cannot be connected as they are of differnet sizes")
    |a,b when searchNetInfoLst inName allOutNetInfos ->
        Error (sprintf "two output nets cannot be connected together")
    // |a,b when List.contains ((List.filter(searchOutNets outName)cLst).Head)(List.filter(searchInNets inName)cLst) && not(fst a) && not(fst b)->
    //     Error (sprintf "two output nets cannot be connected together")
    |_ -> 
        printf "connection made between %s %s, if this is impossible tell Mark\n" inName outName
        Ok ()


/// given input string and output string, make a link between them in connection list
let makeLink outName inName (bLst: SimBlock list) : Link =
    let allOutNetInfos = (List.collect (fun block -> block.outNets) bLst)
    let allInNetInfos = (List.collect (fun block -> block.inNets) bLst)

    let inNetInfo = getNetInfoByName inName allInNetInfos
    let outNetInfo = getNetInfoByName outName allOutNetInfos

    let isClocked' = inNetInfo.isClocked || outNetInfo.isClocked
    
    let newNetInfo = 
        {
            ID = outNetInfo.ID
            isClocked = isClocked'
        }

    {
        inNet = inName
        outNet = outName
        netInfo = newNetInfo
    }



// take link list as an argument
// take in connection list and return updated connection list

let applyLinks (linkLst:Link List) (bLst: SimBlock list): SimBlock list =
 
    // lst of input strings
    let inputNames = List.map (fun x -> x.inNet) linkLst
    // lst of output strings
    let outputNames = List.map (fun x -> x.outNet) linkLst

    let updateNetInfoLst (netInfoLst: SimNetInfo list)  =
        let matchNames name (link:Link) =
            match link.inNet , link.outNet with
            | a,b when a = name || b = name ->
                true
            | _ ->
                false        

        let returnNewNetInfo (netInfo: SimNetInfo) =
            let netID = netInfo.ID
            match netID.Name with 
                | name when List.contains name inputNames || List.contains name outputNames ->
                    let link =  (List.find (matchNames name) linkLst) 
                    link.netInfo                    
                | _ -> 
                    netInfo

        List.map returnNewNetInfo netInfoLst

    let updateBlock(block: SimBlock): SimBlock =     
        {block with inNets = updateNetInfoLst block.inNets; outNets = updateNetInfoLst block.outNets;}
 
    List.map updateBlock bLst


/// example of changing cLst
/// where "jeff" is output of a tLog and "b0" is input of a different tLog
// let a = checkValidConnection "jeff" "b0" c4CLst
// let linkA = makeLinks "jeff" "b0" c4CLst
// let linkLst = [linkA]
