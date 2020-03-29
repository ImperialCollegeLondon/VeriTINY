module SynchronousBlocks
open SharedTypes
open Helper

// list of implemented Synchronous Megablocks
let syncMegaBlockLst: MegablockType list = ["DFF"]

let evaluateDFF (mapIn: Map<NetIdentifier,Net>) (outNetInfoLst: SimNetInfo list): Map<NetIdentifier,Net> =
    let outNetIDLst = List.map extractNetIDFromInfo outNetInfoLst
    let newNet = mapIn |> Map.toList |> List.map snd
    List.zip outNetIDLst newNet |> Map


// given MegablockType name, return the appropriate function for synchronous evaluation
let evaluateSyncBlock (mBlock: MegablockType) =
    match mBlock with
    | "DFF" -> 
        evaluateDFF
    | _ -> 
        failwithf "This synchronous megablock is not supported yet"

