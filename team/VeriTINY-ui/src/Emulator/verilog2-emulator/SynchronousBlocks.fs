module SynchronousBlocks
open SharedTypes
open Helper

// list of implemented Synchronous Megablocks
let syncMegaBlockLst: Megablock list = [Name "DFF"]

let evaluateDFF (mapIn: Map<NetIdentifier,Net>) (mapOut: Map<NetIdentifier,Net>): Map<NetIdentifier,Net> =
    let netID = mapOut |> Map.toList |> List.head |> fst
    let newNet = mapIn |> Map.toList |> List.head |> snd
    Map.add netID newNet mapOut

// given megablock name, return the appropriate function for synchronous evaluation
let evaluateSyncBlock (mBlock: Megablock) =
    match mBlock with
    | Name "DFF" -> 
        evaluateDFF
    | _ -> 
        failwithf "This synchronous megablock is not supported yet"

