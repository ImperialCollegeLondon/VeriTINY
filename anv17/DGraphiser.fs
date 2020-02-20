module DGraphiser
open SharedTypes

//TODO: make slice parser and integrate with AdjListEntry type
//TODO: Use results instead of failwithfs so errors can be propagated to UI

type Expression = (Operator * string * string)
type NetIdentifier = {
    Name: string;
    SliceIndices: (int * int) option
}
type TLogic = {
    Name: string
    ExpressionList: (Operator * string list * string list) list //first string list is nets which are inputs and second is nets which are outputs.TODO: Expression list should be implemented as record
    Inputs: string list
    Outputs: string list
    Wires: string list
}  

type DGraphNetNode = {
    NetName: string
    BusSize: int
}

type DGraphOpNode = Operator
type DGraphNode = |DGraphNetNode|DGraphOpNode
type DGraphEdge = {
    Input: DGraphNode
    Output: DGraphNode
    SliceIndicies: (int * int) option
    IsSliceOfInput: bool
}

//********Lexing and tokenising code - to be replaced by Lexer module - except for NGram definitions ***********
type NetNameToken = |Name of string|OpenSqBracket|SliceIndex of int|SemiColon|CloseSqBracket


//takes netIdentifier string in format name[number:number] or just name and returns a NetIdentifier module with the information
let getNetIDFromStr (netIdentifierStr: string) : NetIdentifier = failwithf "Not implemented yet - Requires Lexer Module"


let formNetIDsFromStrLst netIDStrLst =
    let foldNetIDStr netIDs netIDStr =
        [getNetIDFromStr netIDStr]
        |> List.append netIDs

    List.fold foldNetIDStr [] netIDStrLst

let formNetNodesFromIDs netIDLst =   

    let netIdentifierToNetNode (netID: NetIdentifier) = 

        let node = {
            NetName = netID.Name
            BusSize = 1
        }
           
        match (netID.SliceIndices) with
        |Some (x, y) -> {node with BusSize = x - y }
        |None -> node

    let foldNetIDtoNode nodeLst netID =
        List.append nodeLst [netIdentifierToNetNode netID]
  

    List.fold foldNetIDtoNode [] netIDLst
