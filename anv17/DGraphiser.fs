module DGraphiser
open SharedTypes

//TODO: make slice parser and integrate with AdjListEntry type
//TODO: Use results instead of failwithfs so errors can be propagated to UI
//TODO: Update types in SharedTypes folder and remove them from here

type Expression = (Operator * string list * string list)
type NetIdentifier = {
    Name: string;
    SliceIndices: (int * int option) option //first int is upper index for slicing/IO bus definitions or wire index in bus if a single wire in a bus needs to be selected
    //second int is lower index for slicing/IO bus definitions
    //SliceIndicies can be None if whole bus is to be selected 
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
    BusIndicies: (int * int) option
}

type DGraphOpNode = Operator
type DGraphNode = |NetNode of DGraphNetNode|OpNode of DGraphOpNode
type DGraphEdge = {
    Input: DGraphNode
    Output: DGraphNode
    SliceIndices: (int * int option) option
    IsSliceOfInput: bool
}

//********Lexing and tokenising code - to be replaced by Lexer module - except for NGram definitions ***********
type NetNameToken = |Name of string|OpenSqBracket|SliceIndex of int|SemiColon|CloseSqBracket

//*******Graph forming************
//takes netIdentifier string in format name[number:number] or just name and returns a NetIdentifier module with the information
let getNetIDFromStr (netIdentifierStr: string) : NetIdentifier = failwithf "Not implemented yet - Requires Lexer Module"


let getNetNodeByName name netNodeLst =
    match List.tryFind (fun node -> node.NetName = name) netNodeLst with
    |Some node -> node
    |None -> failwithf "Cannot find DGraphNetNode with name %s" name   

let formNetIDsFromStrLst netIDStrLst =
    let foldNetIDStr netIDs netIDStr =
        [getNetIDFromStr netIDStr]
        |> List.append netIDs

    List.fold foldNetIDStr [] netIDStrLst

let formNetNodesFromIDs netIDLst =   

    let netIdentifierToNetNode (netID: NetIdentifier) = 

        let node = {
            NetName = netID.Name
            BusIndicies = None
        }
           
        match netID.SliceIndices with
        |Some (x, Some y)-> {node with BusIndicies = Some(x,y)}
        |None -> node
        |_ -> failwithf "Expected IO net definition, got %A" netID.SliceIndices


    let foldNetIDtoNode nodeLst netID =
        List.append nodeLst [netIdentifierToNetNode netID]
  

    List.fold foldNetIDtoNode [] netIDLst


let formEdgesAndOpNodes netNodeLst exprLst = 

    let processExpression (op, inputs, outputs) =        

        let exprInputNetIDS = formNetIDsFromStrLst inputs
        let exprOutputsNetIDS = formNetIDsFromStrLst outputs 

        let formInputEdge op (netID : NetIdentifier) = 
            {
                Input = NetNode (getNetNodeByName netID.Name netNodeLst)
                Output = OpNode (op)
                SliceIndices = netID.SliceIndices
                IsSliceOfInput = true
            }
        let formOutputEdge op (netID : NetIdentifier) = 
            {
                Input = OpNode (op)
                Output = NetNode (getNetNodeByName netID.Name netNodeLst)
                SliceIndices = netID.SliceIndices
                IsSliceOfInput = false
            }
        let edgeFolder idToEdge edgLst netID = List.append edgLst [idToEdge op netID]

        let inputEdges = ([], exprInputNetIDS) ||> List.fold (edgeFolder formInputEdge)
        let outputEdges = ([], exprOutputsNetIDS) ||> List.fold (edgeFolder formOutputEdge)

        List.append inputEdges outputEdges

    ([], exprLst) ||> List.fold (fun allEdges expression -> List.append allEdges (processExpression expression))

//*********Graph Execution***********

//******Types********* - TOOD: replace with types in shared types later 
type LogicLevel = | High | Low
type GraphEndPoint = |LogicLevel|BusInput of int //TODO: Change BusInput to BusValue
type Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>
type NamedNet = string * Net

//**************Net Helper functions*******************

let createNewBus (a, b) = 
        [a..b]
        |> List.map (fun x -> (x, Low)) 
        |> Map
        |> Bus

let createNamedNetList netNodes =

    let netNodeToNet node = 
        match node.BusIndicies with 
        |Some busIndices ->  createNewBus busIndices
        |None -> Wire (Map [(0, Low)])

    let netNodeToNamedNet node = 
        let net = netNodeToNet node
        NamedNet(node.NetName, net)

    let foldNetNodesToNamedNets namedNetLst netNode = List.append namedNetLst [netNodeToNamedNet netNode]

    List.fold foldNetNodesToNamedNets [] netNodes

let padBusToLength busMap fullLength =
    if Map.count busMap >= fullLength
    then busMap
    else
        [1..(fullLength - (Map.count busMap))]
        |> List.map (fun _ -> Low)
        |> List.append (List.map snd (Map.toList busMap))
        |> List.mapi (fun i logicLevel -> (i, logicLevel))
        |> Map


let updateBus bus (a,b) newLogicLevels = 
 
    if (b - a + 1) <> (List.length newLogicLevels) || not (Map.containsKey b bus) || not (Map.containsKey a bus)
    then failwith "Cannot update bus, error in given bus slice"
    else
        bus
        |> Map.map (fun key oldVal ->
            if key >= a && key <= b
            then newLogicLevels.[key - a]
            else oldVal
            )

let updateWire wire newLogicLevel = Map.add 0 newLogicLevel wire

let rec uintToLogicLevelList uint logicLvlLst =
    if uint = 0
    then logicLvlLst
    else
        if (uint % 2) = 1
        then uintToLogicLevelList (uint / 2) (logicLvlLst @ [High])
        else uintToLogicLevelList (uint / 2) (logicLvlLst @ [Low])

let logicLevelsToUint logicLvlLst = 
    let getDecimalValue bitPos bit = 
        if bit = High
        then int ((float 2) ** (float bitPos))
        else 0
    List.mapi getDecimalValue logicLvlLst
    |> List.reduce (+)