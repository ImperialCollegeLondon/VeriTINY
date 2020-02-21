module DGraphiser
open SharedTypes
open NetHelper

//TODO: make slice parser and integrate with AdjListEntry type
//TODO: Use results instead of failwithfs so errors can be propagated to UI
//TODO: Update types in SharedTypes folder and remove them from here

type Expression = (Operator * NetIdentifier list * NetIdentifier list)

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

//*******Graph forming************
let getNetNodeByName name netNodeLst =
    match List.tryFind (fun node -> node.NetName = name) netNodeLst with
    |Some node -> node
    |None -> failwithf "Cannot find DGraphNetNode with name %s" name   


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

        let inputEdges = ([], inputs) ||> List.fold (edgeFolder formInputEdge)
        let outputEdges = ([], outputs) ||> List.fold (edgeFolder formOutputEdge)

        List.append inputEdges outputEdges

    ([], exprLst) ||> List.fold (fun allEdges expression -> List.append allEdges (processExpression expression))

//*********Graph Execution***********

let createNamedNetMap netNodes =

    let netNodeToNet node = 
        match node.BusIndicies with 
        |Some busIndices -> createNewBus busIndices
        |None -> Wire (Map [(0, Low)])

    let netNodeToNamedNet node = 
        let net = netNodeToNet node
        NamedNet(node.NetName, net)

    let foldNetNodesToNamedNets namedNetLst netNode = List.append namedNetLst [netNodeToNamedNet netNode]

    List.fold foldNetNodesToNamedNets [] netNodes |> Map

//graph evaluation steps
//1. create all nets 
//2. assign input values
//3. repeatedly evaluate all possible edges in a list until no more edges remain to be evaluated
// - find edges with operator outputs whose inputs are already evaluated
// - evaluate operator node outputs and assign the netnodes associated with them, their values
// - remove evaluated edges from list 
//4. Find output nodes and return their values (in order)

type EDraphNode = DGraphNode * Net //EDGraphNode - Evaluation DGraphNode

let evaluateGraph (graphEdges: DGraphEdge list, graphNodes: DGraphNode list)  (inputMap: Map<string,GraphEndPoint list>) =
    let nets = 
        List.filter (fun node ->
            match node with
            |DGraphNetNode _ -> true
            |_ -> false) graphNodes
        |> createNamedNetMap


    let getNetByName netMap name = 
        match Map.tryFind netMap name with
        |Some net -> net
        |None -> failwithf "Could not find net with name %s in list %A" name netLst

    let assignInputValues inputMap = 
        let assignInputToNet (netName:string) (netValue: GraphEndPoint) =
            let net = getNetByName nets netName             
            match net with
            |Bus busMap -> 
                let newLogicLevels = padLogicLvlListToLength (uintToLogicLevelList netValue) (Map.count net)
                updateBus busMap None newLogicLevels
            |Wire wireMap -> updateWire wireMap netValue
        
        Map.map (fun netName netValue -> 
            if Map.containsKey netName inputMap
            then assignInputToNet netName netValue
            else netValue) nets

    let moduleInputNetNames = 
        Map.toList inputMap
        |> List.map (fst)