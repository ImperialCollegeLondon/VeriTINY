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

let netNodeToNet node = 
    match node.BusIndicies with 
    |Some busIndices -> createNewBus busIndices
    |None -> Wire (Map [(0, Low)])

let createEDGraphNodes dGraphNodes graphEdges =
    let findEdgeWithNodeAsOutput node = 
        match List.tryFind (fun edge -> edge.Output = node) graphEdges with //problem here with custom type instances not being unique ***************
        |Some edge -> edge
        |None -> failwithf "Couldn't find edge with %A node as ouput in %A" node graphEdges

    let rec getNetSize sliceIndices =
        match sliceIndices with
        |Some (_, None)
        |None -> 1
        |Some (x, Some y) -> abs(x - y + 1)

    let opNodeToNet opNode = 
        match opNode with
        |OpNode Concat -> failwith "Concatenation not implemented yet"
        |OpNode And
        |OpNode Or
        |OpNode Not
        |OpNode Pass -> 
            let nodeInpEgde = findEdgeWithNodeAsOutput opNode
            let nodeNetSize = getNetSize nodeInpEgde.SliceIndices
            if nodeNetSize = 1
            then Wire (Map [(0, Low)])
            else createNewBus (0, nodeNetSize - 1)

        |_ -> failwithf "Something went wrong ,expecting opNode, got %A" opNode

    let dGraphNodeToEDGraphNode node = 
        match node with
        |NetNode x -> node, netNodeToNet x
        |OpNode _ -> node, opNodeToNet node


    ([], dGraphNodes) ||>List.fold (fun eDGraphNodes dGraphNode -> List.append eDGraphNodes [dGraphNodeToEDGraphNode dGraphNode])

//graph evaluation steps
//1. create all nets 
//2. assign input values
//3. repeatedly evaluate all possible edges in a list until no more edges remain to be evaluated
// - find edges with operator outputs whose inputs are already evaluated
// - evaluate operator node outputs and assign the netnodes associated with them, their values
// - remove evaluated edges from list 
//4. Find output nodes and return their values (in order)

// type EDraphNode = DGraphNode * Net //EDGraphNode - Evaluation DGraphNode

// let evaluateGraph (graphEdges: DGraphEdge list, graphNodes: DGraphNode list)  (inputMap: Map<string,GraphEndPoint list>) =
//     let evalNodes = createEDGraphNodes graphNodes


//     let getEDGraphNodeByName name = 
//         match Map.tryFind netMap name with
//         |Some net -> net
//         |None -> failwithf "Could not find net with name %s in list %A" name netLst

//     let assignInputValues inputMap = 
//         let assignInputToNet (netName:string) (netValue: GraphEndPoint) =
//             let net = getNetByName nets netName             
//             match net with
//             |Bus busMap -> 
//                 let newLogicLevels = padLogicLvlListToLength (uintToLogicLevelList netValue) (Map.count net)
//                 updateBus busMap None newLogicLevels
//             |Wire wireMap -> updateWire wireMap netValue
        
//         Map.map (fun netName netValue -> 
//             if Map.containsKey netName inputMap
//             then assignInputToNet netName netValue
//             else netValue) nets

//     let moduleInputNetNames = 
//         Map.toList inputMap
//         |> List.map (fst)

    
//     // let rec evaluateNodesWithFilledInputs (evaluatedNets:string list) (remainingEdges: DGraphEdge list) (currentNetState: Map<string, Net>)  =

//     //     let rec evaluateOpEdges (evaluatedNets:string list) (remainingEdges: DGraphEdge list) (currentNetState: Map<string, Net>) = 


//     //     if List.isEmpty remainingEdges
//     //     then currentNetState
//     //     else