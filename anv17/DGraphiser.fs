module DGraphiser

//TODO: make slice parser and integrate with AdjListEntry type
//TODO: Use results instead of failwithfs so errors can be propagated to UI

type Operator = | And | Or | Not | Pass //Pass to just pass signal through in case outputs have slices of intermediate nets 

type Expression = (Operator * string * string)

type TLogic = {
    Name: string
    ExpressionList: Expression list //first string is input net, second string is output net
    Inputs: string list
    Outputs: string list
}

type ExprGraphNode = | Net of (Operator * string) | InputNet of string  //Net string is net name

type AdjListEntry = {
    Input: ExprGraphNode;
    Output: ExprGraphNode;
    SliceIndices: (int * int) option//stores indices of bus slices
} 

let findAllNets (expressionLst: Expression list) =
    let isNewNet netName knownNets = List.contains netName knownNets

    let addIfNewNet netName knownNets = 
        match isNewNet netName knownNets with
        |true -> List.append knownNets [netName]
        |false -> knownNets

    let addNewNetsFromExpr knownNets expression  =
        let _, inpNet, outNet = expression
        addIfNewNet inpNet knownNets |> addIfNewNet outNet

    ([], expressionLst) ||> List.fold addNewNetsFromExpr


let formDGraphNodes (expressionLst: Expression list) uniqueNetLst =   

    let tryFindExprWithNetAsOutput netName = 
        let getExprOutput expr =
            match expr with
            |_, _, outputNet -> outputNet

        let tryMatchNetNameWithOutput expr = (=) (getExprOutput expr) netName

        List.tryFind tryMatchNetNameWithOutput expressionLst
            
    let netToNode netName = 
        match tryFindExprWithNetAsOutput netName with
        |Some (op, _, outNet) -> Net(op, outNet)
        |None -> InputNet(netName)

    let foldNetsIntoNodes nodeLst netName = List.append nodeLst [(netToNode netName)]

    List.fold foldNetsIntoNodes [] uniqueNetLst


let formAdjList (expressionLst: Expression list) nodeLst =
    let getNodeNetName node = 
        match node with
        |Net (_, name)
        |InputNet name -> name

    let tryFindNodeWithName name = List.tryFind (fun node -> (=) (getNodeNetName node) name) nodeLst

    let exprToEdge (_, inpNet, outNet) =
          let inpNode = 
            match tryFindNodeWithName inpNet with
            |Some node -> node
            |None -> failwithf  "Could not find node with name %s" inpNet

          let outNode = 
            match tryFindNodeWithName outNet with
            |Some node -> node
            |None -> failwithf  "Could not find node with name %s" outNet

          {
              Input = inpNode;
              Output = outNode;
              SliceIndices = None
          }
           

    let foldExpressionsToEdges edgeLst expression = List.append edgeLst [exprToEdge expression]

    List.fold foldExpressionsToEdges [] expressionLst
    
let formDGraph tLogicModule =
    findAllNets tLogicModule.ExpressionList
    |> formDGraphNodes tLogicModule.ExpressionList
    |> formAdjList tLogicModule.ExpressionList