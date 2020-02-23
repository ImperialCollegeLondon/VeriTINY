module CombEval

open SharedTypes
open EvalNetHelper
open EvalTypes
open LogicOperations

//TODO:Rename GraphEndPoint


let formAllNets (logicModule: TLogic): Map<NetIdentifier, EvalNet> = 
    let formNetsFromLst (netIDLst: NetIdentifier list) =
        let netIDToNet netIDSliceIndices = 
            match netIDSliceIndices with
            |None -> EvalWire (Map [(0, Some Low)])
            |Some (x, Some y) -> createNewBus (min x y, max x y) None
            |_ -> failwithf "Expected slices for creation of new net, got %A" netIDSliceIndices
        
        ([], netIDLst) ||> List.fold (fun evalNetLst netID -> List.append evalNetLst [(netID, netIDToNet netID.SliceIndices)])
 
    (formNetsFromLst logicModule.Inputs) @ (formNetsFromLst logicModule.Outputs) @ (formNetsFromLst logicModule.Wires)
    |> Map


let assignInputValues (inputMap: Map<NetIdentifier, GraphEndPoint>) (netMap: Map<NetIdentifier, EvalNet>) =
    let updateNetWithNewInput (net: EvalNet) (input: GraphEndPoint) =
        let failTypeMismatch net input = failwithf "Input and net type mismatch, got input %A, net %A" input net

        match input with
        |SingleInput logicLvl -> 
            match net with
            |EvalWire wireMap -> EvalWire(updateWire wireMap logicLvl)
            |_ -> failTypeMismatch net input
        |BusInput busInp ->             
            match net with
            |EvalBus busMap ->
                let paddedLogicLvlLst = padLogicLvlListToLength (intToLogicLevelList busInp []) (Map.count busMap)
                EvalBus(updateBus busMap None paddedLogicLvlLst)
            |_ -> failTypeMismatch net input
    
    netMap |> Map.map (fun netID net ->
    match Map.tryFind netID inputMap with
    |Some input -> updateNetWithNewInput net input
    |None -> net )


let evaluateExprLst (exprToEvaluate: Expression list) (netMap: Map<NetIdentifier, EvalNet>) ((evaluatedNets, netsToEvaluate): NetIdentifier list * NetIdentifier list) =
    let canEvalExpression exprInputs = 
        (true, exprInputs) ||> List.fold (fun expressionEvaluatable inp ->
        if !expressionEvaluatable
        then false
        else List.contains inp evaluatedNets)

    let expressionsToEvaluate = List.filter (fun (_, inpLst, _) -> canEvalExpression inpLst) exprToEvaluate
    let evaluateExpr (op: Operator , inpLst: NetIdentifier list , outLst: NetIdentifier list) =

        //TODO: implement getting slices and operating on them - replace places where netMap is directly accessed 
        let isExpressionOnWire = 
            match netMap.[List.head inpLst] with
            |EvalBus _ -> false
            |EvalWire _ -> true

        let outputBusSize = 
            let _, busIndices = List.head outLst
            match busIndices with
            |Some (x, Some y) -> abs(x - y + 1)
            |None -> 1
            |_ -> failwithf "Expected bus or wire indices, got %A" busIndices

        let getStartIndex busNetID = 
            match busNetID.SliceIndices with
            |Some (x, Some y) -> min x y
            |_ -> failwithf "Expected bus indices, got %A" busIndices

        let formOutput busOperator initValue =
            if isExpressionOnWire
                then
                    let startNet = (0, Some initValue) |> Map
                    List.fold (fun result inpNetID -> 
                        let inpNet = netMap.[inpNetID]
                        busOperator (result, 0) (inpNet, 0)) startNet inpLst
                else
                    let startNet = createNewBus (0, outputBusSize - 1) initValue
                    List.fold (fun result inpNetID ->
                        let inpNet = netMap.[inpNetID]
                        busOperator (result, 0) (inpNet getStartIndex inpNetID)) startNet inpLst



        match op with
        |And -> formOutput ANDOpBus High
        |Or -> formOutput OROpBus Low
        |Not -> NOTOpBus netMap.[List.head inpLst]
        |Pass -> netMap.[List.head inpLst]

        //TODO: Concat Operator
