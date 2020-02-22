module CombEval

open SharedTypes
open EvalNetHelper
open EvalTypes

//TODO:Rename GraphEndPoint


let formAllNets (logicModule: TLogic): Map<NetIdentifier, EvalNet> = 
    let formNetsFromLst (netIDLst: NetIdentifier list) =
        let netIDToNet netIDSliceIndices = 
            match netIDSliceIndices with
            |None -> EvalWire (Map [(0, Some Low)])
            |Some (x, Some y) -> createNewBus(min x y, max x y)
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

    let evaluateExpr (op, inpLst, outLst) =
