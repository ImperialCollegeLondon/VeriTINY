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

        //TODO: Generate errors when bus sizes don't match
        //Expressions can only have single output, change TLogic type
        let outputBusSize = 
            let _, busIndices = List.head outLst
            match busIndices with
            |Some (x, Some y) -> abs(x - y + 1)
            |Some (_, None)
            |None -> 1
            |_ -> failwithf "Expected bus or wire indices, got %A" busIndices

        let getStartIndex busNetID = 
            match busNetID.SliceIndices with
            |Some (x, Some y) -> min x y
            |Some (x, None) -> x
            |None -> 0
            |_ -> failwithf "Expected bus indices, got %A" busIndices

        let getNetByName name = 
            match Map.tryFindKey (fun netID -> netID.Name = name) netMap with
            |Some key -> netMap.[key]
            |None -> failwithf "Could not find net with name %s in netmap %A" name netMap

        let reduceInpLstWithOp busOperator initValue =
            let startNet = createNewBus (0, outputBusSize - 1) initValue
            List.fold (fun result inpNetID ->
                let inpNet = getNetByName(inpNetID.Name)
                busOperator (result, 0) (inpNet, getStartIndex inpNetID)) startNet inpLst

        let resultNet = 
            match op with
            |And -> reduceInpLstWithOp ANDOpBus High
            |Or -> reduceInpLstWithOp OROpBus Low
            |Not ->
                let inpNetID = List.head inpLst //not operations should only have 1 input
                NOTOpBus (getNetByName inpNetID.Name) (getStartIndex inpNetID) outputBusSize
            |Pass -> 
                let inpNetID = List
                PassOpBus (getNetByName inpNetID.Name) (getStartIndex inpNetID) outputBusSize
            |Concat -> failwith "Concatenation not implemented yet"

        

        //TODO: Concat Operator
