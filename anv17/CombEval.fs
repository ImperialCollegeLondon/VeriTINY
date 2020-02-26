module CombEval

open SharedTypes
open EvalNetHelper
open EvalTypes
open LogicOperations

//TODO:Rename GraphEndPoint


let formEvalNets (logicModule: TLogic): Map<NetIdentifier, EvalNet> = 
    let formNetsFromLst (netIDLst: NetIdentifier list) =
        let netIDToNet netIDSliceIndices = 
            match netIDSliceIndices with
            |None -> EvalWire (Map [(0, Some Low)])
            |Some (x, Some y) -> createNewBus (min x y, max x y) None
            |_ -> failwithf "Expected slices for creation of new net, got %A" netIDSliceIndices
        
        ([], netIDLst) ||> List.fold (fun evalNetLst netID -> List.append evalNetLst [(netID, netIDToNet netID.SliceIndices)])
 
    (formNetsFromLst logicModule.Inputs) @ (formNetsFromLst logicModule.Outputs) @ (formNetsFromLst logicModule.Wires)
    |> Map


let assignInputValues (inputMap: Map<NetIdentifier, GraphEndPoint>) (evalNetMap: Map<NetIdentifier, EvalNet>) =
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
    
    evalNetMap |> Map.map (fun netID net ->
    match Map.tryFind netID inputMap with
    |Some input -> updateNetWithNewInput net input
    |None -> net )


let rec evaluateExprLst (exprToEvaluate: Expression list) (evaluatedNets: NetIdentifier list) (evalNetMap: Map<NetIdentifier, EvalNet>) =
    if List.isEmpty exprToEvaluate
    then evalNetMap
    else 
        let canEvalExpression exprInputs = 
            (true, exprInputs) ||> List.fold (fun expressionEvaluatable inp ->
            if not expressionEvaluatable
            then false
            else List.contains inp evaluatedNets)

        let evaluatableExpressions = List.filter (fun (_, inpLst, _) -> canEvalExpression inpLst) exprToEvaluate
        let evaluateExpr (op: Operator , inpLst: NetIdentifier list , outLst: NetIdentifier list) =

            //TODO: Generate errors when bus sizes don't match
            //Expressions can only have single output, change TLogic type
            let outputBusSize = 
                let outputNetID = List.head outLst
                match outputNetID.SliceIndices with
                |Some (x, Some y) ->  (abs (x - y)) + 1
                |Some (_, None)
                |None -> 1

            let getStartIndex (netID: NetIdentifier) = 
                match netID.SliceIndices with
                |Some (x, Some y) -> min x y
                |Some (x, None) -> x
                |None -> 0

            let getNetByName name = 
                match Map.tryFindKey (fun (netID: NetIdentifier) _-> netID.Name = name) evalNetMap with
                |Some key -> key
                |None -> failwithf "Could not find net with name %s in netmap %A" name evalNetMap

            let reduceInpLstWithOp busOperator initValue =
                let startNet = createNewBusMap (0, outputBusSize - 1) (Some initValue)
                List.fold (fun result (inpNetID: NetIdentifier) ->
                    let inpNet = evalNetMap.[getNetByName inpNetID.Name]                 
                    busOperator (EvalBus result, 0) (inpNet, getStartIndex inpNetID)) startNet inpLst

            let resultNet = 
                match op with
                |And -> reduceInpLstWithOp ANDOpNet High
                |Or -> reduceInpLstWithOp OROpNet Low
                |Not ->
                    let inpNetID = List.head inpLst //not operations should only have 1 input
                    NOTOpNet evalNetMap.[getNetByName inpNetID.Name] (getStartIndex inpNetID) outputBusSize
                |Pass -> 
                    let inpNetID = List.head inpLst
                    PassOpNet evalNetMap.[getNetByName inpNetID.Name] (getStartIndex inpNetID) outputBusSize
                |Concat -> failwith "Concatenation not implemented yet"
                
            //TODO: Concat Operator

            let outputID = List.head outLst
            let outputNetKey = getNetByName outputID.Name
            let outputEvalNet = evalNetMap.[outputNetKey]
            let updatedOutputEvalNet = 
                match outputID.SliceIndices with 
                |Some (x, Some y) ->
                    resultNet
                    |> Map.toList
                    |> List.sortBy fst
                    |> List.map (snd >> extractLogicLevel)
                    |> updateBus (extractLLMap outputEvalNet) (Some(min x y, max x y))
                |Some (x, None) -> Map.add x resultNet.[0] (extractLLMap outputEvalNet)
                |None -> updateWire (extractLLMap outputEvalNet) (extractLogicLevel resultNet.[0])

            outputNetKey, updatedOutputEvalNet 

        let updatedEvalNetMap = 
            List.fold (fun (currNetMap: Map<NetIdentifier,EvalNet>) expr ->
                let updateKey, newEvalNet = evaluateExpr expr
                let updatedEvalNet = 
                    match currNetMap.[updateKey] with
                    |EvalBus _ -> EvalBus newEvalNet
                    |EvalWire _ -> EvalWire newEvalNet        
                Map.add updateKey updatedEvalNet currNetMap) evalNetMap evaluatableExpressions

        let updatedExprToEvaluate = List.filter (fun expr -> not (List.contains expr evaluatableExpressions)) exprToEvaluate
        let updatedEvaluatedNets = 
            Map.fold (fun evaluatedNetLst netID evalNet ->
                if isNetEvaluated evalNet
                then List.append evaluatedNets [netID]
                else evaluatedNetLst) [] evalNetMap
        
        evaluateExprLst updatedExprToEvaluate updatedEvaluatedNets updatedEvalNetMap

let formOutputNets (moduleOutputs: NetIdentifier list) (evalNetMap: Map<NetIdentifier, EvalNet>) =
    let outputEvalNets = 
        List.fold (fun outputNetMap outputID ->
            Map.add outputID evalNetMap.[outputID] outputNetMap) (Map []) moduleOutputs

    let evalNetToNet evalNet = 
        let noOptLLMap = 
            extractLLMap evalNet
            |> Map.map (fun _ logicLvlOpt  -> extractLogicLevel logicLvlOpt)

        match evalNet with
        |EvalWire _ -> Wire noOptLLMap
        |EvalBus _ -> Bus noOptLLMap
    
    Map.fold (fun outputNetMap netID evalNet -> Map.add netID (evalNetToNet evalNet) outputNetMap) (Map []) outputEvalNets
//TOOD: testing

//top level function
let evaluateModuleWithInputs (combModule: TLogic) (inputMap: Map<NetIdentifier, GraphEndPoint>) : Map<NetIdentifier, Net> =
    formEvalNets combModule
    |> assignInputValues inputMap
    |> evaluateExprLst combModule.ExpressionList combModule.Inputs
    |> formOutputNets combModule.Outputs
