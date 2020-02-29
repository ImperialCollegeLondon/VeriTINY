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
            |None -> EvalWire (Map [(0, None)])
            |Some (x, Some y) -> createNewBus (min x y, max x y) None
            |_ -> failwithf "Expected slices for creation of new net, got %A" netIDSliceIndices
        
        ([], netIDLst) ||> List.fold (fun evalNetLst netID -> List.append evalNetLst [(netID, netIDToNet netID.SliceIndices)])
 
    (formNetsFromLst logicModule.Inputs) @ (formNetsFromLst logicModule.Outputs) @ (formNetsFromLst logicModule.Wires)
    |> Map


let assignInputValues (inputMap: Map<NetIdentifier, Net>)  (moduleInputIDs: NetIdentifier list) ( allNets: Map<NetIdentifier, EvalNet>) =
        let failTypeMismatch net input = failwithf "Input and net type mismatch, got input %A, net %A" input net

        let isInputMapComplete = 
            List.fold (fun inputMapComplete inpNetID -> if inputMapComplete then Map.containsKey inpNetID inputMap else false) true moduleInputIDs

        let doNetTypesMatch inputNet correspondingEvalNet =
            match inputNet with
            |Bus _ ->
                match correspondingEvalNet with
                |EvalBus _ -> true
                |EvalWire _ -> false
            |Wire _ -> 
                match correspondingEvalNet with
                    |EvalBus _ -> false
                    |EvalWire _ -> true

        if isInputMapComplete
        then
            Map.map (fun netID net ->
                match Map.tryFind netID inputMap with
                |Some inputNet -> 
                    if doNetTypesMatch inputNet net 
                    then netToEvalNet inputNet
                    else failTypeMismatch net inputNet
                |None -> net )  allNets
        else failwithf "Input mapping incomplete, expecting Nets for %A, got mapping %A" moduleInputIDs inputMap


let rec evaluateExprLst (exprToEvaluate: Expression list) ( allNets: Map<NetIdentifier, EvalNet>) =
    if List.isEmpty exprToEvaluate
    then  allNets
    else
            
        let canEvalExpression (exprInputs: NetIdentifier list) = 
            (true, exprInputs) ||> List.fold (fun expressionEvaluatable exprInp ->
            if not expressionEvaluatable
            then false
            else 
                let fullInpMapKey = getNetByName exprInp.Name  allNets
                isNetEvaluatedAtIndices  allNets.[fullInpMapKey] exprInp.SliceIndices)

        let evaluatableExpressions = List.filter (fun (_, _, inpLst) -> canEvalExpression inpLst) exprToEvaluate

        let evaluateExpr (op: Operator , outLst: NetIdentifier list , inpLst: NetIdentifier list) (allNets: Map<NetIdentifier, EvalNet>) =

            //TODO: Generate errors when bus sizes don't match
            //Expressions can only have single output, change TLogic type
            let outputBusSize  = 
                let outputNetID = List.head outLst
                match outputNetID.SliceIndices with
                |Some (x, Some y) ->  (abs (x - y)) + 1
                |Some (_, None) -> 1
                |None -> getBusSize (getNetByName outputNetID.Name allNets)

            let reduceInpLstWithOp busOperator initValue =
                let startNet = createNewBusMap (0, outputBusSize - 1) (Some initValue)
                List.fold (fun result (inpNetID: NetIdentifier) ->
                    let inpNet = allNets.[getNetByName inpNetID.Name allNets]                 
                    busOperator (EvalBus result, 0) (inpNet, getStartIndex inpNetID)) startNet inpLst

            let resultNet = 
                match op with
                |And -> reduceInpLstWithOp ANDOpNet High
                |Or -> reduceInpLstWithOp OROpNet Low
                |Not ->
                    let inpNetID = List.head inpLst //not operations should only have 1 input
                    NOTOpNet allNets.[getNetByName inpNetID.Name allNets] (getStartIndex inpNetID) outputBusSize
                |Pass -> 
                    let inpNetID = List.head inpLst
                    PassOpNet allNets.[getNetByName inpNetID.Name allNets] (getStartIndex inpNetID) outputBusSize
                |Concat -> ConcatOpNet inpLst allNets

            let outputID = List.head outLst
            let outputNetKey = getNetByName outputID.Name allNets
            let outputEvalNet = allNets.[outputNetKey]
            let updatedOutputEvalNet = 
                match outputID.SliceIndices with 
                |Some (x, Some y) -> updateBus (extractLLMap outputEvalNet) (Some(min x y, max x y)) (LLOptMapToLLList resultNet)
                |Some (x, None) -> Map.add x resultNet.[0] (extractLLMap outputEvalNet)
                |None -> updateBus (extractLLMap outputEvalNet) None (LLOptMapToLLList resultNet)
            outputNetKey, updatedOutputEvalNet 

        if List.isEmpty evaluatableExpressions
        then failwithf "No evaluatable expressions exprToEvaluate: %A \n  allNets: %A" exprToEvaluate   allNets
        else
            let updatedAllNets, additionalExprsEvaluated = 
                List.fold (fun (currAllNets: Map<NetIdentifier,EvalNet>, additionalExprs) expr ->
                    let updateKey, newEvalNet = evaluateExpr expr currAllNets

                    //Output Concatenation Handling
                    let revConcatExprLst =
                        List.filter (fun (op, outLst:NetIdentifier list, _) -> 
                            let exprOutputID = List.head outLst
                            op = Concat && exprOutputID.Name = updateKey.Name) exprToEvaluate

                    let concatenationsCompleteAllNets =
                        List.fold (fun newAllNets (_, _, inpLst) -> 
                            reverseConcat newEvalNet inpLst newAllNets
                            ) currAllNets revConcatExprLst

                    let updatedEvalNet = 
                        match currAllNets.[updateKey] with
                        |EvalBus _ -> EvalBus newEvalNet
                        |EvalWire _ -> EvalWire newEvalNet        
                    Map.add updateKey updatedEvalNet concatenationsCompleteAllNets, List.append additionalExprs revConcatExprLst)  (allNets, []) evaluatableExpressions

            let updatedExprToEvaluate = List.filter (fun expr -> not (List.contains expr (evaluatableExpressions @ additionalExprsEvaluated))) exprToEvaluate            
            evaluateExprLst updatedExprToEvaluate updatedAllNets

let formOutputNets (moduleOutputs: NetIdentifier list) (oldOutputs: Map<NetIdentifier, Net>) ( allNets: Map<NetIdentifier, EvalNet>) =
    let outputEvalNets = 
        List.fold (fun outputNetMap outputID ->
            Map.add outputID  allNets.[outputID] outputNetMap) (Map []) moduleOutputs
    
    Map.fold (fun outputNetMap netID evalNet -> Map.add netID (evalNetToNet evalNet oldOutputs.[netID]) outputNetMap) (Map []) outputEvalNets

//top level function
let evaluateModuleWithInputs (combModule: TLogic) (inputMap: Map<NetIdentifier, Net>) (currOutputMap: Map<NetIdentifier, Net>): Map<NetIdentifier, Net> =
    formEvalNets combModule
    |> assignInputValues inputMap combModule.Inputs
    |> evaluateExprLst combModule.ExpressionList
    |> formOutputNets combModule.Outputs currOutputMap