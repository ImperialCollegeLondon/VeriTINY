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


let assignInputValues (inputMap: Map<NetIdentifier, Net>)  (moduleInputIDs: NetIdentifier list) (evalNetMap: Map<NetIdentifier, EvalNet>) =
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
                |None -> net ) evalNetMap
        else failwithf "Input mapping incomplete, expecting Nets for %A, got mapping %A" moduleInputIDs inputMap


let rec evaluateExprLst (exprToEvaluate: Expression list) (evaluatedNets: NetIdentifier list) (evalNetMap: Map<NetIdentifier, EvalNet>) =
    if List.isEmpty exprToEvaluate
    then evalNetMap
    else 
        
        let canEvalExpression (exprInputs: NetIdentifier list) = 
            (true, exprInputs) ||> List.fold (fun expressionEvaluatable inp ->
            if not expressionEvaluatable
            then false
            else 
                match List.tryFind (fun (evaluatedNetID: NetIdentifier) -> evaluatedNetID.Name = inp.Name) evaluatedNets with
                |Some _ -> true
                |None -> false)

        let evaluatableExpressions = List.filter (fun (_, _, inpLst) -> canEvalExpression inpLst) exprToEvaluate
        let evaluateExpr (op: Operator , outLst: NetIdentifier list , inpLst: NetIdentifier list) =

            //TODO: Generate errors when bus sizes don't match
            //Expressions can only have single output, change TLogic type
            let outputBusSize  = 
                let outputNetID = List.head outLst
                match outputNetID.SliceIndices with
                |Some (x, Some y) ->  (abs (x - y)) + 1
                |Some (_, None) -> 1
                |None -> getBusSize (getNetByName outputNetID.Name evalNetMap)

            let reduceInpLstWithOp busOperator initValue =
                let startNet = createNewBusMap (0, outputBusSize - 1) (Some initValue)
                List.fold (fun result (inpNetID: NetIdentifier) ->
                    let inpNet = evalNetMap.[getNetByName inpNetID.Name evalNetMap]                 
                    busOperator (EvalBus result, 0) (inpNet, getStartIndex inpNetID)) startNet inpLst


            let resultNet = 
                match op with
                |And -> reduceInpLstWithOp ANDOpNet High
                |Or -> reduceInpLstWithOp OROpNet Low
                |Not ->
                    let inpNetID = List.head inpLst //not operations should only have 1 input
                    NOTOpNet evalNetMap.[getNetByName inpNetID.Name evalNetMap] (getStartIndex inpNetID) outputBusSize
                |Pass -> 
                    let inpNetID = List.head inpLst
                    PassOpNet evalNetMap.[getNetByName inpNetID.Name evalNetMap] (getStartIndex inpNetID) outputBusSize
                |Concat -> concatOp evalNetMap inpLst
                
            //TODO: Concat Operator

            let outputID = List.head outLst
            let outputNetKey = getNetByName outputID.Name evalNetMap
            let outputEvalNet = evalNetMap.[outputNetKey]
            let updatedOutputEvalNet = 
                match outputNetKey.SliceIndices with 
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
            let exprOutputConcatenation (_, exprOutputLst, _) =
                let exprOutput = List.head exprOutputLst
                List.fold (fun concatLst (op, outLst, inpLst) ->
                    match concatLst with
                    |Some _ -> concatLst
                    |None ->
                        match op with
                        |Concat -> if ((List.head outLst) = exprOutput) then Some inpLst else None
                        |_ -> None) None exprToEvaluate              


            List.fold (fun (currNetMap: Map<NetIdentifier,EvalNet>) expr ->
                let exprOutKey, exprOutEvalNet = evaluateExpr expr
                let updatedEvalNet = 
                    match currNetMap.[exprOutKey] with
                    |EvalBus _ -> EvalBus exprOutEvalNet
                    |EvalWire _ -> EvalWire exprOutEvalNet        
                
                let evalNetMapConcatApplied =
                    match exprOutputConcatenation expr with
                    |Some concatLst -> reverseConcat updatedEvalNet concatLst evalNetMap
                    |None -> evalNetMap
                    
                Map.add exprOutKey updatedEvalNet evalNetMapConcatApplied) evalNetMap evaluatableExpressions    

            

        let updatedExprToEvaluate = List.filter (fun expr -> not (List.contains expr evaluatableExpressions)) exprToEvaluate
        let updatedEvaluatedNets = 
            Map.fold (fun evaluatedNetLst netID evalNet ->
                if isNetEvaluated evalNet
                then List.append evaluatedNets [netID]
                else evaluatedNetLst) [] updatedEvalNetMap
        
        evaluateExprLst updatedExprToEvaluate updatedEvaluatedNets updatedEvalNetMap

let formOutputNets (moduleOutputs: NetIdentifier list) (evalNetMap: Map<NetIdentifier, EvalNet>) =
    let outputEvalNets = 
        List.fold (fun outputNetMap outputID ->
            Map.add outputID evalNetMap.[outputID] outputNetMap) (Map []) moduleOutputs
    
    Map.fold (fun outputNetMap netID evalNet -> Map.add netID (evalNetToNet evalNet) outputNetMap) (Map []) outputEvalNets

//top level function
let evaluateModuleWithInputs (combModule: TLogic) (inputMap: Map<NetIdentifier, Net>) : Map<NetIdentifier, Net> =
    formEvalNets combModule
    |> assignInputValues inputMap combModule.Inputs
    |> evaluateExprLst combModule.ExpressionList combModule.Inputs 
    |> formOutputNets combModule.Outputs     
