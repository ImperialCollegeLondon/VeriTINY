// Learn more about F# at http://fsharp.org

open System
open SharedTypes
open CombEval

let testTLogic = { 
   Name = "bus_mux"
   ExpressionList =
      [(Not, [{ Name = "selb"
                SliceIndices = None }], [{ Name = "sel"
                                           SliceIndices = None }]);
       (And, [{ Name = "out"
                SliceIndices = None }],
        [{ Name = "a"
           SliceIndices = Some (1, None) }; { Name = "sel"
                                              SliceIndices = None }]);
       (And, [{ Name = "out"
                SliceIndices = None }],
        [{ Name = "a"
           SliceIndices = Some (0, None) }; { Name = "selb"
                                              SliceIndices = None }])]
   Inputs = [{ Name = "a"; SliceIndices = Some (1, Some 0) }; { Name = "sel"; SliceIndices = None }]
   Outputs = [{ Name = "out"; SliceIndices = None }]
   Wires = [{ Name = "selb"; SliceIndices = None }] }

let testInputs = Map [
   ({ Name = "a"; SliceIndices = Some (1, Some 0) }, BusInput 2);
   ({ Name = "sel"; SliceIndices = None }, SingleInput Low)
]

// let canEvalExpression exprInputs evaluatedNets = 
//             ((true), exprInputs) ||> List.fold (fun expressionEvaluatable inp ->
//             if not expressionEvaluatable
//             then printfn "state: %A, Expression Inputs: %A, Evaluated Nets: %A" expressionEvaluatable exprInputs evaluatedNets |> fun _ -> false
//             else printfn "state: %A, Expression Inputs: %A, Evaluated Nets: %A, List contains: %A" expressionEvaluatable exprInputs evaluatedNets (List.contains inp evaluatedNets) |> fun _ -> (List.contains inp evaluatedNets) )

// let evaluatableExpressions evaluatedNets exprToEvaluate = List.filter (fun (_, _, inpLst) -> canEvalExpression inpLst evaluatedNets) exprToEvaluate

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
   //  let evalNets = formEvalNets testTLogic
   //  let evalNetsInputAssigned = assignInputValues testInputs evalNets
   // //  let test = evaluatableExpressions testTLogic.Inputs testTLogic.ExpressionList
   //  let evalNetsExprsEvaled = evaluateExprLst testTLogic.ExpressionList testTLogic.Inputs evalNetsInputAssigned
    let outputNets = evaluateModuleWithInputs testTLogic testInputs
    printfn "%A" outputNets
    Console.ReadKey() |> ignore
    0 // return an integer exit code
