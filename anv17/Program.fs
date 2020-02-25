// Learn more about F# at http://fsharp.org

open System
open SharedTypes
open CombEval
open EvalNetHelper


let testTLogic = { 
   Name = "bus_and"
   ExpressionList =
                  [(And, [{ Name = "c"
                            SliceIndices = None }],
                    [{ Name = "a"
                       SliceIndices = Some (3, Some 1) };
                     { Name = "b"
                       SliceIndices = Some (3, Some 1) }]);
                   (And, [{ Name = "d"
                            SliceIndices = None }],
                    [{ Name = "a"
                       SliceIndices = Some (0, None) };
                     { Name = "b"
                       SliceIndices = Some (0, None) }])]
   Inputs =
          [{ Name = "a"
             SliceIndices = Some (3, Some 0) };
           { Name = "b"
             SliceIndices = Some (3, Some 0) }]
   Outputs = [{ Name = "c";SliceIndices = Some (2, Some 0) }; { Name = "d"; SliceIndices = None }]
   Wires = [] 
  
  }

let busFromInt num busLength =
   intToLogicLevelList num [] 
   |> padLogicLvlListToLength busLength
   |> List.mapi (fun i el -> (i, el))
   |> Map
   |> Bus

let testInputs = Map [
   ({ Name = "a"; SliceIndices = Some (3, Some 0) }, busFromInt 3 4);
   ({ Name = "b"; SliceIndices = Some (3, Some 0) }, busFromInt 2 4)
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
