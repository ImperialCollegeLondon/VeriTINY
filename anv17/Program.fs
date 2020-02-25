// Learn more about F# at http://fsharp.org

open System
open SharedTypes
open CombEval

let testTLogic = 
    { Name = "mux2_gate23"
      ExpressionList =
                      [(Not, [{ Name = "selb"
                                SliceIndices = None }], [{ Name = "sel"
                                                           SliceIndices = None }]);
                       (And, [{ Name = "out1"
                                SliceIndices = None }],
                        [{ Name = "a"
                           SliceIndices = None }; { Name = "sel"
                                                    SliceIndices = None }]);
                       (And, [{ Name = "out2"
                                SliceIndices = None }],
                        [{ Name = "b"
                           SliceIndices = None }; { Name = "selb"
                                                    SliceIndices = None }]);
                       (Or, [{ Name = "out"
                               SliceIndices = Some (0, None) }],
                        [{ Name = "out1"
                           SliceIndices = None }; { Name = "out2"
                                                    SliceIndices = None }]);
                       (Not, [{ Name = "out"
                                SliceIndices = Some (1, None) }],
                        [{ Name = "out"
                           SliceIndices = Some (0, None) }]);
                       (And, [{ Name = "out"
                                SliceIndices = Some (2, Some 0) }],
                        [{ Name = "out"
                           SliceIndices = Some (2, Some 0) }])]
      Inputs =
              [{ Name = "a"
                 SliceIndices = None }; { Name = "b"
                                          SliceIndices = None };
               { Name = "sel"
                 SliceIndices = None }]
      Outputs = [{ Name = "out"
                   SliceIndices = Some (2, Some 0) }]
      Wires =
             [{ Name = "out1"
                SliceIndices = None }; { Name = "out2"
                                         SliceIndices = None };
              { Name = "selb"
                SliceIndices = None }] }

let 

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let evalNets = formEvalNets testTLogic
    printfn "%A" evalNets
    Console.ReadKey() |> ignore
    0 // return an integer exit code
