module Tests
open SharedTypes
open Expecto
open CombEval



type TopLevelTestRec= {
    TestName: string
    Module:TLogic
    Inputs: Map<NetIdentifier, Net>
    ExpectedOutputs: Map<NetIdentifier, Net>
}

let testModules = [
    {
        Name = "SimpleAND"
        ExpressionList = [(And, [{Name= "c"; SliceIndices = None}],[{Name = "a"; SliceIndices = None}; {Name = "b"; SliceIndices = None}] )]
        Inputs = [{Name = "a"; SliceIndices = None}; {Name = "b"; SliceIndices = None}]
        Outputs = [{Name= "c"; SliceIndices = None}]
        Wires = []
    };

    {
        Name = "SimpleOR"
        ExpressionList = [(Or, [{Name= "c"; SliceIndices = None}],[{Name = "a"; SliceIndices = None}; {Name = "b"; SliceIndices = None}] )]
        Inputs = [{Name = "a"; SliceIndices = None}; {Name = "b"; SliceIndices = None}]
        Outputs = [{Name= "c"; SliceIndices = None}]
        Wires = []
    };

    { 
        Name = "FSMNextState"
        ExpressionList =
                  [(Not, [{ Name = "notCurrstate"; SliceIndices = None }], [{ Name = "currState"; SliceIndices = None }]);
                   (And, [{ Name = "nextState"
                            SliceIndices = Some (0, None) }],
                    [{ Name = "notCurrstate"
                       SliceIndices = Some (0, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (1, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (2, None) }]);
                   (And, [{ Name = "nextState"
                            SliceIndices = Some (1, None) }],
                    [{ Name = "notCurrstate"
                       SliceIndices = Some (0, None) };
                     { Name = "currState"
                       SliceIndices = Some (1, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (2, None) }]);
                   (And, [{ Name = "fetch"
                            SliceIndices = None }],
                    [{ Name = "notCurrstate"
                       SliceIndices = Some (0, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (1, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (2, None) }]);
                   (And, [{ Name = "exec1"
                            SliceIndices = None }],
                    [{ Name = "currState"
                       SliceIndices = Some (0, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (1, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (2, None) }]);
                   (And, [{ Name = "exec2"
                            SliceIndices = None }],
                    [{ Name = "notCurrstate"
                       SliceIndices = Some (0, None) };
                     { Name = "currstate"
                       SliceIndices = Some (1, None) };
                     { Name = "notCurrstate"
                       SliceIndices = Some (2, None) }])]
        Inputs = [{ Name = "currState";
              SliceIndices = Some (2, Some 0) }]
        Outputs =
           [{ Name = "nextState"
              SliceIndices = Some (2, Some 0) }; { Name = "fetch"
                                                   SliceIndices = None };
            { Name = "exec1"
              SliceIndices = None }; { Name = "exec2"
                                       SliceIndices = None }]
        Wires = [{ Name = "notCurrstate";
             SliceIndices = Some (2, Some 0) }] }


]
  

let testCases = [

    {
        TestName = "test1"
        Module = testModules.[0]
        Inputs = Map [
            ({Name = "a"; SliceIndices =None}, [0,Low] |> Map |> Wire);
            ({Name = "b"; SliceIndices =None}, [0,High] |> Map |> Wire);
        ]

        ExpectedOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,Low] |> Map |> Wire)
        ]
    }
    

    {
        TestName = "test2"
        Module = testModules.[0]

        Inputs = Map [
            ({Name = "a"; SliceIndices =None}, [0,High] |> Map |> Wire);
            ({Name = "b"; SliceIndices =None}, [0,High] |> Map |> Wire);
        ]

        ExpectedOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,High] |> Map |> Wire)
        ]
    }

    {
        TestName = "test3"
        Module = testModules.[1]

        Inputs = Map [
            ({Name = "a"; SliceIndices =None}, [0,Low] |> Map |> Wire);
            ({Name = "b"; SliceIndices =None}, [0,High] |> Map |> Wire);
        ]

        ExpectedOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,High] |> Map |> Wire)
        ]
    }

    {
        TestName = "test4"
        Module = testModules.[1]

        Inputs = Map [
            ({Name = "a"; SliceIndices =None}, [0,Low] |> Map |> Wire);
            ({Name = "b"; SliceIndices =None}, [0,Low] |> Map |> Wire);
        ]

        ExpectedOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,Low] |> Map |> Wire)
        ]
    }

    {
        TestName = "test5"
        Module = testModules.[2]

        Inputs = Map [
            ({Name = "currState"; SliceIndices = Some(2, Some 0)}, [(0,Low); (1, Low); (2, Low)] |> Map |> Bus);
        ]

        ExpectedOutputs = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0,High); (1, Low); (2, Low)] |> Map |> Bus);
            ({Name = "exec"; SliceIndices = None}, [(0,High)] |> Map |> Wire);
            ({Name = "exec2"; SliceIndices = None}, [(0,Low)] |> Map |> Wire);
        ]
    }
]



let formEvalExprTest (testCaseRec:TopLevelTestRec)  =
    testCase (sprintf "Testing evalModuleWithInputs, test case %s" testCaseRec.TestName) <| (fun () ->
        let testDescriptor = sprintf "Testing EvalExprLst with Module %A and Inputs %A. Expecting outputs %A" testCaseRec.Module testCaseRec.Inputs testCaseRec.ExpectedOutputs
        Expect.equal (evaluateModuleWithInputs testCaseRec.Module testCaseRec.Inputs) testCaseRec.ExpectedOutputs testDescriptor)

let evalExprTestLst = testList "evalExpr Tests" (List.map formEvalExprTest testCases)



