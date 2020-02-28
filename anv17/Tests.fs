module Tests
open SharedTypes
open Expecto
open CombEval
open LogicOperations
open EvalTypes



let concatenationTests = 

    let allNets = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0, Some High); (1, Some Low); (2, Some Low)] |> Map |> EvalBus);
            ({Name = "exec1"; SliceIndices = None}, [(0, Some Low)] |> Map |> EvalWire);
            ({Name = "exec2"; SliceIndices = None}, [(0, Some Low)] |> Map |> EvalWire);
            ({Name = "fetch"; SliceIndices = None }, [(0,Some High)] |> Map |> EvalWire);
    ]
    let concatTestDescriptor = "Testing concatenation opertator in LogicOperations.fs"
    let revConcatTestDescriptor = "Testing reverse concatenation opertator in LogicOperations.fs"

    testList "Concatenation Tests" [
    testCase "Concatenation operator test 1" <| (fun () ->
    
    let concatInputs = 
        [{ Name = "nextState"; SliceIndices = Some (2, Some 0) }; 
        { Name = "fetch"; SliceIndices = None };
        { Name = "exec1"; SliceIndices = None }; 
        { Name = "exec2"; SliceIndices = None }]

    let expected = [Low; Low; High; High; Low; Low] |> List.mapi (fun i el -> (i, Some el)) |> Map
    Expect.equal (ConcatOpNet concatInputs allNets) expected concatTestDescriptor ); 

    testCase "Concatenation operator test 2" <| (fun () ->
    let concatInputs = 
        [{ Name = "fetch"; SliceIndices = None };   
        { Name = "nextState"; SliceIndices = Some (2, Some 0) };
        { Name = "exec1"; SliceIndices = None }; 
        { Name = "exec2"; SliceIndices = None }]

    let expected = [Low; Low; High; Low; Low; High] |> List.mapi (fun i el -> (i, Some el)) |> Map
    Expect.equal (ConcatOpNet concatInputs allNets) expected concatTestDescriptor ); 

    testCase "Reverse Concatenation operator test 1" <| (fun () ->
    let concatInputs = 
        [{ Name = "fetch"; SliceIndices = None };   
        { Name = "nextState"; SliceIndices = Some (2, Some 0) };
        { Name = "exec1"; SliceIndices = None }; 
        { Name = "exec2"; SliceIndices = None }]

    let concatenatedNet = [Low; Low; High; Low; Low; High] |> List.mapi (fun i el -> (i, Some el)) |> Map |> EvalBus
    let expected = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0, Some High); (1, Some Low); (2, Some Low)] |> Map |> EvalBus);
            ({Name = "exec1"; SliceIndices = None}, [(0, Some Low)] |> Map |> EvalWire);
            ({Name = "exec2"; SliceIndices = None}, [(0, Some Low)] |> Map |> EvalWire);
            ({Name = "fetch"; SliceIndices = None }, [(0,Some High)] |> Map |> EvalWire);
    ]
    Expect.equal (reverseConcat concatenatedNet concatInputs allNets) expected revConcatTestDescriptor); 

    testCase "Reverse Concatenation operator test 2" <| (fun () ->
    let concatInputs = 
        [{ Name = "nextState"; SliceIndices = Some (2, Some 0) };
        { Name = "exec1"; SliceIndices = None }; 
        { Name = "fetch"; SliceIndices = None };           
        { Name = "exec2"; SliceIndices = None }]

    let concatenatedNet = [High; Low; High; Low; High; High] |> List.mapi (fun i el -> (i, Some el)) |> Map |> EvalBus
    let expected = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0, Some Low); (1, Some High); (2, Some High)] |> Map |> EvalBus);
            ({Name = "exec1"; SliceIndices = None}, [(0, Some High)] |> Map |> EvalWire);
            ({Name = "exec2"; SliceIndices = None}, [(0, Some High)] |> Map |> EvalWire);
            ({Name = "fetch"; SliceIndices = None }, [(0,Some Low)] |> Map |> EvalWire);
    ]
    Expect.equal (reverseConcat concatenatedNet concatInputs allNets) expected revConcatTestDescriptor); 

]



//****Top Level function Tests*****
type TopLevelTestRec= {
    TestName: string
    Module:TLogic
    Inputs: Map<NetIdentifier, Net>
    CurrOutputs: Map<NetIdentifier, Net>
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
                     { Name = "currState"
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

        CurrOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,Low] |> Map |> Wire)
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

        CurrOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,Low] |> Map |> Wire)
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

        CurrOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,Low] |> Map |> Wire)
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

        CurrOutputs = Map [
            ({Name = "c"; SliceIndices =None}, [0,Low] |> Map |> Wire)
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

        CurrOutputs = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0,Low); (1, Low); (2, Low)] |> Map |> Bus);
            ({Name = "exec1"; SliceIndices = None}, [(0,Low)] |> Map |> Wire);
            ({Name = "exec2"; SliceIndices = None}, [(0,Low)] |> Map |> Wire);
            ({Name = "fetch"; SliceIndices = None }, [(0,Low)] |> Map |> Wire);
        ]

        ExpectedOutputs = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0,High); (1, Low); (2, Low)] |> Map |> Bus);
            ({Name = "exec1"; SliceIndices = None}, [(0, Low)] |> Map |> Wire);
            ({Name = "exec2"; SliceIndices = None}, [(0,Low)] |> Map |> Wire);
            ({Name = "fetch"; SliceIndices = None }, [(0,High)] |> Map |> Wire);
        ]
    }
]



let formEvalExprTest (testCaseRec:TopLevelTestRec)  =
    testCase (sprintf "Testing evalModuleWithInputs, test case %s" testCaseRec.TestName) <| (fun () ->
        let testDescriptor = sprintf "Testing EvalExprLst with Module %A and Inputs %A. Expecting outputs %A" testCaseRec.Module testCaseRec.Inputs testCaseRec.ExpectedOutputs
        Expect.equal (evaluateModuleWithInputs testCaseRec.Module testCaseRec.Inputs testCaseRec.CurrOutputs) testCaseRec.ExpectedOutputs testDescriptor)

let evalExprTestLst = testList "evalExpr Tests" (List.map formEvalExprTest testCases)

