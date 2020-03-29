module Tests
open SharedTypes
open Expecto
open CombEval
open LogicOperations
open EvalTypes

//*****Logic operations Concatenation functions tests*******

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

    let concatenatedNet = [Low; Low; High; Low; Low; High] |> List.mapi (fun i el -> (i, Some el)) |> Map 
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

    let concatenatedNet = [High; Low; High; Low; High; High] |> List.mapi (fun i el -> (i, Some el)) |> Map 
    let expected = Map [
            ({Name = "nextState"; SliceIndices = Some(2, Some 0)}, [(0, Some Low); (1, Some High); (2, Some High)] |> Map |> EvalBus);
            ({Name = "exec1"; SliceIndices = None}, [(0, Some High)] |> Map |> EvalWire);
            ({Name = "exec2"; SliceIndices = None}, [(0, Some High)] |> Map |> EvalWire);
            ({Name = "fetch"; SliceIndices = None }, [(0,Some Low)] |> Map |> EvalWire);
    ]
    Expect.equal (reverseConcat concatenatedNet concatInputs allNets) expected revConcatTestDescriptor); 

]

//*****Top level module pipeline function tests ********
let topLevelModuleTests = testList "Top Level module pipeline function tests" [

    testCase "formEvalNets test 1" <| (fun () ->
        let testMod =
           {
                Name = "TestMod"
                ExpressionList = [(And, [{Name= "c"; SliceIndices = None}],[{Name = "a"; SliceIndices = None}; {Name = "b"; SliceIndices = None}] )]
                Inputs = [{Name = "a"; SliceIndices = Some(2,Some 1)}; {Name = "b"; SliceIndices = Some(3, Some 2)}]
                Outputs = [{Name= "c"; SliceIndices = Some(1, Some 0)}]
                Wires = [
                    {Name = "tempNet"; SliceIndices = Some(1, Some 0)};
                    {Name = "tempNet1"; SliceIndices = None}
                ]
            };

        let expectedAllNetsMap = Map [
                ({Name = "a"; SliceIndices = Some(2,Some 1)}, [(2,None); (1, None)] |> Map |> EvalBus);
                ({Name = "b"; SliceIndices = Some(3, Some 2)}, [(3, None); (2, None)] |> Map |> EvalBus);
                ({Name= "c"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
                ({Name = "tempNet"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
                ({Name = "tempNet1"; SliceIndices = None},  [(0, None)] |> Map |> EvalWire)
        ]
        Expect.equal (formEvalNets testMod) expectedAllNetsMap "Testing Normal Case"
    )

    testCase "formEvalNets test 2" <| (fun () ->
        let testMod =
           {
                Name = "TestMod"
                ExpressionList = [(And, [{Name= "c"; SliceIndices = None}],[{Name = "a"; SliceIndices = None}; {Name = "b"; SliceIndices = None}] )]
                Inputs = [{Name = "a"; SliceIndices = Some(0,Some 0)}; {Name = "b"; SliceIndices = None}]
                Outputs = [{Name= "c"; SliceIndices = Some(0, Some 0)}]
                Wires = []
            };

        let expectedAllNetsMap = Map [
                ({Name = "a"; SliceIndices = Some(0, Some 0)}, [(0,None)] |> Map |> EvalBus);
                ({Name = "b"; SliceIndices = None}, [(0, None)] |> Map |> EvalWire);
                ({Name = "c"; SliceIndices = Some(0, Some 0)}, [(0,None)] |> Map |> EvalBus);
        ]
        Expect.equal (formEvalNets testMod) expectedAllNetsMap "Testing using multi wire busses to represent single wires"
    )

    testCase "assignInputValues test 1" <| (fun () ->
        let allNets = Map [
            ({Name = "a"; SliceIndices = Some(2,Some 1)}, [(2,None); (1, None)] |> Map |> EvalBus);
            ({Name = "b"; SliceIndices = Some(3, Some 2)}, [(3, None); (2, None)] |> Map |> EvalBus);
            ({Name= "c"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
        ]

        let inputIDs = [
            {Name = "a"; SliceIndices = Some(2,Some 1)};
            {Name = "b"; SliceIndices = Some(3, Some 2)};
        ]

        let inputValueMap = Map [
            (inputIDs.[0], Map [(1, Low); (2,High)] |> Bus);
            (inputIDs.[1], Map [(2, Low); (3,Low)] |> Bus);
        ]

        let expected = Map [
            ({Name = "a"; SliceIndices = Some(2,Some 1)}, [(2,Some High); (1, Some Low)] |> Map |> EvalBus);
            ({Name = "b"; SliceIndices = Some(3, Some 2)}, [(3, Some Low); (2, Some Low)] |> Map |> EvalBus);
            ({Name = "c"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
        ]


        Expect.equal (assignInputValues inputValueMap inputIDs allNets) expected "Testing Normal Case"
    )

    testCase "assignInputValues test 2" <| (fun () ->
        let allNets = Map [
            ({Name = "a"; SliceIndices = None}, [(0, None)] |> Map |> EvalWire);
            ({Name = "b"; SliceIndices = Some(2, Some 2)}, [(2, None)] |> Map |> EvalBus);
            ({Name = "c"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
        ]

        let inputIDs = [
            {Name = "a"; SliceIndices = None};
            {Name = "b"; SliceIndices = Some(2, Some 2)};
        ]

        let inputValueMap = Map [
            (inputIDs.[0], Map [(0, High)] |> Wire);
            (inputIDs.[1], Map [(2, High)] |> Bus);
            ({Name = "randomNet"; SliceIndices = None}, Map [(7, Low); (1,Low)] |> Bus);
        ]

        let expected = Map [
            ({Name = "a"; SliceIndices = None}, [(0, Some High)] |> Map |> EvalWire);
            ({Name = "b"; SliceIndices = Some(2, Some 2)}, [(2, Some High)] |> Map |> EvalBus);
            ({Name= "c"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
        ]


        Expect.equal (assignInputValues inputValueMap inputIDs allNets) expected "Testing using multi wire busses to represent single wires, as well as using EvalWires and with an irrelevant net in the input net value map"
    )

    testCase "formOutputNets test 1" <| (fun () ->
        let allNets = Map [
            ({Name = "a"; SliceIndices = None}, [(0, None)] |> Map |> EvalWire);
            ({Name = "b"; SliceIndices = Some(2, Some 2)}, [(2, None)] |> Map |> EvalBus);
            ({Name = "c"; SliceIndices = Some(1, Some 0)}, [(0,None); (1, None)] |> Map |> EvalBus);
        ]

        let outputIDs = [
            {Name = "c"; SliceIndices = Some(1, Some 0)};
        ]

        let oldOutputs = Map [
            (outputIDs.[0], Map [(0, High); (1, Low)] |> Bus);
        ]

        let expected = Map [
            (outputIDs.[0], [(0, High); (1, Low)] |> Map |> Bus);
        ]


        Expect.equal (formOutputNets outputIDs oldOutputs allNets) expected "Testing assignment of default outputs"
    )

    testCase "formOutputNets test 2" <| (fun () ->
        let allNets = Map [
            ({Name = "a"; SliceIndices = None}, [(0, None)] |> Map |> EvalWire);
            ({Name = "b"; SliceIndices = Some(2, Some 2)}, [(2, None)] |> Map |> EvalBus);
            ({Name = "c"; SliceIndices = Some(1, Some 0)}, [(0, Some Low); (1, Some High)] |> Map |> EvalBus);
        ]

        let outputIDs = [
            {Name = "c"; SliceIndices = Some(1, Some 0)};
        ]

        let oldOutputs = Map [
            (outputIDs.[0], Map [(0, High); (1, Low)] |> Bus);
        ]

        let expected = Map [
            (outputIDs.[0], [(0, Low); (1, High)] |> Map |> Bus);
        ]


        Expect.equal (formOutputNets outputIDs oldOutputs allNets) expected "Testing normal case"
    )

    testCase "formOutputNets test 3" <| (fun () ->
        let allNets = Map [
            ({Name = "a"; SliceIndices = None}, [(0, None)] |> Map |> EvalWire);
            ({Name = "b"; SliceIndices = Some(2, Some 2)}, [(2, None)] |> Map |> EvalBus);
            ({Name = "c"; SliceIndices = Some(0, Some 0)}, [(0, Some High)] |> Map |> EvalBus);
        ]

        let outputIDs = [
            {Name = "c"; SliceIndices = Some(0, Some 0)};
        ]

        let oldOutputs = Map [
            (outputIDs.[0], Map [(0, High)] |> Bus);
        ]

        let expected = Map [
            (outputIDs.[0], [(0, High)] |> Map |> Bus);
        ]


        Expect.equal (formOutputNets outputIDs oldOutputs allNets) expected "Testing using multi wire busses to represent single wires"
    )

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

    {
        Name = "OutputConcatAND"
        Inputs = [
            {
                Name = "a"
                SliceIndices = Some(3, Some 0)
            }

            {
                Name = "b"
                SliceIndices = Some(3, Some 0)
            }
        ]

        Outputs = [
            {
                Name = "d"
                SliceIndices = Some(2, Some 0)
            }

            {
                Name = "e"
                SliceIndices = None
            }
        ]

        ExpressionList = [
            (And,
                [
                    {
                        Name = "Net1"
                        SliceIndices = None
                    }
                ],
                [
                    {
                        Name = "a"
                        SliceIndices = None
                    };
                    {
                        Name = "b"
                        SliceIndices = None
                    }
                ]
            );
            (Concat, 
                [
                    {
                        Name = "Net1"
                        SliceIndices = None
                    }
                ],
                [
                    {
                        Name = "d"
                        SliceIndices = None
                    };
                    {
                        Name = "e"
                        SliceIndices = None
                    }
                ]
            )
        ]

        Wires = [
            {
                Name = "Net1"
                SliceIndices = Some(3, Some 0)
            }
        ]
    }

    {
        Name = "IOConcatNOT"
        Inputs = [
            {
                Name = "a"
                SliceIndices = Some(1, Some 0)
            };
            {
                Name = "b"
                SliceIndices = Some(2, Some 0)
            }
        ]

        Outputs = [
            {
                Name = "c"
                SliceIndices = Some(2, Some 0)
            }
            {
                Name = "d"
                SliceIndices = None
            }
        ]

        ExpressionList = [
            (Not,
                [
                    {
                        Name = "Net1"
                        SliceIndices = None
                    }
                ],
                [
                    {
                        Name = "Net2"
                        SliceIndices = None
                    }
                ]
            );

            (Concat,
                [
                    {
                        Name = "Net2"
                        SliceIndices = None
                    }
                ],
                [
                    {
                        Name = "a"
                        SliceIndices = Some(0, None)
                    };
                    {
                        Name = "b"
                        SliceIndices = Some(2, Some 1)
                    }
                ]
            );
            (Concat,
                [
                    {
                        Name = "Net1"
                        SliceIndices = None
                    }
                ],
                [
                    {
                        Name = "c"
                        SliceIndices = Some(1, Some 0)
                    };
                    {
                        Name = "d"
                        SliceIndices = None
                    }
                ]
            )               
        ]

        Wires = [
            {
                Name = "Net1"
                SliceIndices = Some(2, Some 0)
            };
            {
                Name = "Net2"
                SliceIndices = Some(2, Some 0)
            }
        ]
    };

    { 
        Name = "hex_to_7seg"
        ExpressionList =
            [
                (
                    Concat, 
                    [{ Name = "6"; SliceIndices = Some (0, Some 3) }],

                    [
                        {Name = "in"; SliceIndices = Some (3, Some 2)};
                        {Name = "in"; SliceIndices = Some (2, Some 1)}
                    ]
                );  
                (
                    And, 
                    [{ Name = "out"; SliceIndices = Some (6, None) }],
                    [
                        { Name = "in"; SliceIndices = Some (3, None) };
                        { Name = "in"; SliceIndices = Some (0, None) }
                    ]
                );
                (
                    And, 
                    [{ Name = "out"; SliceIndices = Some (5, None) }],
                    [
                        { Name = "3"; SliceIndices = None }; 
                        { Name = "4"; SliceIndices = None }
                    ]
                );
                (
                    Pass, 
                    [{ Name = "3"; SliceIndices = None }],
                    [{ Name = "in"; SliceIndices = Some (2, None) }]
                );
                (
                    Not, 
                    [{ Name = "4"; SliceIndices = None }], 
                    [{ Name = "5"; SliceIndices = None }]
                );
                (
                    Pass,
                    [{ Name = "5"; SliceIndices = None }],
                    [{ Name = "in"; SliceIndices = Some (3, None) }]
                );
                (
                    Or,
                    [{ Name = "out"; SliceIndices = Some (4, Some 1) }],
                    [
                        { Name = "0"; SliceIndices = None }; 
                        { Name = "1"; SliceIndices = None }
                    ]
                );
                (
                    Pass,
                    [{ Name = "0"; SliceIndices = None }],
                    [{ Name = "6"; SliceIndices = None }]
                );
                (
                    Pass, 
                    [{ Name = "1"; SliceIndices = None }],
                    [{ Name = "2"; SliceIndices = None }]
                );
                (
                    Pass, 
                    [{ Name = "2"; SliceIndices = None }],
                    [{ Name = "in"; SliceIndices = Some (3, Some 0) }]
                )
            ];
        Inputs = [{ Name = "in"; SliceIndices = Some (3, Some 0) }]
        Outputs = [{ Name = "out"; SliceIndices = Some (6, Some 0) }]
        Wires =
            [
                { Name = "6"; SliceIndices = Some (0, Some 3) }; 
                { Name = "4"; SliceIndices = None };
                { Name = "5"; SliceIndices = None }; 
                { Name = "3"; SliceIndices = None };
                { Name = "1"; SliceIndices = Some (4, Some 1) };
                { Name = "2"; SliceIndices = Some (4, Some 1) };
                { Name = "0"; SliceIndices = Some (4, Some 1) }
            ] 
    }     


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

    {
        TestName = "test6"
        Module = testModules.[3]
        Inputs = Map[
            ({Name = "a"; SliceIndices = Some(3, Some 0)}, [Low;Low;Low;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "b"; SliceIndices = Some(3, Some 0)}, [Low;Low;Low;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus)
        ]

        ExpectedOutputs = Map [
            ({Name = "d"; SliceIndices = Some(2, Some 0)}, [Low;Low;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "e"; SliceIndices = None}, [Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Wire)
        ]

        CurrOutputs = Map [
            ({Name = "d"; SliceIndices = Some(2, Some 0)}, [Low;Low;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "e"; SliceIndices = None}, [Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Wire)
        ]
    }

    {
        TestName = "test7"
        Module = testModules.[3]
        Inputs = Map[
            ({Name = "a"; SliceIndices = Some(3, Some 0)}, [High;High;High;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "b"; SliceIndices = Some(3, Some 0)}, [High;Low;Low;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus)
        ]

        ExpectedOutputs = Map [
            ({Name = "d"; SliceIndices = Some(2, Some 0)}, [Low;Low;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "e"; SliceIndices = None}, [High] |> List.mapi (fun i el -> (i,el)) |> Map |> Wire)
        ]

        CurrOutputs = Map [
            ({Name = "d"; SliceIndices = Some(2, Some 0)}, [Low;Low;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "e"; SliceIndices = None}, [Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Wire)
        ]
    }

    {
        TestName = "test8"
        Module = testModules.[4]
        Inputs = Map[
            ({Name = "a"; SliceIndices = Some(1, Some 0)}, [High;High] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "b"; SliceIndices = Some(2, Some 0)}, [High;Low;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus)
        ]

        ExpectedOutputs = Map [
            ({Name = "c"; SliceIndices = Some(2, Some 0)}, [High;Low;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "d"; SliceIndices = None}, [High] |> List.mapi (fun i el -> (i,el)) |> Map |> Wire)
        ]

        CurrOutputs = Map [
            ({Name = "c"; SliceIndices = Some(2, Some 0)}, [Low;Low;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
            ({Name = "d"; SliceIndices = None}, [Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Wire)
        ]
    }

    {
        TestName = "test9"
        Module = testModules.[5]
        Inputs = Map[
            ({ Name = "in"; SliceIndices = Some (3, Some 0) }, [Low;High;High;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
        ]

        ExpectedOutputs = Map [
            ({ Name = "out"; SliceIndices = Some (6, Some 0) }, [Low;High;High;High;Low;High;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
        ]

        CurrOutputs = Map [
            ({ Name = "out"; SliceIndices = Some (6, Some 0) }, [Low;Low;Low;Low;Low;Low;Low] |> List.mapi (fun i el -> (i,el)) |> Map |> Bus);
        ]
    }
]



let formEvalModuleTest (testCaseRec:TopLevelTestRec)  =
    testCase (sprintf "Testing evalModuleWithInputs, test case %s" testCaseRec.TestName) <| (fun () ->
        let testDescriptor = sprintf "Testing EvalExprLst with Module %A and Inputs %A. Expecting outputs %A" testCaseRec.Module testCaseRec.Inputs testCaseRec.ExpectedOutputs
        Expect.equal (evaluateModuleWithInputs testCaseRec.Module testCaseRec.Inputs testCaseRec.CurrOutputs) testCaseRec.ExpectedOutputs testDescriptor)

let evalModuleLst = testList "Top level function Tests" (List.map formEvalModuleTest testCases)

