module ExampleTypes
open SharedTypes
open SimulationTypes
open Helper

// Module which contains all examples used in Expecto Tests


let gNetLstA : GeneralNet list =
    [false, ("A0", Wire (Map [0, Low]));
    false, ("A1", Wire (Map [0, Low]))
    ]

let mapA : Map<NetIdentifier,Net> =
    Map [
        { Name = "A0"; SliceIndices = None }, Wire (Map [0, Low]);
        { Name = "A1"; SliceIndices = None }, Wire (Map [0, Low]);
    ]

let gNetLstB : GeneralNet list = 
    [false, ("B0", Bus (Map [0, Low; 1, Low; 2, High; 3,High]));
    false, ("B1", Bus (Map [3, Low; 4, High; 5, Low]));
    false, ("B2", Bus (Map [5, High]));
    ]

let mapB : Map<NetIdentifier,Net> =
    Map [
        { Name = "B0"; SliceIndices = Some(3, Some 0) }, Bus (Map [0, Low; 1, Low; 2, High; 3,High]);
        { Name = "B1"; SliceIndices = Some(5, Some 3) }, Bus (Map [3, Low; 4, High; 5, Low]);
        { Name = "B2"; SliceIndices = Some(5, None) }, Bus (Map [5, High]);
    ]

let mapC1 = 
    Map [
        { Name = "C0"; SliceIndices = None }, Wire (Map [0, Low]);
        { Name = "C1"; SliceIndices = Some(5, Some 3) }, Bus (Map [3, Low; 4, High; 5, Low]);
        { Name = "C2"; SliceIndices = Some(5, None) }, Bus (Map [5, High]);
        { Name = "D0"; SliceIndices = Some(5, None) }, Bus (Map [5, High]);
    ]

let mapC2 = 
    Map [
        { Name = "C0"; SliceIndices = None }, Wire (Map [0, High]);
        { Name = "C1"; SliceIndices = Some(5, Some 3) }, Bus (Map [3, High; 4, High; 5, Low]);
        { Name = "C2"; SliceIndices = Some(5, None) }, Bus (Map [5, Low]);
        { Name = "A0"; SliceIndices = None }, Wire (Map [0, High]);
    ]

let mapC3 = 
    Map [
        { Name = "C0"; SliceIndices = None }, Wire (Map [0, High]);
        { Name = "C1"; SliceIndices = Some(5, Some 3) }, Bus (Map [3, High; 4, High; 5, Low]);
        { Name = "C2"; SliceIndices = Some(5, None) }, Bus (Map [5, Low]);
        { Name = "A0"; SliceIndices = None }, Wire (Map [0, High]);
        { Name = "D0"; SliceIndices = Some(5, None) }, Bus (Map [5, High]);
    ]

let bLstEx: Block list =
    [Name "A", mapA, mapA;
    Name "DFF", mapA, mapA;
    Name "DFF", mapA, mapA;
    Name "B", mapA, mapA]

let bLstSeperated: Block list * Block list =
    [Name "DFF", mapA, mapA; Name "DFF", mapA, mapA],
    [Name "A", mapA, mapA; Name "B", mapA, mapA] 
    

let andIn: Map<NetIdentifier,Net> = 
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]) ] 

let andOut =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map[0,Low]) ]

let andOutput = 
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map[0,High]) ]

let orIn: Map<NetIdentifier,Net> = 
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, Low]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]) ] 

let orOut =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map[0,Low]) ]

let orOutput = 
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map[0,High]) ]

let notIn =
    Map [{ Name = "in"; SliceIndices = None}, Wire (Map[0,High]) ]

let notOut =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map[0,High]) ]

let notOutput =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map[0,Low]) ]


// tLogics for tests 
let tLogicEx1 : TLogic= { 
    Name = "bus_and"
    ExpressionList =
        [(And, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = Some (3, Some 1) };
            { Name = "b"; SliceIndices = Some (3, Some 1) }]);
        (And, [{ Name = "d"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = Some (0, None) };
            { Name = "b"; SliceIndices = Some (0, None) }])]
    Inputs = [{ Name = "a"; SliceIndices = Some (3, Some 1) };
            { Name = "b"; SliceIndices = Some (3, Some 1) }]
    Outputs = [{ Name = "c"; SliceIndices = None }]
    Wires = [] }

let tLogicEx2 : TLogic= { 
    Name = "simpAND"
    ExpressionList =
        [(And, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None };
            { Name = "b"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None };
        { Name = "b"; SliceIndices = None }]
    Outputs = [{ Name = "c"; SliceIndices = None }]
    Wires = [] }

let tLogicEx3 : TLogic= { 
    Name = "simpOR"
    ExpressionList =
        [(Or, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None };
            { Name = "b"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None };
        { Name = "b"; SliceIndices = None }]
    Outputs = [{ Name = "c"; SliceIndices = None }]
    Wires = [] }

let tLogicEx4 : TLogic= { 
    Name = "simpNOT"
    ExpressionList =
        [(Not, 
            [{ Name = "b"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None }]
    Outputs = [{ Name = "b"; SliceIndices = None }]
    Wires = [] }

let tLogicLstEx = [tLogicEx1; tLogicEx2; tLogicEx3; tLogicEx4]


// test circuits (bLsts)

// Circuit 1
// two AND blocks 
// Logic levels in block lists don't matter
let c1initMap =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b1"; SliceIndices = None}, Wire (Map [0, High])] 

let c1and1In = 
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]) ] 

let c1and1Out =
    Map [{ Name = "b0"; SliceIndices = None}, Wire (Map [0, High])] 

let c1and2In = 
    Map [{ Name = "b0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b1"; SliceIndices = None}, Wire (Map [0, High]) ] 

let c1and2Out =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map [0, High])] 


let testCircuit1 : Block list =
    [Name "simpAND", c1and1In, c1and1Out;
    Name "simpAND", c1and2In, c1and2Out]

let c1mapOfVals =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b1"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "out"; SliceIndices = None}, Wire (Map [0, High]);
    ] 

// Circuit 2
// 1 OR gate feeding into 1 AND gate
// note that the values of nets in block lists don't matter
let c2initMap =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, Low]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b1"; SliceIndices = None}, Wire (Map [0, High])] 

let c2or1In = 
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]) ] 

let c2or1Out =
    Map [{ Name = "b0"; SliceIndices = None}, Wire (Map [0, High])] 

let c2and1In = 
    Map [{ Name = "b0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b1"; SliceIndices = None}, Wire (Map [0, High]) ] 

let c2and1Out =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map [0, High])] 


// 1 OR gate feeding into 1 AND gate
let testCircuit2 : Block list =
    [Name "simpAND", c2and1In, c2and1Out;
    Name "simpOR", c2or1In, c2or1Out]

let c2mapOfVals =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, Low]);
    { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b0"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "b1"; SliceIndices = None}, Wire (Map [0, High]);
    { Name = "out"; SliceIndices = None}, Wire (Map [0, High]);
    ] 

// dffUpdate test
let dffInWire =
    Map [{ Name = "dffIn"; SliceIndices = None}, Wire (Map [0, High])]

let dffOutWire =
    Map [{ Name = "dffOut"; SliceIndices = None}, Wire (Map [0, Low])]

let dffOutputWire = 
    Map [{ Name = "dffOut"; SliceIndices = None}, Wire (Map [0, High])]

let dffInBus =
    Map [{ Name = "dffIn"; SliceIndices = Some(2, Some 0)}, Bus (Map [0, Low; 1, Low; 2, High])]

let dffOutBus =
    Map [{ Name = "dffIn"; SliceIndices = Some(2, Some 0)}, Bus (Map [0, High; 1, High; 2, High])]

let dffOutputBus = 
    Map [{ Name = "dffIn"; SliceIndices = Some(2, Some 0)}, Bus (Map [0, Low; 1, Low; 2, High])]

// synchronous evaluation test (to make sure everything evaluated in parallel)

// DFF cascade circuit
// 3 dff in cascade 
let cascadeInitMap =
    Map [{ Name = "d1"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "d2"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "d3"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, High]);
        ] 

// outputs don't actually matter 
let cascadeDFF1In =
    Map[{ Name = "d1"; SliceIndices = None}, Wire (Map [0, Low])]

let cascadeDFF1Out =
    Map[{ Name = "d2"; SliceIndices = None}, Wire (Map [0, Low])]

let cascadeDFF2In =
    Map[{ Name = "d2"; SliceIndices = None}, Wire (Map [0, High])]

let cascadeDFF2Out =
    Map[{ Name = "d3"; SliceIndices = None}, Wire (Map [0, Low])]

let cascadeDFF3In =
    Map[{ Name = "d3"; SliceIndices = None}, Wire (Map [0, Low])]

let cascadeDFF3Out =
    Map[{ Name = "out"; SliceIndices = None}, Wire (Map [0, Low])]

let cascadeBLst =
    [Name "DFF", cascadeDFF1In, cascadeDFF1Out;
    Name "DFF", cascadeDFF2In, cascadeDFF2Out;
    Name "DFF", cascadeDFF3In, cascadeDFF3Out]

let cascadeOutput =
    Map [{ Name = "d2"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "d3"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, Low]);
        ] 
        
// both asynchronous and synchronous blocks test

// Circuit 3
// example with output of AND gate feeding into 2 DFFs
let c3InitMap =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "c"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, High])
        ] 

// expected Map of Vals (after asynchronous evaluation)
let c3MapOfVals =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "b"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "c"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, High])
        ] 

// expected Next state
let c3NextState =
    Map [{ Name = "c"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, Low])
        ]

let c3Output =
    c3MapOfVals, c3NextState


let c3and1In: GeneralNet list =
    [false, ("a0", Wire(Map [0, High]));
    false, ("a1", Wire(Map [0, Low]))]

let c3and1Out: GeneralNet list =
    [false, ("b", Wire(Map [0, High]))]

let c3dff1In: GeneralNet list =
    [false, ("b", Wire(Map [0, High]))]

let c3dff1Out: GeneralNet list =
    [true, ("c", Wire(Map [0, High]))]

let c3dff2In: GeneralNet list =
    [true, ("c", Wire(Map [0, High]))]

let c3dff2Out: GeneralNet list =
    [true, ("out", Wire(Map [0, High]))]


let c3CLst :Connection list =
    [Name "simpAND", c3and1In, c3and1Out;
    Name "DFF", c3dff1In, c3dff1Out;
    Name "DFF", c3dff2In, c3dff2Out]

let c3BLst : Block list =
    [Name "simpAND", gLstToMap c3and1In, gLstToMap c3and1Out;
    Name "DFF", gLstToMap c3dff1In, gLstToMap c3dff1Out;
    Name "DFF", gLstToMap c3dff2In, gLstToMap c3dff2Out]

// list of inputs into circuit 3 (multiple cycle simulation)
let c3InputLstofLst : GeneralNet list list =
    [
        [false, ("a0", Wire(Map [0, High]));
        false, ("a1", Wire(Map [0, High]))];

        [false, ("a0", Wire(Map [0, Low]));
        false, ("a1", Wire(Map [0, High]))]

        [false, ("a0", Wire(Map [0, High]));
        false, ("a1", Wire(Map [0, Low]))]

        [false, ("a0", Wire(Map [0, High]));
        false, ("a1", Wire(Map [0, High]))]

    ]

// expected result from simulating multiple clock cycles (final synchronous states)
let c3MultipleOutput =
    Map [{ Name = "c"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, Low])
        ]

let c3AsyncBLst: Block list =
    [Name "simpAND", gLstToMap c3and1In, gLstToMap c3and1Out]

let c3SyncBLst: Block list = 
    [Name "DFF", gLstToMap c3dff1In, gLstToMap c3dff1Out;
    Name "DFF", gLstToMap c3dff2In, gLstToMap c3dff2Out]


/// mixed example with asynchronous and synchronous

// Circuit 4
/// example with output of AND gate feeding into 2 DFFs
let c4InitMap =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "d"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "b1"; SliceIndices = None}, Wire (Map [0, Low])
        ] 

// expected Map of Vals (after asynchronous evaluation)
let c4MapOfVals =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "a1"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "b0"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "b1"; SliceIndices = None}, Wire (Map [0, Low]);
        { Name = "d"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "out"; SliceIndices = None}, Wire (Map [0, Low])
        ] 

// expected Next state
let c4NextState =
    Map [{ Name = "b1"; SliceIndices = None}, Wire (Map [0, High])
        ]

//expected output of advanceState Test
let c4Output =
    c4MapOfVals, c4NextState


// Logic levels in bLst/cLst don't actually matter
let c4or1In =
    [false, ("a0", Wire(Map [0, High]));
    false, ("a1", Wire(Map [0, Low]))];

let c4or1Out =
    [false, ("b0", Wire(Map [0, High]))]

let c4dff1In = 
    [false, ("d", Wire(Map [0, High]))]

let c4dff1Out =
   [true, ("b1", Wire(Map [0, High]))]


let c4and1In =
    [false, ("b0", Wire(Map [0, High]));
    false, ("b1", Wire(Map [0, Low]))];

let c4and1Out =
   [false, ("out", Wire(Map [0, High]))]

let c4CLst :Connection list =
    [Name "simpAND", c4and1In, c4and1Out;
    Name "DFF", c4dff1In, c4dff1Out;
    Name "simpOR", c4or1In, c4or1Out]

// does not have to be in order
let c4BLst :Block list =
    [Name "simpAND", gLstToMap c4and1In, gLstToMap c4and1Out;
    Name "DFF", gLstToMap c4dff1In, gLstToMap c4dff1Out;
    Name "simpOR", gLstToMap c4or1In, gLstToMap c4or1Out]

let c4AsyncBLst: Block list =
    [Name "simpAND", gLstToMap c4and1In, gLstToMap c4and1Out;
    Name "simpOR", gLstToMap c4or1In, gLstToMap c4or1Out]

let c4SyncBLst: Block list = 
    [Name "DFF", gLstToMap c4dff1In, gLstToMap c4dff1Out]

