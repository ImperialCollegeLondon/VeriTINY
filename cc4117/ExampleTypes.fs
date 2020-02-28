module ExampleTypes
open SharedTypes
open SimulationTypes

// GeneralNet = bool * NamedNet
// NamedNet = string * Net
// Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>
// test purposes



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

// two AND blocks 
// note that the values of nets in block lists don't matter
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

/// example with output of AND gate feeding into 2 DFFs
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


// values in bLst don't actually matter
let c3and1In =
    Map [{ Name = "a0"; SliceIndices = None}, Wire (Map [0, High]);
        { Name = "a1"; SliceIndices = None}, Wire (Map [0, High])
        ] 


let c3and1Out =
    Map [{ Name = "b"; SliceIndices = None}, Wire (Map [0, High])
        ] 

let c3dff1In = 
    Map [{ Name = "b"; SliceIndices = None}, Wire (Map [0, High])]

let c3dff1Out =
    Map [{ Name = "c"; SliceIndices = None}, Wire (Map [0, High])]


let c3dff2In =
    Map [{ Name = "c"; SliceIndices = None}, Wire (Map [0, High])]

let c3dff2Out =
    Map [{ Name = "out"; SliceIndices = None}, Wire (Map [0, High])]

   
let c3BLst :Block list =
    [Name "simpAND", c3and1In, c3and1Out;
    Name "DFF", c3dff1In, c3dff1Out;
    Name "DFF", c3dff2In, c3dff2Out]

let c3AsyncBLst =
    [Name "simpAND", c3and1In, c3and1Out]

let c3SyncBLst = 
    [Name "DFF", c3dff1In, c3dff1Out;
    Name "DFF", c3dff2In, c3dff2Out]


let testInputsLstofLst : GeneralNet list list =
    [
        [false, ("a0", Wire(Map [0, High]));
        false, ("a1", Wire(Map [0, High]))];

        [false, ("a0", Wire(Map [0, Low]));
        false, ("a1", Wire(Map [0, High]))]
    ]


/// mixed example with asynchronous and synchronous

let aIn =  
    [false, ("a0", Wire(Map [0, High]));
    false, ("a1", Wire(Map [0, High]));
    ]

let aOut =  
    [false, ("c0", Wire(Map [0, High]));
    ]

let bIn = 
    [false, ("b0", Wire(Map [0, High]));
    false, ("b1", Wire(Map [0, High]));
    ]

let bOut =  
    [false, ("c2", Wire(Map [0, High]));
    ]

let dIn1 = 
    [false, ("d1", Wire(Map [0, High]))]

let dOut1 =
    [true, ("c1", Wire(Map [0, High]))]

let dIn2 = 
    [false, ("d2", Wire(Map [0, High]))]

let dOut2 =
    [true, ("b0", Wire(Map [0, High]))]

let dIn3 = 
    [false, ("d3", Wire(Map [0, High]))]

let dOut3 =
    [true, ("e0", Wire(Map [0, High]))]

let cIn = 
    [false, ("c0", Wire(Map [0, High]));
    true, ("c1", Wire(Map [0, High]));
    false, ("c2", Wire(Map [0, High]))
    ]
 
let cOut =
    [false, ("d3", Wire(Map [0, High]))]

let eIn = 
    [true, ("e0", Wire(Map [0, High]));
    false, ("e1", Wire(Map [0, High]))
    ]

let eOut =
    [false, ("out", Wire(Map [0, High]))]

let knownInputs = aIn @ dIn1 @ dIn2 @ [false, ("b1", Wire(Map [0, High]))] @ [false, ("e1", Wire(Map [0, High]))]

let newCLst : Connection List =
    [Name "A", aIn, aOut;
    Name "B", bIn, bOut;
    Name "DFF", dIn1, dOut1;
    Name "DFF", dIn2, dOut2;
    Name "DFF", dIn3, dOut3;
    Name "C", cIn, cOut;
    Name "E", eIn, eOut]


let simpSim =
    [
    // cycle 1
    [false, ("a0", Wire(Map [0, Low]));
    false, ("d1", Wire(Map [0, High]))];
    // cycle 2
    [false, ("a0", Wire(Map [0, Low]));
    false, ("d1", Wire(Map [0, Low]))]
    // cycle 3
    [false, ("a0", Wire(Map [0, High]));
    false, ("d1", Wire(Map [0, High]))];
    // cycle 4
    [false, ("a0", Wire(Map [0, High]));
    false, ("d1", Wire(Map [0, Low]))]
    // cycle 5
    [false, ("a0", Wire(Map [0, High]));
    false, ("d1", Wire(Map [0, Low]))]
    ]