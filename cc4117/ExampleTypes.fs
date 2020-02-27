module ExampleTypes
open SharedTypes

// GeneralNet = bool * NamedNet
// NamedNet = string * Net
// Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>
// test purposes

let GenNetListInA : GeneralNet list = 
    [false, ("A0", Wire (Map [0, Low]));
    false, ("A1", Wire (Map [0, Low]))
    ]

let GenNetListOutA : GeneralNet List =
    [false, ("C0", Wire (Map [0, Low]))]

// test purposes
let GenNetListInB : GeneralNet list = 
    [false, ("B0", Wire (Map [0, Low]));
    false, ("B1", Wire (Map [0, Low]))
    ]

let GenNetListOutB : GeneralNet list =
    [false, ("BOut", Wire (Map [0, Low]))]

let GenNetListInDFF : GeneralNet list = 
    [false, ("BOut", Wire (Map [0, High]))]

let GenNetListOutDFF : GeneralNet list =
    [true, ("C1", Wire (Map [0, High]))]
    

let GenNetListInC : GeneralNet list =
    [false, ("C0", Wire (Map [0, Low]));
     true, ("C1", Wire (Map [0, Low]))
    ]

let GenNetListOutC : GeneralNet list = 
    [false, ("COut", Wire (Map [0, Low]))]

// type Megablock = Name of string
// type connection = Megablock, GenNet list , GenNet list
/// example connection list
let connectListEx : Connection list= 
    [(Name "Module A", GenNetListInA, GenNetListOutA);
    (Name "Module B", GenNetListInB, GenNetListOutB);
    (Name "DFF", GenNetListInDFF, GenNetListOutDFF);
    (Name "Module C", GenNetListInC, GenNetListOutC)
    ]

let cLstSimple : Connection list =
    [(Name "Module A", GenNetListInA, GenNetListOutA);
    (Name "Module B", GenNetListInB, GenNetListOutB);
    ]

let connectSyncEx : Connection =
    Name "DFF", GenNetListInDFF, GenNetListOutDFF

let connectMixedEx: Connection =
    Name "DFF", 
        [true, ("test", Bus (Map [0, High; 1, High; 2, High]));
        true, ("C1", Wire (Map [0, High]));
        false, ("hehe", Bus (Map [0, High; 1, High; 2, High]))
        ],
        [true, ("C1", Wire (Map [0, High]));
        true, ("test", Bus (Map [0, High; 1, High; 2, High]));
        false, ("hehe", Bus (Map [0, High; 1, High; 2, High]))
        ]
   

// to update a genNet need to know:
// if bus: which wire in a bus (0, 1, 2) 
// if wire: just change the wire (0) to new logicLevel
// can't update a map, so need to rewrite the whole map...
// GenNet type example: false, ("BOut", Wire (Map [1, Low]))
let BusEx : GeneralNet =
    false, ("BusA", Bus(Map [0, Low; 1, High; 2, Low; 3, High]))
let wireEx : GeneralNet =
    false, ("WireA", Wire(Map [0, Low]))
let mixedEx : GeneralNet list =
    [false, ("BusA", Bus(Map [0, Low; 1, High; 2, Low; 3, High]));
    false, ("WireA", Wire(Map [0, Low]));
    false, ("WireB", Wire(Map [0, High]));
    false, ("WireC", Wire(Map [0, High]))
    ]

let dffMixedIn : GeneralNet list =
    [false, ("Wire A", Wire(Map [0, High]));
    false, ("Bus B", Wire(Map [0, High; 1, High]));
    false, ("Wire C", Wire(Map [0, Low]));
    ]

let dffMixedOut : GeneralNet list =
    [true, ("Bus D", Bus(Map [0, Low; 1, Low; 2, Low]));
    true, ("Wire E", Wire(Map [0, Low]));
    ]

let overallInputs : GeneralNet list =
    GenNetListInA @ GenNetListInB

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



let netIdLstIn = 
    [{ Name = "a"; SliceIndices = Some (3, Some 0) };
    { Name = "b"; SliceIndices = Some (3, Some 0) }]

let netIdLstOut =
    [{ Name = "c"; SliceIndices = Some (2, Some 0) }; 
    { Name = "d"; SliceIndices = None }]

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
    Inputs = netIdLstIn
    Outputs = netIdLstOut
    Wires = [] }

let tLogicEx2 : TLogic= { 
    Name = "bus_and2"
    ExpressionList =
        [(And, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = Some (3, Some 1) };
            { Name = "b"; SliceIndices = Some (3, Some 1) }]);
        (And, [{ Name = "d"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = Some (0, None) };
            { Name = "b"; SliceIndices = Some (0, None) }])]
    Inputs =
        [{ Name = "a"; SliceIndices = Some (3, Some 0) };
        { Name = "b"; SliceIndices = Some (3, Some 0) }]
    Outputs = [{ Name = "c"; SliceIndices = Some (2, Some 0) }; 
        { Name = "d"; SliceIndices = None }]
    Wires = [] }


let netIdEx1 = { Name = "a"; SliceIndices = Some (3, Some 0) }
let netEx1 = Bus (Map [0, High; 1, Low; 2, High; 3, Low ])
let netIdEx2 = { Name = "b"; SliceIndices = None }
let netEx2 = Wire (Map [0, High])
let netIdEx3 = { Name = "c"; SliceIndices = Some (2, Some 0) }
let netEx3 = Bus (Map [0, Low; 1, Low; 2, High])
let netIdEx4 = { Name = "d"; SliceIndices = None }
let netEx4 = Wire (Map [0, Low])

let evaluateOutEx = Map [netIdEx1, netEx1; netIdEx2, netEx2; netIdEx3, netEx3; netIdEx4, netEx4]

let tLogicLstEx = [tLogicEx1; tLogicEx2]

let aSIn =
    [false, ("a0", Wire(Map [0, High]));
    true, ("a1", Wire(Map [0, High]))]

let aSOut =
    [false, ("out", Wire(Map [0, High]))]

let dSIn =
    [false, ("d1", Wire(Map [0, Low]))]

let dSOut =
    [false, ("a1", Wire(Map [0, High]))]

let kSIn = 
    [false, ("a0", Wire(Map [0, High]));
    false, ("d1", Wire(Map [0, High]))]

let simpCLst : Connection List =
    [Name "A", aSIn, aSOut;
    Name "DFF", dSIn, dSOut]

let notIn = 
    [false, ("n", Wire(Map [0, Low]))]

let notOut = 
    [false, ("a0", Wire(Map [0, Low]))]

let and1In = 
    [false, ("a0", Wire(Map [0, Low]));
    false, ("a1", Wire(Map [0, High]))]

let and1Out =
    [false, ("b0", Wire(Map [0, High]))]

let or1In =
    [false, ("b0", Wire(Map [0, High]));
    false, ("b1", Wire(Map [0, High]))]

let or1Out =
    [false, ("out", Wire(Map [0, High]))]

let testCLst1 : Connection List =
    [Name "simpAND", and1In, and1Out;
    Name "simpOR", or1In, or1Out;
    Name "simpNOT", notIn, notOut]

let tLogicEx3 : TLogic= { 
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

let tLogicEx4 : TLogic= { 
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

let tLogicEx5 : TLogic= { 
    Name = "simpNOT"
    ExpressionList =
        [(Not, 
            [{ Name = "b"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None }]
    Outputs = [{ Name = "b"; SliceIndices = None }]
    Wires = [] }

let tLst1 = [tLogicEx3; tLogicEx4; tLogicEx5]

let testInputs1 : GeneralNet list =
    [false, ("n", Wire(Map [0, Low]));
    false, ("a1", Wire(Map [0, High]));
    false, ("b1", Wire(Map [0, Low]))]


let withAndIn =
    [false, ("a0", Wire(Map [0, Low]));
    false, ("a1", Wire(Map [0, Low]))]

let withAndOut =
    [false, ("b", Wire(Map [0, Low]))]

let dff1In = 
    [false, ("b", Wire(Map [0, High]))]

let dff1Out =
    [true, ("c", Wire(Map [0, Low]))]

let dff2In =
    [true, ("c", Wire(Map [0, Low]))]

let dff2Out =
    [true, ("out", Wire(Map [0, Low]))]

   
let withSyncClst:Connection list =
    [Name "simpAND", withAndIn, withAndOut;
    Name "DFF", dff1In, dff1Out;
    Name "DFF", dff2In, dff2Out]

let testInputsLstofLst : GeneralNet list list =
    [
        [false, ("a0", Wire(Map [0, High]));
        false, ("a1", Wire(Map [0, High]))];

        [false, ("a0", Wire(Map [0, Low]));
        false, ("a1", Wire(Map [0, High]))]
    ]



