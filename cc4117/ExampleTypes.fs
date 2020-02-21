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
    [false, ("AOut", Wire (Map [0, Low]))]

// test purposes
let GenNetListInB : GeneralNet list = 
    [false, ("B0", Wire (Map [0, Low]));
    false, ("B1", Wire (Map [0, Low]))
    ]

let GenNetListOutB : GeneralNet list =
    [false, ("BOut", Wire (Map [0, Low]))]

let GenNetListInDFF : GeneralNet list = 
    [false, ("BOut", Wire (Map [1, High]))]

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
