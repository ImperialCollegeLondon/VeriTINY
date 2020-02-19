module Evaluator
open SharedTypes


// GeneralNet = bool * NamedNet
// NamedNet = string * Net
// Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>

let hehe = 2
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
    [false, ("BOut", Wire (Map [1, Low]))]

let GenNetListOutDFF : GeneralNet list =
    [true, ("C1", Wire (Map [0, Low]))]

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

// given Connection list, find all synchronous elements
let updateDFF (input:Connection) = 
    let Name block, genNetIn, genNetOut = input
    if block = "DFF" then sprintf "hi" else sprintf "no"
