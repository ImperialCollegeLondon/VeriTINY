module Evaluator
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


let extractNet (genNetIn: GeneralNet): Net =
    match genNetIn with
    | (_, (_, Wire netMap)) -> 
        Wire netMap
    | (_, (_, Bus netMap)) ->
        Bus netMap
    
let extractLogLevel (netIn: Net) =
    match netIn with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map snd

let netSize (netIn: Net): int =
    match netIn with
    | Wire netMap
    | Bus netMap ->
    netMap |> Map.toList |> List.map fst |> List.length

/// find length of output nets from genNetOut list 
let findLength (lst: GeneralNet list) =
    List.map (extractNet >> netSize) lst

// tail recursion
// generate list of lists of LogicLevel of appropriate size for output
let rec splitPls acc (logLst:LogicLevel list) (lstOfLengths: int list) =
    match lstOfLengths with
    | hd::tl ->
        let acc', logLst' = List.splitAt hd logLst
        splitPls (acc @ [acc']) logLst' tl
    | [] -> 
        acc

let test' = splitPls [] [High;Low;High;Low;High;Low;High;Low] [1;3;3;2]

/// generate list of logic level updates (in order)
let generateNewLogList (genNetLst: GeneralNet list) =
    List.collect (extractNet >> extractLogLevel) genNetLst


let extractfromGenNet (genNetIn: GeneralNet) =
    genNetIn |> extractNet |> extractLogLevel 
    //let listOfLog = genNetIn |> extractNet |> extractLogLevel 
    //let n = List.length listOfLog // don't necessarily know the length corresponds with input
    //([0..n-1], listOfLog) ||> List.zip 

// let newMapGen (genNetLst: GeneralNet list) = 
//     genNetLst 
//     |> List.map (extractfromGenNet >> Map.ofList)   



// variant of below, which takes in a Net type
let netUpdateOld (netIn: Net) (index: int) (newLog:LogicLevel) : Net =
    match netIn with
    | Bus netMap ->
        Bus(Map.add index newLog netMap)
    | Wire netMap -> 
        Wire(Map.add 0 newLog netMap)


// currently requires index and newlogic level, accepts genNet type e.g. Bus/Wire 
let genNetUpdate (genNetIn: GeneralNet) (index: int) (newLog:LogicLevel) : GeneralNet =
    match genNetIn with
    | (sync, (str, Bus netMap)) ->
        sync, (str, Bus(Map.add index newLog netMap))
    | (sync, (str, Wire netMap)) -> 
        sync, (str, Wire(Map.add 0 newLog netMap))

//given Connection list, find all synchronous elements
// let updateDFF (input:Connection) : Connection = 
//     let Name block, genNetIn, genNetOut = input
//     let sync, (str, Bus )
//     if block = "DFF" then
//     let indexList = m |> Map.toList |> List.map fst
//     else
//     input

    
// let genNetWireUpdate (inp: GeneralNet) newLog : GeneralNet = 
//     match inp with
//     | false, (str, net) ->
//         false, (str, wireUpdate newLog)
//     | true, (str, net) -> 
//         true, (str, wireUpdate newLog)


// how to update maps 
let ym = Map ["cat",7 ; "the",3]
let ym' = Map.add "cat" 5 ym

    

