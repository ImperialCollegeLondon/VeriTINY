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

let dffMixedIn : GeneralNet list =
    [false, ("Wire A", Wire(Map [0, Low]));
    false, ("Bus B", Wire(Map [0, Low; 1, High]));
    false, ("Wire C", Wire(Map [0, High]));
    ]

let dffMixedOut : GeneralNet list =
    [true, ("Bus D", Bus(Map [0, Low; 1, Low; 2, Low]));
    true, ("Wire E", Wire(Map [0, Low]));
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
let rec splitLst acc (logLst:LogicLevel list) (lstOfLengths: int list) =
    match lstOfLengths with
    | hd::tl ->
        let acc', logLst' = List.splitAt hd logLst
        splitLst (acc @ [acc']) logLst' tl
    | [] -> 
        acc

let splitTest = splitLst [] [High;Low;High;Low;High;Low;High;Low] [1;3;3;1]

// rebuild GeneralNet with newMap
let reconstructNet (genNetIn: GeneralNet) newMap =
    match genNetIn with
    | (sync, (str, Wire _)) -> 
        sync, (str, Wire newMap)
    | (sync, (str, Bus _)) ->
        sync, (str, Bus newMap)

/// operate on two lists 
let rec lstOpParallel acc f lstA lstB =
        match lstA, lstB with
        | h1::t1, h2::t2 ->
            let acc' = f h1 h2
            lstOpParallel (acc @ [acc']) f t1 t2
        | _ -> 
            acc

let updateDFF genNetLstIn genNetLstOut =
    let generateList n = [0..n-1]
    let lstOfLog = List.collect (extractNet >> extractLogLevel) genNetLstIn
    let lstOfLengths = findLength genNetLstOut
    let grpLog = splitLst [] lstOfLog lstOfLengths
    let grpNum = List.map generateList lstOfLengths
    let zipOut = lstOpParallel [] List.zip grpNum grpLog
    let newMaps = zipOut |> List.map Map.ofList
    lstOpParallel [] reconstructNet genNetLstOut newMaps

let updateDFFTest = updateDFF dffMixedIn dffMixedOut


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



    

