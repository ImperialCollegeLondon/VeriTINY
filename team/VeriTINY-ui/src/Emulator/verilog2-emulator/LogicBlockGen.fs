module LogicBlockGen

open Parser
open SharedTypes
open EvalNetHelper

let getFirst (a, _, _) = a

let getSecond (_, b, _) = b

let getThird (_, _, c) = c 

let genNewName (usedNames: int list) = 
    string (List.length usedNames)

let removeHead lst = 
    match lst with 
    | hd :: tl -> tl 
    | [] -> []     
      
///Generates a skeleton concat net with size and a unique name (integers starting from 0)
let genConcatNetList (usedNames: int list) : NetIdentifier list = 
    [{Name = genNewName usedNames; SliceIndices = None}]

let genWireNetList (wire: string list) : NetIdentifier list = 
    wire |> List.collect (fun name -> [{Name = name; SliceIndices = None}]) 

let genThinSliceNetList (wire: int * string list) : NetIdentifier list = 
    wire |> snd |> List.collect (fun name -> [{Name = name; SliceIndices = Some (fst wire, None)}])

let genThickSliceNetList (bus: int * int * string list) : NetIdentifier list = 
    bus |> getThird |> List.collect (fun name -> [{Name = name; SliceIndices = Some (getFirst bus, Some (getSecond bus))}])

let correctConcatExp (allNets: NetIdentifier list) (concatExp: Expression) : Expression = 
    
    let getConcatSize concatNetTerms allNets =
        let folder totalSize netID = 
            let currNetSize = 
                match netID.SliceIndices with
                | Some (x, Some y) -> abs (x - y) + 1
                | Some (x, None) -> 1
                | None -> getBusSize (List.find (fun x -> x.Name = netID.Name) allNets)
            totalSize + currNetSize
        List.fold folder 0 concatNetTerms

    (getFirst concatExp, [{(getSecond concatExp).Head with SliceIndices = Some (0, Some (getConcatSize (getThird concatExp) allNets - 1))}], getThird concatExp)

///Deconstructs continuous assign expressions in AST down to gate level
let assignToGates ((ast, record, usedNames): ModuleType * TLogic * int list) (assignModItem: TerminalType * ExpressionType) : ModuleType * TLogic * int list =
    let modName = match ast with
                  | MODULE (name, _, _) -> name
    
    let portList = match ast with 
                   | MODULE (_, ports, _) -> ports
    
    let modItemList = match ast with 
                      | MODULE (_, _, modItemLst) -> modItemLst

    let genOutNet (outTerm: TerminalType) : NetIdentifier list = 
        match outTerm with 
        | TERMID wire -> genWireNetList [wire] 
        | TERMIDWire (wire, num) ->  genThinSliceNetList (num, [wire])
        | TERMIDBus (bus, num1, num2) -> genThickSliceNetList (num1, num2, [bus])
        | _ -> failwithf "What?"

    let genIntermediateWires (outNet: NetIdentifier) (name: string) (record: TLogic) : NetIdentifier list = 
        match outNet.SliceIndices with 
        | Some (num1, num2) -> 
            match num2 with 
            | Some num2val -> genThickSliceNetList (num1, num2val, [name])
            | None -> genWireNetList ([name])
        | None -> 
            (record.Wires @ record.Inputs @ record.Outputs) 
            |> List.find (fun netID -> netID.Name = outNet.Name) 
            |> fun netID -> [{Name = name; SliceIndices = netID.SliceIndices}]

    let rec breakDown (expression: ExpressionType) ((record, usedNames): TLogic * int list) (outNet: NetIdentifier) : ModuleItemType list * TLogic * int list = 
        match expression with 
        | OREXP (exp1, exp2) -> 
            let newName1 = genNewName usedNames
            let usedNames' = usedNames @ [List.length usedNames]
            let (gateList1, record', usedNames'') = breakDown exp1 (record, usedNames') outNet

            let newName2 = genNewName usedNames''
            let usedNames''' = usedNames'' @ [List.length usedNames'']
            let (gateList2, record'', usedNames'''') = breakDown exp2 (record', usedNames''') outNet

            let intermediateWire = genIntermediateWires outNet (string (List.last usedNames)) record''
            let updatedRecord = {record'' with Wires = intermediateWire @ record''.Wires}
            ([GATEINST (OR, "", [TERMID (string (List.last usedNames)); TERMID newName1; TERMID newName2])] @ gateList1 @ gateList2, updatedRecord, usedNames'''')

        | ANDEXP (exp1, exp2) ->
            let newName1 = genNewName usedNames
            let usedNames' = usedNames @ [List.length usedNames]
            let (gateList1, record', usedNames'') = breakDown exp1 (record, usedNames') outNet

            let newName2 = genNewName usedNames''
            let usedNames''' = usedNames'' @ [List.length usedNames'']
            let (gateList2, record'', usedNames'''') = breakDown exp2 (record', usedNames''') outNet

            let intermediateWire = genIntermediateWires outNet (string (List.last usedNames)) record''
            let updatedRecord = {record'' with Wires = intermediateWire @ record''.Wires}
            ([GATEINST (AND, "", [TERMID (string (List.last usedNames)); TERMID newName1; TERMID newName2])] @ gateList1 @ gateList2, updatedRecord, usedNames'''')

        | NOTEXP exp -> 
            let newName1 = genNewName usedNames
            let usedNames' = usedNames @ [List.length usedNames]
            let (gateList, record', usedNames'') = breakDown exp (record, usedNames') outNet

            let intermediateWire = genIntermediateWires outNet (string (List.last usedNames)) record'
            let updatedRecord = {record' with Wires = intermediateWire @ record'.Wires}
            ([GATEINST (NOT, "", [TERMID (string (List.last usedNames)); TERMID newName1])] @ gateList, updatedRecord, usedNames'')

        | TERMEXP term -> 
            match term with 
            | EXP exp -> 
                breakDown exp (record, usedNames) outNet
            | _ -> 
                let intermediateWire = genIntermediateWires outNet (string (List.last usedNames)) record
                let updatedRecord = {record with Wires = intermediateWire @ record.Wires}
                ([GATEINST (PASS, "", [TERMID (string (List.last usedNames)); term])], updatedRecord, usedNames)

    //Break down highest level gate first
    let assignBrokenDown: ModuleItemType list * TLogic * int list =
        let outNet = List.head (genOutNet (assignModItem |> fst)) //here genOutNet produces a list of one element only
        match assignModItem |> snd with 
        | OREXP (exp1, exp2) -> 
            let newName1 = genNewName usedNames
            let usedNames' = usedNames @ [List.length usedNames]
            let (gateList1, record', usedNames'') = breakDown exp1 (record, usedNames') outNet

            let newName2 = genNewName usedNames''
            let usedNames''' = usedNames'' @ [List.length usedNames'']
            let (gateList2, record'', usedNames'''') = breakDown exp2 (record', usedNames''') outNet

            ([GATEINST (OR, "", [assignModItem |> fst; TERMID newName1; TERMID newName2])] 
             @ gateList1
             @ gateList2
             @ modItemList, record'', usedNames'''')
        | ANDEXP (exp1, exp2) -> 
            let newName1 = genNewName usedNames
            let usedNames' = usedNames @ [List.length usedNames]
            let (gateList1, record', usedNames'') = breakDown exp1 (record, usedNames') outNet

            let newName2 = genNewName usedNames''
            let usedNames''' = usedNames'' @ [List.length usedNames'']      
            let (gateList2, record'', usedNames'''') = breakDown exp2 (record', usedNames''') outNet

            ([GATEINST (AND, "", [assignModItem |> fst; TERMID newName1; TERMID newName2])] 
             @ gateList1
             @ gateList2
             @ modItemList, record'', usedNames'''')
        | NOTEXP exp -> 
            let newName1 = genNewName usedNames
            let usedNames' = usedNames @ [List.length usedNames]
            let (gateList, record', usedNames'') = breakDown exp (record, usedNames') outNet

            ([GATEINST (NOT, "", [assignModItem |> fst; TERMID newName1])] 
             @ gateList
             @ modItemList, record', usedNames'')
        | TERMEXP term -> 
            ([GATEINST (PASS, "", [assignModItem |> fst; term])] 
            @ modItemList, record, usedNames)

    (MODULE (modName, portList, getFirst assignBrokenDown), getSecond assignBrokenDown, getThird assignBrokenDown)
    

let convertAST (ast: ModuleType) : TLogic =

    ///Only one depth of concatenation is allowed, so TERMCONCAT not in match pattern
    let genTermNetList (terminal: TerminalType) : NetIdentifier list = 
        match terminal with 
        | TERMID wire -> genWireNetList [wire] 
        | TERMIDWire (wire, num) ->  genThinSliceNetList (num, [wire])
        | TERMIDBus (bus, num1, num2) -> genThickSliceNetList (num1, num2, [bus])
        | _ -> failwithf "What?"

    let updateTerm ((ast, record, usedNames): ModuleType * TLogic * int list) (term: TerminalType) : ModuleType * TLogic * int list = 
        match term with        
        | TERMCONCAT termlist -> 
            //If terminal is a concatenation, add a concat exp in ExpressionList but corrected to include bus size
            let tmp = {record with ExpressionList = [(Concat, genConcatNetList usedNames, termlist |> List.collect genTermNetList) |> correctConcatExp (record.Inputs @ record.Outputs @ record.Wires)] @ record.ExpressionList}

            //Also add it in Wires
            let tmp' = {tmp with Wires = [(getSecond (tmp.ExpressionList.Head)).Head] @ tmp.Wires}

            //Update the terminal list of the most recently added expression in ExpressionList 
            ast, {tmp' with ExpressionList = match List.rev tmp'.ExpressionList with 
                                             | (op, output, termList) :: tl ->
                                                 (op, output, termList @ genConcatNetList usedNames) :: tl |> List.rev
                                             | _ -> failwithf "What?"}, usedNames @ [List.length usedNames]                        
        | _ -> 
            ast, {record with ExpressionList = match List.rev record.ExpressionList with 
                                               | (op, output, termList) :: tl -> 
                                                    (op, output, termList @ genTermNetList term) :: tl |> List.rev
                                               | _ -> failwithf "What?"}, usedNames              

    let updateTermList ((ast, record, usedNames): ModuleType * TLogic * int list) (termList: TerminalType list) : ModuleType * TLogic * int list = 
        termList |> List.fold updateTerm (ast, record, usedNames)

    let convToOp gatetype = 
        match gatetype with 
        | AND -> And
        | OR -> Or
        | NOT -> Not
        | PASS -> Pass

    let getModItem ((ast, record, usedNames): ModuleType * TLogic * int list) modItem : ModuleType * TLogic * int list = 
        let unwrapped = match ast with 
                        | MODULE (name, portList, modItems) -> (name, portList, modItems)
        let newModList = getThird unwrapped |> removeHead
        let ast' = MODULE (getFirst unwrapped, getSecond unwrapped, newModList)

        match modItem with 
        | INPWire inpwire -> 
            ast', {record with Inputs = record.Inputs @ genWireNetList inpwire}, usedNames
        | INPBus (num1, num2, buslist) -> 
            ast', {record with Inputs = record.Inputs @ genThickSliceNetList (num1, num2, buslist)}, usedNames
        | OUTWire outwire -> 
            ast', {record with Outputs = record.Outputs @ genWireNetList outwire}, usedNames
        | OUTBus (num1, num2, buslist) -> 
            ast', {record with Outputs = record.Outputs @ genThickSliceNetList (num1, num2, buslist)}, usedNames
        | WIRE wire -> 
            ast', {record with Wires = record.Wires @ genWireNetList wire}, usedNames
        | WIREBus (num1, num2, buslist) -> 
            ast', {record with Wires = record.Wires @ genThickSliceNetList (num1, num2, buslist)}, usedNames 
        | GATEINST (gatetype, name, termlist) -> 
            match termlist with 
            | hd :: tl -> 
                match hd with //Get the output terminal first
                | TERMCONCAT outputterms -> 
                    //If output terminal is a concatenation, add a concat exp in ExpressionList but corrected to include bus size
                    let tmp = {record with ExpressionList = [(Concat, genConcatNetList usedNames, outputterms |> List.collect genTermNetList) |> correctConcatExp (record.Inputs @ record.Outputs @ record.Wires)] @ record.ExpressionList}

                    //Also add it in Wires
                    let tmp' = {tmp with Wires = [(getSecond (tmp.ExpressionList.Head)).Head] @ tmp.Wires}

                    //Add a new gate inst to ExpressionList then add its terminal list by calling updateTermList
                    updateTermList (ast', {tmp' with ExpressionList = tmp'.ExpressionList @ [convToOp gatetype, genConcatNetList usedNames, []]}, usedNames @ [List.length usedNames]) tl
                | _ ->
                    updateTermList (ast', {record with ExpressionList = record.ExpressionList @ [convToOp gatetype, genTermNetList hd, []]}, usedNames) tl
            | _ -> failwithf "What?"
        | ASSIGN (outTerm, expList) -> 
            assignToGates (ast, record, usedNames) (outTerm, expList)

    let pickGateInstOnly (modItemLst: ModuleItemType list) = 
        List.filter (fun modItem -> match modItem with 
                                    | GATEINST (gatetype, name, termlist) -> true
                                    | _ -> false) modItemLst

    match ast with 
    | MODULE (name, portlist, moditems) ->
        moditems
        |> List.fold getModItem (ast, {Name = name; ExpressionList = []; Inputs = []; Outputs = []; Wires = []}, []) 
        |> fun (ast', tlogic, usedNames) -> 
            match ast' with 
            | MODULE (name, portlist, moditems') -> 
                moditems' |> pickGateInstOnly |> List.fold getModItem (ast', tlogic, usedNames)
        |> getSecond