module LogicBlockGen

open Parser
open SharedTypes

let getFirst threeTuple = 
    match threeTuple with 
    | (a, b, c) -> a

let getSecond threeTuple = 
    match threeTuple with 
    | (a, b, c) -> b
    
let getThird threeTuple = 
    match threeTuple with 
    | (a, b, c) -> c

let convertAST (ast: ModuleType) =

    ///Generates a concat net with a unique name (integers starting from 0)
    let genConcatNetList (usedNames: int list) : NetIdentifier list = 
        let rec increaseCount num = 
            if List.exists ((=) num) usedNames 
            then increaseCount (num + 1)
            else string num
        [{Name = increaseCount 0; SliceIndices = None}]

    let genWireNetList (wire: string list) : NetIdentifier list = 
        wire |> List.collect (fun name -> [{Name = name; SliceIndices = None}]) 
    
    let genThinSliceNetList (wire: int * string list) : NetIdentifier list = 
        wire |> snd |> List.collect (fun name -> [{Name = name; SliceIndices = Some (fst wire, None)}])

    let genThickSliceNetList (bus: int * int * string list) : NetIdentifier list = 
        bus |> getThird |> List.collect (fun name -> [{Name = name; SliceIndices = Some (getFirst bus, Some (getSecond bus))}])

    ///Only one depth of concatenation is allowed, so TERMCONCAT not in match pattern
    let rec genTermNetList (terminal: TerminalType) : NetIdentifier list = 
        match terminal with 
        | TERMID wire -> genWireNetList [wire] 
        | TERMIDWire (wire, num) ->  genThinSliceNetList (num, [wire])
        | TERMIDBus (bus, num1, num2) -> genThickSliceNetList (num1, num2, [bus])

    let updateTerm ((record, usedNames): TLogic * int list) (term: TerminalType) : TLogic * int list = 
        match term with        
        | TERMCONCAT termlist -> 
            //If terminal is a concatenation, add a concat net in ExpressionList
            let tmp = {record with ExpressionList = [Concat, genConcatNetList usedNames, termlist |> List.collect genTermNetList] @ record.ExpressionList}

            //Also add it as a wire net in Wires
            let tmp' = {tmp with Wires = genConcatNetList usedNames @ tmp.Wires}

            //Update the terminal list of the most recently added expression in ExpressionList 
            {tmp' with ExpressionList = match List.rev tmp'.ExpressionList with 
                                       | (op, output, termList) :: tl ->
                                            (op, output, termList @ genConcatNetList usedNames) :: tl |> List.rev
                                       | _ -> failwithf "What?"}, usedNames @ [List.length usedNames]
        | _ -> 
            {record with ExpressionList = match List.rev record.ExpressionList with 
                                          | (op, output, termList) :: tl -> 
                                                (op, output, termList @ genTermNetList term) :: tl |> List.rev
                                          | _ -> failwithf "What?"}, usedNames              

    let updateTermList ((record, usedNames): TLogic * int list) (termList: TerminalType list) : TLogic * int list = 
        termList |> List.fold updateTerm (record, usedNames)

    let convToOp gatetype = 
        match gatetype with 
        | AND -> And
        | OR -> Or
        | NOT -> Not

    let getModItem ((record, usedNames): TLogic * int list) modItem : TLogic * int list = 
        match modItem with 
        | INPWire inpwire -> 
            {record with Inputs = record.Inputs @ genWireNetList inpwire}, usedNames
        | INPBus (num1, num2, buslist) -> 
            {record with Inputs = record.Inputs @ genThickSliceNetList (num1, num2, buslist)}, usedNames
        | OUTWire outwire -> 
            {record with Outputs = record.Outputs @ genWireNetList outwire}, usedNames
        | OUTBus (num1, num2, buslist) -> 
            {record with Outputs = record.Outputs @ genThickSliceNetList (num1, num2, buslist)}, usedNames
        | WIRE wire -> 
            {record with Wires = record.Wires @ genWireNetList wire}, usedNames
        | GATEINST (gatetype, name, termlist) -> 
            match termlist with 
            | hd :: tl -> 
                match hd with //Get the output terminal first
                | TERMCONCAT outputterms -> 
                    //If output terminal is a concatenation, add a concat net in ExpressionList
                    let tmp = {record with ExpressionList = [Concat, genConcatNetList usedNames, outputterms |> List.collect genTermNetList] @ record.ExpressionList}

                    //Also add it as a wire net in Wires
                    let tmp' = {tmp with Wires = genConcatNetList usedNames @ tmp.Wires}

                    //Add a new gate inst to ExpressionList then add its terminal list by calling updateTermList
                    updateTermList ({tmp' with ExpressionList = tmp'.ExpressionList @ [convToOp gatetype, genConcatNetList usedNames, []]}, usedNames @ [List.length usedNames]) tl
                | _ ->
                    updateTermList ({record with ExpressionList = record.ExpressionList @ [convToOp gatetype, genTermNetList hd, []]}, usedNames) tl
            | _ -> failwithf "What?"

    match ast with 
    | MODULE (name, portlist, moditems) ->
        moditems |> List.fold getModItem ({Name = name; ExpressionList = []; Inputs = []; Outputs = []; Wires = []}, []) |> fst