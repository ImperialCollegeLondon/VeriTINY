module LogicBlockGen

open SharedTypes
open Parser

//TODO: delete types later
type NetIdentifier = {
    Name: string;
    SliceIndices: (int * int option) option 
}
type TLogic = {
    Name: string
    ExpressionList: (Operator * NetIdentifier list * NetIdentifier list) list
    Inputs: NetIdentifier list
    Outputs: NetIdentifier list
    Wires: NetIdentifier list
}  

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

    let genWireNetList (wire: string list) : NetIdentifier list = 
        wire |> List.collect (fun name -> [{Name = name; SliceIndices = None}]) 
    
    let genThinSliceNetList (wire: int * string list) : NetIdentifier list = 
        wire |> snd |> List.collect (fun name -> [{Name = name; SliceIndices = Some (fst wire, None)}])

    let genThickSliceNetList (bus: int * int * string list) : NetIdentifier list = 
        bus |> getThird |> List.collect (fun name -> [{Name = name; SliceIndices = Some (getFirst bus, Some (getSecond bus))}])
   
    let genTermNetList (terminal: TerminalType) : NetIdentifier list = 
        match terminal with 
        | TERMID wire -> genWireNetList [wire] 
        | TERMIDWire (wire, num) ->  genThinSliceNetList (num, [wire])
        | TERMIDBus (bus, num1, num2) -> genThickSliceNetList (num1, num2, [bus])

    let updateTerm (record: TLogic) (term: TerminalType) : TLogic = 
        {record with ExpressionList = match record.ExpressionList with 
                                      | (op, output, termList) :: tl -> 
                                            (op, output, genTermNetList term @ termList) :: tl
                                      | _ -> failwithf "What?"}

    let updateTermList (record: TLogic) (termList: TerminalType list) : TLogic = 
        termList |> List.fold updateTerm record

    let convToOp gatetype = 
        match gatetype with 
        | AND -> And
        | OR -> Or
        | NOT -> Not

    let getModItem (record: TLogic) modItem : TLogic = 
        match modItem with 
        | INPWire inpwire -> {record with Inputs = record.Inputs @ genWireNetList inpwire}
        | INPBus (num1, num2, buslist) -> {record with Inputs = record.Inputs @ genThickSliceNetList (num1, num2, buslist)}
        | OUTWire outwire -> {record with Outputs = record.Outputs @ genWireNetList outwire}
        | OUTBus (num1, num2, buslist) -> {record with Outputs = record.Outputs @ genThickSliceNetList (num1, num2, buslist)}
        | WIRE wire -> {record with Wires = record.Wires @ genWireNetList wire}
        | GATEINST (gatetype, name, termlist) -> 
            match termlist with 
            | hd :: tl -> updateTermList {record with ExpressionList = record.ExpressionList @ [convToOp gatetype, genTermNetList hd, []]} tl
            | _ -> failwithf "What?"

    match ast with 
    | MODULE (name, portlist, moditems) ->
        moditems |> List.fold getModItem {Name = name; ExpressionList = []; Inputs = []; Outputs = []; Wires = []}

    

    
 