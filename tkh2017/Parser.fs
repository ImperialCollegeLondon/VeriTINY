module Parser 
//TODO: recursive module?

open Lexer

type GateType = AND | OR | NOT
type TerminalType = TERMID of string | TERMIDWire of string * int | TERMIDBus of string * int * int
type GateInstantiationType = GATEINST of GateType * string * TerminalType list
type DeclarationType = 
    | OUTWire of string list 
    | OUTBus of int * int * string list
    | INPWire of string list 
    | INPBus of int * int * string list
    | WIRE of string list 

/// Match single value token
let (|MATCHSINGLE|_|) tokenToMatch (tokList: Result<Token list, Token list>) =
    match tokList with
    | Error tokList' -> Some (Error tokList')
    | Ok (hd :: tl) when tokenToMatch = hd -> Some (Ok tl)
    | _ -> None

/// Match list of single value tokens
let (|MATCHMULT|_|) tokensToMatch (tokList: Result<Token list, Token list>) =
    match tokList with
    | Error tokList' -> Some (None, Error tokList')
    | Ok (hd :: tl) when List.contains hd tokensToMatch -> Some (Some hd, Ok tl)
    | _ -> None    

/// Match identifier multivalue token 
let (|MATCHID|_|) (tokList: Result<Token list, Token list>) =
    match tokList with
    | Error tokList' -> Some (None, Error tokList')
    | Ok (Identifier value :: tl) -> Some (Some value, Ok tl)
    | _ -> None

/// Match number multivalue token 
let (|MATCHNUM|_|) (tokList: Result<Token list, Token list>) =
    match tokList with
    | Error tokList' -> Some (None, Error tokList')
    | Ok (Number value :: tl) -> Some (Some value, Ok tl)
    | _ -> None  


//------------------- Parse rules -------------------------

let (|TERMINAL|_|) tokList = 
    match tokList with 
    | Error tokList' -> 
        Some (None, Error tokList')
    | MATCHID (Some idname, MATCHSINGLE OpSqBracket (MATCHNUM (Some number, MATCHSINGLE ClSqBracket (Ok tokList')))) -> 
        Some (Some (TERMIDWire (idname, number)), Ok tokList')
    | MATCHID (Some idname, MATCHSINGLE OpSqBracket (MATCHNUM (Some number1, MATCHSINGLE Colon (MATCHNUM (Some number2, MATCHSINGLE ClSqBracket (Ok tokList')))))) -> 
        Some (Some (TERMIDBus (idname, number1, number2)), Ok tokList')
    | MATCHID (Some idname, Ok tokList') -> Some (Some (TERMID idname), Ok tokList')
    | _ -> None 

let rec (|LISTTERMINAL|_|) tokList = 
    match tokList with 
    | Error tokList' -> 
        Some (None, Error tokList')
    | TERMINAL (Some parsed, MATCHSINGLE Comma (LISTTERMINAL (Some parsed', Ok tokList'))) ->  
        Some (Some ([parsed] @ parsed'), Ok tokList')
    | TERMINAL (Some parsed, Ok tokList') -> Some (Some [parsed], Ok tokList')
    | _ -> None 

let (|GATEINSTANCE|_|) tokList = 
    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MATCHID (Some gateid, MATCHSINGLE OpRoundBracket (LISTTERMINAL (Some termlist, MATCHSINGLE ClRoundBracket (Ok tokList')))) -> 
        Some (Some (gateid, termlist), Ok tokList')
    | _ -> None

let (|GATEINSTANTIATION|_|) tokList = 
    let convertToGateType token = 
        match token with 
        | And -> AND
        | Or -> OR
        | Not -> NOT
        | _ -> failwithf "Shouldn't happen"

    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MATCHMULT [And; Or; Not] (Some gatetype, GATEINSTANCE (Some (gateid,termlist), MATCHSINGLE Semicolon (Ok tokList'))) -> 
        Some (Some (GATEINST (convertToGateType gatetype, gateid, termlist)), Ok tokList')
    | _ -> None

let (|RANGE|_|) tokList = 
    match tokList with 
    | Error tokList' -> 
        Some (None, Error tokList')
    | MATCHSINGLE OpSqBracket (MATCHNUM (Some number1, MATCHSINGLE Colon (MATCHNUM (Some number2, MATCHSINGLE ClSqBracket (Ok tokList'))))) -> 
        Some (Some (number1, number2), Ok tokList')
    | _ -> None

let rec (|LISTVAR|_|) tokList = 
    match tokList with 
    | Error tokList' -> 
        Some (None, Error tokList')
    | MATCHID (Some parsed, MATCHSINGLE Comma (LISTVAR (Some parsed', Ok tokList'))) ->  
        Some (Some ([parsed] @ parsed'), Ok tokList')
    | MATCHID (Some parsed, Ok tokList') -> 
        Some (Some [parsed], Ok tokList')
    | _ -> None 

let (|INPDECLARATION|_|) tokList = 
    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MATCHSINGLE Input (RANGE (Some (num1, num2), LISTVAR (Some varlist, MATCHSINGLE Semicolon (Ok tokList')))) -> 
        Some (Some (INPBus (num1, num2, varlist)), Ok tokList')
    | MATCHSINGLE Input (LISTVAR (Some varlist, MATCHSINGLE Semicolon (Ok tokList'))) -> 
        Some (Some (INPWire varlist), Ok tokList')
    | _ -> None

let (|OUTDECLARATION|_|) tokList = 
    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MATCHSINGLE Output (RANGE (Some (num1, num2), LISTVAR (Some varlist, MATCHSINGLE Semicolon (Ok tokList')))) -> 
        Some (Some (OUTBus (num1, num2, varlist)), Ok tokList')
    | MATCHSINGLE Output (LISTVAR (Some varlist, MATCHSINGLE Semicolon (Ok tokList'))) -> 
        Some (Some (OUTWire varlist), Ok tokList')
    | _ -> None

let (|NETDECLARATION|_|) tokList = 
    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MATCHSINGLE Wire (LISTVAR (Some varlist, MATCHSINGLE Semicolon (Ok tokList'))) -> 
        Some (Some (WIRE varlist), Ok tokList')
    | _ -> None

let (|MODULEITEM|_|) tokList = 
    match tokList with 
    | Error tokList' -> 
        Some (None, Error tokList') 
    | INPDECLARATION (Some input, Ok tokList') -> 
        Some (Some input, Ok tokList')
    | OUTDECLARATION (Some output, Ok tokList') -> 
        Some (Some output, Ok tokList')
    | NETDECLARATION (Some net, Ok tokList') -> 
        Some (Some net, Ok tokList')
    | _ -> None
               
let rec (|LISTMODITEM|_|) tokList = 
    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MODULEITEM (Some moditem, LISTMODITEM (Some moditem', Ok tokList')) -> 
        Some (Some ([moditem] @ moditem'), Ok tokList')
    | MODULEITEM (Some moditem, Ok tokList') -> 
        Some (Some [moditem], Ok tokList')
    | _ -> None 


//TODO: Final parse function
let parse inpTokList =
    match Ok inpTokList with
    | LISTMODITEM (Some ast, Ok []) -> Ok ast
    | LISTMODITEM (_, Error lst) //?
    | LISTMODITEM (_, Ok lst) -> Error <| (List.length inpTokList - List.length lst, lst)
    | _ -> failwithf "What?"


let sampleCode = Seq.toList "output a, b, c; input[3:0] d, e; wire f;"

tokenise sampleCode |> parse