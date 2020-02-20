module rec Parser 

open Lexer

type GateType = AND | OR | NOT
type TerminalType = TERMID of string | TERMIDWire of string * int | TERMIDBus of string * int * int
type GateInstantiationType = GATEINST of GateType * string * TerminalType list

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

//TODO: rec needed ?
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

    match tokList with 
    | Error tokList' ->
        Some (None, Error tokList')
    | MATCHMULT [And; Or; Not] (Some gatetype, GATEINSTANCE (Some (gateid,termlist), MATCHSINGLE Semicolon (Ok tokList'))) -> 
        Some (Some (GATEINST (convertToGateType gatetype, gateid, termlist)), Ok tokList')
    | _ -> None


//TODO: Final parse function
let parse inpTokList =
    match Ok inpTokList with
    | GATEINSTANTIATION (Some ast, Ok []) -> Ok ast
    | GATEINSTANTIATION (_, Error lst) //?
    | GATEINSTANTIATION (_, Ok lst) -> Error <| (List.length inpTokList - List.length lst, lst)
    | _ -> failwithf "What?"


let sampleCode = Seq.toList "and a1 (out, b);"

tokenise sampleCode |> parse