module rec Parser 

open Lexer

type Terminal = TERMID of string | TERMIDWire of string * int | TERMIDBus of string * int * int

/// Match single value token
let (|MATCHSINGLE|_|) tokenToMatch (tokList: Result<Token list, Token list>) =
    match tokList with
    | Error tokList' -> Some (Error tokList')
    | Ok (hd :: tl) when tokenToMatch = hd -> Some (Ok tl)
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

let parse inpTokList =
    match Ok inpTokList with
    | TERMINAL (Some ast, Ok []) -> Ok ast
    | TERMINAL (_, Error lst) //?
    | TERMINAL (_, Ok lst) -> Error <| (List.length inpTokList - List.length lst, lst)
    | _ -> failwithf "What?"

let sampleCode = Seq.toList "a32_aa"

tokenise sampleCode |> parse