module Lexer
open NGrams

type Lexer = char list -> (char list * char list) option
type NGram = (char list * bool * bool) list
type Token = | Module | ModuleName of string | Semicolon | Colon | Comma | OpRoundBracket | ClRoundBracket | Output of string | Input of string | OpBrace | ClBrace | Wire of string | OpSqBracket | ClSqBracket | And | Or | Not | EndModule  

let lexNGram (ngram: NGram) (cLst: char list) : (char list * char list) option =
    
    let takeIfInChars chars (acc,lst) isOpt = 
        match lst with 
        | hd::tl when List.exists ((=) hd) chars ->
            Some(acc @ [hd], tl)
        | _ when isOpt -> 
            Some(acc, lst)
        | _ -> None

    let rec takeWhileInChars chars (acc,lst) isOpt =
        match lst with 
        | hd::tl when List.exists ((=) hd) chars -> 
            if takeIfInChars chars (acc @ [hd], tl) isOpt = None 
            then Some (acc @ [hd], tl)
            else takeWhileInChars chars (acc @ [hd], tl) isOpt
        | _ when isOpt ->
            Some (acc, lst)        
        | _ -> None

    let folder state (charsLst, canRepeat, isOptional) =
        let consumeCharsFunc =
            (if canRepeat then takeWhileInChars else takeIfInChars) charsLst
        match state with 
        | Some gapBuf -> consumeCharsFunc gapBuf isOptional
        | None -> None

    (Some ([], cLst), ngram) ||> List.fold folder

let (<|>) lex1 lex2 =
    fun clst ->
        Option.orElse (lex2 clst) (lex1 clst)

let combinedLexers =
    [moduleTok; punctuationTok; andTok; orTok; notTok; inputTok; outputTok; endModuleTok; identifierTok] //tries to lex from the beginning of the list 
    |> List.map lexNGram
    |> List.reduce (<|>)

let rec lexMoreThanOne cLst = 
    match combinedLexers cLst with 
    | Some (tokens, rest) -> 
        match lexMoreThanOne rest with 
        | Some (tokens', rest') -> Some ([tokens] @ tokens', rest')
        | None -> Some ([tokens], rest)
    | None -> None

let charListToString lst = 
    lst |> List.map string |> List.reduce (+)

let trim (inpstring: string) = 
    inpstring.Trim()
    
let implodeLexedChars inpstring = 
    match lexMoreThanOne (Seq.toList inpstring) with 
    | Some (lexedList, remaining) -> 
        Some (lexedList |> List.map (charListToString >> trim), remaining)
    | _ -> None  

let tokenise inpstring = 
    let implodeLexedChars inpstring 
 


lexMoreThanOne (Seq.toList "module a99 (out, a, b); ")
implodeLexedChars "module a99 (out, a, b); output out; input a, b; and a1 (a, b); endmodule"