module Lexer
open NGrams
open System

type Lexer = char list -> (char list * char list) option
type NGram = (char list * bool * bool) list
type Token = | Module | Identifier of string | Number of string | Semicolon | Colon | Comma | OpRoundBracket | ClRoundBracket | Output | Input | OpBrace | ClBrace | Wire | OpSqBracket | ClSqBracket | And | Or | Not | EndModule  

let lexNGram (ngram: NGram) (cLst: char list) =
    
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

let rec lexMoreThanOne cLst = 

    let (<|>) lex1 lex2 =
        fun clst ->
            Option.orElse (lex2 clst) (lex1 clst)

    let combinedLexers =
        [moduleTok; punctuationTok; andTok; orTok; notTok; inputTok; outputTok; endModuleTok; identifierTok; numberTok; emptyLine] //tries to lex from the beginning of the list 
        |> List.map lexNGram
        |> List.reduce (<|>)
        
    match combinedLexers cLst with 
    | Some (tokens, rest) -> 
        match lexMoreThanOne rest with 
        | Some (tokens', rest') -> Some ([tokens] @ tokens', rest')
        | None -> Some ([tokens], rest)
    | None -> None
    
let lexAndImplode inpstring = 

    let charListToString lst = 
        lst |> List.map string |> List.reduce (+)

    let trim (inpstring: string) = 
        inpstring.Trim()
        
    match lexMoreThanOne (Seq.toList inpstring) with 
    | Some (lexedList, remaining) -> 
        Some (lexedList |> List.map (charListToString >> trim), remaining)
    | _ -> None  

let tokenise inpstring = 
    let unwrapped =  lexAndImplode inpstring |> Option.defaultValue ([],[])

    let takeIfNotInStrings strings (acc,lst) = 
        match lst with 
        | hd::tl when not (List.exists ((=) hd) strings) ->
            Some(acc @ [hd], tl)
        | _ -> None

    let rec takeWhileNotInStrings strings (acc,lst) =
        match lst with 
        | hd::tl when not (List.exists ((=) hd) strings) -> 
            if takeIfNotInStrings strings (acc @ [hd], tl) = None 
            then Some (acc @ [hd], tl)
            else takeWhileNotInStrings strings (acc @ [hd], tl)
        | _ -> None

    let containsOnlyNumbers (s:string) = System.Int32.TryParse s |> fst

    let rec replaceWithTokens stringList = 
        match stringList with 
        | "module"::rest -> [Module] @ replaceWithTokens rest
        | "("::rest -> [OpRoundBracket] @ replaceWithTokens rest
        | ")"::rest -> [ClRoundBracket] @ replaceWithTokens rest 
        | "["::rest -> [OpSqBracket] @ replaceWithTokens rest
        | "]"::rest -> [ClSqBracket] @ replaceWithTokens rest
        | "{"::rest -> [OpBrace] @ replaceWithTokens rest
        | "}"::rest -> [ClBrace] @ replaceWithTokens rest
        | ";"::rest -> [Semicolon] @ replaceWithTokens rest
        | ":"::rest -> [Colon] @ replaceWithTokens rest
        | ","::rest -> [Comma] @ replaceWithTokens rest
        | ""::rest -> replaceWithTokens rest //to deal with newlines
        | "and"::rest -> [And] @ replaceWithTokens rest
        | "or"::rest -> [Or] @ replaceWithTokens rest
        | "not"::rest -> [Not] @ replaceWithTokens rest
        | "endmodule"::rest -> [EndModule] @ replaceWithTokens rest
        | "input"::rest -> [Input] @ replaceWithTokens rest
        | "output"::rest -> [Output] @ replaceWithTokens rest
        | "wire"::rest -> [Wire] @ replaceWithTokens rest
        | number::rest when containsOnlyNumbers number -> [Number number] @ replaceWithTokens rest
        | name::rest -> [Identifier name] @ replaceWithTokens rest            
        | [] -> [] 

    replaceWithTokens (fst unwrapped)

// lexMoreThanOne (Seq.toList "module a99 (out, a, b); ")
// implodeLexedChars "module a99 (out, a, b); output out; input a, b; and a1 (a, b); endmodule"
// tokenise "module a99 (out, a, b); output out; input a, b; and a1 (a, b); endmodule"
// tokenise "module mux(out, sel, a, b); output out; input sel, a, b; wire c; or orgate (a, b, c); endmodule"

let sampleCode = Seq.toList (System.IO.File.ReadAllText "sampleverilog.v")

tokenise sampleCode