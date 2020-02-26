module Lexer
open NGrams
open System

//TODO: assign

type Lexer = char list -> (char list * char list) option
type NGram = (char list * bool * bool) list
type Token = | Module | Identifier of string | Number of int | Semicolon | Colon | Comma | OpRoundBracket | ClRoundBracket | Output | Input | OpBrace | ClBrace | Wire | OpSqBracket | ClSqBracket | AndTok | OrTok | NotTok | EndModule  

let lexNGram (ngram: NGram) (cLst: char list) =
    let takeIfInChars chars (acc,lst) isOpt = 
        match lst with 
        | hd :: tl when List.exists ((=) hd) chars ->
            Some(acc @ [hd], tl)
        | _ when isOpt -> 
            Some(acc, lst)
        | _ -> None

    let rec takeWhileInChars chars (acc,lst) isOpt =
        match lst with 
        | hd :: tl when List.exists ((=) hd) chars -> 
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

let tokenMap = Map ["module", Module; "(", OpRoundBracket; ")", ClRoundBracket; "[", OpSqBracket; "]", ClSqBracket; 
                    "{", OpBrace; "}", ClBrace; ";", Semicolon; ":", Colon; ",", Comma; "and", AndTok; "or", OrTok; "not", NotTok;
                    "endmodule", EndModule; "input", Input; "output", Output; "wire", Wire]

let (|SingleValTok|_|) inpstring =
    Map.tryFind inpstring tokenMap

let (|MultValTok|_|) inpstring = 
    if System.Int32.TryParse (inpstring: string) |> fst
    then Some (Number (System.Int32.TryParse (inpstring: string) |> snd))
    else Some (Identifier inpstring)

let tokenise inpstring = 
    let unwrapped =  lexAndImplode inpstring |> Option.defaultValue ([],[])

    let rec replaceWithTokens stringList = 
        match stringList with 
        | "" :: rest -> replaceWithTokens rest //to deal with newline
        | SingleValTok token :: rest -> [token] @ replaceWithTokens rest  
        | MultValTok token :: rest -> [token] @ replaceWithTokens rest   
        | [] -> [] 
        | _ -> failwithf "What?"

    replaceWithTokens (fst unwrapped)