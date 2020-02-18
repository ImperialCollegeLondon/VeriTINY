// module Lexer

type Lexer = char list -> (char list * char list) option

let lexNGram (ngram: (char list * bool * bool) list) (cLst: char list) : (char list * char list) option =
    
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

let moduleTok =
    [['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false;
     [' '], false, false]

let endModuleTok =
    [['e'], false, false;
     ['n'], false, false;
     ['d'], false, false;
     ['m'], false, false;
     ['o'], false, false;
     ['d'], false, false;
     ['u'], false, false;
     ['l'], false, false;
     ['e'], false, false]

let moduleNameTok =
    [['a'..'z']@['A'..'Z'], true, false;
     ['0'..'9'], true, true;
     [' '], true, true]

let (<|>) lex1 lex2 =
    fun clst ->
        Option.orElse (lex2 clst) (lex1 clst)

let combinedLexers =
    [moduleTok; moduleNameTok] //tries to lex from the beginning of the list 
    |> List.map lexNGram
    |> List.reduce (<|>)

let rec lexMoreThanOne cLst = 
    match combinedLexers cLst with 
    | Some (tokens, rest) -> 
        match lexMoreThanOne rest with 
        | Some (tokens', rest') -> Some ([tokens] @ tokens', rest')
        | None -> Some ([tokens], rest)
    | None -> None

lexMoreThanOne (Seq.toList "module a9")
let test = lexNGram moduleTok (Seq.toList "module")