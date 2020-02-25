open System
open Lexer
open Parser
open LogicBlockGen

[<EntryPoint>]
let main argv =
    let sampleCode = Seq.toList (System.IO.File.ReadAllText "tkh2017/sampleverilog.v")
    printfn "%A" (tokenise sampleCode |> parse)
    printfn "%A" (match tokenise sampleCode |> parse with 
                  | Ok ast -> convertAST ast)
    
    Console.ReadKey() |> ignore
    0 // return an integer exit code
