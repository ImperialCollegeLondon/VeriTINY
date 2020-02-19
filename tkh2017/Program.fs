open System
open Lexer
open Parser

[<EntryPoint>]
let main argv =
    let sampleCode = Seq.toList (System.IO.File.ReadAllText "sampleverilog.v")
    printfn "%A" (tokenise sampleCode)
    Console.ReadKey() |> ignore
    0 // return an integer exit code
