// Learn more about F# at http://fsharp.org

open System
open Tests
open Expecto

[<EntryPoint>]
let main argv =
   printfn "Hello"
   runTests defaultConfig evalExprTestLst |> ignore
   runTests defaultConfig concatenationTests |> ignore
   Console.ReadKey() |> ignore
   0
