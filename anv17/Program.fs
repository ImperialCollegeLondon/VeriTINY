// Learn more about F# at http://fsharp.org

open System
open Tests
open Expecto

[<EntryPoint>]
let main argv =
   printfn "Hello"
   runTests defaultConfig concatenationTests |> ignore
   runTests defaultConfig topLevelModuleTests |> ignore
   runTests defaultConfig evalModuleLst |> ignore
   Console.ReadKey() |> ignore
   0
