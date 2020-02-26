// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Evaluator
open Helper


let splitTest = groupLogic [] [High;Low;High;Low;High;Low;High;Low] [1;3;3;1]
let updateDFFTest = updateDFF dffMixedIn dffMixedOut


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
