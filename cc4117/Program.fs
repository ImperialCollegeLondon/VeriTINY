// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open Evaluator
open Simulator
open CombEval




[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let finalState = simulate testInputsLstofLst withSyncClst tLst1

    Console.ReadKey() |> ignore
    
    //let test = evaluateModuleWithInputs tLogicEx3 (and1In |> gLstToMap) 

    //let updateDFFTest= updateDFF (gLstToMap dff1In) (gLstToMap dff1Out)
    
    0 // return an integer exit code
