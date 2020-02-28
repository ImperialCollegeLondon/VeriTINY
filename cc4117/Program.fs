// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open SynchronousBlocks
open Simulator
open CombEval

open Expecto


[<Tests>]
let expectoFSCheckTest1 = 
    testCase "gLstToMap Test 1 (Wires)" <| fun () ->
        let expected = mapA
        Expect.equal (gLstToMap gNetLstA) expected "Wrong map output"

[<Tests>]
let expectoFSCheckTest2 = 
    testCase "gLstToMap Test 2 (Busses)" <| fun () ->
        let expected = mapB
        Expect.equal (gLstToMap gNetLstB) expected "Wrong map output"


        
let testListWithExpecto =
    testList "A test group" [
        expectoFSCheckTest1
        expectoFSCheckTest2
    ]

let testsWithExpecto() =
    runTests defaultConfig testListWithExpecto |> ignore



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    //let finalState = simulate testInputsLstofLst syncCLst tLst1

    testsWithExpecto() |> ignore
    
    Console.ReadKey() |> ignore

    
    //let test = evaluateModuleWithInputs tLogicEx3 (and1In |> gLstToMap) 

    //let updateDFFTest= updateDFF (gLstToMap dff1In) (gLstToMap dff1Out)
    
    0 // return an integer exit code
