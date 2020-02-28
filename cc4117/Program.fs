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
    
// [<Tests>]
// let expectoFSCheckTest1 = 
//     testCase "ParseT3 Test1" <| fun () ->
//         let expected = Error(1, "Unmatched Tokens")
//         Expect.equal (parseT3 tList1) expected "Expected Error 1, Unmatched Tokens"

// [<Tests>]
// let expectoFSCheckTest2 = 
//     testCase "ParseT3 Test2" <| fun () ->
//         let expected = Error(3, "Unmatched Tokens")
//         Expect.equal (parseT3 tList2) expected "Expected Error 3, Unmatched Tokens"

// [<Tests>]
// let expectoFSCheckTest3 = 
//     testCase "ParseT3 Test3" <| fun () ->
//         let expected = Error(0, "Not a valid expression")
//         Expect.equal (parseT3 tList3) expected "Expected Error 0, Not a valid expression"

        
// let testListWithExpecto =
//     testList "A test group" [
//         expectoFSCheckTest1
//         expectoFSCheckTest2
//         expectoFSCheckTest3
//     ]

// let testsWithExpecto() =
//     runTests defaultConfig testListWithExpecto |> ignore



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let finalState = simulate testInputsLstofLst withSyncClst tLst1

    Console.ReadKey() |> ignore
    
    //let test = evaluateModuleWithInputs tLogicEx3 (and1In |> gLstToMap) 

    //let updateDFFTest= updateDFF (gLstToMap dff1In) (gLstToMap dff1Out)
    
    0 // return an integer exit code
