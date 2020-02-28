// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open SynchronousBlocks
open Simulator
open CombEval
open TestFunctions

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

[<Tests>]
let expectoFSCheckTest3 = 
    testCase "updateMap Test" <| fun () ->
        let expected = mapC3
        Expect.equal (updateMap mapC1 mapC2) expected "Wrong map output"

[<Tests>]
let expectoFSCheckTest4 = 
    testCase "seperateMegaBlocks Test" <| fun () ->
        let expected = bLstSeperated
        Expect.equal (seperateMegaBlocksTest bLstEx) expected "bLst not seperated properly"

[<Tests>]
let expectoFSCheckTest5 = 
    testCase "evaluateTLogic Test (AND)" <| fun () ->
        let expected = andOutput 
        Expect.equal (evaluateTLogicTest andIn andOut tLogicEx2) expected "Expected High output"

[<Tests>]
let expectoFSCheckTest6 = 
    testCase "evaluateTLogic Test (OR)" <| fun () ->
        let expected = orOutput 
        Expect.equal (evaluateTLogicTest orIn orOut tLogicEx3) expected "Expected High output"

[<Tests>]
let expectoFSCheckTest7 = 
    testCase "evaluateTLogic Test (NOT)" <| fun () ->
        let expected = notOutput 
        Expect.equal (evaluateTLogicTest notIn notOut tLogicEx4) expected "Expected Low output"

        
let testListWithExpecto =
    testList "A test group" [
        expectoFSCheckTest1
        expectoFSCheckTest2
        expectoFSCheckTest3
        expectoFSCheckTest4
        expectoFSCheckTest5
        expectoFSCheckTest6
        expectoFSCheckTest7
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
