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
let mapTest1 = 
    testCase "gLstToMap conversion Test 1 (Wires)" <| fun () ->
        let expected = mapA
        Expect.equal (gLstToMap gNetLstA) expected "Wrong map output"

[<Tests>]
let mapTest2 = 
    testCase "gLstToMap conversion Test 2 (Busses)" <| fun () ->
        let expected = mapB
        Expect.equal (gLstToMap gNetLstB) expected "Wrong map output"

[<Tests>]
let mapTest3 = 
    testCase "updateMap Test 1" <| fun () ->
        let expected = mapC3
        Expect.equal (updateMap mapC1 mapC2) expected "Wrong map output"

[<Tests>]
let mapTest4 = 
    testCase "updateMap Test 2" <| fun () ->
        let expected = mapC6
        Expect.equal (updateMap mapC4 mapC5) expected "Wrong map output"

[<Tests>]
let bLstSeperateTest1 = 
    testCase "seperateMegaBlocks Test 1" <| fun () ->
        let expected = bLstSeperated1
        Expect.equal (seperateMegaBlocksTest bLstEx1) expected "bLst not seperated properly"

[<Tests>]
let bLstSeperateTest2 = 
    testCase "seperateMegaBlocks Test 2" <| fun () ->
        let expected = bLstSeperated2
        Expect.equal (seperateMegaBlocksTest bLstEx2) expected "bLst not seperated properly"

[<Tests>]
let bLstSeperateTest3 = 
    testCase "seperateMegaBlocks Test 3 (no synchronous)" <| fun () ->
        let expected = bLstSeperated3
        Expect.equal (seperateMegaBlocksTest bLstEx3) expected "bLst not seperated properly"

[<Tests>]
let bLstSeperateTest4 = 
    testCase "seperateMegaBlocks Test 4 (no asynchronous)" <| fun () ->
        let expected = bLstSeperated4
        Expect.equal (seperateMegaBlocksTest bLstEx4) expected "bLst not seperated properly"

[<Tests>]
let tLogicTest1 = 
    testCase "evaluateTLogic Test (AND)" <| fun () ->
        let expected = andOutput 
        Expect.equal (evaluateTLogicTest andIn andOut tLogicEx2) expected "Expected High output"

[<Tests>]
let tLogicTest2= 
    testCase "evaluateTLogic Test (OR)" <| fun () ->
        let expected = orOutput 
        Expect.equal (evaluateTLogicTest orIn orOut tLogicEx3) expected "Expected High output"

[<Tests>]
let tLogicTest3 = 
    testCase "evaluateTLogic Test (NOT)" <| fun () ->
        let expected = notOutput 
        Expect.equal (evaluateTLogicTest notIn notOut tLogicEx4) expected "Expected Low output"

[<Tests>]
let tLogicTest4 = 
    testCase "AND gate with unmapped output" <| fun () ->
        let expected = c6Output
        Expect.equal (evaluateTLogicTest (gLstToMap c5and1In) (gLstToMap c5and1Out) tLogicEx5) expected "Output of advance state not as expected"


[<Tests>]
let simulationTest1 = 
    testCase "advanceState Asynchronous Test (2 AND Gates - Circuit 1)" <| fun () ->
        let expected = c1mapOfVals 
        Expect.equal (evaluateAsyncTest c1initMap testCircuit1 tLogicLstEx) expected "Asynchronous simulation unsuccessful"

[<Tests>]
let simulationTest2 = 
    testCase "advanceState Asynchronous Test (1 OR feeding into 1 AND - Circuit 2)" <| fun () ->
        let expected = c2mapOfVals 
        Expect.equal (evaluateAsyncTest c2initMap testCircuit2 tLogicLstEx) expected "Asynchronous simulation unsuccessful"

[<Tests>]
let simulationTest3 = 
    testCase "Synchronous DFF update Test (Wire)" <| fun () ->
        let expected = dffOutputWire
        Expect.equal (evaluateDFF dffInWire dffOutWire) expected "Expected High output"

[<Tests>]
let simulationTest4 = 
    testCase "Synchronous DFF update Test (Bus)" <| fun () ->
        let expected = dffOutputBus
        Expect.equal (evaluateDFF dffInBus dffOutBus) expected "Expected Low Low High output"

[<Tests>]
let simulationTest5 = 
    testCase "cascaded DFF test (evaluation in parallel)" <| fun () ->
        let expected = cascadeOutput
        Expect.equal (evaluateSyncTest cascadeInitMap cascadeBLst) expected "Expected Low High Low output"

[<Tests>]
let simulationTest6 = 
    testCase "combined asynchronous and synchronous test 1 (Circuit 3)" <| fun () ->
        let expected = c3Output
        Expect.equal (advanceStateTest c3InitMap c3AsyncBLst c3SyncBLst tLogicLstEx) expected "Output of advance state not as expected"

[<Tests>]
let simulationTest7 = 
    testCase "combined asynchronous and synchronous test 2 (Circuit 4)" <| fun () ->
        let expected = c4Output
        Expect.equal (advanceStateTest c4InitMap c4AsyncBLst c4SyncBLst tLogicLstEx) expected "Output of advance state not as expected"

[<Tests>]
let simulationTest8 = 
    testCase "simulating multiple clock cycles test (Circuit 3)" <| fun () ->
        let expected = c3MultipleOutput
        Expect.equal (simulate c3InputLstofLst c3CLst tLogicLstEx) expected "Output of advance state not as expected"


        
let mapTests =
    testList "Map Tests" [
        mapTest1
        mapTest2
        mapTest3
        mapTest4
    ]

let bLstSeperateTests =
    testList "Seperating Block list into synchronous and asynchronous lists" [
        bLstSeperateTest1
        bLstSeperateTest2
        bLstSeperateTest3
        bLstSeperateTest4
    ]

let tLogicTests =
    testList "TLogic Evaluation Tests" [
        tLogicTest1
        tLogicTest2
        tLogicTest3
        tLogicTest4
    ]

let simulationTests =
    testList "Simulation tests (asynchronous, synchronous and both)" [
        simulationTest1
        simulationTest2
        simulationTest3
        simulationTest4
        simulationTest5
        simulationTest6
        simulationTest7
        simulationTest8
    ]

let testsWithExpecto() =
   runTests defaultConfig mapTests |> ignore
   runTests defaultConfig bLstSeperateTests |> ignore
   runTests defaultConfig tLogicTests |> ignore
   runTests defaultConfig simulationTests |> ignore


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    testsWithExpecto() |> ignore

    Console.ReadKey() |> ignore

    0 // return an integer exit code
