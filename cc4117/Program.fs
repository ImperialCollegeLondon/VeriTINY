// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Evaluator
open Simulator
open Helper


let splitTest = groupLogic [] [High;Low;High;Low;High;Low;High;Low] [1;3;3;1]
let updateDFFTest = updateDFF dffMixedIn dffMixedOut

//works
// all initialized to low
let syncNets:GeneralNet list = returnSyncNets testCLst1 
//works -> but redo the gLst to map<NetId,net> function
let bLst = cLstToBlockLst testCLst1
// doesn't work...
let dFFList = seperateDFF bLst

// works
let initMap = getInitMap testInputs1 syncNets

// doesn't work...
let nextState = advanceState testInputs1 syncNets bLst tLst1  



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    // all initialized to low
    let syncNets:GeneralNet list = returnSyncNets testCLst1 
    //works -> but redo the gLst to map<NetId,net> function
    let bLst = cLstToBlockLst testCLst1
    // doesn't work...
    let dFFList = seperateDFF bLst

    // works
    let initMap = getInitMap testInputs1 syncNets

    // doesn't work...
    let nextState = advanceState testInputs1 syncNets bLst tLst1  
    
    0 // return an integer exit code
