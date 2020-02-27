// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Evaluator
open Simulator
open CombEval
open Helper



//works
// all initialized to low
let syncNets:GeneralNet list = returnSyncNets testCLst1 
//works -> but redo the gLst to map<NetId,net> function
//let bLst = cLstToBlockLst testCLst1
// doesn't work...
//let dFFList = seperateDFF bLst

// works
//let initMap = getInitMap testInputs1 syncNets

// doesn't work...
//let nextState = advanceState testInputs1 syncNets bLst tLst1  



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    // all initialized to low
    //let syncNets:GeneralNet list = returnSyncNets testCLst1 
    //works -> but redo the gLst to map<NetId,net> function
    //let bLst = cLstToBlockLst testCLst1
    // doesn't work...
    //let dFFList = seperateDFF bLst

    // works
    //let initMap = getInitMap testInputs1 syncNets

    // doesn't work...
    //let nextState = advanceState testInputs1 syncNets bLst tLst1 

    
    
    //let test = evaluateModuleWithInputs tLogicEx3 (and1In |> gLstToMap) 

    let dIn = dffMixedIn |> gLstToMap |> mapToGLst
    let dOut = dffMixedOut |> gLstToMap |> mapToGLst
    let updateDFFTest= updateDFF dIn dOut
    
    0 // return an integer exit code
