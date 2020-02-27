// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open SimulationTypes
open ExampleTypes
open Helper
open Evaluator
open Simulator
open CombEval



//workflow!!!
// all initialized to low
// let syncNets:Map<NetIdentifier,Net> = returnSyncNets withSyncClst |> gLstToMap
// //works -> but redo the gLst to map<NetId,net> function
// let bLst = cLstToBlockLst withSyncClst
// // works -> returns bLst of DFFs
// let (syncBLst, asyncBLst) = seperateMegaBlocks bLst

// // works
// let initMap = getInitMap (gLstToMap testInputs2) syncNets

// // works!!
// let nextState = advanceState initMap asyncBLst syncBLst tLst1 

// let nextInitMap = getInitMap (gLstToMap testInputs3) nextState 

// let nextnextState = advanceState nextInitMap asyncBLst syncBLst tLst1



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let finalState = simulate testInputsLstofLst withSyncClst tLst1

    
    //let test = evaluateModuleWithInputs tLogicEx3 (and1In |> gLstToMap) 

    //let updateDFFTest= updateDFF (gLstToMap dff1In) (gLstToMap dff1Out)
    
    0 // return an integer exit code
