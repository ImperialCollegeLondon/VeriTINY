// Learn more about F# at http://fsharp.org

open System

open SharedTypes
open Connector

let andIn: GeneralNet list =
    [false, ("a", Wire(Map [0, High]));
    false, ("b", Wire(Map [0, Low]))]

let andOut: GeneralNet list =
    [false, ("c", Wire(Map [0, High]))]

let dff1In: GeneralNet list =
    [false, ("dInA", Wire(Map [0, High]))]

let dff1Out: GeneralNet list =
    [true, ("dOutA", Wire(Map [0, High]))]

let dff2In: GeneralNet list =
    [true, ("dInB", Wire(Map [0, High]))]

let dff2Out: GeneralNet list =
    [true, ("dOutB", Wire(Map [0, High]))]


let connectionA = Name "simpAND", andIn, andOut
let connectionB = Name "DFF", dff1In, dff1Out
let connectionC = Name "DFF", dff2In, dff2Out

let cLst :Connection list =
    [connectionA; connectionB; connectionC]

// let searchInNets str (conn:Connection)=
//     List.contains str (List.map (fun x->(fst(snd x))) (second conn))

[<EntryPoint>]
let main argv = 
    printfn "Hello World from F#!"
    
    //let hi = searchGNetLst "a0" andIn
    let hi = refactor cLst

    0 // return an integer exit code
