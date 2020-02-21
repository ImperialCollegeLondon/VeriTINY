// Learn more about F# at http://fsharp.org

open System
open DGraphiser


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let decNum = 5
    let binNum = uintToLogicLevelList decNum []
    let binNumBus = 
        List.mapi (fun i logLvl -> (i, logLvl)) binNum
        |> Map
    let paddedBinNum = padBusToLength binNumBus 6
    printfn "%A" paddedBinNum
    Console.ReadKey() |> ignore
    0 // return an integer exit code
