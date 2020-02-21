// Learn more about F# at http://fsharp.org

open System
open DGraphiser


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let decNum = 5
    let binNum = uintToLogicLevelList decNum []
    let paddedBinNum = padLogicLvlListToLength binNum 6
    
    printfn "%A" paddedBinNum
    Console.ReadKey() |> ignore
    0 // return an integer exit code
