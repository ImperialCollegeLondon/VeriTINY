// Learn more about F# at http://fsharp.org

open System

open Module1


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    deleteThisSample()
    Console.ReadKey()|>ignore
    0 // return an integer exit code

