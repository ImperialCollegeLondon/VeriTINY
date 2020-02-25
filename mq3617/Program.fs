open System
open Connectioniser

let connectionList = UserIn ()
let inputs =findUnconnected connectionList
[<EntryPoint>]
let main argv =
    Console.ReadKey()|>ignore
    0 // return an integer exit code
