﻿open System
open Connectioniser

UserIn |>ignore
[<EntryPoint>]
let main argv =
    Console.ReadKey()|>ignore
    0 // return an integer exit code

