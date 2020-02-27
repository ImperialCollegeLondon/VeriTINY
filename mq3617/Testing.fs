open System
open Connector
open ConnectionTools

UserIn()

///for testing
open Expecto
open SharedTypes

[<Tests>]
let refactor1 =
  testCase "label simple list" <| fun () ->
    let inp = [(Name "Test",      //// Two megablocks, Test and DFF
                [(false, ("a", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                [(false, ("out", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                (Name "DFF", [(false, ("a", Bus (Map [(0, Low); (1, Low)])))], 
                    [(true, ("out", Bus (Map [(0, Low); (1, Low)])))])]  
    let expected = [(Name "Test", [(false, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                       [(false, ("out2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                       (Name "DFF", [(false, ("a1", Bus (Map [(0, Low); (1, Low)])))],
                           [(true, ("out1", Bus (Map [(0, Low); (1, Low)])))])]
    Expect.equal (refactor inp) expected "label simple list"

[<Tests>]
let refactor2 =
  testCase "label list with repeating entires" <| fun () ->
    let inp = [(Name "Test",   //// Two megablocks, Test twice 
                [(false, ("a", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                [(false, ("out", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                (Name "Test",
                    [(false, ("a", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                    [(false, ("out", Bus (Map [(0, Low); (1, Low); (2, Low)])))])]
    let expected = [(Name "Test", [(false, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                       [(false, ("out2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                       (Name "Test", [(false, ("a1", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                           [(false, ("out1", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);]
    Expect.equal (refactor inp) expected "label list with repeating entires"

[<Tests>]
let uncon1 =
    testCase "Single Megablock, 2 unconnected input" <| fun () ->
    let inp = [(Name "Test",   //// Just Test
                [(false, ("a", Bus (Map [(0, Low); (1, Low); (2, Low)])));(false, ("b", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                [(false, ("out", Bus (Map [(0, Low); (1, Low); (2, Low)])))])]
    let expected = [(false, ("a", Bus (Map [(0, Low); (1, Low); (2, Low)])));(false, ("b", Bus (Map [(0, Low); (1, Low); (2, Low)])))]
    Expect.equal (findUnconnectedIn inp) expected "Single Megablock, 2 unconnected input"

[<Tests>]
let uncon2=
    testCase "Setup with no unconnected inputs" <| fun () ->
    let inp = [(Name "DFF", [(true, ("a", Bus (Map [(0, Low); (1, Low)])))],   //// DFF connected like a frequency divider
                    [(true, ("a", Bus (Map [(0, Low); (1, Low)])))])]
    let expected = []
    Expect.equal (findUnconnectedIn inp) expected "Setup with no unconnected inputs"

[<Tests>]
let netUpdate1=
    testCase "No connections made to megablocks" <| fun () ->
    let inpBlocklst = [(Name "Test", [(false, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                        [(false, ("out2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                        (Name "DFF", [(false, ("a1", Bus (Map [(0, Low); (1, Low)])))],
                           [(true, ("out1", Bus (Map [(0, Low); (1, Low)])))])]
    let inpLinks =[]
    let expected = [(Name "Test", [(false, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                       [(false, ("out2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                       (Name "DFF", [(false, ("a1", Bus (Map [(0, Low); (1, Low)])))],
                           [(true, ("out1", Bus (Map [(0, Low); (1, Low)])))])]
    Expect.equal (List.map (fun x -> (first x,updateNets (second x) inpLinks,updateNets (third x) inpLinks)) inpBlocklst) expected "No connections made to megablocks"

[<Tests>]
let netUpdate2=
    testCase "megablocks with connections, clocked" <| fun () ->
    let inpBlocklst = [(Name "Test", [(false, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                        [(false, ("out2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                        (Name "DFF", [(false, ("a1", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                           [(true, ("out1", Bus (Map [(0, Low); (1, Low); (2, Low)])))])]
    let inpLinks =["a2","out1",(true, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]
    let expected = [(Name "Test", [(true, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                       [(false, ("out2", Bus (Map [(0, Low); (1, Low); (2, Low)])))]);
                       (Name "DFF", [(false, ("a1", Bus (Map [(0, Low); (1, Low); (2, Low)])))],
                           [(true, ("a2", Bus (Map [(0, Low); (1, Low); (2, Low)])))])]
    Expect.equal (List.map (fun x -> (first x,updateNets (second x) inpLinks,updateNets (third x) inpLinks)) inpBlocklst) expected "megablocks with connections, clocked"


let testListWithExpecto =
  testList "A test group" [
    refactor1
    refactor2
    uncon1
    uncon2
    netUpdate1
    netUpdate2
  ]

let testsWithExpecto() =
    runTests defaultConfig testListWithExpecto |> ignore

[<EntryPoint>]
let main argv =
    printf "tests with expecto"
    testsWithExpecto() |> ignore
    Console.ReadKey()|>ignore
    0 // return an integer exit code
