open System
open Lexer
open Parser
open LogicBlockGen
open Tests
open Expecto

[<Tests>]
let lexerTest1 = 
    testCase "Lexer test 1" <| fun () -> 
        let expected = lexerTest1Ans
        Expect.equal (tokenise sampleCode1) expected "Lexer test 1"

[<Tests>]
let lexerTest2 = 
    testCase "Lexer test 2" <| fun () -> 
        let expected = lexerTest2Ans
        Expect.equal (tokenise sampleCode2) expected "Lexer test 2"

[<Tests>]
let lexerTest3 = 
    testCase "Lexer test 3" <| fun () -> 
        let expected = lexerTest3Ans
        Expect.equal (tokenise sampleCode3) expected "Lexer test 3"

[<Tests>]
let lexerTest4 = 
    testCase "Lexer test 4" <| fun () -> 
        let expected = lexerTest4Ans
        Expect.equal (tokenise sampleCode4) expected "Lexer test 4"

[<Tests>]
let parserTest1 = 
    testCase "Parser test 1" <| fun () -> 
        let expected = parserTest1Ans
        Expect.equal (parse lexerTest1Ans) expected "Parser test 1"    

[<Tests>]
let parserTest2 = 
    testCase "Parser test 2" <| fun () -> 
        let expected = parserTest2Ans
        Expect.equal (parse lexerTest2Ans) expected "Parser test 2"   

[<Tests>]
let parserTest3 = 
    testCase "Parser test 3" <| fun () -> 
        let expected = parserTest3Ans
        Expect.equal (parse lexerTest3Ans) expected "Parser test 3"   

[<Tests>]
let parserTest4 = 
    testCase "Parser test 4" <| fun () -> 
        let expected = parserTest4Ans
        Expect.equal (parse lexerTest4Ans) expected "Parser test 4"    

[<Tests>]
let logicBlockGeTest1 = 
    testCase "Logic Block Gen test 1" <| fun () -> 
        let expected = logicBlockGenTest1Ans
        Expect.equal (match parserTest1Ans with | Ok ast -> convertAST ast | _ -> failwithf "What?") expected "Logic Block Gen test 1"   
 
[<Tests>]
let logicBlockGeTest2 = 
    testCase "Logic Block Gen test 2" <| fun () -> 
        let expected = logicBlockGenTest2Ans
        Expect.equal (match parserTest2Ans with | Ok ast -> convertAST ast | _ -> failwithf "What?") expected "Logic Block Gen test 2" 

[<Tests>]
let logicBlockGeTest3 = 
    testCase "Logic Block Gen test 3" <| fun () -> 
        let expected = logicBlockGenTest3Ans
        Expect.equal (match parserTest3Ans with | Ok ast -> convertAST ast | _ -> failwithf "What?") expected "Logic Block Gen test 3" 

[<Tests>]
let logicBlockGeTest4 = 
    testCase "Logic Block Gen test 4" <| fun () -> 
        let expected = logicBlockGenTest4Ans
        Expect.equal (match parserTest4Ans with | Ok ast -> convertAST ast | _ -> failwithf "What?") expected "Logic Block Gen test 4" 

let allTestsWithExpecto() =
        runTestsInAssembly defaultConfig [||]
    
[<EntryPoint>]
let main argv =
    allTestsWithExpecto() |> ignore
    let test = "module test(out, a, b, c);
                    input  a, b, c;
                    output out;

                    assign out = (a | b) | c;
                endmodule"
    // printf "%A" (test |> tokenise)
    // printf "%A" (test |> tokenise |> parse) 
    // printf "%A" (test |> tokenise |> parse |> (fun x -> match x with | Ok ast -> convertAST ast | _ -> failwithf "What?"))
    Console.ReadKey() |> ignore
    0 // return an integer exit code
