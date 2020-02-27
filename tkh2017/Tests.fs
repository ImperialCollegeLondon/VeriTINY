module Tests

open System 
open SharedTypes
open Lexer
open Parser
open LogicBlockGen

let sampleCode1 = System.IO.File.ReadAllText "tkh2017/samples/sampleverilog1.v"
let sampleCode2 = System.IO.File.ReadAllText "tkh2017/samples/sampleverilog2.v"   
let sampleCode3 = System.IO.File.ReadAllText "tkh2017/samples/sampleverilog3.v"   

let lexerTest1Ans = [Module; Identifier "mux_2x1"; OpRoundBracket; Identifier "a"; Comma;
                     Identifier "b"; Comma; Identifier "sel"; Comma; Identifier "out";
                     ClRoundBracket; Semicolon; Input; Identifier "a"; Comma; Identifier "b"; Comma;
                     Identifier "sel"; Semicolon; Output; Identifier "out"; Semicolon; Wire;
                     Identifier "sel_n"; Semicolon; Wire; Identifier "out_0"; Semicolon; NotTok;
                     OpRoundBracket; Identifier "sel_n"; Comma; Identifier "sel"; ClRoundBracket;
                     Semicolon; AndTok; OpRoundBracket; Identifier "out_0"; Comma; Identifier "a";
                     Comma; Identifier "sel"; ClRoundBracket; Semicolon; AndTok; OpRoundBracket;
                     Identifier "out_1"; Comma; Identifier "b"; Comma; Identifier "sel_n";
                     ClRoundBracket; Semicolon; OrTok; OpRoundBracket; Identifier "out"; Comma;
                     Identifier "out_0"; Comma; Identifier "out_1"; ClRoundBracket; Semicolon;
                     EndModule]

let lexerTest2Ans = [Module; Identifier "testblock"; OpRoundBracket; Identifier "a"; Comma;
                     Identifier "b"; Comma; Identifier "out"; ClRoundBracket; Semicolon; Input;
                     OpSqBracket; Number 1; Colon; Number 0; ClSqBracket; Identifier "a"; Comma;
                     Identifier "b"; Semicolon; Output; OpSqBracket; Number 1; Colon; Number 0;
                     ClSqBracket; Identifier "out"; Semicolon; Wire; Identifier "c"; Semicolon; AndTok;
                     OpRoundBracket; Identifier "c"; Comma; Identifier "a"; OpSqBracket; Number 1;
                     ClSqBracket; Comma; Identifier "b"; OpSqBracket; Number 0; ClSqBracket;
                     ClRoundBracket; Semicolon; OrTok; OpRoundBracket; Identifier "out"; Comma; OpBrace;
                     Identifier "a"; OpSqBracket; Number 0; ClSqBracket; Comma; Identifier "b";
                     OpSqBracket; Number 1; ClSqBracket; ClBrace; Comma; OpBrace; Identifier "c";
                     Comma; Identifier "a"; OpSqBracket; Number 0; ClSqBracket; ClBrace;
                     ClRoundBracket; Semicolon; EndModule]

let lexerTest3Ans = [Module; Identifier "testblock"; OpRoundBracket; Identifier "a"; Comma;
                     Identifier "b"; Comma; Identifier "c"; Comma; Identifier "out"; ClRoundBracket;
                     Semicolon; Input; OpSqBracket; Number 2; Colon; Number 0; ClSqBracket;
                     Identifier "a"; Comma; Identifier "b"; Semicolon; Input; Identifier "c";
                     Semicolon; Output; OpSqBracket; Number 2; Colon; Number 0; ClSqBracket;
                     Identifier "out"; Semicolon; Wire; Identifier "d"; Semicolon; Wire; OpSqBracket;
                     Number 1; Colon; Number 0; ClSqBracket; Identifier "e"; Semicolon; AndTok;
                     OpRoundBracket; Identifier "d"; Comma; Identifier "a"; OpSqBracket; Number 1;
                     ClSqBracket; Comma; Identifier "b"; OpSqBracket; Number 0; ClSqBracket;
                     ClRoundBracket; Semicolon; AndTok; OpRoundBracket; OpBrace; Identifier "e";
                     OpSqBracket; Number 1; ClSqBracket; Comma; Identifier "out"; OpSqBracket;
                     Number 1; Colon; Number 0; ClSqBracket; ClBrace; Comma; Identifier "a"; Comma;
                     OpBrace; Identifier "b"; OpSqBracket; Number 1; Colon; Number 0; ClSqBracket;
                     Comma; Identifier "d"; ClBrace; ClRoundBracket; Semicolon; EndModule]       
     
let parserTest1Ans = Ok (MODULE ("mux_2x1",["a"; "b"; "sel"; "out"],
                         [INPWire ["a"; "b"; "sel"]; OUTWire ["out"]; WIRE ["sel_n"];
                          WIRE ["out_0"]; GATEINST (NOT,"",[TERMID "sel_n"; TERMID "sel"]);
                          GATEINST (AND,"",[TERMID "out_0"; TERMID "a"; TERMID "sel"]);
                          GATEINST (AND,"",[TERMID "out_1"; TERMID "b"; TERMID "sel_n"]);
                          GATEINST (OR,"",[TERMID "out"; TERMID "out_0"; TERMID "out_1"])])) 

let parserTest2Ans = Ok (MODULE ("testblock",["a"; "b"; "out"],
                         [INPBus (1,0,["a"; "b"]); OUTBus (1,0,["out"]); WIRE ["c"];
                          GATEINST (AND,"",[TERMID "c"; TERMIDWire ("a",1); TERMIDWire ("b",0)]);
                          GATEINST (OR,"",[TERMID "out"; TERMCONCAT [TERMIDWire ("a",0); TERMIDWire ("b",1)]; TERMCONCAT [TERMID "c"; TERMIDWire ("a",0)]])]))

let parserTest3Ans = Ok (MODULE ("testblock",["a"; "b"; "c"; "out"],
                         [INPBus (2,0,["a"; "b"]); INPWire ["c"]; OUTBus (2,0,["out"]); WIRE ["d"];
                          WIREBus (1,0,["e"]); GATEINST (AND,"",[TERMID "d"; TERMIDWire ("a",1); TERMIDWire ("b",0)]);
                          GATEINST (AND,"",[TERMCONCAT [TERMIDWire ("e",1); TERMIDBus ("out",1,0)]; TERMID "a"; TERMCONCAT [TERMIDBus ("b",1,0); TERMID "d"]])]))

let logicBlockGenTest1Ans = {Name = "mux_2x1";
                             ExpressionList = [(Not, [{ Name = "sel_n"; SliceIndices = None }], 
                                                     [{ Name = "sel"; SliceIndices = None }]);
                                               (And, [{ Name = "out_0"; SliceIndices = None }],
                                                     [{ Name = "a"; SliceIndices = None }; 
                                                      { Name = "sel"; SliceIndices = None }]);
                                               (And, [{ Name = "out_1"; SliceIndices = None }],
                                                     [{ Name = "b"; SliceIndices = None }; 
                                                      { Name = "sel_n"; SliceIndices = None }]);
                                               (Or, [{ Name = "out"; SliceIndices = None }],
                                                    [{ Name = "out_0"; SliceIndices = None }; 
                                                     { Name = "out_1"; SliceIndices = None }])]
                             Inputs = [{ Name = "a"; SliceIndices = None };
                                       { Name = "b"; SliceIndices = None };
                                       { Name = "sel"; SliceIndices = None }]
                             Outputs = [{ Name = "out"; SliceIndices = None }]
                             Wires = [{ Name = "sel_n"; SliceIndices = None }; 
                                      { Name = "out_0"; SliceIndices = None }]}             

let logicBlockGenTest2Ans = {Name = "testblock";
                             ExpressionList = [(Concat, [{ Name = "1"; SliceIndices = Some (0, Some 1) }],
                                                        [{ Name = "c"; SliceIndices = None }; 
                                                         { Name = "a"; SliceIndices = Some (0, None) }]);
                                               (Concat, [{ Name = "0"; SliceIndices = Some (0, Some 1) }],
                                                        [{ Name = "a"; SliceIndices = Some (0, None) };
                                                         { Name = "b"; SliceIndices = Some (1, None) }]);
                                               (And, [{ Name = "c"; SliceIndices = None }],
                                                     [{ Name = "a"; SliceIndices = Some (1, None) };
                                                      { Name = "b"; SliceIndices = Some (0, None) }]);
                                               (Or, [{ Name = "out"; SliceIndices = None }],
                                                    [{ Name = "0"; SliceIndices = None }; { Name = "1"; SliceIndices = None }])]
                             Inputs = [{ Name = "a"; SliceIndices = Some (1, Some 0) };
                                       { Name = "b"; SliceIndices = Some (1, Some 0) }]
                             Outputs = [{ Name = "out"; SliceIndices = Some (1, Some 0) }]
                             Wires = [{ Name = "1"; SliceIndices = Some (0, Some 1) };
                                      { Name = "0"; SliceIndices = Some (0, Some 1) }; 
                                      { Name = "c"; SliceIndices = None }]}           

let logicBlockGenTest3Ans = {Name = "testblock"; 
                             ExpressionList = [(Concat, [{ Name = "1"; SliceIndices = Some (0, Some 2) }],
                                                        [{ Name = "b"; SliceIndices = Some (1, Some 0) };
                                                         { Name = "d"; SliceIndices = None }]);
                                               (Concat, [{ Name = "0"; SliceIndices = Some (0, Some 2) }],
                                                        [{ Name = "e"; SliceIndices = Some (1, None) };
                                                         { Name = "out"; SliceIndices = Some (1, Some 0) }]);
                                               (And, [{ Name = "d"; SliceIndices = None }],
                                                     [{ Name = "a"; SliceIndices = Some (1, None) };
                                                      { Name = "b"; SliceIndices = Some (0, None) }]);
                                               (And, [{ Name = "0"; SliceIndices = None }],
                                                     [{ Name = "a"; SliceIndices = None }; 
                                                      { Name = "1"; SliceIndices = None }])]
                             Inputs = [{ Name = "a"; SliceIndices = Some (2, Some 0) };
                                       { Name = "b"; SliceIndices = Some (2, Some 0) }; 
                                       { Name = "c"; SliceIndices = None }]
                             Outputs = [{ Name = "out"; SliceIndices = Some (2, Some 0) }]
                             Wires = [{ Name = "1"; SliceIndices = Some (0, Some 2) };
                                      { Name = "0"; SliceIndices = Some (0, Some 2) }; 
                                      { Name = "d"; SliceIndices = None };
                                      { Name = "e"; SliceIndices = Some (1, Some 0) }]}

