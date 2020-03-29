module Tests

open SharedTypes
open Lexer
open Parser
open LogicBlockGen

let sampleCode1 = "module mux_2x1 (a, b, sel, out);

                        input a, b, sel;
                        output out;
                        wire sel_n;
                        wire out_0;

                        not (sel_n, sel);
                        and(out_0, a, sel);
                        and (out_1, b, sel_n);
                        or (out, out_0, out_1);

                   endmodule"

let sampleCode2 = "module testblock(a, b, out);

                        input[1:0]a, b;
                        output[1:0] out;
                        wire c;

                        and (c, a[1], b[0]);
                        or (out, {a[0], b[1]}, {c, a[0]});
                   endmodule"   

let sampleCode3 = "module testblock(a, b, c, out);

                        input[2:0] a, b;
                        input c;
                        output[2:0] out;
                        wire d;
                        wire[1:0] e;

                        and (d, a[1], b[0]);
                        and ({e[1], out[1:0]}, a, {b[1:0], d});
                   endmodule"

let sampleCode4 = "module test(out, a, b, c);
                        input  a, b, c;
                        output out;

                        assign out = (a | b) | c;
                   endmodule"  

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

let lexerTest4Ans = [Module; Identifier "test"; OpRoundBracket; Identifier "out"; Comma; Identifier "a"; Comma; Identifier "b"; Comma; Identifier "c"; ClRoundBracket;
                     Semicolon; Input; Identifier "a"; Comma; Identifier "b"; Comma; Identifier "c";
                     Semicolon; Output; Identifier "out"; Semicolon; AssignTok; Identifier "out";
                     Equals; OpRoundBracket; Identifier "a"; OrOpTok; Identifier "b"; ClRoundBracket;
                     OrOpTok; Identifier "c"; Semicolon; EndModule]   
    
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

let parserTest4Ans = Ok (MODULE ("test",["out"; "a"; "b"; "c"],
                          [INPWire ["a"; "b"; "c"]; OUTWire ["out"];
                           ASSIGN (TERMID "out", OREXP (TERMEXP (EXP (OREXP (TERMEXP (TERMID "a"),TERMEXP (TERMID "b")))), TERMEXP (TERMID "c")))]))


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

let logicBlockGenTest4Ans = {Name = "test";
                             ExpressionList = [(Or, [{ Name = "out"; SliceIndices = None }],
                                                    [{ Name = "0"; SliceIndices = None }; 
                                                     { Name = "3"; SliceIndices = None }]);
                                               (Or, [{ Name = "0"; SliceIndices = None }],
                                                    [{ Name = "1"; SliceIndices = None }; 
                                                     { Name = "2"; SliceIndices = None }]);
                                               (Pass, [{ Name = "1"; SliceIndices = None }], 
                                                      [{ Name = "a"; SliceIndices = None }]);
                                               (Pass, [{ Name = "2"; SliceIndices = None }], 
                                                      [{ Name = "b"; SliceIndices = None }]);
                                               (Pass, [{ Name = "3"; SliceIndices = None }], 
                                                      [{ Name = "c"; SliceIndices = None }])]
                             Inputs = [{ Name = "a"; SliceIndices = None }; 
                                        { Name = "b"; SliceIndices = None };
                                        { Name = "c"; SliceIndices = None }]
                             Outputs = [{ Name = "out"; SliceIndices = None }]
                             Wires = [{ Name = "3"; SliceIndices = None }; 
                                       { Name = "0"; SliceIndices = None };
                                       { Name = "2"; SliceIndices = None }; 
                                       { Name = "1"; SliceIndices = None }]}