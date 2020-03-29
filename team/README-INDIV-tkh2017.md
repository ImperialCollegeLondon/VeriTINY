# Individual Statement for Team Code Submission - tkh2017

During the group phase of the project, I first continued to work on the `Lexer`, `Parser` and `LogicBlockGen` modules. Specifically, I added support for continuous assigns, bracketed expressions and also fixed some bugs in concatenations. 

The following BNF definitions were added to VeriTINY: 

```
16. \<continuous_assign> ::= "assign" \<terminal> "=" \<assignment> ";" //output terminal with concatenation is not allowed at present

17. \<assignment> ::= \<or_exp> 

18. \<terminal> ::= \<identifier> | \<identifier> "\[" \<number> "]" | \<identifier> "\[" \<number> ":" \<number> "\]" |  | "(" \<or_exp> ")" | "{" \<terminal_list> "}" //only 1 depth of concatenation is allowed at present

19. \<not_exp> ::= \<terminal> | "~" \<not_exp> 

20. \<and_exp> ::= \<not_exp> | \<not_exp> "&" \<and_exp>

21. \<xor_exp> ::= \<and_exp> | \<and_exp> "^" \<xor_exp>

22. \<or_exp> ::= \<xor_exp> | \<xor_exp> "|" \<or_exp>
```

For continuous assigns, 4 binary operators were implemented and, as in Verilog, "~" has the highest precedence, followed by "^", "&" and finally "|". 

VeriTINY is right associative, hence the statement `assign out = a|b|c` is the same as `assign out = a|(b|c)`.

After our team interview on 25/3/2020, support for the xor operator and bracketed expressions were added so that the need of having to declare many intermediate connections is reduced.

During the final stages of the group phase, I worked on the integration of the compilation process with the GUI. This was done in the `CompilationManager` module which manages the lexing, parsing, AST deconstructing and module generating work when the user clicks the compile button.  

Finally, I also worked on the syntax highlighting and designed a logo for our program. 

<img src="https://github.com/channen2/Verilog2/blob/master/visual.png" width="64" height="64">
