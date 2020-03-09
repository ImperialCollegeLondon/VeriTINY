# Individual Statement - tkh2017

## Lexer

The top level function for this module is `tokenise: string -> Token list` which takes as input a string of VeriTINY code (a small subset of Verilog currently allowing circuit design at gate level; continuous assign will be implemented in the group stage) and outputs a list of Tokens. The cases of the `Token` DU were worked out from the BNF of VeriTINY - anything in quotes + numbers and identifiers (definitions 13, 14). The `Token` list will then be piped into the parser for parsing. 

## Parser

The top level function for this module is `parse: Token list -> Result<ModuleType, (int * Token list)>`. In the group stage the return type will be modified to `Result<ModuleType, string>` to handle error messages. The AST is the single case `ModuleType` DU derived from BNF definition 1 and is a 3-tuple containing the module name (`string`), list of ports (`string list`) and list of module items (`ModuleItemType`, e.g. declarations, gate instantiations, continuous assigns). `ModuleItemType` is another DU representing BNF definitions 4, 5, 6, 9. 

## LogicBlockGen

The top level function for this module is `convertAST: ModuleType -> TLogic` which deconstructs the AST from the parser into a more useful `record` type. `TLogic` has been revised since the team work plan submission to decouple further AST deconstruction work from other team member's modules. `TLogic` now contains explicit information about slicing/busses.  

## Module Ordering & Program Flow

Compilation will be in the following order since all modules have dependencies on modules above them:
1. Lexer
2. Parser 
3. LogicBlockGen

The `TLogic` output from LogicBlockGen will be used by modules written by other team members such as `Connector`, `Simulator` and `CombEval` for simulation.  

## Syntax of VeriTINY in BNF

1. \<module> ::= "module" \<module_name> "(" \<variable_list> ")" ";" \<module_item_list> "endmodule"

2. \<module_name> ::= \<identifier>
  
3. \<module_item> ::= \<input_declaration> | \<output_declaration> | \<net_declaration> | \<gate_instantiation> | \<continuous_assign>

4. \<input_declaration> ::= "input" \<variable_list> ";" | "input" \<range> \<variable_list> ";"

5. \<output_declaration> ::= "output" \<variable_list> ";" | "output" \<range> \<variable_list> ";"

6. \<net_declaration> ::= "wire" \<variable_list> ";" | "wire" \<range> \<variable_list> ";"

7. \<variable_list> ::= \<identifier> | \<identifier> "," \<variable_list>

8. \<range> ::= "\[" \<number> ":" \<number> "\]" 

9. \<gate_instantiation> ::= <gate_type> <gate_instance> ";"

10. \<gate_type> ::= "and" | "or" | "not"

11. \<gate_instance> ::= \<identifier> "(" \<terminal_list> ")" | "(" \<terminal_list> ")"

12. \<number> ::= //any sequence of 0..9 

13. \<identifier> ::= //any sequence of letters (case-sensitive), digits and underscore, first character must be a letter/underscore

14. \<module_item_list> ::= \<module_item> | \<module_item> \<module_item_list>

15. \<terminal_list> ::= \<terminal> | \<terminal> "," \<terminal_list>

16. \<continuous_assign> ::= "assign" \<terminal> "=" \<assignment> ";"

17. \<assignment> ::= \<expression> 

18. \<terminal> ::= \<identifier> | \<identifier> "\[" \<number> "]" | \<identifier> "\[" \<number> ":" \<number> "\]" | "{" \<terminal_list> "}" //only 1 depth of concatenation is allowed at present

19. \<not_exp> ::= \<terminal> | "~" \<not_exp>

20. \<and_exp> ::= \<not_exp> | \<not_exp> "&" \<and_exp>

21. \<or_exp> ::= \<and_exp> | \<and_exp> "|" \<or_exp>

**No other code in this directory except for one function used in LogicBlockGen (**`getBusSize` from `EvalNetHelper`**) is written by other team members**

**No code in this directory is written for other team members**