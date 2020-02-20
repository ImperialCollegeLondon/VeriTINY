**Syntax of VeriTINY in BNF**

1. \<module> ::= "module" \<module_name> "(" \<variable_list> ")" ";" \<module_item_list> "endmodule"

2. \<module_name> ::= \<identifier>
  
3. \<module_item> ::= \<input_declaration> | \<output_declaration> | \<net_declaration> | \<gate_instantiation> 

4. \<input_declaration> ::= "input" \<variable_list> ";" | "input" \<range> \<variable_list> ";"

5. \<output_declaration> ::= "output" \<variable_list> ";" | "output" \<range> \<variable_list> ";"

6. \<net_declaration> ::= "wire" \<variable_list> ";" | "wire" \<range> \<variable_list> ";"

7. \<variable_list> ::= \<identifier> | \<identifier> "," \<variable_list>

8. \<range> ::= "\[" \<number> ":" \<number> "\]" 

9. \<gate_instantiation> ::= <gate_type> <gate_instance> ";"

10. \<gate_type> ::= "and" | "or" | "not"

11. \<gate_instance> ::= \<identifier> "(" \<terminal_list> ")"

12. \<terminal> ::= \<identifier> | \<identifier> "\[" \<number> "]" | \<identifier> "\[" \<number> ":" \<number> "\]"  

13. \<number> ::= //any sequence of 0..9 

14. \<identifier> ::= //any sequence of letters (case-sensitive), digits and underscore, first character must be a letter/underscore

15. \<module_item_list> ::= \<module_item> | \<module_item> \<module_item_list>

16. \<terminal_list> ::= \<terminal> | \<terminal> "," \<terminal_list>
