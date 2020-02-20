**Syntax of VeriTINY in BNF**

1. \<module> ::= "module" \<module_name> "(" \<port_list> ")" ";" \<module_item_list> "endmodule"

2. \<module_name> ::= \<identifier>

3. \<port_list> ::= \<identifier> | \<identifier> "," \<port_list>
  
4. \<module_item> ::= \<input_declaration> | \<output_declaration> | \<net_declaration> | \<gate_instantiation> 

5. \<input_declaration> ::= "input" \<variable_list> ";" | "input" \<range> \<variable_list> ";"

6. \<output_declaration> ::= "output" \<variable_list> ";" | "output" \<range> \<variable_list> ";"

7. \<net_declaration> ::= "wire" \<variable_list> ";" | "wire" \<range> \<variable_list> ";"

8. \<variable_list> ::= \<identifier> | \<identifier> "," \<variable_list>

9. \<range> ::= "\[" \<number> ":" \<number> "\]" 

10. \<gate_instantiation> ::= <gate_type> <gate_instance> ";"

11. \<gate_type> ::= "and" | "or" | "not"

12. \<gate_instance> ::= \<identifier> "(" \<terminal_list> ")"

13. \<terminal> ::= \<identifier> | \<identifier> "\[" \<number> "]" | \<identifier> "\[" \<number> ":" \<number> "\]"  

14. \<number> ::= //any sequence of 0..9 

15. \<identifier> ::= //any sequence of letters (case-sensitive), digits and underscore, first character must be a letter/underscore

16. \<module_item_list> ::= \<module_item> | \<module_item> \<module_item_list>

17. \<terminal_list> ::= \<terminal> | \<terminal> "," \<terminal_list>
