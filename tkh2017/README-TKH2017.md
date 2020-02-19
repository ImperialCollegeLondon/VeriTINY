**Syntax of VeriTINY in BNF**

1. \<module> ::= "module" \<module_name> "(" \<port_list> ")" ";" \<module_item_list> "endmodule"

2. \<module_name> ::= \<identifier>

3. \<port_list> ::= \<port> | \<port> "," \<port_list>
  
4. \<port> ::= \<identifier>
  
5. \<module_item> ::= \<input_declaration> | \<output_declaration> | \<net_declaration> | \<gate_instantiation> | 

6. \<input_declaration> ::= "input" \<variable_list> ";" | input" \<range> \<variable_list> ";"

7. \<output_declaration> ::= "output" \<variable_list> ";" | "output" \<range> \<variable_list> ";"

8. \<net_declaration> ::= "net" \<variable_list> ";" | "net" \<range> \<variable_list> ";"

9. \<net_type> ::= wire

10. \<variable_list> ::= \<variable_name> | \<variable_name> "," \<variable_list>

11. \<variable_name> ::= \<identifier>

12. \<range> ::= "\[" \<number> ":" \<number> "\]" 

13. \<gate_instantiation> ::= <gate_type> <gate_instance> ";"

14. \<gate_type> ::= "and" | "or" | "not"

15. \<gate_instance> ::= \<gate_instance_name> "(" \<terminal_list> ")"
  
16. \<gate_instance_name> ::= \<identifier>

17. \<terminal> ::= \<identifer> | \<expression>

19. \<expression> ::= \<identifier> | \<identifier> "\[" \<number> "]" | \<identifier> "\[" \<number> ":" \<number> "\]"  

20. \<number> ::= //any sequence of 0..9 

21. \<identifier> ::= //any sequence of letters (case-sensitive), digits and underscore, first character must be a letter/underscore

22. \<module_item_list> ::= \<module_item> | \<module_item> \<module_item_list>

23. \<terminal_list> ::= \<terminal> | \<terminal> "," \<terminal_list>
