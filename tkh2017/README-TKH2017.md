**Syntax of VeriTINY in BNF**

1. \<module> ::= "module" \<module_name> \<port_list>? ";" \<module_item>* "endmodule"

2. \<module_name> ::= \<identifier>

3. \<port_list> ::= "(" \<port> \<, <port>>* ")"
  
4. \<port> ::= \<identifier>
  
5. \<module_item> ::= \<input_declaration> | \<output_declaration> | \<net_declaration> | \<gate_instantiation> 

6. \<input_declaration> ::= "input" \<range>? \<variable_list> ";"

7. \<output_declaration> ::= "output" \<range>? \<variable_list> ";"

8. \<net_declaration> ::= "net" \<range>? \<variable_list> ";"

9. \<net_type> ::= wire

10. \<variable_list> ::= \<variable_name> \<"," <variable_name>>*

11. \<variable_name> ::= \<identifier>

12. \<range> ::= "\[" \<expression> ":" \<expression> "\]"

13. \<gate_instantiation> ::= <gate_type> <gate_instance> ";"

14. \<gate_type> ::= "and" | "or" | "not"

15. \<gate_instance> ::= \<gate_instance_name>? "(" \<terminal> \<, <terminal>>* ")"
  
16. \<gate_instance_name> ::= \<identifier>

17. \<terminal> ::= \<identifer> | \<expression>

18. \<expression> ::= \<primary>

19. \<primary> ::= \<identifier> | \<number> | \<identifier> "\[" \<expression> "]" | \<identifier> "\[" \<expression> ":" \<expression> "\]"  

20. \<number> ::= //any sequence of 0..9 

21. \<identifier> ::= //any sequence of letters (case-sensitive), digits and underscore, first character must be a letter/underscore
