# Syntax of VeriTINY in BNF

<module> ::= "module" <module_name> <port_list>? ";" <module_item>* "endmodule"

<module_name> ::= <identifier>

<port_list> ::= "(" <port> <,<port>>* ")"
  
<port> ::= <identifier>
  
<module_item> ::= <input_declaration> | <output_declaration> | <net_declaration> | <gate_instantiation> 

<input_declaration> ::= "input" <range>? <variable_list> ";"
  

  


