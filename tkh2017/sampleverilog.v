module mux_gate (out, outbar, a, b, sel);

    output  out, outbar;
    input a, b, sel; 
    wire out1, out2, selb; 
    
    not i1 (selb, sel);
    and a1 (out1, a, sel);
    and a2 (out2, b, selb);
    or o1 (out, out1, out2);
    not i2 (outbar, out)
    
    endmodule