module mux2_gate23 (out, outbar, a, b, sel);

    output[2:0]  out;
    input a, b, sel; 
    wire out1, out2, selb; 
    
    not i1 (selb, sel);
    and a1 (out1, a, sel);
    and a2 (out2, b, selb);
    or o1 (out[0], out1, out2);
    not i2 (out[1], out[0]);

endmodule