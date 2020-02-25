module bus_mux (out, a, sel);

    output out;
    input[1:0] a;
    input sel; 
    wire selb; 

    and a1 ({out[2], out[1]}, {a[1], selb}, sel);

endmodule