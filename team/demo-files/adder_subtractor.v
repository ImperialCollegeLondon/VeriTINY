module adder_subtractor(ctrl, s, a, b);
    output[4:0] s;
    input ctrl;
    input[2:0] a,b;
    wire c0, c1, c2;

    assign s[0] = ((a[0] ^ control) ^ b[0]) ^ ctrl;
    assign s[1] = ((a[1] ^ control) ^ b[1]) ^ c0;
    assign s[2] = ((a[2] ^ control) ^ b[2]) ^ c1;
    assign s[3] = s[4] ^ s[2];
    assign s[4] = c1 ^ c2;
    assign c0 = ((a[0] ^ control) & b[0]) | (ctrl & ((a[0] ^ ctrl) | b[0]));
    assign c1 = ((a[1] ^ control) & b[1]) | (c0 & ((a[1] ^ ctrl) | b[1]));
    assign c2 = ((a[2] ^ control) & b[2]) | (c1 & ((a[2] ^ ctrl) | b[2]));

endmodule



    