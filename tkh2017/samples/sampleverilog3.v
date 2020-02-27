module testblock(a, b, c, out);

    input[2:0] a, b;
    input c;
    output[2:0] out;
    wire d;
    wire[1:0] e;

    and (d, a[1], b[0]);
    and ({e[1], out[1:0]}, a, {b[1:0], d});

endmodule