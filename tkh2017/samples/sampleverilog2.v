module testblock(a, b, out);

    input[1:0]a, b;
    output[1:0] out;
    wire c;

    and (c, a[1], b[0]);
    or (out, {a[0], b[1]}, {c, a[0]});

endmodule