module deca4FSM(currState, exec1, exec2, fetch, nextState, extra);

    input[2:0] currState;
    output[2:0] nextState;

    input extra;
    output exec1, exec2, fetch;

    assign fetch = ~currState[0] & ~currState[1] & ~currState[0];
    assign exec1 = currState[0];
    assign exec2 = currState[1];

    assign nextState[0] = ~currState[2] & ~currState[1] & ~currState[0];
    assign nextState[1] = ~currState[2] & ~currState[1] & currState[0] & extra;

endmodule