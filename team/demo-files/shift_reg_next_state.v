module shift_reg_next_state(currParallelOuput, currInput, nextOutput, currSerOutput);
    input[3:0] currParallelOuput;
    input currInput;
    output[3:0] nextOutput;
    output currSerOutput;

    assign nextOutput = {currParallelOuput[2:0], currInput};
    assign currSerOutput = currParallelOuput[3];

endmodule