# Individual Statement for Individual Code Submission - ANV17

This folder contains the following modules in order of compilation (all modules have dependencies on every module above them): 
* `EvalTypes.fs` - Contains the EvalNet type definition and has a dependency on the `SharedTypes.fs` module in the `shared` folder, which contains types need to be shared by all people on the team.
* `EvalNetHelper.fs` - Contains functions used to analyse and manipulate nets of the `EvalNet` type which encapsulates `Map<int, LogicLevel>`. The `getBusSize` function in this module is used in the `LogicBlockGen.fs` module (tkh2017) to precalculate the bus indices for temporary nets created for concatenation.
* `LogicOperations` - Contains functions to apply each `Operator` (excluding Concat for now) to EvalNets.
* `CombEval.fs` - Contains functions used to evaluate purely combinational Verilog modules, represented using the `TLogic` type. This module contains the top level function `evaluateModuleWithInputs` which is to be used in the `Simulation.fs` module (cc4117) to evaluate TLogic modules in the overall simulation. 

`evaluateModuleWithInputs` has 2 parameters: 
* `(combModule: TLogic)` - The combinational module to be evaluated. This will be an output of the `LogicBlockGen.fs` module.
* `(inputMap: Map<NetIdentifier, Net>)` - The inputs to the combinational module represented as a map from the input net identifier to the input net's value encapsulated by the `Net` type. The Map must contain all NetIdentifiers from the `Inputs` NetIdentifer list in `combModule`.

and gives one output of the type `Map<NetIdentifier, Net>` representing the outputs of the combinational module for the given inputs. This map will contain all NetIdentifiers from the `Outputs` NetIdentifier list in `combModule`.

My code is fairly self contained, with the only types used in the interface being `TLogic`, `NetIdentifier` and `Net`. 

`NetIdentifier` was initially created by me to allow for clearer representation of net definitions and accesses (slices/individual elements/whole nets). It was later adopted by the rest of the team and added to the `TLogic` module. 

The `NetIdentifier` is a Record type containing the fields:
* `Name: string` - The name of the net.
* `SliceIndices: (int * int option)` - This field can be used to represent wire definitions `None`, bus defintions `input[3:0] bus-> Some(3, Some 0)`, bus slices `bus[2:1] -> Some(2, Some 0)`, single wire access in busses `bus[1] -> Some(1, None)`, and full bus/wire access `bus -> None`. 

The `TLogic` and `Net` types are explained in the README in the shared folder linked here.
