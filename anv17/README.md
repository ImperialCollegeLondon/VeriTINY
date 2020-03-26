# Individual Statement for Individual Code Submission - ANV17

This folder contains the following modules in order of compilation (all modules have dependencies on every module above them): 
* `EvalTypes.fs` - Contains the EvalNet type definition and has a dependency on the `SharedTypes.fs` module in the `shared` folder, which contains types need to be shared by all people on the team.
* `EvalNetHelper.fs` - Contains functions used to analyse and manipulate nets of the `EvalNet` type which encapsulates `Map<int, LogicLevel>`. The `getBusSize` function in this module is used in the `LogicBlockGen.fs` module (tkh2017) to precalculate the bus indices for temporary nets created for concatenation.
* `LogicOperations` - Contains functions to apply each `Operator` to EvalNets.
* `CombEval.fs` - Contains functions used to evaluate purely combinational VeriTINY modules, represented using the `TLogic` type. This module contains the top level function `evaluateModuleWithInputs` which is to be used in the `Simulation.fs` module (cc4117) to evaluate TLogic modules in the overall simulation. 

`evaluateModuleWithInputs` has 3 parameters: 
* `(combModule: TLogic)` - The combinational module to be evaluated. This will be an output of the `LogicBlockGen.fs` module.
* `(inputMap: Map<NetIdentifier, Net>)` - The inputs to the combinational module represented as a map from the input net identifier to the input net's value encapsulated by the `Net` type. The Map must contain all NetIdentifiers from the `Inputs` NetIdentifer list in `combModule`.
* `(currOutputMap: Map<NetIdentifier, Net>)` - The current outputs of the module in the simulation, in order to be used as defaults to resort to, when any part of the output is not updated when the module is evaluated. 

and gives one output of the type `Map<NetIdentifier, Net>` representing the outputs of the combinational module for the given inputs. This map will contain all NetIdentifiers from the `Outputs` NetIdentifier list in `combModule`.

My code is fairly self contained, with the only types used in the interfaces being `TLogic`, `NetIdentifier` and `Net`. 

`NetIdentifier` was initially created by me to allow for clearer representation of net definitions and accesses (slices/individual elements/whole nets). It was later adopted by the rest of the team and added to the `TLogic` module. 

The `NetIdentifier` is a Record type containing the fields:
* `Name: string` - The name of the net.
* `SliceIndices: (int * int option)` - This field can be used to represent wire definitions `None`, bus defintions `input[3:0] bus-> Some(3, Some 0)`, bus slices `bus[2:1] -> Some(2, Some 0)`, single wire access in busses `bus[1] -> Some(1, None)`, and full bus/wire access `bus -> None`. 

The `TLogic` type encapsulates all of the information to evaluate a purely combinational module written in VeriTINY. Its definition is as follows:
```
type Expression = (Operator * NetIdentifier list * NetIdentifier list)
type TLogic = {
    Name: string
    ExpressionList: Expression list
    Inputs: NetIdentifier list
    Outputs: NetIdentifier list
    Wires: NetIdentifier list
}

```

The Expression type is a tuple consisting of an Operator, a NetIdentifier list identifying the outputs of the operation, and a NetIdentifier list identifying the operands. 
The Inputs and Outputs fields are self explanatory, they are the inputs and outputs of the module represented by the TLogic record instance. Each expression can only have one output, therefore it is planned to change the output NetIdentifier list to just NetIdentifier in the future.
The Wires field is a list of all intermediate nets in the module, such as ones explicitly declared by the user via the `wire` keyword, or as a result of temporary net generation for concatenation in `LogicBlockGen`.

The `Net` type encapsulates a mapping from bus indices to to logic levels. Its definition is given below.
`type Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>`
Wire is used to identify single wire nets and Bus, as the name indicates is used to identify multi-wire nets (i.e. busses).

An extension of the `Net` type, the `EvalNet` type is defined in the `EvalTypes.fs` module. This changes the LogicLevels to LogicLevel options in order to more easily compute whether a particular net value has already been computed. 


During code review, my main contribution was helping cc4417 understand how to use my top level function in his module, and also after looking at his code, I changed the input net type of my top level function from `GraphEndPoint` to `Net` (see [commit](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/commit/a664f22d33ddedca0b08cfacf3520d7b49f0e2fc#diff-820df22d0da2a760d2c10896b0be09eb)). I have also been working closely with tkh2017 to define the `Expression` type and `ExpressionList` field in the `TLogic` type in order to interface the `LogicBlockGen` and `CombEval` modules.
