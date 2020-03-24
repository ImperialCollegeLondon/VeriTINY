module EvalTypes
open SharedTypes

type EvalNet = | EvalWire of Map<int,LogicLevel option> | EvalBus of Map<int,LogicLevel option>
