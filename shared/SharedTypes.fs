module SharedTypes

type LogicLevel = | High | Low
type Operator = | And | Or | Not | Concat | Pass 
type Megablock = | Name of string

type Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>
type NamedNet = string * Net
type GeneralNet = (bool * NamedNet) 

type NGram = (char list * bool * bool) list
type Lexer = char list -> (char list * char list) option

type TLogic = {
    Name: string
    ExpressionList: (Operator * string * string option) list
    Inputs: string list
    Outputs: string list
    Wires: string list
}  

type Connection = (Megablock * GeneralNet list * GeneralNet) list 

type GraphEndPoint = |LogicLevel |BusInput of int

type DGraphNetNode = {
    NetName: string
    BusSize: int
}

type DGraphOpNode = Operator
type DGraphNode = |DGraphNetNode|DGraphOpNode
type DGraphEdge = {
    Input: DGraphNode
    Output: DGraphNode
    SliceIndicies: (int * int) option
    IsSliceOfInput: bool
}



