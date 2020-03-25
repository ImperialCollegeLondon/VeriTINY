module SharedTypes

type LogicLevel = | High | Low
type Operator = | And | Or | Not | Concat | Pass 
type Megablock = | Name of string

type Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>
// type NamedNet = string * Net
// type GeneralNet = (bool * NamedNet) 


type NGram = (char list * bool * bool) list
type Lexer = char list -> (char list * char list) option

type NetIdentifier = {
    Name: string;
    SliceIndices: (int * int option) option 
    //first int is upper index for slicing/IO bus definitions or wire index in bus if a single wire in a bus needs to be selected
    //second int is lower index for slicing/IO bus definitions
    //SliceIndicies can be None if whole bus is to be selected 
}

type Expression = (Operator * NetIdentifier list * NetIdentifier list) //output net list first

type TLogic = {
    Name: string
    ExpressionList: Expression list
    Inputs: NetIdentifier list
    Outputs: NetIdentifier list
    Wires: NetIdentifier list
}  

type SimNetWrapper = {
        ID: NetIdentifier   
        isClocked: bool
        Net: Net
}

type SimBlock = {
    BlockType: string
    inNets: SimNetWrapper list
    outNets: SimNetWrapper list
    //possibility to add block instance names here
}
    

// type Connection = Megablock * GeneralNet list * GeneralNet list 

