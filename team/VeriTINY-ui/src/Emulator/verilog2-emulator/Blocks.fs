module Blocks
open SharedTypes

///Placeholder Tlogic Blocks
let b0= {
    Name="Test1"
    ExpressionList =[] 
    Inputs = [{Name="a";SliceIndices =Some(2,Some 0)};{Name="b";SliceIndices=Some(2,Some 0)}]
    Outputs =[{Name="x";SliceIndices=Some(2,Some 0)}]
    Wires =[] 
}
let b1= {
    Name="Test2"
    ExpressionList =[] 
    Inputs = [{Name="a";SliceIndices =Some(2,Some 0)};{Name="b";SliceIndices=Some(2,Some 0)};{Name="c";SliceIndices =Some(2,Some 0)}]
    Outputs =[{Name="x";SliceIndices=Some(2,Some 0)}]
    Wires =[] 
}
let b2= {
    Name="Test3"
    ExpressionList =[] 
    Inputs = [{Name="a";SliceIndices =Some(2,Some 0)};{Name="b";SliceIndices=Some(2,Some 0)}]
    Outputs =[{Name="x";SliceIndices=Some(2,Some 0)};{Name="y";SliceIndices=Some(2,Some 0)}]
    Wires =[] 
}
let avaliableTBlocks =[b0;b1;b2]


