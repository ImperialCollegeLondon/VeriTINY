MQ3617 Individual code submission

Modules Connector and ConnectionTools handle the building of a defined type Connection List

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type Net = | Wire of Map<int,LogicLevel> | Bus of Map<int,LogicLevel>
type NamedNet = string * Net
type GeneralNet = (bool * NamedNet) 

type Megablock = | Name of string
type Connection = Megablock * GeneralNet list * GeneralNet list 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Connection list contains the relevent information needed for simulation, the names of the Megablocks used as well as the defined connections.

Connector:
Before simulating a block circuit, the Megablocks included as well as the connections involved must be defined be the user. UserIn() describes the pipeline for the operation.
First addMegaBlock() takes a user input and creates new "connection" with the specified name of the megablock as well as lists of (general) nets coressponding to the input and output nets of the block.
Currently addMegaBlock is compatible with Tlogic blocks ceated by the LogicBlockGen module as well as a DFF block of configurable size.
The "connection" list is refactored, this is to make sure that the inputs and outputs of the individual megablocks are unique. 