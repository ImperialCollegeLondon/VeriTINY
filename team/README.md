# VeriTINY  <img src="https://github.com/channen2/Verilog2/blob/master/visual.png" width="26" height="26">

## Introduction

Simple Verilog simulator.
Built in F# with the help of using fable and electron.
Features a Verllog code editor and a block interface.

## Workflow

Type Verilog code in the text editor. Allows use of a limited set of instrucions:


<image src="https://github.com/channen2/Verilog2/blob/master/Help1.PNG">

Verlilog code is parsed and stored as a logic block.

Use buttons in block interface to add to SVG environment in `Connections` . Allows the adding of:
    -Logic Blocks built in verilog
    -DFF`s of dynamic size

Make connection between blocks by usinf the drop-down menu


<image src="https://github.com/channen2/Verilog2/blob/master/Help2.PNG">

Simulate using the step button, avaliable in the `Simulation` tab.

<image src="https://github.com/channen2/Verilog2/blob/master/Help3.PNG">

## Project Structure

#### FIles in src\Emulator\verilog2-emulator

#### SharedTypes
Contains type definitions used among multiple other modules

##### Lexer.fs
Takes Verilog/VeriTINY code and lexes it into a list of tokens

##### Parser.fs
Takes list of tokens and parses it, generating an AST as a DU of type `ModuleType`

#### LogicBockGen.fs
Translates the AST to a record of type `TLogic`, which can be used to generates blocks in the interface

##### CombEval.fs
Evaluates `TLogic` blocks from LogicBlockGen so it can be used in simulation

###### EvalTypes.fs 
Contains the EvalNet type definition used in CombEval

###### EvalNetHelper.fs`
Contains functions used to analyse and manipulate nets of the `EvalNet` type 

###### LogicOperations.fs 
Contains Definitions for Operations e.g. and,or 

#### Connector.fs
Creates a list of connections from `TLogic` and DFF blocks
###### ConnectionTools.fs
Set of functions for interacting with connections list

#### Simulator.fs
Takes a list of connections, and user defined inputs and simulates the circuit

###### SimulationTypes.fs
Contains a type definition `block` used in simulation

###### Evaluator / SynchronousBlocks
Contains functions for updating nets during simulation. (One of these should not exists, they do the same thing)

###### Helper 
Even more helper functions for simulation

#### Blocks
Example code used for testing

#### FIles in src\Renderer

#### BlockGraphics
Takes connections list and renders it using SVG

#### CompilationManager
Bridges the text editor and lexer

#### SimulationManager
Handles the gui elements of the `Connections` tab. 

#### ConnectionsManager
Handles the gui elements of the `Simulation` tab. 

#### Editors
From Visual2, handles the Monaco Text Editor

#### Files
From Visual2, handles the file saving/loading

#### ErrorDocs
Not used,at some point this was mistakenly added back -remove it

#### Integration
From Visual2, mostly unused, now just contains a reset function

#### MenuBar
From Visual2, handles GUI elements for the toolbar

#### Refs
From Visual2, contains helper functions, javascripts interfaces, mutable variable definitions and other important references

#### Renderer
From Visual2, contains the attachers for most UI elements

#### Settings
From Visual2, handles settings menu, mostly unused

#### Stats
From Visual2, tracks program info, mostly unused

#### Settings
From Visual2, handles settings menu, completely unused

#### Tabs
From Visual2, handles the text editor tabs

#### Testbench
From Visual2, used for testing, unused

#### Tests
From Visual2, used for testing, unused

#### Tooltips
From Visual2, renders tootips when hovering over objects

#### Views
From Visual2, handles the side menu tabs  


## Visual2

See full readme.

[](https://github.com/ImperialCollegeLondon/Visual2)

[](https://github.com/ImperialCollegeLondon/Visual2/wiki/Acknowledgements)


`This project is loosely based on a starter template from https://github.com/filangel/arm-monaco.

The target language is `F#`, which is transpiled to `Javascript` (`js`) thanks to [Fable](https://fable.io) v2.x. [Electron](https://electronjs.org/) is then used to convert the developed web-app to a cross-platform native application, providing access to platform-level commands (i.e. file-system, path, multiple processes), which are unavailable to (vanilla) browser web-apps.

[Webpack](https://webpack.js.org/) is the module bundler, responsible for the Javascript concatenation and automated building process.

Finally, [Monaco Editor](https://microsoft.github.io/monaco-editor/) is  a self-contained Javascript component that implements a programmer`s editor window, with many features, which the `F#` code can interact with.`
