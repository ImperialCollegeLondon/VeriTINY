# HLP2020-Verilog2

The project deliverable is an easy to use tool for designing and simulating simple synchronous single-clock hardware. The deliverable will execute circuits that are designed using a subset of Verilog (VeriTINY) at gate level and configurable megablocks for components such as registers, RAMs, adders/subtractors and multiplexers.

Extensions for this deliverable include high quality hints/error messages in the program and display of waveforms during simulation.

# VeriTINY
![logo](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/veriTINY-logo.png)

## Introduction

VeriTINY is a simple Verilog simulator that supports a subset of the Verilog HDL. It features a Verilog code editor, a block connections interface, and a digital circuit simulator.


## Installation Instructions
The installation process is similar to the instructions found in Visual2. The full set of instructions which includes in-depth explanations for each command can be found [here](https://github.com/ImperialCollegeLondon/Visual2).


1. Follow instructions to install [yarn](https://yarnpkg.com/lang/en/docs/install/) (which tell you to install Node as well).

2. Download and install the latest (2.x) [Dotnet Core SDK](https://www.microsoft.com/net/learn/get-started).  
For Mac users, download and install [Mono](http://www.mono-project.com/download/stable/) from official website (the version from brew is incomplete, may lead to MSB error on step 7).

1. Download & unzip this repo.

2. Navigate to `\team\VeriTINY-ui` in a command-line interpreter. 


3. Fetch the required `npm` packages by executing `yarn install`. This project consistently uses `yarn` Node package manager instead of `npm`.

4. On macOS or linux ensure you have [paket installed](https://fsprojects.github.io/Paket/installation.html). Run `setup.bat` (on Windows) or `sh setup.sh` (on linux or macOS). 


5. In a terminal window execute `yarn start` (shortcut for `yarn run start`).

6. Open your `electron` app in a new terminal tab by running `yarn launch`. This command will start the application and also _hot reload_ it whenever source files are recompiled, or CSS files changed. 

7.  Run `yarn pack-win, yarn pack-linux, yarn pack-osx` at any time to create a set of system-specific self-contained binaries in `./dist/os-name/*` and a zip in `./dist`. For osx, the easiest way to run Visual2 once it has been built is to navigate to `./dist/VeriTINY-darwin-x64` and execute `open -a VeriTINY.app` in terminal. Note that some host-target combinations will not correctly generate: `pack-osx must be executed on os-x. 


## Workflow

1. Type Verilog code in the text editor. VeriTINY currently supports the following language features:

* Gate instantiation 
  * AND, OR, NOT only  
  * Example: `and a1 (out, a, b)`
* Continuous Assigns
  * "&", "|", "~" 
  * Example: `assign out = a & b`
* Bus slicing
  * Example: `a[2:0]`
* Single-level concatenations
  * Example: `{currState[3], prevState[2:0]}`

Verilog code may also be saved and loaded as .v files.


![WF1](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow1.png)


2. Press the `compile` button to convert Verilog code into a block. The block will appear in the `block list` drop down in the `Connections` tab.

![WF2](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-2.png)



3. Use the buttons in the `Connections` tab to add and connect blocks in the `Blocks Workspace`.  
   * `Add Block` - Add selected block from the `block list` dropdown to the workspace
     * You may also add `DFF` blocks from the dropdown, you must enter a positive integer for DFF size
   * `Make Connection` - Connect the selected output and input nets from the relevant dropdowns. These nets will be renamed when a successful connection is made
   * `Clear Blocks` - Clears the `Blocks Workspace`
   * `Reset` - Clear the `block list` and the `Blocks Workspace`


![WF3](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-3.png)

4. Use the buttons in the `Simulation` tab to enter inputs to simulation. These will be displayed in the `Simulation Inputs Table`.
   * Enter the number of cycles you wish to simulate for using the textbox at the top of the `Simulation` tab and press `Enter`
    * `Refresh Connections` - Load the block diagram that is defined in the `Connections Tab` for simulation
   * Select a `Net` using the dropdown and enter a decimal value to apply to the net into the `Input Value` textbox and press `Enter`
     * Use the left and right buttons  `<` `>` to apply values to the nets for different cycles

![WF4](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-4.png)
  

5. Use the buttons in the bottom half of the `Simulation` tab to simulate the circuit. The values of the nets will be displayed in the `Simulation Table`.
   * `Run` - Simulate all cycles and display the final state
   * `Step` - Simulate the next cycle and display the current state
   * `Reset` - Reset simulation to the initial state
   * `Show Output Nets only / Show All Nets` - Toggle between showing output nets and all nets in the `Simulation Table`


![WF5](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-5.png)

#### Notes
* You can switch between the `Connections` and `Simulation` tab freely, however if you make changes in the `Blocks Workspace` then you *must* press the `Refresh Connections` button to load your changes. Doing this will erase the simulation state and inputs.
* You can change the *upcoming* simulation inputs *at any point* during simulation. Changing past inputs will have no effect on simulation.




## Project Structure

#### Files in VeriTINY-ui\src\Emulator\verilog2-emulator

#### SharedTypes.fs
Contains type definitions used in multiple modules

#### NGrams.fs 
Contains NGram definitions for Lexer

#### Lexer.fs
Takes Verilog/VeriTINY code and lexes it into a list of tokens

#### Parser.fs
Takes list of tokens and parses it, generating an AST as a DU of type `ModuleType`

##### EvalTypes.fs 
Contains the EvalNet type definition 

##### EvalNetHelper.fs
Contains functions used to analyse and manipulate nets of the `EvalNet` type 

#### LogicBockGen.fs
Translates the AST to a record of type `TLogic`, which can be used to generates blocks in the interface

##### LogicOperations.fs 
Contains Definitions for Operations (e.g. `AND`, `OR`, `NOT`) used in CombEval 

#### CombEval.fs
Evaluates `TLogic` blocks from LogicBlockGen so it can be used in simulation

##### Helper.fs
Contains helper functions for simulation

#### Connector.fs
Creates a list of connections from `TLogic` and DFF blocks

##### ConnectionTools.fs
Set of functions for interacting with connections list

##### SynchronousBlocks.fs
Contains implemented synchronous blocks

#### Simulator.fs
Contains functions to take a list of connections and user defined inputs and simulates the circuit


#### Files in VeriTINY-ui\src\Renderer

#### Refs
From Visual2, contains helper functions, javascripts interfaces. Added mutable variable definitions and HTML references for VeriTINY 

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

#### Integration
From Visual2, mostly unused, now just contains a reset function

#### MenuBar
From Visual2, handles GUI elements for the toolbar

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
From Visual2, modified tooltips when hovering over objects. 

#### Views
From Visual2, handles the side menu tab switching. Also added helper functions for updating UI elements.




## Visual2

The VeriTINY UI is based off the Visual2 repo linked below:

[Visual2 Github Repo](https://github.com/ImperialCollegeLondon/Visual2)

[Visual2 Acknowledgements](https://github.com/ImperialCollegeLondon/Visual2/wiki/Acknowledgements)
