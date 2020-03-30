
# VeriTINY <img src="https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/veriTINY-logo.png" width="40" height="40"> 

VeriTINY is a simple and easy to use Verilog simulator for designing and simulating simple synchronous single-clock hardware. It supports a subset of the Verilog HDL and features a Verilog code editor, a block connections interface, and a digital circuit simulator.

## Workflow

1. Type Verilog code in the text editor. VeriTINY currently supports the following language features:

* Gate Instantiation 
  * AND, OR, NOT, XOR only  
  * Example: `and a1 (out, a, b)`
* Continuous Assigns
  * `&`, `|`, `~`, `^`
  * Example: `assign out = a & b`
* Bus Slicing
  * Example: `a[2:0]`
* Single-level Concatenations
  * Example: `{currState[3], prevState[2:0]}`
* Bracketed Expressions
  * Example: `assign out = ~(a & b)`

Verilog code may also be saved and loaded as .v files.

<img src="https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow1.png">

2. Press the `compile` button to convert Verilog code into a block. The block will appear in the `block list` drop down in the `Connections` tab.

<img src="https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-2.png">

3. Use the buttons in the `Connections` tab to add and connect blocks in the `Blocks Workspace`.  
   * `Add Block` - Add selected block from the `block list` dropdown to the workspace
     * You may also add `DFF` blocks from the dropdown, you must enter a positive integer for DFF size
   * `Make Connection` - Connect the selected output and input nets from the relevant dropdowns. These nets will be renamed when a successful connection is made
   * `Clear Blocks` - Clears the `Blocks Workspace`
   * `Reset` - Clear the `block list` and the `Blocks Workspace`

<img src="https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-3.png">

4. Use the buttons in the `Simulation` tab to enter inputs to simulation. These will be displayed in the `Simulation Inputs Table`.
   * Enter the number of cycles you wish to simulate for using the textbox at the top of the `Simulation` tab and press `Enter`
    * `Refresh Connections` - Load the block diagram that is defined in the `Connections Tab` for simulation
   * Select a `Net` using the dropdown and enter a decimal value to apply to the net into the `Input Value` textbox and press `Enter`
     * Use the left and right buttons  `<` `>` to apply values to the nets for different cycles

<img src="https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-4.png">

5. Use the buttons in the bottom half of the `Simulation` tab to simulate the circuit. The values of the nets will be displayed in the `Simulation Table`.
   * `Run` - Simulate all cycles and display the final state
   * `Step` - Simulate the next cycle and display the current state
   * `Reset` - Reset simulation to the initial state
   * `Show Output Nets only / Show All Nets` - Toggle between showing output nets and all nets in the `Simulation Table`

<img src="https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-5.png">

#### Notes
* You can switch between the `Connections` and `Simulation` tab freely, however if you make changes in the `Blocks Workspace` then you *must* press the `Refresh Connections` button to load your changes. Doing this will erase the simulation state and inputs.
* You can change the *upcoming* simulation inputs *at any point* during simulation. Changing past inputs will have no effect on simulation.

## Getting Started

1. Download the [latest VeriTINY binary release](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/releases) for your platform.

### Windows

* Unzip the download anywhere on your system: the extracted files will use approximately 200MB.
* You can then delete the downloaded zip file.
* To run the program double-click the executable `VeriTINY.exe` which can be found in the top-level unzipped directory.
* No installation is required

### OS-X

* Open the downloaded DMG file
* Drag the VeriTINY-darwin-x64 directory into the applications directory as normal to install
* The following warning might appear when you first try to launch the application: "VeriTINY" can't be opened because it is from an unidentified developer. In that case:
    * Open `System Preferences`
    * Go to `Security & Privacy`
    * In the bottom rigth corner, under `General`, click 'Open anyway'
    * Click 'Open' in the pop up window
    * If VeriTINY does not seem to start, simply quit the application and re-launch

## Build Instructions
The build process is similar to the instructions found in Visual2. The full set of instructions which includes in-depth explanations for each command can be found [here](https://github.com/ImperialCollegeLondon/Visual2).

1. Follow instructions to install [yarn](https://yarnpkg.com/lang/en/docs/install/) (which tell you to install Node as well).

2. Download and install the latest (2.x) [Dotnet Core SDK](https://dotnet.microsoft.com/download).  
For Mac users, download and install [Mono](http://www.mono-project.com/download/stable/) from official website (the version from brew is incomplete, may lead to MSB error on step 7).

1. Download & unzip this repo.

2. Navigate to `\team\VeriTINY-ui` in a command-line interpreter. 

3. Fetch the required `npm` packages by executing `yarn install`. This project consistently uses `yarn` Node package manager instead of `npm`.

4. On macOS or linux ensure you have [paket installed](https://fsprojects.github.io/Paket/installation.html). Run `setup.bat` (on Windows) or `sh setup.sh` (on linux or macOS). 

5. In a terminal window execute `yarn start` (shortcut for `yarn run start`).

6. Open your `electron` app in a new terminal tab by running `yarn launch`. This command will start the application and also _hot reload_ it whenever source files are recompiled, or CSS files changed. 

7. Run `yarn pack-win, yarn pack-linux, yarn pack-osx` at any time to create a set of system-specific self-contained binaries in `./dist`. Note that some host-target combinations will not correctly generate: pack-osx must be executed on os-x. 


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

#### EvalTypes.fs 
Contains the EvalNet type definition 

#### EvalNetHelper.fs
Contains functions used to analyse and manipulate nets of the `EvalNet` type 

#### LogicBockGen.fs
Translates the AST to a record of type `TLogic`, which can be used to generates blocks in the interface

#### LogicOperations.fs 
Contains Definitions for Operations (e.g. `AND`, `OR`, `NOT`) used in CombEval 

#### CombEval.fs
Evaluates `TLogic` blocks from LogicBlockGen so it can be used in simulation

#### Helper.fs
Contains helper functions for simulation

#### Connector.fs
Creates a list of connections from `TLogic` and DFF blocks

#### ConnectionTools.fs
Set of functions for interacting with connections list

#### SynchronousBlocks.fs
Contains implemented synchronous blocks

#### Simulator.fs
Contains functions to take a list of connections and user defined inputs and simulates the circuit

#### Files in VeriTINY-ui\src\Renderer

#### BlockGraphics
Takes connections list and renders it using SVG

#### CompilationManager
Bridges the text editor and lexer

#### SimulationManager
Handles the gui elements of the `Connections` tab. 

#### ConnectionsManager
Handles the gui elements of the `Simulation` tab. 

## Visual2

The VeriTINY UI is based off the Visual2 repo linked below:

[Visual2 Github Repo](https://github.com/ImperialCollegeLondon/Visual2)

[Visual2 Acknowledgements](https://github.com/ImperialCollegeLondon/Visual2/wiki/Acknowledgements)
