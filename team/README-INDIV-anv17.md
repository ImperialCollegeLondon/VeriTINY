# Individual Statement for Team Code Submission - ANV17

I was responsible for the `Connections` tab in the UI which allows blocks (VeriTINY modules or DFFs)  to be added to a workspace and connected togther. A screenshot of the UI is given below. 

![WF3](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-3.png)

In order to make the tab I wrote two F# modules: 
* `BlockGraphics.fs` - This module contains functions which are used to create SVGs to represent the `SimBlock` blocks on the UI. It uses React to form the SVG elements. 
* `ConnectionsManager.fs`  -  Manages updating the internal circuit model state based on user interaction, and also updating the Connections tab based on the state. 
  * In order to calculate the next state from the current state and user interaction, the `Connector.fs` module in the `Emulator` project is used.


I also helped in testing and debugging the `Simulation` tab as that interacts closely with the `CombEval.fs` module I wrote during the individual phase of the project. 

Finally, after the demo, I helped in refactoring the program to move from using the `Connection` and `GeneralNet` types to the `SimBlock` and `SimNetInfo` types.

