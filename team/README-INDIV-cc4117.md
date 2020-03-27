# Individual Statement for Team Code Submission - cc4117

I was responsible for the `Simulation` tab in the VeriTINY UI.

The top half of the tab allows the user to apply inputs for simulation as well as set the number of cycles to simulate for. 

![WF4](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-4.png)

The bottom half of the tab contains the `Simulation Table` where the simulation takes place.


![WF5](https://github.com/ImperialCollegeLondon/hlp2020-verilog2/blob/team-README/team/readme-screenshots/workflow-5.png)

In the group phase, I refactored the `Connector.fs` module to work with the UI. 

I also created the `SimulationManager.fs` module which handles all interactions in the `Simulation` tab. It makes use of the `Simulator.fs` module that I created in the individual phase. The module and simulation tab has gone through extensive testing and debugging during the group phase with the help of anv17.

Lastly, after the demo I helped refactor VeriTINY to use the new types `Simblock` and `SimNetInfo` which replaced the old `Connection` and `GeneralNet` types respectively.  

