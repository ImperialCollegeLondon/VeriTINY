module SimulationManager

open SharedTypes
open Refs
open Views
open ConnectionTools
open Helper
open EvalNetHelper
open EEExtensions

// used to convert map to string tuple list for display 
let mapToTableRows (m:Map<NetIdentifier,Net>): (string * string)list =
    
    let logToStr log =
        match log with
        | Low -> "0"
        | High -> "1"

    let extractLogLevel (net:Net) =
        match net with
        | Wire netMap 
        | Bus netMap ->
            netMap
            |> Map.toList 
            |> List.map snd

    let netNameLst = m |> Map.toList |> List.map (fst >> (fun netId -> netId.Name))

    let logicLst = 
        m 
        |> Map.toList 
        |> List.map (snd >> extractLogLevel >> List.rev)
    
    // for display
    let stringLogicLst = 
        logicLst 
        |> List.map (List.map logToStr
        >> List.reduce(+))
     
    List.zip netNameLst stringLogicLst


let updateNetDropDown() =
    let inputNetNames = blockLst |> findUnconnectedIn |> List.map extractNameFromInfo

    netDropDown1.innerHTML <- ""

    List.iter (fun (netName) ->
        addOptionToDropDown netDropDown1 netName netName) inputNetNames


let makeNetWithVal (netID: NetIdentifier) (netValue: string): Net =

    let oldNet = currentInputs.[netID]
    let netSize = getBusSize netID
    
    let busOrMap (n:int) =
        if n = 1 then Wire else Bus
    
    let newLogLst = intToLogicLevelList (int netValue) []
                   |> padLogicLvlListToLength netSize

    let newNetLen = newLogLst.Length

    if newNetLen > netSize
    then 
        showAlert "Input value larger than net size" "Error" 
        oldNet
    else
        List.zip [0..newNetLen-1] newLogLst 
        |> Map 
        |> busOrMap newNetLen

let updateCurrentInputsTable() =
    let tableRows = mapToTableRows currentInputs
    updateTable tableRows currList

let updateSimulationTable()  =
    let returnOutputNetStates (currentState: Map<NetIdentifier,Net>): Map<NetIdentifier,Net> = 
        let outputNetIds = findUnconnectedOut blockLst |> List.map (fun block -> block.ID)
        let outputNetLst = List.map (fun x -> currentState.[x]) outputNetIds
        
        List.zip outputNetIds outputNetLst 
        |> Map.ofList
         
    if simulationCount = -1 then () else
    let tableRows = 
        if showAllNets then 
            mapToTableRows allStates
        else
            returnOutputNetStates allStates
            |> mapToTableRows

    updateTable tableRows memList
 
let applyInput() =
    
    let getNetIdByName (name: string) (netIdLst: NetIdentifier list): NetIdentifier =
        List.find (fun (netID:NetIdentifier) -> 
            name = netID.Name) netIdLst

    let netVal = getInputValFromText()
    // first character is info on synchronous, the rest is the netName
    let netName = getNameFromDropDown()
    let netIdLst = currentInputs |> Map.toList |> List.map fst
    let netId = getNetIdByName netName netIdLst
    let newNet = makeNetWithVal netId netVal
        
    currentInputs <- Map.add netId newNet currentInputs
    updateCurrentInputsTable()
    
    inputs <- inputs |> List.mapi (fun i v -> if i = inputsIndex then currentInputs else v)

let displaySimulationCycle() =
    let hd = getHtml "simulationCycle"
    hd.innerHTML <- ""
    let text = makeElement "hd" "hd" (sprintf "Cycle: %A" simulationCount)
    addToDOM hd [text] |> ignore

let displayInput() =
    //show new elements in input list
    let hd = getHtml "inputNumber"
    hd.innerHTML <- ""
    let text = makeElement "hd" "hd" (sprintf "Cycle: %A" inputsIndex)
    addToDOM hd [text] |> ignore
    
    let currentInputs' = inputs.[inputsIndex]
    currentInputs <- currentInputs'
 
    updateCurrentInputsTable()

let nextInput() =
    if List.isEmpty inputs then showAlert "No inputs to view" "Error!" else

    if inputsIndex = maxInputLength - 1
        then showAlert "No more inputs to view!" "Oops!"
    else 
        inputsIndex <- inputsIndex + 1
    displayInput()

let prevInput() =
    if List.isEmpty inputs then showAlert "No inputs to view" "Error!" else

    if inputsIndex = 0
        then showAlert "No more inputs to view!" "Oops!"
    else 
        inputsIndex <- inputsIndex - 1
    displayInput()


let initInputs() =
    let currInputPairs =
        let inputNetInfos = findUnconnectedIn blockLst
        List.map (fun netInfo -> netInfo.ID, makeNetFromInfo netInfo) inputNetInfos
    currentInputs <- Map.ofList currInputPairs
    inputs <- List.map (fun _ -> currentInputs) [1..maxInputLength]


let refreshConnections() =
    if List.isEmpty blockLst then showAlert "Connection list is empty! Please define connections in the Connections tab." "Error!" else
    simulationCount <- -1
    inputsIndex <- 0
    
    initInputs ()
    updateNetDropDown()
    displayInput ()
    clearSimulationTable()
    showAlert "Connections have been refreshed" "Success!"

let updateMaxInputLength() =
    let maxInputLength' = getMaxLengthFromText()
    if int maxInputLength <= 0 then showAlert "Please enter a positive integer." "Error!" else
    maxInputLength <- int maxInputLength'
    refreshConnections()


let initSimulation() =
    let initSyncMap,(syncBlocks, asyncBlocks) = Simulator.setupSimulation blockLst
    syncBLst <- syncBlocks
    asyncBLst <- asyncBlocks
    synchronousState <- initSyncMap


let stepSimulation() =
    let step() =
        simulationCount <- simulationCount + 1
        let currState, syncState = Simulator.iterateState synchronousState inputs.[simulationCount] asyncBLst syncBLst TLogicList
        synchronousState <- syncState
        allStates <- currState
        displaySimulationCycle()

        updateSimulationTable()

    if List.isEmpty inputs then showAlert "No inputs to simulate" "Error!" else 

    if simulationCount = -1 then 
        initSimulation()
        step()
    elif simulationCount = maxInputLength - 1 then
        showAlert "Simulation complete" "Success!"
    else 
        step()
    

let runSimulation() =
    if List.isEmpty inputs then showAlert "No inputs to simulate" "Error!" else
    initSimulation()
    simulationCount <- inputs.Length - 1
    let finalState,finalSyncState = Simulator.simulateInputList inputs blockLst TLogicList
    synchronousState <- finalSyncState
    allStates <- finalState
    displaySimulationCycle()

    updateSimulationTable()
    showAlert "Displaying final states of nets" "Simulation Complete!" 

let resetSimulation() =
    simulationCount <- -1
    if List.isEmpty inputs then () else clearSimulationTable()
       
let updateShowNetsBtnTxt() = 
    if showAllNets then showNetsBtn.innerHTML <- "Show Output Nets Only" else showNetsBtn.innerHTML <- "Show All Nets"

let showNetsBtnClickListener() = 
    showAllNets <- not showAllNets
    updateShowNetsBtnTxt()
    updateSimulationTable()


