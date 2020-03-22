module SimulationManager

open SharedTypes
open Refs
open Views
open ConnectionTools
open Helper
open EvalNetHelper
open EEExtensions

let updateNetDropDown() =
    //let allConnNetNames = getAllConnNetNames() 
    let inputNetNames = connLst |> findUnconnectedIn |> List.map (snd >> fst)

    netDropDown1.innerHTML <- ""

    List.iter (fun (netName) ->
        addOptionToDropDown netDropDown1 netName netName) inputNetNames


let initCurrInputs() = 
    let inputGNets = findUnconnectedIn connLst
    List.map (fun gNet -> gNetName gNet,gNet) inputGNets


let gNetLstToMap (gLst:GeneralNet list): Map<string,GeneralNet> =
    let gNetNames = List.map gNetName gLst
    List.zip gNetNames gLst 
    |> Map.ofList

let gNetMapToList (gMap: Map<string,GeneralNet>): GeneralNet list =
    gMap 
    |> Map.toList 
    |> List.map snd

let currInputToTableRow (m:Map<string, GeneralNet>): (string * string) list =

    let toStr (x:int) = sprintf "%A" x
    let netValueFromGNet (gNet:GeneralNet): string = gNet |> extractLogLevel |> logicLevelsToint |> toStr

    let netNames = m |> Map.toList |> List.map fst 
    let netValues = m |> Map.toList |> List.map (snd >> netValueFromGNet)

    List.zip netNames netValues


let simulationMapToRow (m:Map<NetIdentifier,Net>) =

    let extractLogLevel (netIn: Net) =
        match netIn with
        | Wire netMap
        | Bus netMap ->
            netMap 
            |> Map.toList 
            |> List.map snd
    
    let logToStr log =
        match log with
        | Low -> "0"
        | High -> "1"

    let rec toStr acc (lst:string list) =
        match lst with
        | hd::tl -> 
            toStr (acc + hd) tl
        | _ -> acc

    let nameLst = m |> Map.toList |> List.map (fst >> (fun netId -> netId.Name))
    let logLst = m |> Map.toList |> List.map (snd >> extractLogLevel >> List.map logToStr >> toStr "")
    
    List.zip nameLst logLst
    //|> List.map (fun (a,b) -> [a;b])



let makeGNet (netName: string) (netValue: string): GeneralNet =

    let gNet = currentInputs.[netName]
    let size = gNet |> snd |> snd |> netSize
    
    let busOrMap (n:int) =
        if n = 1 then Wire else Bus
    
    let lstOfLog = intToLogicLevelList (int netValue) []
                   |> padLogicLvlListToLength size 
    let n = lstOfLog.Length

    if n > size 
    then 
        showAlert "Input value larger than net size" "Error" 
        gNet
    else
        let newMap = List.zip [0..n-1] lstOfLog |> Map
        updateGenNet gNet newMap


let saveInputs() = 
    if List.isEmpty inputs then showAlert "No inputs to save!" "Error!" else
    originalInputs <- inputs
    showAlert "Inputs saved" "Success!"

let updateCurrentInputsTable() =
    let tableRows = currInputToTableRow currentInputs
    updateTable tableRows currList

let updateSimulationTable(stateMap)  =
    let tableRows = simulationMapToRow stateMap
    updateTable tableRows memList

let initSimulation() =
    let initSyncMap,(sync, async) = Simulator.setupSimulation connLst
    syncBLst <- sync
    asyncBLst <- async
    state <- initSyncMap
    saveInputs()
   
let updateInputs() =
    let pos, value = inputsIndex, gNetMapToList currentInputs
    inputs <- inputs |> List.mapi (fun i v -> if i = pos then value else v)

 
let applyInput() =
    let netVal = getInputValFromText()
    // first character is info on synchronous, the rest is the netName
    let netName = getNameFromDropDown()
    let newGNet = makeGNet netName netVal

    currentInputs <- Map.add netName newGNet currentInputs
    updateCurrentInputsTable()
    updateInputs()

let initInputs() =
    let currInputPairs = initCurrInputs()
    currentInputs <- Map.ofList currInputPairs
    let gNets = List.map snd currInputPairs
    inputs <- List.map (fun x -> gNets) [1..maxInputLength]

let clearInputs() =
    if List.isEmpty inputs then showAlert "Inputs are already empty" "Huh?" else
    initInputs()
    updateCurrentInputsTable()

let refreshConnections() =
    if List.isEmpty connLst then showAlert "Connection list is empty! Please define connections in the Connections tab." "Error!" else
    initInputs()
    updateCurrentInputsTable()
    updateNetDropDown()
    showAlert "Connections have been refreshed" "Success!"

let updateMaxInputLength() =
    let maxInputLength' = getMaxLengthFromText()
    if int maxInputLength <= 0 then showAlert "Please enter a positive integer." "Error!" else
    maxInputLength <- int maxInputLength'
    refreshConnections()

let displayInputNumber() =
    let hd = getHtml "inputNumber"
    hd.innerHTML <- ""
    let text = makeElement "hd" "hd" (sprintf "Cycle: %A" inputsIndex)
    addToDOM hd [text] |> ignore

let displaySimulationCycle() =
    let hd = getHtml "simulationCycle"
    hd.innerHTML <- ""
    let text = makeElement "hd" "hd" (sprintf "Cycle: %A" simulationCount)
    addToDOM hd [text] |> ignore

let displayInput() =
    //show new elements in input list
    let gNets = inputs.[inputsIndex]
    let currentInputs' = gNetLstToMap gNets
    currentInputs <- currentInputs'
    updateCurrentInputsTable()

    
let nextInput() =
    if inputsIndex = maxInputLength
        then showAlert "No more inputs to view!" "Oops!"
    else 
        inputsIndex <- inputsIndex + 1
    displayInputNumber()
    displayInput()

let prevInput() =
    if inputsIndex = 1 
        then showAlert "No more inputs to view!" "Oops!"
    else 
        inputsIndex <- inputsIndex - 1
    displayInputNumber()
    displayInput()

let resetSimulation() =
    if List.isEmpty originalInputs then showAlert "There are no inputs to load!" "Error!" else
    inputs <- originalInputs
    simulationCount <- 0
    displaySimulationCycle()
    clearSimulationTable()

let stepSimulation() =
    let step() =
        simulationCount <- originalInputs.Length - inputs.Length
        let currState, syncState = Simulator.iterateState state inputs.Head asyncBLst syncBLst TLogicList
        state <- syncState
        displaySimulationCycle()
        updateSimulationTable(currState)
        inputs <- inputs.Tail
    if simulationCount = 0 
    then 
        initSimulation()
        step()
    else 
        step()
    

let runSimulation() =
    if List.isEmpty inputs then showAlert "There are no inputs!" "Error!" else
    initSimulation()
    simulationCount <- originalInputs.Length - 1
    let finalState,finalSyncState = Simulator.simulateInputList inputs connLst TLogicList
    state <- finalSyncState
    displaySimulationCycle()
    updateSimulationTable(finalState)
    inputs <- []
    showAlert "Displaying final states of all nets" "Simulation Complete!" 




    
    
