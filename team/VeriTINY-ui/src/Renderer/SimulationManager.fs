module SimulationManager

open SharedTypes
open Refs
open Views
open ConnectionTools
open Helper
open EvalNetHelper
open EEExtensions
open Fable.Import.Browser

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

let logToStr log =
        match log with
        | Low -> "0"
        | High -> "1"

let rec toStr acc (lst:string list) =
        match lst with
        | hd::tl -> 
            toStr (acc + hd) tl
        | _ -> acc

let currInputToTableRow (m:Map<string, GeneralNet>): (string * string) list =

    let intToStr (x:int) = sprintf "%A" x
    
    let netNames = m |> Map.toList |> List.map fst 
    let netValues = m |> Map.toList |> List.map (snd >> extractLogLevel >> List.rev >> List.map logToStr >> toStr "")    

    List.zip netNames netValues


let simulationMapToRow (m:Map<NetIdentifier,Net>) =

    let extractLogLevel (netIn: Net) =
        match netIn with
        | Wire netMap
        | Bus netMap ->
            netMap 
            |> Map.toList
            |> List.map snd

    let nameLst = m |> Map.toList |> List.map (fst >> (fun netId -> netId.Name))
    let logLst = m |> Map.toList |> List.map (snd >> extractLogLevel >> List.rev >>List.map logToStr >> toStr "") 
    
    List.zip nameLst logLst



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

let returnOutputNetStates (currentState: Map<NetIdentifier,Net>): Map<NetIdentifier,Net> =
    let outputNetNames = findUnconnectedOut connLst |> List.map gNetName
    
    let netIdByName (map:Map<NetIdentifier,Net>) (name: string): NetIdentifier list =
        let mapKeys = map |> Map.toList |> List.map fst
        mapKeys |> List.filter (fun x -> x.Name = name) 

    let netIdLst = List.collect (netIdByName currentState) outputNetNames 
    List.map (fun x -> currentState.[x]) netIdLst |> List.zip netIdLst |> Map.ofList

let updateCurrentInputsTable() =
    let tableRows = currInputToTableRow currentInputs
    updateTable tableRows currList

let updateSimulationTable()  =
    if simulationCount = -1 then () else
    let tableRows = 
        if showAllNets then 
            simulationMapToRow allStates
        else
            returnOutputNetStates allStates
            |> simulationMapToRow

    updateTable tableRows memList

let initSimulation() =
    let initSyncMap,(sync, async) = Simulator.setupSimulation connLst
    syncCLst <- sync
    asyncCLst <- async
    synchronousState <- initSyncMap

   
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
    
    let gNets = inputs.[inputsIndex]
    let currentInputs' = gNetLstToMap gNets
    currentInputs <- currentInputs'
 
    updateCurrentInputsTable()


let refreshConnections() =
    if List.isEmpty connLst then showAlert "Connection list is empty! Please define connections in the Connections tab." "Error!" else
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

let resetSimulation() =
    simulationCount <- -1
    if List.isEmpty inputs then () else clearSimulationTable()


let stepSimulation() =
    let step() =
        simulationCount <- simulationCount + 1
        let currState, syncState = Simulator.iterateState synchronousState inputs.[simulationCount] asyncCLst syncCLst TLogicList
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
    let finalState,finalSyncState = Simulator.simulateInputList inputs connLst TLogicList
    synchronousState <- finalSyncState
    allStates <- finalState
    displaySimulationCycle()

    updateSimulationTable()
    showAlert "Displaying final states of all nets" "Simulation Complete!" 
       
let updateShowNetsBtnTxt() = 
    if showAllNets then showNetsBtn.innerHTML <- "Show Output Nets Only" else showNetsBtn.innerHTML <- "Show All Nets"

let showNetsBtnClickListener() = 
    showAllNets <- not showAllNets
    updateShowNetsBtnTxt()
    updateSimulationTable()


