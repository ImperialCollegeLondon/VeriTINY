(*
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Renderer
    Description: Top-level Electron Renderer Process
*)

/// Top-level renderer process function: calls everything else

module Renderer
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser


// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
Browser.console.log "Hi from renderer.fs" |> ignore

open Refs
open MenuBar
open Views

/// Hack to provide a constant global variable
/// set from command line arguments of main process.
/// 0 => production. 1 => dev. 2 => debug.
let setDebugLevel() =
    let argV =
        electron.remote.``process``.argv
        |> Seq.toList
        |> List.tail
        |> List.map (fun s -> s.ToLower())
    let isArg s = List.contains s argV
    debugLevel <-
        if isArg "--debug" || isArg "-d" then 2
        elif isArg "-w" then 1
        else 0


/// Attach a click event on each of the map elements to a function f
/// which accepts the map element as an argument
let mapClickAttacher map (refFinder : 'a -> HTMLElement) f =
    let attachRep ref = (refFinder ref).addEventListener_click(fun _ -> f ref)
    map
    |> Map.toList
    |> List.map (fst >> attachRep)
    |> ignore

/// Called via IPC message from main process whenever main window is resized.
/// Work out any CSS dimensions that must change in response and set
/// them. Note use of CSS custom variables to control multiple
/// CSS properties
let resizeGUI() =
    let headerHeight = (getHtml "vis-header").offsetHeight
    setCustomCSS "--header-height" (sprintf "%.1fpx" headerHeight)

/// Initialization after `index.html` is loaded.
/// Equivalent of main() function
let init() =

    setDebugLevel()
    printfn "Debug level = %d" debugLevel
    // Show the body once we are ready to go!
    document.getElementById("vis-body").classList.remove("invisible")

    // Set up window close interlock using IPC from/to main process
    electron.ipcRenderer.on ("closingWindow", (fun event ->
        MenuBar.ExitIfOK()
        )) |> ignore

    electron.ipcRenderer.on ("resizeWindow", (fun event ->
        resizeGUI()
        )) |> ignore


    // Actions for the buttons
    Refs.openFileBtn.addEventListener_click (fun _ -> MenuBar.interlock "open file" MenuBar.openFile)

    Refs.saveFileBtn.addEventListener_click (fun _ -> MenuBar.interlock "save file" Files.saveFile)

    Refs.maxInputLengthButton.addEventListener_click(fun _ ->
        Browser.console.log "Entering new max input length"
        SimulationManager.updateMaxInputLength()
    )

    Refs.runSimulationBtn.addEventListener_click (fun _ ->
       Browser.console.log "Run simulation hook here" |> ignore
       SimulationManager.runSimulation()
    )

    stepForwardBtn.addEventListener_click (fun _ ->
       Browser.console.log "Step hook here"
       SimulationManager.stepSimulation()
    )


    resetSimulationBtn.addEventListener_click (fun _ ->
        Browser.console.log "Reset hook here"
        SimulationManager.resetSimulation()
    )
    
    (Refs.applyValButton).addEventListener_click(fun _ ->
        Browser.console.log "Apply Val Button clicked" |> ignore
        SimulationManager.applyInput()
    )

    (Refs.nextButton).addEventListener_click(fun _ ->
        SimulationManager.nextInput()
    )

    (Refs.backButton).addEventListener_click(fun _ ->
        SimulationManager.prevInput()
    )


    (Refs.byteViewBtn).addEventListener_click(fun _ ->
        Browser.console.log "Toggling byte view" |> ignore
        Views.toggleByteView()
    )

    (Refs.reverseViewBtn).addEventListener_click(fun _ ->
        Browser.console.log "Toggling reverse view" |> ignore
        Views.toggleReverseView()
    )


    (Refs.addBlockButton).addEventListener_click(fun _ ->
        Browser.console.log "Add Block Btn clicked"  |> ignore
        ConnectionsManager.addBlockBtnClickListener ()
    )

    (Refs.clearBlocksButton).addEventListener_click(fun _ ->
        Browser.console.log "Clear Blocks Btn clicked"  |> ignore
        ConnectionsManager.clearBlocksBtnClickListener ()
    )

    (Refs.makeConnectionBtn).addEventListener_click(fun _ ->
        Browser.console.log "Make Connection Btn clicked" |> ignore
        ConnectionsManager.makeConnectionsBtnClickListener ()
    )

    (Refs.clearConnectionBtn).addEventListener_click(fun _ ->
        Browser.console.log "Clear Connections Btn clicked" |> ignore
        ConnectionsManager.clearConnectionsBtnClickListener ()
    )

    (Refs.applyValButton).addEventListener_click(fun _ ->
        Browser.console.log "Apply Val Button clicked" |> ignore
        let textBoxContents = getInputValFromText()
        Browser.console.log textBoxContents |> ignore
    )

    (Refs.refreshConnsButton).addEventListener_click(fun _ ->
        Browser.console.log "Refresh Connections Button clicked" |> ignore
        SimulationManager.refreshConnections ()
    )

    (Refs.showNetsBtn).addEventListener_click(fun _ ->
        SimulationManager.showNetsBtnClickListener()
    )

    (Refs.compileButton).addEventListener_click(fun _ -> 
        Browser.console.log "Compile Button clicked" |> ignore 
        CompilationManager.compileSourceCode()
    )

    mapClickAttacher viewToIdTab Refs.viewTab (fun view ->
        Browser.console.log (sprintf "View changed to %A" view) |> ignore
        Views.setView view :> obj
    )

    (Refs.newFileTab).addEventListener_click(fun _ ->
        Browser.console.log "Creating a new file tab" |> ignore
        MenuBar.interlock "create a new tab" (fun () -> Tabs.createFileTab() |> ignore))

    // create electron menus
    MenuBar.mainMenu()

    // Create an empty tab to start with
    Tabs.createFileTab() |> ignore
    printfn "Ending renderer init"
    vSettings <- checkSettings (getJSONSettings())
    Editors.updateAllEditors false

    Tooltips.addFixedToolTips()

/// top-level function that runs the renderer code
let handleMonacoReady = { new EventListenerObject with
    member x.handleEvent (_ : Event) = init()
 }

let handlePreventDefault = { new EventListenerObject with
    member x.handleEvent (e : Event) = e.preventDefault()
 }


let handleDrop = { new EventListenerObject with
    member x.handleEvent (e : Event) =
        e.preventDefault()
        let files = (e :?> DragEvent).dataTransfer.files
        let num = (files.length |> int)
        let paths =
                [ 0..num - 1 ]
                |> List.map (fun s -> files.[s]?path)
        interlock "open files" (fun () -> openListOfFiles paths |> ignore) |> ignore
 }

document.addEventListener ("monaco-ready", U2.Case2 handleMonacoReady)
document.addEventListener ("dragover", U2.Case2 handlePreventDefault)
document.addEventListener ("drop", U2.Case2 handleDrop)

