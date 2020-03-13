(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Views
    Description: Display registers, memory or symbol table in Views Panel
*)

/// implement views panel
module Views

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser
open Refs
open Fable
open ExecutionTop


open SharedTypes
open BlockGraphics

let maxSymbolWidth = 30
let maxDataSymbolLength = 16


let nameSquash maxW name =
    let nameLen = String.length name
    if nameLen <= maxW then name
    else
        let fp = (float maxW) * 0.65 |> int
        let lp = maxW - (fp + 3)
        name.[0..fp - 1] + "..." + name.[nameLen - lp..nameLen - 1]

let calcDashboardWidth() =
    let w = "50"
    printf "Setting width to %s" w
    w |> setDashboardWidth

/// Toggle memory direction
let toggleReverseView() =
    reverseDirection <- not reverseDirection
    match reverseDirection with
    | true ->
        reverseViewBtn.classList.add ("btn-byte-active")
        reverseViewBtn.innerHTML <- "Disable Reverse Direction"
    | false ->
        reverseViewBtn.classList.remove ("btn-byte-active")
        reverseViewBtn.innerHTML <- "Enable Reverse Direction"


/// Toggle byte / word view
let toggleByteView() =
    byteView <- not byteView
    match byteView with
    | true ->
        byteViewBtn.classList.add ("btn-byte-active")
        byteViewBtn.innerHTML <- "Disable Byte View"
    | false ->
        byteViewBtn.classList.remove ("btn-byte-active")
        byteViewBtn.innerHTML <- "Enable Byte View"


let makeElement (id : string) (css : string) (inner : string) =
        let el = document.createElement id
        el.classList.add css
        el.innerHTML <- inner
        el

/// make an HTML element
/// id = element name
/// css = css class names to add to classlist
let makeEl (id : string) (css : string) =
        let el = document.createElement id
        el.classList.add css
        el
/// appends child node after last child in parent node, returns parent
/// operator is left associative
/// child: child node
/// node: parent node.
let (&>>) (node : Node) child =
    node.appendChild child |> ignore
    node

let createDOM (parentID : string) (childList : Node list) =
    let parent = document.createElement parentID
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent

let addToDOM (parent : Node) (childList : Node list) =
    List.iter (fun ch -> parent &>> ch |> ignore) childList
    parent


let setView view =
    (// Change the active tab
    viewTab currentView).classList.remove("active")
    (viewTab view).classList.add("active")

    (// Change the visibility of the views
    viewView currentView).classList.add("invisible")
    (viewView view).classList.remove("invisible")

    // new mutability again, update the variable
    currentView <- view

let addOptionToDropDown (dropDown: HTMLElement) optionDisp optionVal = 
    let newOption = makeElement "option" "option" optionDisp
    newOption.setAttribute ("value", optionVal)
    dropDown.appendChild(newOption) |> ignore

let getSelectedOptionFromDropDown ()  = 
    let dropDown = blockDropDown :?> HTMLSelectElement
    dropDown.value    

let getDFFSizeInp () =
    let input = DFFSizeInput :?> HTMLInputElement
    input.value

let testSVG () = 
    let newSVG = drawBlocks c5CLst tLogicLstEx

    updateBlockDiagram newSVG 

let updateTable (mem) =     
    // old makeRow uses address, 
    let makeRow (strA, strB) =
        let tr = makeEl "tr" "tr-head-mem"
        let rowDat =
            [strA; strB]
        let makeNode txt = makeElement "td" "selectable-text" txt :> Node

        addToDOM tr (List.map makeNode rowDat)

    let makeContig (lst : (string * string) list) =
        let table = makeEl "table" "table-striped"
        let makeNode txt = makeElement "th" "th-mem" txt :> Node
        let tr = createDOM "tr" <| List.map makeNode (["Net Name"; "Value" ])

        // Add each row to the table from lst
        let rows =
            lst
            |> List.map makeRow

        addToDOM table <| [ tr ] @ rows
        |> ignore

        let li = makeEl "li" "list-group-item"
        li.style.padding <- "0px"

        addToDOM li [ table ]

    memList.innerHTML <- ""

    // Add the new memory list
    mem
    |> List.map makeContig
    |> List.iter (fun html -> memList.appendChild (html) |> ignore)