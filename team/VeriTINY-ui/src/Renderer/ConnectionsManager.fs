module ConnectionsManager

open Refs
open Views
open BlockGraphics
open Connector
open SharedTypes
open Fable.Import.Browser


let getAllConnOutNetNames () =   
    List.fold (fun allNetNames block ->
        let outNetNames = block.outNets |> List.map Helper.extractNameFromInfo
        allNetNames @ outNetNames) [] blockLst
    |> List.distinct

let getAllConnInpNetNames () =   
    List.fold (fun allNetNames block ->
        let inNetNames = block.inNets |> List.map Helper.extractNameFromInfo
        allNetNames @ inNetNames) [] blockLst
    |> List.distinct

let updateConnNetDropDowns () =
    let allConnInpNetNames = getAllConnInpNetNames ()
    let allConnOutNetNames = getAllConnOutNetNames ()

    connDropDown1.innerHTML <- ""
    connDropDown2.innerHTML <- ""

    List.iter (fun netName -> addOptionToDropDown connDropDown1 netName netName) allConnOutNetNames
    List.iter (fun netName -> addOptionToDropDown connDropDown2 netName netName) allConnInpNetNames

let updateConnLst (newBlockLst: SimBlock list) = 
    blockLst <- newBlockLst
    let updatedSVG = drawBlocks blockLst TLogicList

    updateBlockDiagram updatedSVG
    updateConnNetDropDowns ()

let addToBlockDropDown name =
    addOptionToDropDown blockDropDown name name


let addBlockBtnClickListener () =
    let nameBlockToAdd = getSelectedOptionFromDropDown blockDropDown

    match nameBlockToAdd with
    |"DFF" ->     
        let DFFSize = getDFFSizeInp () |> int
        //TODO: add error checking here
        let DFFConnection = addDFF DFFSize       
        updateConnLst (Connector.addConnection blockLst DFFConnection)
    |blockName ->
        let combBlockConnection = addTLogic nameBlockToAdd TLogicList
        updateConnLst (Connector.addConnection blockLst combBlockConnection)
        console.log (sprintf "Conn block: %A" combBlockConnection)

let makeConnectionsBtnClickListener () =
    let net1 = getSelectedOptionFromDropDown connDropDown1
    let net2 = getSelectedOptionFromDropDown connDropDown2

    let connectionValid = checkValidConnection net1 net2 blockLst
    match connectionValid with
    |Ok _ ->
        let newLink = makeLink net1 net2 blockLst
        let newConnLst = applyLinks [newLink] blockLst
        updateConnLst newConnLst
    |Error str ->
        showAlert str "Error!"

let clearConnectionsBtnClickListener () =
    updateConnLst []

let clearBlocksBtnClickListener () = 
    TLogicList <- []
    blockDropDown.innerHTML <- ""
    addOptionToDropDown blockDropDown "DFF" "DFF"

    updateConnLst []