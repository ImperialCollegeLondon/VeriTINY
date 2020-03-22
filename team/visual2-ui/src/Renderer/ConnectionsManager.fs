module ConnectionsManager

open Refs
open Views
open BlockGraphics
open Connector
open SharedTypes
open Fable.Import.Browser


let getAllConnOutNetNames () =   
    List.fold (fun allNetNames (Name blockName, _, outGenNets) ->     
        let outNetNames = List.map Connector.getName outGenNets
        allNetNames @ outNetNames) [] connLst
    |> List.distinct

let getAllConnInpNetNames () =   
    List.fold (fun allNetNames (Name blockName, inpGenNets, _) ->
        let inpNetNames = List.map Connector.getName inpGenNets       
        allNetNames @ inpNetNames) [] connLst
    |> List.distinct

let updateConnNetDropDowns () =
    let allConnInpNetNames = getAllConnInpNetNames ()
    let allConnOutNetNames = getAllConnOutNetNames ()

    connDropDown1.innerHTML <- ""
    connDropDown2.innerHTML <- ""

    List.iter (fun netName -> addOptionToDropDown connDropDown1 netName netName) allConnOutNetNames
    List.iter (fun netName -> addOptionToDropDown connDropDown2 netName netName) allConnInpNetNames

let updateConnLst (newConnLst: Connection list) = 
    connLst <- newConnLst
    let updatedSVG = drawBlocks connLst TLogicList

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
        updateConnLst (Connector.addConnection connLst DFFConnection)
    |blockName ->
        let combBlockConnection = addTLogic nameBlockToAdd TLogicList
        updateConnLst (Connector.addConnection connLst combBlockConnection)

let makeConnectionsBtnClickListener () =
    let net1 = getSelectedOptionFromDropDown connDropDown1
    let net2 = getSelectedOptionFromDropDown connDropDown2

    let connectionValid = checkValidConnection net1 net2 connLst
    match connectionValid with
    |Ok _ ->
        let newLink = makeLink net1 net2 connLst
        let newConnLst = applyLinks [newLink] connLst
        updateConnLst newConnLst
    |Error str ->
        showAlert str "Error"

let clearConnectionsBtnClickListener () =
    updateConnLst []