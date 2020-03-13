module ConnectionsManager

open Refs
open Views
open BlockGraphics
open Connector
open SharedTypes

let addToBlockDropDown name =
    addOptionToDropDown blockDropDown name name


let getAllConnNetNames () = 
    List.fold (fun allNetNames (Name blockName, inpGenNets, outGenNets) ->
        let genNetName genNet = sprintf "%s.%s" blockName (genNet |> snd |> fst)
        let inpNetNames = List.map genNetName inpGenNets
        let outNetNames = List.map genNetName outGenNets
        allNetNames @ inpNetNames @ outNetNames) [] connLst

let updateConnNetDropDowns () =
    let allConnNetNames = getAllConnNetNames ()

    connDropDown1.innerHTML <- ""
    connDropDown2.innerHTML <- ""

    List.iter (fun netName ->
        addOptionToDropDown connDropDown1 netName netName
        addOptionToDropDown connDropDown2 netName netName) allConnNetNames

let addBlockBtnClickListener () =
    let nameBlockToAdd = getSelectedOptionFromDropDown ()

    match nameBlockToAdd with
    |"DFF" ->     
        let DFFSize = getDFFSizeInp () |> int
        //TODO: add error checking here
        let DFFConnection = addDFF DFFSize       
        connLst <- connLst @ [DFFConnection]
    |blockName ->
        let combBlockConnection = addTLogic nameBlockToAdd TLogicList
        connLst <- connLst @ [combBlockConnection]

    connLst <- giveUniqueNames ([], []) connLst

    let updatedSVG = drawBlocks connLst TLogicList

    updateBlockDiagram updatedSVG
    updateConnNetDropDowns ()

// let makeConnectionsBtnClickListener () =
