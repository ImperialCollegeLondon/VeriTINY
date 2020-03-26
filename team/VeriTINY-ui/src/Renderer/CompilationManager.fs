module CompilationManager

open EEExtensions
open Views
open Refs
open Fable.Import
open SharedTypes

let doesBlockExist blockName = 
    match List.tryFind (fun (el: TLogic) -> el.Name = blockName) TLogicList with
    |Some _ -> true
    |None -> false

let compileSourceCode() = 
    Browser.console.log "Compiling code" |> ignore
    let editorCode = currentFileTabId |> getCode
    Browser.console.log editorCode |> ignore
    let logicBlock = editorCode 
                     |> Lexer.tokenise 
                     |> Parser.parse 
                     |> function 
                        | Ok ast ->
                            ast |> LogicBlockGen.convertAST
                        | Error _ -> 
                            showAlert "Error in source code" "Oh no!" 
                            failwithf "Error in source code" // to return same type 
    
    Browser.console.log logicBlock |> ignore
    if doesBlockExist logicBlock.Name
    then showAlert "There is already a block with this name in the simulation" "Error!"
    else
        TLogicList <- TLogicList @ [logicBlock]
        addOptionToDropDown blockDropDown logicBlock.Name logicBlock.Name   
