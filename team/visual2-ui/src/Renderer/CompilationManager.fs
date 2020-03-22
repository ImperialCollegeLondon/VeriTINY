module CompilationManager

open EEExtensions
open Tabs
open Views
open CommonData
open ParseTop
open ExecutionTop
open Errors
open Refs
open Editors
open TestLib
open Testbench
open Fable.Core.JsInterop
open Fable.Import

let compileSourceCode() = 
    Browser.console.log "Compiling code" |> ignore
    let editorCode = currentFileTabId |> getCode
    Browser.console.log editorCode |> ignore
    let logicBlock = editorCode 
                     |> Lexer.tokenise 
                     |> Parser.parse 
                     |> fun (Ok ast) -> ast |> LogicBlockGen.convertAST
    Browser.console.log logicBlock |> ignore
    TLogicList <- TLogicList @ [logicBlock]
    addOptionToDropDown blockDropDown logicBlock.Name logicBlock.Name   
