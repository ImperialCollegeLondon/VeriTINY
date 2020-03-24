(*
    VisUAL2 @ Imperial College London
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compiler )
    Module: Renderer.Integration
    Description: Code to integrate the emulator with the renderer
*)

/// integrate emulator code with renderer
module Integration

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

let resetEmulator() =
    printfn "Resetting"
    // Tooltips.deleteAllContentWidgets()
    // Editors.removeEditorDecorations currentFileTabId
    // Editors.enableEditors()
    // memoryMap <- Map.empty
    // symbolMap <- Map.empty
    // regMap <- initialRegMap
    // setMode ResetMode
    // updateClockTime (0uL, 0uL)
