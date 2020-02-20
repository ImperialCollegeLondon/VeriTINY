module Main

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node.Api

type Winstate = {
    width:int
    height:int
    manage: BrowserWindow ->unit
}
// A global reference to the window object is required in order to prevent garbage collection
let mutable mainWindow: BrowserWindow option = None

let createMainWindow () =
  let mainWinState = {
      width = 500
      height=600
      manage = (fun x->())
  }
 
  let win =
    main.BrowserWindow.Create(jsOptions<BrowserWindowOptions>(fun o ->
      o.width <- mainWinState.width
      o.height <- mainWinState.height
      o.autoHideMenuBar <- true
      o.webPreferences <- jsOptions<WebPreferences>(fun w ->
        w.nodeIntegration <- true
      )
      o.show <- false
    ))

  win.onceReadyToShow(fun _ ->
    win.show()
    mainWinState.manage win
  ) |> ignore

  win.onClosed(fun _ -> mainWindow <- None) |> ignore

  mainWindow <- Some win


// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
main.app.onReady(fun _ _ -> createMainWindow ()) |> ignore


// Quit when all windows are closed.
main.app.onWindowAllClosed(fun _ ->
  // On OS X it's common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  //if ``process``.platform <> Node.Base.Platform.Darwin then
    main.app.quit()
) |> ignore


main.app.onActivate(fun _ _ ->
  // On OS X it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if mainWindow.IsNone then createMainWindow ()
) |> ignore

