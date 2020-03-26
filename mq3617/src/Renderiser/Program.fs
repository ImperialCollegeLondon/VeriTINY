// code based of example @ https://github.com/fable-compiler/fable2-samples

module Program

open Fable.Core.JsInterop
open Fable.Import

let window = Browser.Dom.window

let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "myCanvas"  // canvas @ public/index.html

let ctx = myCanvas.getContext_2d()

let steps = 30
let squareSize = 20

// gridWidth needs a float wo we cast tour int operation to a float using the float keyword
let gridWidth = float (steps * squareSize) 

myCanvas.width <- (gridWidth*20.0)
myCanvas.height <- gridWidth

[0..steps] // this is a list
  |> Seq.iter( fun x -> // we iter through the list using an anonymous function
      let v = float ((x) * squareSize) 
      ctx.moveTo(v, 0.)
      ctx.lineTo(v, gridWidth)
      ctx.moveTo(0., v)
      ctx.lineTo(gridWidth, v)
    ) 
ctx.strokeStyle <- !^"#abb" // color
ctx.stroke() 

ctx.textAlign <- "center"
ctx.fillText("Testo", gridWidth * 0.5, gridWidth * 0.5)

printfn "done!"


