module BlockGraphics

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.Import.React
open Fable.Helpers.React
module HTMLProps =  Fable.Helpers.React.Props
open SharedTypes
open Refs
open EEExtensions


let DFFTLogic (size: int) : TLogic =
    let sliceIndices = 
        match size with
        |1 -> None
        |_ -> Some(size - 1, Some 0)
    let iNetID =    
        {
            Name = "D";
            SliceIndices = sliceIndices
        }

    let oNetID =    
        {
            Name = "Q"
            SliceIndices = sliceIndices
        }
     
    {   Name = "DFF"
        Inputs = [iNetID]
        Outputs = [oNetID]
        ExpressionList = []
        Wires = []
    }

let makeLabelElements xPosition yOffset anchorPos  = 
    List.mapi (fun i (inpName: string) -> 
            text [
            HTMLProps.Y (sprintf "%i" (i * 30 + yOffset + 55));
            HTMLProps.X xPosition;
            HTMLProps.TextAnchor anchorPos;
            HTMLProps.Fill "black"
            ] [str inpName]
        )


let makeTLogicSVG (block: TLogic) xPos yPos=
    let numRows = (max (List.length block.Inputs)  (List.length block.Outputs))
    let blockHeight = (50 + (30 * numRows))

    let netIDToString truncateTo (netID: NetIdentifier)  = 
        let sliceStr =
            match netID.SliceIndices with
            |Some (x , Some y) -> sprintf "[%i:%i]" (max x y) (min x y)
            |Some (x, None) -> sprintf "[%i]" x
            |None -> ""

        netID.Name.[0..truncateTo] + sliceStr

    let inputLabels = 
        List.map (netIDToString 5) block.Inputs 
        |> makeLabelElements "25%" 0 "middle"

    let outputLabels = 
        List.map (netIDToString 5) block.Outputs 
        |> makeLabelElements "75%" 0 "middle"

    let svgChildrenBase = [
        rect [
            HTMLProps.Width "100%";
            HTMLProps.Height "100%";
            HTMLProps.Fill "white";
            HTMLProps.Stroke "blue";
        ] []

        text [
            HTMLProps.Y "20%";
            HTMLProps.X "50%";
            HTMLProps.TextAnchor "middle";
            HTMLProps.Fill "black"
        ] [str block.Name]
    ] 

   

    (svg [
        HTMLProps.Y yPos;
        HTMLProps.X xPos;
        HTMLProps.Width "200";
        HTMLProps.Height (sprintf "%i" blockHeight);
    ]  (svgChildrenBase @ inputLabels @ outputLabels), blockHeight)

let blockToSVG (block: SimBlock) (tLogicLst: TLogic list) xPos yPos =

    let blockTLogic = 
        match block.MegablockType with
        |"DFF" -> DFFTLogic (block.inNets |> List.head |> Helper.extractNetIDFromInfo |> EvalNetHelper.getBusSize)
        |combBlockName -> List.find (fun tlogic -> tlogic.Name = combBlockName) tLogicLst

    let formNetLines x1 x2 numLines =
        [0..numLines-1]
            |> List.map (fun i -> 
                line [
                    HTMLProps.X1 (sprintf "%i" x1)
                    HTMLProps.X2 (sprintf "%i" x2)
                    HTMLProps.Y1 (sprintf "%i" ((i * 30 + 55)))
                    HTMLProps.Y2 (sprintf "%i" ((i * 30 + 55)))
                    HTMLProps.Stroke "black"
                ] []) 

    let inputNetLines = formNetLines 0 75 (List.length blockTLogic.Inputs)
    let outputNetLines = formNetLines 275 350 (List.length blockTLogic.Outputs)

    let getTruncatedNetName truncateTo netInfo = 
        let name = Helper.extractNameFromInfo netInfo
        let prefixRemovedName = 
            name
            |> Seq.toList
            |> List.rev
            |> List.takeWhile (fun el -> el <> '.')
            |> List.rev
            |> String.ofList
        prefixRemovedName.[0..truncateTo]

    let inputNetLables = 
        List.map (getTruncatedNetName 6) block.inNets
        |> makeLabelElements "0" -3 "start"

    let outputNetLables = 
        List.map (getTruncatedNetName 6) block.outNets
        |> makeLabelElements "280" -3 "start"

    let tLogicBlockSVG, blockHeight = makeTLogicSVG blockTLogic "75" "0"

    (svg [
        HTMLProps.Y yPos;
        HTMLProps.X xPos;
        HTMLProps.Width "350";
        HTMLProps.Height (sprintf "%i" blockHeight);
    ] ([tLogicBlockSVG] @ inputNetLines @ outputNetLines @ inputNetLables @ outputNetLables), blockHeight)



let drawBlocks (blockLst: SimBlock list) (tLogicLst: TLogic list) =
    let _, _, _, svgHeight, svgLst =
        List.fold (fun (i, nextY, lastBlockHeight, _, svgLst) block ->    
            if i%2 = 0
            then 
                let blockSVG, blockHeight = blockToSVG block tLogicLst 25 nextY
                i+1, nextY, blockHeight, nextY + blockHeight, (svgLst @ [blockSVG])
            else 
                let blockSVG, blockHeight = blockToSVG block tLogicLst 425 nextY
                i+1, nextY + (max lastBlockHeight blockHeight) + 20, blockHeight, nextY + (max lastBlockHeight blockHeight), (svgLst @ [blockSVG])
                
            ) (0, 0, 0, 0, []) blockLst

    svg [
        HTMLProps.Y "0";
        HTMLProps.X "0";
        HTMLProps.ViewBox (sprintf "0 0 800 %i" svgHeight);
    ] svgLst


let updateBlockDiagram svg  = ReactDom.render (svg, blocksSVGContainer)