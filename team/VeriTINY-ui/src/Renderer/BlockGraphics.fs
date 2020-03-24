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


// tLogics for tests 
let tLogicEx1 : TLogic= { 
    Name = "bus_and"
    ExpressionList =
        [(And, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = Some (3, Some 1) };
            { Name = "b"; SliceIndices = Some (3, Some 1) }]);
        (And, [{ Name = "d"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = Some (0, None) };
            { Name = "b"; SliceIndices = Some (0, None) }])]
    Inputs = [{ Name = "a"; SliceIndices = Some (3, Some 1) };
            { Name = "b"; SliceIndices = Some (3, Some 1) }]
    Outputs = [{ Name = "c"; SliceIndices = None }]
    Wires = [] }

let tLogicEx2 : TLogic= { 
    Name = "simpAND"
    ExpressionList =
        [(And, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None };
            { Name = "b"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None };
        { Name = "b"; SliceIndices = None }]
    Outputs = [{ Name = "c"; SliceIndices = None }]
    Wires = [] }

let tLogicEx3 : TLogic= { 
    Name = "simpOR"
    ExpressionList =
        [(Or, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None };
            { Name = "b"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None };
        { Name = "b"; SliceIndices = None }]
    Outputs = [{ Name = "c"; SliceIndices = None }]
    Wires = [] }

let tLogicEx4 : TLogic= { 
    Name = "simpNOT"
    ExpressionList =
        [(Not, 
            [{ Name = "b"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None }]
    Outputs = [{ Name = "b"; SliceIndices = None }]
    Wires = [] }

// D is not mapped (should retain prev value)
let tLogicEx5 : TLogic= { 
    Name = "AND"
    ExpressionList =
        [(And, 
            [{ Name = "c"; SliceIndices = None }],
            [{ Name = "a"; SliceIndices = None };
            { Name = "b"; SliceIndices = None }])]
    Inputs =
        [{ Name = "a"; SliceIndices = None };
        { Name = "b"; SliceIndices = None }]
    Outputs = [{ Name = "c"; SliceIndices = None }; { Name = "d"; SliceIndices = None }]
    Wires = [] }

let tLogicLstEx = [tLogicEx1; tLogicEx2; tLogicEx3; tLogicEx4; tLogicEx5]


let c5and1In: GeneralNet list =
    [false, ("andIn0", Wire(Map [0, High]));
    false, ("andIn1", Wire(Map [0, Low]))]

let c5and1Out: GeneralNet list =
    [false, ("andOut1", Wire(Map [0, Low]));
    false, ("unMappedOutput", Wire(Map [0, High]))]

let c5CLst = 
    [Name "AND", c5and1In, c5and1Out;
    //  Name "AND", c5and1In, c5and1Out;
    //  Name "AND", c5and1In, c5and1Out;
    //  Name "AND", c5and1In, c5and1Out;
     Name "DFF", c5and1In, c5and1Out;
    Name "AND", c5and1In, c5and1Out;]



let first (x,_,_) = x
let second (_,x,_) = x
let third (_,_,x) = x

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

let connToSVG (conn: Connection) (tLogicLst: TLogic list) xPos yPos =
    let (Name megaBlockName)  = first conn
    let connTLogic = 
        match megaBlockName with
        |"DFF" -> DFFTLogic (Helper.netSize (conn |> second |> List.head |> Helper.extractNet))
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

    let inputNetLines = formNetLines 0 75 (List.length connTLogic.Inputs)
    let outputNetLines = formNetLines 275 350 (List.length connTLogic.Outputs)

    let getTruncatedGeneralNetName truncateTo genNet = 
        let name = genNet |> Connector.getName
        let prefixRemovedName = 
            name
            |> Seq.toList
            |> List.rev
            |> List.takeWhile (fun el -> el <> '.')
            |> List.rev
            |> String.ofList
        prefixRemovedName.[0..truncateTo]

    let inputNetLables = 
        List.map (getTruncatedGeneralNetName 6) (second conn)
        |> makeLabelElements "0" -3 "start"

    let outputNetLables = 
        List.map (getTruncatedGeneralNetName 6) (third conn)
        |> makeLabelElements "280" -3 "start"

    let tLogicBlockSVG, blockHeight = makeTLogicSVG connTLogic "75" "0"

    (svg [
        HTMLProps.Y yPos;
        HTMLProps.X xPos;
        HTMLProps.Width "350";
        HTMLProps.Height (sprintf "%i" blockHeight);
    ] ([tLogicBlockSVG] @ inputNetLines @ outputNetLines @ inputNetLables @ outputNetLables), blockHeight)



let drawBlocks (connLst: Connection list) (tLogicLst: TLogic list) =
    let _, _, _, svgHeight, svgLst =
        List.fold (fun (i, nextY, lastBlockHeight, _, svgLst) conn ->    
            if i%2 = 0
            then 
                let connSVG, blockHeight = connToSVG conn tLogicLst 25 nextY
                i+1, nextY, blockHeight, nextY + blockHeight, (svgLst @ [connSVG])
            else 
                let connSVG, blockHeight = connToSVG conn tLogicLst 425 nextY
                i+1, nextY + (max lastBlockHeight blockHeight) + 20, blockHeight, nextY + (max lastBlockHeight blockHeight), (svgLst @ [connSVG])
                
            ) (0, 0, 0, 0, []) connLst

    svg [
        HTMLProps.Y "0";
        HTMLProps.X "0";
        HTMLProps.ViewBox (sprintf "0 0 800 %i" svgHeight);
    ] svgLst


let updateBlockDiagram svg  = ReactDom.render (svg, blocksSVGContainer)