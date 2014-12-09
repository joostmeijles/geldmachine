open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Drawing

open Deedle

open GeldMachine.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint
open GeldMachine.Indicator.Trend

let printStockData (data:StockData) =
    printfn "%A" data.Headers
    Seq.take 10 data.Rows |> Seq.iter (fun d -> printfn "%A " d)

let makeSPColumn (sps:list<OHLC>) data =
    List.map (fun d -> if (List.exists (fun s -> s == d) sps) then true else false) data

let tryFind' f (lst:list<'a*'b>) = 
    let found = List.tryFind f lst
    match found with
    | Some e -> Some (snd e)
    | None   -> None

let analyzeData symbolName =
    let data   = getStockData 60 symbolName
    let frame  = toFrame data
    
    let sphs  = getSPHs data
    let sphs' = makeSPColumn sphs data
    frame?SPH <- sphs'
    
    let spls  = getSPLs data
    let spls' = makeSPColumn spls data
    frame?SPL <- spls'
    
    let trends  = getTrends data spls sphs
    let trends' = List.map (fun d -> tryFind' (fun (b,t) -> b == d) trends) data
    frame?TrendChange <- trends'

    let frame60 = Frame.takeLast 60 frame
    printf "SPHs: %A" frame60?SPH

    symbolName, frame60

//let aapl = analyzeData "AAPL"
//let msft = analyzeData "MSFT"
let gspc = analyzeData "^GSPC"
//let fb = analyzeData "FB"

let addToForm (form : Form) (symbolName, data) =
    form.Controls.Add(new ChartControl(symbolName, data))

[<EntryPoint>]
let main argv = 
    let form = new Form(Visible = true, Width = 700, Height = 500)
    addToForm form gspc
    //let dates = gspc.RowKeys |> Seq.map (fun d -> d.ToString("dd/MM/yy")) |> Seq.last
    //printfn "%A" dates
    Application.Run(form);
    0 //exit code
