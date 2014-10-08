open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Drawing

open GeldMachine.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint
open GeldMachine.Indicator.Trend

let printStockData (data:StockData) =
    printfn "%A" data.Headers
    Seq.take 10 data.Rows |> Seq.iter (fun d -> printfn "%A " d)

let analyzeData stock =
    let data   = getStockDataOffline 60 stock
    let sphs   = getSPHs data
    let spls   = getSPLs data
    let trends = getTrends data spls sphs
    data, sphs, spls, trends

let aapl = analyzeData "AAPL"
let msft = analyzeData "MSFT"
let gspc = analyzeData "^GSPC"

let addToForm (form : Form) (data, sphs, spls, trends) =
    printfn "%A" trends 
    form.Controls.Add(new ChartControl(data, sphs, spls, trends, Dock = DockStyle.Fill))

[<EntryPoint>]
let main argv = 
    let form = new Form(Visible = true, Width = 700, Height = 500)
    addToForm form msft
    Application.Run(form);
    0 //exit code
