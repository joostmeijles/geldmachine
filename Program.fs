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


let gspc = getStockData 60 "^GSPC"
let msft = getStockData 60 "MSFT"

let msftSPHs = getSPHs msft
let msftSPLs = getSPLs msft

[<EntryPoint>]
let main argv = 
    let t = getTrends msft msftSPLs msftSPHs
    printfn "%A" t
    let form = new Form(Visible = true, Width = 700, Height = 500)
    form.Controls.Add(new ChartControl(msft, msftSPHs, msftSPLs, Dock = DockStyle.Fill))
    Application.Run(form);
    0 //exit code
