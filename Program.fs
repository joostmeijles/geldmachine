open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting
open System.Drawing

open GeldMachine.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint

let printStockData (data:StockData) =
    printfn "%A" data.Headers
    Seq.take 10 data.Rows |> Seq.iter (fun d -> printfn "%A " d)


let gspc = getStockData "^GSPC"
let msft = 
    let data = getStockData "MSFT"
    Seq.take 60 data.Rows

[<EntryPoint>]
let main argv = 
    let form = new Form(Visible = true, Width = 700, Height = 500)
    form.Controls.Add(new ChartControl(msft, Dock = DockStyle.Fill))
    Application.Run(form);
    0 //exit code
