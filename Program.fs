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

let translate (row:StockData.Row) = {Date = row.Date; Open = row.Open; High = row.High; Low = row.Low; Close = row.Close; Volume = row.Volume; Adjusted = row.``Adj Close``}

let gspc = getStockData "^GSPC"
let msft = 
    let data = getStockData "MSFT"
    let ohlc = Seq.map translate (Seq.take 60 data.Rows)
    List.rev (Seq.toList ohlc)

let msftSPHs = getSPHs msft
let msftSPLs = getSPLs msft

[<EntryPoint>]
let main argv = 
    printfn "%A" msft
    printfn "--------------------------------------------------------"
    printfn "SPHs: %A" msftSPHs
    printfn "--------------------------------------------------------"
    printfn "SPLs: %A" msftSPLs

    let form = new Form(Visible = true, Width = 700, Height = 500)
    form.Controls.Add(new ChartControl(msft, msftSPHs, msftSPLs, Dock = DockStyle.Fill))
    Application.Run(form);
    0 //exit code
