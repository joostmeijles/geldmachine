open System
open System.Windows.Forms
open System.Drawing
open FSharp.Charting
open GeldMachine.Data

let printStockData (data:StockData) =
    printfn "%A" data.Headers
    Seq.take 10 data.Rows |> Seq.iter (fun d -> printfn "%A " d)

let toHLOC (data:StockData.Row) = data.Date, data.High, data.Low, data.Open, data.Close
let minRows rows = Seq.min (Seq.map (fun (r:StockData.Row) -> r.Low) rows)
let maxRows rows = Seq.max (Seq.map (fun (r:StockData.Row) -> r.High) rows)

let candleStickChart rows =
    let min = float (minRows rows)
    let max = float (maxRows rows)
    let HLOC = Seq.map toHLOC rows  
    Chart.Candlestick(HLOC).WithYAxis(Min = min, Max = max)

let gspc = getStockData "^GSPC"
let msft = getStockData "MSFT"

let msftChart = candleStickChart (Seq.take 60 msft.Rows)

[<EntryPoint>]
let main argv = 
    printStockData msft
    printStockData gspc

    //Console.ReadLine() |> ignore

    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    form.Controls.Add(new ChartTypes.ChartControl(msftChart, Dock = DockStyle.Fill))
    Application.Run(form);

    0 //exit code
