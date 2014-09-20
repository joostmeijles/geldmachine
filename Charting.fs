module GeldMachine.Charting

open FSharp.Charting
open GeldMachine.Data
open System.Windows.Forms
open System.Drawing

type ChartControl (rows:StockData.Row seq) as self = 
    inherit UserControl()
    
    let toHLOC (data:StockData.Row) = data.Date, data.High, data.Low, data.Open, data.Close
    let minRows rows = Seq.min (Seq.map (fun (r:StockData.Row) -> r.Low) rows)
    let maxRows rows = Seq.max (Seq.map (fun (r:StockData.Row) -> r.High) rows)

    let candleStickChart =
        let min = float (minRows rows)
        let max = float (maxRows rows)
        let HLOC = Seq.map toHLOC rows  
        Chart.Candlestick(HLOC).WithYAxis(Min = min, Max = max)

    let barChart =
        let V = Seq.map (fun (r:StockData.Row) -> r.Date, r.Volume) rows
        Chart.Column(V)

    do
        let pricingChart = new ChartTypes.ChartControl(candleStickChart, Dock = DockStyle.Fill)
        //self.Controls.Add(pricingChart)

        let volumeChart = new ChartTypes.ChartControl(barChart, Dock = DockStyle.Bottom)
        //self.Controls.Add(volumeChart)

        let combine = Chart.Rows [candleStickChart; barChart]
        let combineChart = new ChartTypes.ChartControl(combine, Dock = DockStyle.Fill)
        //combineChart |> FSharp.Charting
        self.Controls.Add(combineChart)

        