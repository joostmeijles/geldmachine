module GeldMachine.Charting

open FSharp.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint

open System
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

type ChartControl (rows:OHLC seq, sphs:list<OHLC>, spls:list<OHLC>) as self = 
    inherit UserControl()
    
    let toHLOC (data:OHLC) = data.Date, data.High, data.Low, data.Open, data.Close
    let HLOC  = Seq.map toHLOC rows
    let dates = Seq.map (fun r -> r.Date.ToString("dd/MM/yy")) rows
    let minRows rows = Seq.min (Seq.map (fun r -> r.Low)  rows)
    let maxRows rows = Seq.max (Seq.map (fun r -> r.High) rows)
    let sphIndices = Seq.map (fun r -> Seq.findIndex (fun a -> r.Date = a.Date) rows) sphs
    let splIndices = Seq.map (fun r -> Seq.findIndex (fun a -> r.Date = a.Date) rows) spls

    let priceChart =
        let min = float (minRows rows)
        let max = float (maxRows rows)
        Chart.Candlestick(HLOC).WithYAxis(Min = min, Max = max)

    let volumeChart =
        let V = Seq.map (fun r -> (*r.Date,*) r.Volume) rows
        Chart.Column(V)

    let findControl (controls:Control.ControlCollection) =
        let mutable (found:Chart Option) = None
        found <- None
        for c in controls do
            if c.GetType() = typeof<Chart> then
                found <- Some(c :?> Chart)
        found   

    let addSwingpointAnnotation (chart:Chart) spIndices text =
        for i in spIndices do 
            let dp = chart.Series.[0].Points.[i]
            let ta = new TextAnnotation()
            ta.Text <- text
            ta.AnchorDataPoint <- dp
            chart.Annotations.Add(ta) 

    let formatDates (chart:Chart) =
        let mutable i = 0
        for d in dates do
            chart.Series.[0].Points.[i].AxisLabel <- d
            chart.Series.[0].Points.[i].ToolTip   <- d + " High=#VALY1, Low=#VALY2, Open=#VALY3, Close=#VALY4"
            chart.Series.[1].Points.[i].ToolTip   <- d + " Volume=#VALY1"
            i <- i + 1

    do
        let combine = Chart.Rows [priceChart; volumeChart]
        let combineChart = new ChartTypes.ChartControl(combine, Dock = DockStyle.Fill)

        let c = findControl combineChart.Controls
        match c with
        | Some(c) ->
            c.ChartAreas.[1].AxisX.LabelStyle.Enabled <- false
            c.ChartAreas.[0].AlignWithChartArea <- "Area_2"
            c.ChartAreas.[1].Position.Height <- c.ChartAreas.[1].Position.Height / (float32 2) 
            formatDates c
            addSwingpointAnnotation c sphIndices "SPH"
            addSwingpointAnnotation c splIndices "SPL"
        | None -> ignore()

        self.Controls.Add(combineChart)