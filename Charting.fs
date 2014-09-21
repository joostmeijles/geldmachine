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
    let minRows rows = Seq.min (Seq.map (fun r -> r.Low)  rows)
    let maxRows rows = Seq.max (Seq.map (fun r -> r.High) rows)
    let sphIndices = Seq.map (fun r -> Seq.findIndex (fun a -> r.Date = a.Date) rows) sphs
    let splIndices = Seq.map (fun r -> Seq.findIndex (fun a -> r.Date = a.Date) rows) spls

    let priceChart =
        let min = float (minRows rows)
        let max = float (maxRows rows)
        let HLOC = Seq.map toHLOC rows
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

    do
        let combine = Chart.Rows [priceChart; volumeChart]
        let combineChart = new ChartTypes.ChartControl(combine, Dock = DockStyle.Fill)

        let c = findControl combineChart.Controls
        match c with
        | Some(c) ->
            c.ChartAreas.[1].AxisX.LabelStyle.Enabled <- false
            c.ChartAreas.[0].AlignWithChartArea <- "Area_2"
            c.ChartAreas.[0].AxisX.LabelStyle.Format <- "dd/MM/yy"
            addSwingpointAnnotation c sphIndices "SPH"
            addSwingpointAnnotation c splIndices "SPL"
        | None -> ignore()

        self.Controls.Add(combineChart)