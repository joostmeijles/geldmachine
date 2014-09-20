module GeldMachine.Charting

open FSharp.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

type ChartControl (rows:OHLC seq, sphs:list<OHLC>, spls:list<OHLC>) as self = 
    inherit UserControl()
    
    let toHLOC (data:OHLC) = data.Date, data.High, data.Low, data.Open, data.Close
    let minRows rows = Seq.min (Seq.map (fun (r:OHLC) -> r.Low) rows)
    let maxRows rows = Seq.max (Seq.map (fun (r:OHLC) -> r.High) rows)
    let sphIndices = Seq.map (fun (r:OHLC) -> Seq.findIndex (fun (a:OHLC) -> r.Date = a.Date) rows) sphs
    let splIndices = Seq.map (fun (r:OHLC) -> Seq.findIndex (fun (a:OHLC) -> r.Date = a.Date) rows) spls

    let priceChart =
        let min = float (minRows rows)
        let max = float (maxRows rows)
        let HLOC = Seq.map toHLOC rows
        Chart.Candlestick(HLOC).WithYAxis(Min = min, Max = max)

    let volumeChart =
        let V = Seq.map (fun (r:OHLC) -> r.Date, r.Volume) rows
        Chart.Column(V)

    let findControl (controls:Control.ControlCollection) =
        let mutable (found:Chart Option) = None
        found <- None
        for c in controls do
            if c.GetType() = typeof<Chart> then
                found <- Some(c :?> Chart)
        found   

    do
        let combine = Chart.Rows [priceChart; volumeChart]
        let combineChart = new ChartTypes.ChartControl(combine, Dock = DockStyle.Fill)

        let c = findControl combineChart.Controls
        match c with
        | Some(c) ->
            for sphi in sphIndices do 
                let dp = c.Series.[0].Points.[sphi]
                let ta = new TextAnnotation()
                ta.Text <- "SPH"
                ta.AnchorDataPoint <- dp
                c.Annotations.Add(ta)

            for spli in splIndices do 
                let dp = c.Series.[0].Points.[spli]
                let ta = new TextAnnotation()
                ta.Text <- "SPL"
                ta.AnchorDataPoint <- dp
                c.Annotations.Add(ta)
        | None -> ignore()

        self.Controls.Add(combineChart)     