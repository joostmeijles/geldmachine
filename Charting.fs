﻿module GeldMachine.Charting

open FSharp.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint
open GeldMachine.Indicator.Trend

open System
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

type ChartControl (rows:OHLC seq, sphs:list<OHLC>, spls:list<OHLC>, trends:list<OHLC * (Trend*Strength)>) as self = 
    inherit UserControl()
    
    let toHLOC (data:OHLC) = data.Date, data.High, data.Low, data.Open, data.Close
    let HLOC  = Seq.map toHLOC rows
    let dates = Seq.map (fun r -> r.Date.ToString("dd/MM/yy")) rows
    let minRows rows = Seq.min (Seq.map (fun r -> r.Low)  rows)
    let maxRows rows = Seq.max (Seq.map (fun r -> r.High) rows)
    let sphIndices = Seq.map (fun b -> Seq.findIndex (fun r -> r.Date = b.Date) rows) sphs
    let splIndices = Seq.map (fun b -> Seq.findIndex (fun r -> r.Date = b.Date) rows) spls
    let trendIndices = Seq.map (fun (b,s) -> s,(Seq.findIndex (fun r -> r.Date = b.Date) rows)) trends

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
            
    let addTrendAnnotation (chart:Chart) trendIndices =
        for (b,s),i in trendIndices do
            let dp = chart.Series.[0].Points.[i]
            let aa = new TextAnnotation() //TODO: Use ArrowAnnotation
            //aa.ArrowStyle <- ArrowStyle.Simple
            let color = match s with
                        | Ambivalent -> Color.Black
                        | Confirmed  -> Color.Green
                        | Suspect    -> Color.Red
            let text = match b with
                       | Sideways -> "Side"
                       | Bullish  -> "Bull"
                       | Bearish  -> "Bear"
            aa.Text <- text
            //aa.ArrowSize <- 100
            aa.ForeColor <- color
            aa.AnchorDataPoint <- dp
            chart.Annotations.Add(aa)

    let alignCharts (chart:Chart) = 
        chart.ChartAreas.[0].AlignWithChartArea <- "Area_2"
        chart.ChartAreas.[1].Position.Height    <- chart.ChartAreas.[1].Position.Height / (float32 2) 
        chart.ChartAreas.[1].Position.Y         <- chart.ChartAreas.[1].Position.Y - (float32 5)

    let formatDates (chart:Chart) =
        chart.ChartAreas.[1].AxisX.LabelStyle.Enabled <- false
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
            alignCharts c    
            formatDates c
            addSwingpointAnnotation c sphIndices "SPH"
            addSwingpointAnnotation c splIndices "SPL"
            addTrendAnnotation c trendIndices
        | None -> ignore()

        self.Controls.Add(combineChart)