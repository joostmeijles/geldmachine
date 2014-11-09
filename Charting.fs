module GeldMachine.Charting

open Deedle
open FSharp.Charting
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint
open GeldMachine.Indicator.Trend

open System
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting

let rec private DHLOC' accum (d:list<DateTime>) (h:list<float>) (l:list<float>) (o:list<float>) (c:list<float>) = 
    match d with
    | _ :: _ -> DHLOC' ((d.Head,h.Head,l.Head,o.Head,c.Head) :: accum) d.Tail h.Tail l.Tail o.Tail c.Tail
    | _      -> accum
    
let private DHLOC (data:Frame<DateTime,string>) = 
    let d = data.RowKeys |> Seq.toList
    let o = Series.values data?Open  |> Seq.toList
    let h = Series.values data?High  |> Seq.toList
    let l = Series.values data?Low   |> Seq.toList
    let c = Series.values data?Close |> Seq.toList
    DHLOC' [] d h l o c

let private presentIndices (data:Frame<DateTime,string>) columnName = 
    data.GetColumn<bool>(columnName) |> Series.indexOrdinally |> Series.filter (fun k v -> v) |> Series.keys 

let private getTrendIndices (data:Frame<DateTime,string>) =
    data.GetColumn<(Trend*Strength) Option>("TrendChange") 
    |> Series.indexOrdinally 
    |> Series.filter (fun k v -> Option.isSome v)
    |> Series.map (fun k v -> v.Value)
    |> Series.observations

type ChartControl (rows:OHLC seq, sphs:list<OHLC>, spls:list<OHLC>, trends:list<OHLC * (Trend*Strength)>, data:Frame<DateTime,string>) as self = 
    inherit UserControl()
    
    let dates = Seq.map (fun r -> r.Date.ToString("dd/MM/yy")) rows
    let min = float (Seq.min (Series.values data?Low))
    let max = float (Seq.max (Series.values data?High))
    let sphIndices = presentIndices data "SPH"
    let splIndices = presentIndices data "SPL"
    let trendIndices = getTrendIndices data

    let priceChart =
        Chart.Candlestick(DHLOC data).WithYAxis(Min = min, Max = max)

    let volumeChart =
        let V = Series.values data?Volume
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
        for i,(t,s) in trendIndices do
            let dp = chart.Series.[0].Points.[i]
            let aa = new TextAnnotation() //TODO: Use ArrowAnnotation
            //aa.ArrowStyle <- ArrowStyle.Simple
            let color = match s with
                        | Ambivalent -> Color.Black
                        | Confirmed  -> Color.Green
                        | Suspect    -> Color.Red
            let text = match t with
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