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
    let d = data.RowKeys |> Seq.toList |> List.rev
    let o = Series.values data?Open  |> Seq.toList |> List.rev
    let h = Series.values data?High  |> Seq.toList |> List.rev
    let l = Series.values data?Low   |> Seq.toList |> List.rev
    let c = Series.values data?Close |> Seq.toList |> List.rev
    DHLOC' [] d h l o c

let private presentIndices (data:Frame<DateTime,string>) columnName = 
    data.GetColumn<bool>(columnName) |> Series.indexOrdinally |> Series.filter (fun k v -> v) |> Series.keys 

let private getTrendIndices (data:Frame<DateTime,string>) =
    data.GetColumn<(Trend*Strength) Option>("TrendChange") 
    |> Series.indexOrdinally 
    |> Series.filter (fun k v -> Option.isSome v)
    |> Series.map (fun k v -> v.Value)
    |> Series.observations

let private createSeries name chartType =
        let s = new Series()
        s.ChartArea <- name
        s.ChartType <- chartType
        s.Name <- name
        s.IsVisibleInLegend <- false
        s.XValueType <- System.Windows.Forms.DataVisualization.Charting.ChartValueType.DateTime
        s

let private createChartArea name x y =
        let ca = new ChartArea()
        ca.Name <- name
        ca.Position.Auto <- false 
        ca.Position.Height <- float32 42
        ca.Position.Width <- float32 88
        ca.Position.X <- float32 x
        ca.Position.Y <- float32 y
        ca

type ChartControl (data:Frame<DateTime,string>) as self = 
    inherit UserControl(Dock = DockStyle.Fill)
    
    let dates = data.RowKeys |> Seq.map (fun d -> d.ToShortDateString() (* d.ToString("dd/MM/yy")*) )
    let min = float (Seq.min (Series.values data?Low))
    let max = float (Seq.max (Series.values data?High))
    let sphIndices = presentIndices data "SPH"
    let splIndices = presentIndices data "SPL"
    let trendIndices = getTrendIndices data
    
    let priceSeries = 
        let s = createSeries "Price" SeriesChartType.Candlestick 
        s.YValuesPerPoint <- 4
        s.["PriceUpColor"]   <- "Green"
        s.["PriceDownColor"] <- "Red"
        s.["PointWidth"] <- "0.8"
        s

    let volSeries = 
        let s = createSeries "Volume" SeriesChartType.Column
        s.["PointWidth"] <- "0.5"
        s
        
    let priceChartArea = 
        let ca = createChartArea "Price" 3 10
        ca.AxisY.Maximum <- max
        ca.AxisY.Minimum <- min
        ca

    let volChartArea = createChartArea "Volume" 3 52

    let chart = 
        let c = new Chart()
        c.ChartAreas.Add(priceChartArea)
        c.ChartAreas.Add(volChartArea)
        c.Series.Add(priceSeries)
        c.Series.Add(volSeries)
        c.Size <- new System.Drawing.Size(446, 296)
        c.Location <- new System.Drawing.Point(16, 48)
        c.Dock <- System.Windows.Forms.DockStyle.Fill 
        c

    let addSwingpointAnnotation (chart:Chart) spIndices text =
        for i in spIndices do 
            let dp = chart.Series.["Price"].Points.[i]
            dp.Label <- text
            
    let addTrendAnnotation (chart:Chart) trendIndices =
        for i,(t,s) in trendIndices do
            let text = match t with
                       | Sideways -> "sideways"
                       | Bullish  -> "bullish"
                       | Bearish  -> "bearish"
            let color = match s with
                        | Ambivalent -> "black"
                        | Confirmed  -> "green"
                        | Suspect    -> "red"
            let dp = chart.Series.["Price"].Points.[i]
            dp.MarkerImage <- text + "_" + color + ".png" 

    let addPrice (chart:Chart) =
        let dhloc = DHLOC data
        let d,h,l,o,c = Seq.head dhloc
        chart.Series.["Price"].Points.AddXY(d, h) |> ignore
        chart.Series.["Price"].Points.[0].YValues.[1] <- l
        chart.Series.["Price"].Points.[0].YValues.[2] <- o 
        chart.Series.["Price"].Points.[0].YValues.[3] <- c

        let mutable i = 1
        for d,h,l,o,c in List.tail (Seq.toList dhloc) do
            chart.Series.["Price"].Points.AddXY(d, h) |> ignore
            chart.Series.["Price"].Points.[i].YValues.[1] <- l
            chart.Series.["Price"].Points.[i].YValues.[2] <- o 
            chart.Series.["Price"].Points.[i].YValues.[3] <- c
            let x = chart.Series.["Price"].Points.[i-1].XValue
            //chart.Series.["Price"].Points.[i].XValue <- x + 1.0
            chart.Series.["Price"].Points.[i].ToolTip <- " High=#VALY1, Low=#VALY2, Open=#VALY3, Close=#VALY4"
            i <- i + 1

    let addVolume (chart:Chart) = 
        let V = Series.observations data?Volume
        let d,vol = Seq.head V
        chart.Series.["Volume"].Points.AddXY(d, vol) |> ignore
        let mutable i = 1
        for d,vol in List.tail (Seq.toList V) do
            chart.Series.["Volume"].Points.AddXY(d, vol) |> ignore
            let x = chart.Series.["Volume"].Points.[i-1].XValue
            //chart.Series.["Volume"].Points.[i].XValue <- x + 1.0
            chart.Series.["Volume"].Points.[i].ToolTip <- " Volume=#VALY1"
            i <- i + 1

    do
        addPrice(chart)
        addVolume(chart)
        addSwingpointAnnotation chart sphIndices "SPH"
        addSwingpointAnnotation chart splIndices "SPL"
        addTrendAnnotation chart trendIndices

        self.Size <- new System.Drawing.Size(728, 360)
        self.Controls.Add(chart)
        