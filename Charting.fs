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

let private drawCrossHair (g:Graphics) (center:Point) width height (offset:Point) = 
        let pen = new System.Drawing.Pen(System.Drawing.Color.LightGray, float32 2.0)
        
        let fromVertical = new Point(center.X, offset.Y)
        let toVertical   = new Point(center.X, offset.Y + height)
        g.DrawLine(pen, fromVertical, toVertical)

        let fromHorizontal = new Point(offset.X,         center.Y)
        let toHorizontal   = new Point(offset.X + width, center.Y)
        g.DrawLine(pen, fromHorizontal, toHorizontal)


type ChartControl (symbolName:string, data:Frame<DateTime,string>) as self = 
    inherit UserControl(Dock = DockStyle.Fill)

    let mutable mousePosition = Point(0,0)

    let width  = 500
    let height = 300

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

    let volChartArea = 
        let ca = createChartArea "Volume" 3 52
        ca.AxisY.LabelStyle.Format <- "#,,.M;(#,,.M);0"
        ca

    let chart = 
        let c = new Chart()
        c.ChartAreas.Add(priceChartArea)
        c.ChartAreas.Add(volChartArea)
        c.Series.Add(priceSeries)
        c.Series.Add(volSeries)
        c.Dock <- DockStyle.Fill 
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
            //chart.Series.["Price"].Points.[i].ToolTip <- "#VALX{dd/MM/yyyy} High=#VALY1, Low=#VALY2, Open=#VALY3, Close=#VALY4"
            i <- i + 1

    let addVolume (chart:Chart) = 
        let V = Series.observations data?Volume
        let d,vol = Seq.head V
        chart.Series.["Volume"].Points.AddXY(d, vol) |> ignore
        let mutable i = 1
        for d,vol in List.tail (Seq.toList V) do
            chart.Series.["Volume"].Points.AddXY(d, vol) |> ignore
            let x = chart.Series.["Volume"].Points.[i-1].XValue
            //chart.Series.["Volume"].Points.[i].ToolTip <- " Volume=#VALY1"
            i <- i + 1

    let label = 
        let l = new Label()
        l.Name <- "Label"
        l.Text <- symbolName
        l.Size <- Size(width,25)
        l.Dock <- DockStyle.Top
        l.TextAlign <- ContentAlignment.MiddleLeft
        l.BackColor <- Color.White
        l

    let panel =
        let p = new Panel() 
        p.Name <- "Panel"
        p.Dock <- DockStyle.Fill
        p.BackColor <- Color.White
        p

    let findNearestPoint x (chartArea:ChartArea) (series:Series) = 
        let v = chartArea.AxisX.PixelPositionToValue(x)
        let dps = Seq.map (fun (d:DataPoint) ->  d,Math.Abs(d.XValue - v)) series.Points
        let (nearest,_) = Seq.minBy (fun (d,dist) -> dist) dps
        nearest

    let findNearestIndex x (chartArea:ChartArea) (series:Series) =
        let v = chartArea.AxisX.PixelPositionToValue(x)
        let indices = {0..(Seq.length series.Points)}
        let dists = Seq.map (fun (i,(d:DataPoint)) ->  i,Math.Abs(d.XValue - v)) (Seq.zip indices series.Points)
        let (nearest,_) = Seq.minBy (fun (i,dist) -> dist) dists
        nearest

    let mouseMove (c:Chart) (l:Label) (e:MouseEventArgs) = 
        mousePosition <- e.Location
        let nearestPrice = findNearestPoint (float e.X) chart.ChartAreas.["Price"] chart.Series.["Price"]
        let nearestVol   = findNearestPoint (float e.X) chart.ChartAreas.["Volume"] chart.Series.["Volume"]
        let nearestDate  = nearestPrice.XValue |> DateTime.FromOADate
        let date = nearestDate.ToShortDateString()
        let sp = data.Rows.[nearestDate]
        
        let isSPH = data.GetColumn<bool>("SPH").[nearestDate]
        let isSPL = data.GetColumn<bool>("SPL").[nearestDate]
        let spStr =
            match (isSPH, isSPL) with
            | true, true  -> "[SPH, SPL]"
            | true, false -> "[SPH]"
            | false, true -> "[SPL]"
            | _, _ -> "" 
        
        let trendChange = data.GetColumn<(Trend*Strength) Option>("TrendChange").TryGet(nearestDate)
        let trendStr =
            if trendChange.HasValue && trendChange.Value.IsSome then
                let t,s = trendChange.Value.Value
                showStrengthTrend s t
            else ""

        let str = sprintf "%s %s High=%.2f, Low=%.2f, Open=%.2f, Close=%.2f, Vol=%.0f %s %s" 
                          symbolName date sp?High sp?Low sp?Open sp?High sp?Volume spStr trendStr
        l.Text <- str
        c.Invalidate()

    let onPaint (c:Chart) (e:PaintEventArgs) =
        drawCrossHair e.Graphics mousePosition c.Width c.Height c.Location

    let mouseEnter (e:EventArgs) =
        System.Windows.Forms.Cursor.Hide()

    let mouseLeave (e:EventArgs) =
        System.Windows.Forms.Cursor.Show()

    let onResize (l:Label) (e:EventArgs) =
        l.Size <- Size(self.Size.Width, l.Size.Height)

    do
        addPrice(chart)
        addVolume(chart)
        addSwingpointAnnotation chart sphIndices "SPH"
        addSwingpointAnnotation chart splIndices "SPL"
        addTrendAnnotation chart trendIndices

        self.Size <- Size(width, height)
        
        panel.Controls.Add(chart)
        panel.Controls.Add(label)
        self.Controls.Add(panel)

        chart.MouseMove.Add(mouseMove chart label)
        chart.MouseEnter.Add(mouseEnter)
        chart.MouseLeave.Add(mouseLeave)
        chart.Paint.Add(onPaint chart)

        self.Resize.Add(onResize label)
