module Load

open HttpClient
open FSharp.Data
open System
open System.IO

let STOCK_DATA_PATH = "data/"
let YAHOO_URL       = "http://ichart.finance.yahoo.com/table.csv"

type StockData = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=SPX">

type StockRow  = (DateTime * decimal * decimal * decimal * decimal * int64 * decimal)
type StockRows = StockRow seq

let toStockRows (data : StockData) = 
    Seq.map (fun (r : StockData.Row) -> r.Date, r.Open, r.High, r.Low, r.Close, r.Volume, r.``Adj Close``) data.Rows

let urlForAll symbol = YAHOO_URL + "?s=" + symbol

let urlForRange symbol (startDate:DateTime) (endDate:DateTime) = 
    sprintf "%s?s=%s&a=%i&b=%i&c=%i&d=%i&e=%i&f=%i" 
        YAHOO_URL
        symbol
        (startDate.Month - 1) (startDate.Day + 1) startDate.Year 
        (endDate.Month   - 1) endDate.Day         endDate.Year 

let filenameFor symbol = symbol + ".csv"

let loadFromFile symbol = 
    let filename = filenameFor symbol
    let exists   = File.Exists(filename)
    if exists
        then let data = StockData.Load(filename)
             Some(data)
        else None

let saveToFile symbol (headers:string [] option) (data:StockRows) =
    let headers'  = if headers.IsSome then headers.Value else [|""|]
    let headers'' = String.concat "," headers' 
    let data'     = Seq.map (fun (d,o,h,l,c,v,a) -> sprintf "%A,%M,%M,%M,%M,%d,%M" d o h l c v a) data
    let data''    = String.concat "\r\n" data'
    let filename  = (filenameFor symbol)
    try
        let f = File.CreateText(filename)
        f.WriteLine(headers'')
        f.Write(data'')
        f.Close()
        printfn "Saved to file: %s" filename
    with
        err -> printfn "Failed save file %s: %A" filename err 

let stockData symbol = 
    let data = loadFromFile symbol
    match data with
    | Some(data) ->
        let rows = toStockRows data 
        let (latest_date,_,_,_,_,_,_) = Seq.head rows
        let url = urlForRange symbol latest_date DateTime.Now
        try
            let test = createRequest Get url |> getResponse //FSharp.Data doesn't handle exceptions well?!
            printfn "test = %A" test.StatusCode
            if test.StatusCode = 200 then
                let missing_data = StockData.Load(url)
                let all = Seq.append missing_data.Rows data.Rows
                printfn "latest_date = %A, url = %A, missing = %A, all = %A" latest_date url missing_data.Rows (Seq.take 10 all)
        with
            _ -> printfn "Failed to download data" 
        data
    | None       -> 
        let data = StockData.Load(urlForAll symbol)
        let rows = toStockRows data
        let (latest_date,_,_,_,_,_,_) = Seq.head rows
        printfn "latest_date = %A" latest_date
        saveToFile symbol data.Headers rows
        //data.Save(filenameFor symbol)
        printfn "%A" data.Headers
        data
