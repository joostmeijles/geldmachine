module GeldMachine.Data

open HttpClient
open FSharp.Data
open System
open System.IO

[<StructuralEquality>]
[<StructuralComparison>]
type OHLC = {
    Date     : DateTime
    Open     : decimal
    High     : decimal
    Low      : decimal
    Close    : decimal
    Volume   : int64
    Adjusted : decimal }
    with
        static member (==) (a:OHLC, b:OHLC)= a.Equals b

let private STOCK_DATA_PATH = "data/"
let private YAHOO_URL       = "http://ichart.finance.yahoo.com/table.csv"

type StockData = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=^GSPC">

type private StockRow  = (DateTime * decimal * decimal * decimal * decimal * int64 * decimal)
type private StockRows = StockRow seq

let private toStockRows (data : StockData) = 
    Seq.map (fun (r : StockData.Row) -> r.Date, r.Open, r.High, r.Low, r.Close, r.Volume, r.``Adj Close``) data.Rows

let private translateRow (row:StockData.Row) = {Date = row.Date; Open = row.Open; High = row.High; Low = row.Low; Close = row.Close; Volume = row.Volume; Adjusted = row.``Adj Close``}
let private translate n (rows:StockData.Row seq) = 
    let ohlc = Seq.map translateRow rows
    List.rev (Seq.toList (Seq.take n ohlc))

let private urlForAll symbol = YAHOO_URL + "?s=" + symbol

let private urlForRange symbol (startDate:DateTime) (endDate:DateTime) = 
    sprintf "%s?s=%s&a=%i&b=%i&c=%i&d=%i&e=%i&f=%i" 
        YAHOO_URL
        symbol
        (startDate.Month - 1) (startDate.Day + 1) startDate.Year 
        (endDate.Month   - 1) endDate.Day         endDate.Year 

let private filenameFor symbol = symbol + ".csv"

let private loadFromFile symbol = 
    let filename = filenameFor symbol
    let exists   = File.Exists(filename)
    if exists
        then let data = StockData.Load(filename)
             Some(data)
        else None

let private loadFromUrl url =
    try
        let test = createRequest Get url |> getResponse //FSharp.Data doesn't handle exceptions well?!
        if test.StatusCode = 200 then
            let data = StockData.Load(url)
            Some(data)
        else
            None
    with
        err -> None

let private stockRowToString (d,o,h,l,c,v,a) = sprintf "%A,%M,%M,%M,%M,%d,%M" d o h l c v a

let private saveToFile symbol (headers:string [] option) (data:StockRows) =
    let headers'  = if headers.IsSome then headers.Value else [|""|]
    let headers'' = String.concat "," headers' 
    let data'     = Seq.map stockRowToString data
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

let private latestDate rows = 
    let (latest,_,_,_,_,_,_) = Seq.head rows
    latest

let private updateStockData symbol data =
    let rows   = toStockRows data
    let latest = latestDate rows
    let now    = DateTime.Now
    if latest.Date < now then
        let url    = urlForRange symbol latest now
        let data'  = loadFromUrl url
        match data' with
        | Some(data') -> 
            let rows' = Seq.append (toStockRows data') rows
            saveToFile symbol data.Headers rows'
            StockData.Load(filenameFor symbol)
        | None -> data
    else 
        data

let getStockData n symbol = 
    let data = loadFromFile symbol
    match data with
    | Some(data) -> 
        let data' = updateStockData symbol data
        translate n data'.Rows
    | None -> 
        let data = StockData.Load(urlForAll symbol)
        let rows = toStockRows data
        saveToFile symbol data.Headers rows
        translate n data.Rows

let getStockDataOffline n symbol =
    let data = loadFromFile symbol
    match data with
    | Some(data) -> translate n data.Rows
    | None       -> printfn "Unable to get offline data from disk (%A): try the online version..." (filenameFor symbol)
                    []
