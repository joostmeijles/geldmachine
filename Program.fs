open System
open FSharp.Data

type StockData = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=SPX">

[<EntryPoint;STAThread>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
