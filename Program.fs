open System
open Load

let gspc = Load.stockData "^GSPC"
let msft = Load.stockData "MSFT"

let printStockData (data:Load.StockData) =
    printfn "%A" data.Headers
    Seq.take 10 data.Rows |> Seq.iter (fun d -> printfn "%A " d)

[<EntryPoint;STAThread>]
let main argv = 
    printStockData msft
    printStockData gspc

    Console.ReadLine() |> ignore
    0 //exit code
