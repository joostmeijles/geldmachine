open System
open GeldMachine.Data

let gspc = getStockData "^GSPC"
let msft = getStockData "MSFT"

let printStockData (data:StockData) =
    printfn "%A" data.Headers
    Seq.take 10 data.Rows |> Seq.iter (fun d -> printfn "%A " d)

[<EntryPoint;STAThread>]
let main argv = 
    printStockData msft
    printStockData gspc

    Console.ReadLine() |> ignore
    0 //exit code
