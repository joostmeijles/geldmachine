open System
open Load

let msft = Load.stockData "MSFT"

[<EntryPoint;STAThread>]
let main argv = 
    printfn "%A" msft.Headers
    Seq.take 10 (msft.Rows) |> Seq.iter (fun d -> printfn "%A " d)
    Console.ReadLine() |> ignore
    0 //exit code
