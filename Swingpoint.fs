module GeldMachine.Indicator.Swingpoint

open GeldMachine.Data

type Swingpoint =
    | SPH
    | SPL

let private getSPs (data:StockData.Row seq) (property:StockData.Row->decimal) (compare:decimal->decimal->bool) = 
    let mutable potentialSwingPoint = decimal 0
    let mutable successive = 0
    let mutable sps = []

    for row in data do
        if(compare (property row) potentialSwingPoint) then
            successive <- successive + 1
            if(successive >= 6) then
                sps <- List.append [potentialSwingPoint] sps
                potentialSwingPoint <- property row
                successive <- 0
        else
            potentialSwingPoint <- property row
            successive <- 0 

    sps

let getSPLs (data:StockData.Row seq) = getSPs data (fun row -> row.Low) (fun current best -> current > best)
let getSPHs (data:StockData.Row seq) = getSPs data (fun row -> row.High) (fun current best -> current < best)
 
(*
let rec getSPHs (rows:StockData.Row list) sphs =
    match rows with
    | h :: t ->
        if Seq.length t >= 5 then 
            let next5 = Seq.take 5 t
            let high5 = Seq.map (fun (r:StockData.Row) -> r.High) next5
            let max5  = Seq.max high5
            if h.High > max5
                then getSPHs (List.ofSeq (Seq.skip 6 rows)) (Seq.append [(SPH, h)] sphs) 
                else getSPHs (List.ofSeq t) sphs
        else sphs
    | _ -> sphs

let getSwingpoints (rows:StockData.Row seq) =
    getSPHs (List.ofSeq rows) []
*)