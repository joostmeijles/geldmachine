module GeldMachine.Indicator.Swingpoint

open System
open GeldMachine.Data

type Swingpoint =
    | SPH
    | SPL

let private getSPs (data:OHLC list) (property:OHLC -> decimal) (compare:decimal -> decimal -> bool) = 
    match data with
    | h :: t ->
        let mutable potentialSwingPoint = h
        let mutable successive = 0
        let mutable sps = []

        for row in data do
            if(compare (property row) (property potentialSwingPoint)) then
                successive <- successive + 1
                if(successive >= 6) then
                    sps <- List.append [potentialSwingPoint] sps
                    potentialSwingPoint <- row
                    successive <- 0
            else
                potentialSwingPoint <- row
                successive <- 0 
        sps
    | _ -> []

let getSPLs (data:OHLC list) = getSPs data (fun row -> row.Low)  (fun current best -> current > best)
let getSPHs (data:OHLC list) = getSPs data (fun row -> row.High) (fun current best -> current < best)