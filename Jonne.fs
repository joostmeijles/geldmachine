open GeldMachine.Data

module Jonne = 

    let getSPs (data:StockData.Row seq) (property:StockData.Row->decimal) (compare:decimal->decimal->bool) = 
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
 