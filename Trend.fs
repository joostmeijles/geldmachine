module GeldMachine.Indicator.Trend

open GeldMachine.Data

type Trend =
    | Bearish
    | Sideways
    | Bullish

type private SwingpointTest =
    | SWPTestLow
    | SWPTestHigh

let private transition = function 
    | Bearish,  SWPTestLow  -> Bearish
    | Bearish,  SWPTestHigh -> Sideways
    | Sideways, SWPTestLow  -> Bearish
    | Sideways, SWPTestHigh -> Bullish
    | Bullish,  SWPTestLow  -> Sideways
    | Bullish,  SWPTestHigh -> Bullish

let previousSwingpoints p (swps:list<OHLC>) = 
    List.filter (fun swp -> swp.Date < p.Date) swps

let swingpointLowTest p (spls:list<OHLC>) t =
    match spls with
    | h :: _ ->
        let first = Seq.last spls
        if (p.Close < first.Low) then
            transition (t, SWPTestLow)
            //TODO: swingpointLowTest p (List.tail spls) t'
        else
            t
    | _ -> t

let swingpointHighTest p (sphs:list<OHLC>) t =
    match sphs with
    | h :: _ ->
        let first = Seq.last sphs
        if (p.Close > first.High) then
            transition (t, SWPTestHigh)
            //TODO: swingpointLowTest p (List.tail spls) t'
        else
            t
    | _ -> t

let getTrends (data:list<OHLC>) (spls:list<OHLC>) (sphs:list<OHLC>) = 
    let mutable t = []
    for p in data do
        let t0 = if t.IsEmpty then Sideways else Seq.last t
        let spls' = previousSwingpoints p spls
        let sphs' = previousSwingpoints p sphs
        let t1 = swingpointLowTest  p spls' t0
        let t2 = swingpointHighTest p sphs' t1
        t <- List.append t [t2]
    t

