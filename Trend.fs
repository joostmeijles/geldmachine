module GeldMachine.Indicator.Trend

open GeldMachine.Data

type Trend =
    | Bearish
    | Sideways
    | Bullish

type Strength =
    | Ambivalent
    | Suspect
    | Confirmed

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

let strengthTest (p:OHLC) (swp:OHLC) = 
    if p.Volume > swp.Volume then
        Confirmed
    else
        Suspect

let rec swingpointLowTest p (spls:list<OHLC>) (trend,strength) =
    match spls with
    | h :: t ->
        if (p.Close < h.Low) then
            let newTrend    = transition (trend, SWPTestLow)
            let newStrength = strengthTest p h
            swingpointLowTest p t (newTrend,newStrength)
        else
            (trend,strength), spls
    | _ -> (trend,strength), spls

let rec swingpointHighTest p (sphs:list<OHLC>) (trend,strength) =
    match sphs with
    | h :: t ->
        if (p.Close > h.High) then
            let newTrend    = transition (trend, SWPTestHigh)
            let newStrength = strengthTest p h
            swingpointHighTest p t (newTrend,newStrength)
        else
            (trend,strength), sphs
    | _ -> (trend,strength), sphs

let updateInUseSWPs (p:OHLC) (all:list<OHLC>) inUse = 
    match all with
    | (h::t) ->
        if System.DateTime.Equals(h.Date, p.Date) then
            t, (h::inUse)
        else
            all, inUse
    | _ -> all, inUse


//TODO: re-generate and re-test?
let getTrends (data:list<OHLC>) (spls:list<OHLC>) (sphs:list<OHLC>) = 
    let mutable allSPLs = List.sortBy (fun e -> e.Date) spls
    let mutable allSPHs = List.sortBy (fun e -> e.Date) sphs
    let mutable useSPLs = []
    let mutable useSPHs = []
    let mutable trend   = Sideways, Ambivalent
    let mutable trendChanges = [(List.head data, trend)]
    
    for p in data do
        let allSPLs', useSPLs' = updateInUseSWPs p allSPLs useSPLs
        let allSPHs', useSPHs' = updateInUseSWPs p allSPHs useSPHs
        
        let (trend' , useSPLs'') = swingpointLowTest  p useSPLs' trend 
        let (trend'', useSPHs'') = swingpointHighTest p useSPHs' trend'

        if trend <> trend'' then
            trendChanges <- (p,trend'') :: trendChanges 

        trend <- trend''
        allSPLs <- allSPLs'
        allSPHs <- allSPHs'
        useSPLs <- useSPLs''
        useSPHs <- useSPHs''
    trendChanges
