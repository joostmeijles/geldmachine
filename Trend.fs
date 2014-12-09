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

type SwingpointTest =
    | SWPTestLow
    | SWPTestHigh

let private transition = function 
    | Bearish,  SWPTestLow  -> Bearish
    | Bearish,  SWPTestHigh -> Sideways
    | Sideways, SWPTestLow  -> Bearish
    | Sideways, SWPTestHigh -> Bullish
    | Bullish,  SWPTestLow  -> Sideways
    | Bullish,  SWPTestHigh -> Bullish 

let showTrend t =
    match t with
    | Bearish  -> "Bearish"
    | Bullish  -> "Bullish"
    | Sideways -> "Sideways"

let showStrength s =
    match s with
    | Ambivalent -> "Ambivalent"
    | Suspect    -> "Suspect"
    | Confirmed  -> "Confirmed"

let showStrengthTrend s t = (showStrength s) + " " + (showTrend t)

let strengthTest (p:OHLC) (swp:OHLC) = 
    if p.Volume > swp.Volume then
        Confirmed
    else
        Suspect

let smallerThan a b = a < b
let greaterThan a b = a > b

let rec swingpointTest cmp spOp test p (sps:list<OHLC>) (trend,strength) =
    match sps with
    | h :: t ->
        if cmp p.Close (spOp h) then
            let newTrend    = transition (trend, test)
            let newStrength = strengthTest p h
            swingpointTest cmp spOp test p t (newTrend,newStrength)
        else
            (trend,strength), sps
    | _ -> (trend,strength), sps

let swingpointLowTest  = swingpointTest smallerThan low  SWPTestLow   
let swingpointHighTest = swingpointTest greaterThan high SWPTestHigh 

let updateInUseSWPs (p:OHLC) (all:list<OHLC>) inUse = 
    match all with
    | (h::t) ->
        if System.DateTime.Equals(h.Date, p.Date) then
            t, (h::inUse)
        else
            all, inUse
    | _ -> all, inUse

//TODO: Add ambivalent sideways transition: no swingpoint tests resulting in (2 lower SPH & higher SPL) or (2 higher SPH & lower SPL) -> See aapl chart
let getTrends (data:list<OHLC>) (spls:list<OHLC>) (sphs:list<OHLC>) = 
    let mutable allSPLs = List.sortBy (fun e -> e.Date) spls
    let mutable allSPHs = List.sortBy (fun e -> e.Date) sphs
    let mutable useSPLs = []
    let mutable useSPHs = []
    let mutable trend   = Sideways, Ambivalent
    let mutable trendChanges = [(List.head data, trend)]
    
    for p in data do
        let (trend' , useSPLs') = swingpointLowTest  p useSPLs trend 
        let (trend'', useSPHs') = swingpointHighTest p useSPHs trend'

        if trend <> trend'' then
            trendChanges <- (p,trend'') :: trendChanges 

        let allSPLs', useSPLs'' = updateInUseSWPs p allSPLs useSPLs'
        let allSPHs', useSPHs'' = updateInUseSWPs p allSPHs useSPHs'

        trend <- trend''
        allSPLs <- allSPLs'
        allSPHs <- allSPHs'
        useSPLs <- useSPLs''
        useSPHs <- useSPHs''
    trendChanges
