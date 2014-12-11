module GeldMachine.Indicator.AnchorBar

open Deedle
open System

let largeThreshold = 1.8 //180%
let maxAnchorBars = 6

let takeAtMax n (series:Series<'a,'b>) =
    let max = if series.ValueCount > n then n else series.ValueCount
    let obs = Seq.map (fun i -> series.GetKeyAt(i),series.GetAt(i)) {1..max}
    Series.ofObservations obs

let anchorVolBars (data:Frame<DateTime,string>) =
    let vols = data?Volume
    let meanVol = Stats.mean vols
    let relativeVols = Series.map (fun k v -> v / meanVol) vols
    let largeVols = Series.filter (fun k v -> v > largeThreshold) relativeVols
    largeVols

let anchorWideSpreadBars (data:Frame<DateTime,string>) =
    let lows  = data?Low
    let highs = data?High
    let spreads = Series.zip lows highs |> Series.map (fun k (l,h) -> h.Value - l.Value)
    let meanSpread = Stats.mean spreads
    let relativeSpreads = Series.map (fun k v -> v / meanSpread) spreads
    let largeSpreads = Series.filter (fun k v -> v > largeThreshold) relativeSpreads
    largeSpreads

let anchorBars (data:Frame<DateTime,string>) = 
    let volBars = anchorVolBars data
    let wideSpreadBars = anchorWideSpreadBars data
    let allBars = Series.merge volBars wideSpreadBars |> Series.sort
    //let topBars = takeAtMax maxAnchorBars allBars
    printfn "%A" allBars