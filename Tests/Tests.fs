module Tests

open System

open FsCheck
open FsCheck.Xunit
open GeldMachine.Data
open GeldMachine.Indicator.Swingpoint

[<Property(StartSize=100, EndSize=1000)>]
let ``SPL all higher`` (a:list<OHLC>) = 
    let mutable res = []
    let spls = getSPLs a
    for s in spls do
        let i    = List.findIndex (fun j -> j = s) a
        let lst  = Seq.take 5 (Seq.skip (i+1) a)
        let lows = Seq.map (fun r -> r.Low) lst
        res <- List.append [((Seq.min lows) > s.Low)] res
    Seq.forall (fun r -> r = true) res

[<Property(StartSize=100, EndSize=1000)>]
let ``SPH all lower`` (a:list<OHLC>) = 
    let mutable res = []
    let sphs = getSPHs a
    for s in sphs do
        let i     = List.findIndex (fun j -> j = s) a
        let lst   = Seq.take 5 (Seq.skip (i+1) a)
        let highs = Seq.map (fun r -> r.High) lst
        res <- List.append [((Seq.max highs) < s.High)] res
    Seq.forall (fun r -> r = true) res