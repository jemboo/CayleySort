module Benchmarks

open System
open BenchmarkDotNet.Attributes
open CayleySort.Core.SequenceProperties

type DistanceSquaredBenchmark() =
    let rnd = Random(42)
    let mutable a : float[] = [||]
    let mutable b : float[] = [||]
    let mutable longArray : float[] = [||]
    let mutable shortArray : float[] = [||]
    let mutable m : int = 0

    [<Params(100, 1000, 10000)>]
    member val ArrayLength = 0 with get, set

    [<Params(10, 100, 1000)>]
    member val SegmentCount = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        a <- Array.init this.ArrayLength (fun _ -> rnd.NextDouble())
        b <- Array.init this.ArrayLength (fun _ -> rnd.NextDouble())
        m <- this.SegmentCount
        shortArray <- Array.init this.ArrayLength (fun _ -> rnd.NextDouble())
        longArray <- Array.init (this.ArrayLength * m) (fun _ -> rnd.NextDouble())

    //[<Benchmark(Baseline = true)>]
    //member this.DistanceSquaredSafe() =
    //    distanceSquared a b |> ignore

    //[<Benchmark>]
    //member this.DistanceSquaredUnsafe() =
    //    distanceSquaredUnsafe a b |> ignore

    [<Benchmark>]
    member this.DistanceSquaredArraySegmentSafe() =
        distanceSquaredArraySegmentSafe longArray shortArray  |> ignore

    [<Benchmark>]
    member this.DistanceSquaredArraySegmentUnsafe() =
        distanceSquaredArraySegmentUnsafe longArray shortArray |> ignore
