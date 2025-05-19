open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let summary = 
                BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmarks.DistanceSquaredBenchmark>()
    0

