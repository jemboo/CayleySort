namespace CayleySort.Core.Tests

open Xunit
open System
open CayleySort.Core
open FSharp.UMX

type RandoTests() =

    // Helper function for chi-squared test for uniformity
    let chiSquaredTest (values: int[]) buckets =
        let n = values.Length
        let expected = float n / float buckets
        let counts = Array.zeroCreate buckets
        for v in values do
            counts.[v] <- counts.[v] + 1
        let chi2 =
            counts
            |> Array.map (fun count -> (float count - expected) ** 2.0 / expected)
            |> Array.sum
        // Critical value for 10% significance level, degrees of freedom = buckets - 1
        let criticalValues = Map [
            9, 18.307 // 10 buckets - 1, 10% significance
            99, 123.225 // 100 buckets - 1
        ]
        chi2, Map.tryFind (buckets - 1) criticalValues

    // Test randomLcg with explicit UMX tagging
    [<Theory>]
    [<InlineData(42UL)>]
    [<InlineData(123456789UL)>]
    let ``randomLcg produces values in expected ranges`` (seed: uint64) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng = randomLcg taggedSeed :> IRando
        let uintVal = rng.NextUInt()
        let intVal = rng.NextPositiveInt()
        let floatVal = rng.NextFloat()
        let indexVal = rng.NextIndex 10
        let ulongVal = rng.NextULong()
        let guidVal = rng.NextGuid()
    
        Assert.InRange(uintVal, 0u, UInt32.MaxValue)
        Assert.InRange(intVal, 0, Int32.MaxValue)
        Assert.InRange(floatVal, 0.0, 1.0 - Double.Epsilon)
        Assert.InRange(indexVal, 0, 9)
        Assert.InRange(ulongVal, 0UL, UInt64.MaxValue)
        Assert.NotEqual(Guid.Empty, guidVal)
        Assert.Equal(40, rng.ByteCount)

    [<Theory>]
    [<InlineData(42)>]
    [<InlineData(123456789)>]
    let ``randomNet produces values in expected ranges`` (seed: int32) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng = randomNet taggedSeed :> IRando
        let uintVal = rng.NextUInt()
        let intVal = rng.NextPositiveInt()
        let floatVal = rng.NextFloat()
        let indexVal = rng.NextIndex 10
        let ulongVal = rng.NextULong()
        let guidVal = rng.NextGuid()
    
        Assert.InRange(uintVal, 0u, UInt32.MaxValue)
        Assert.InRange(intVal, 0, Int32.MaxValue)
        Assert.InRange(floatVal, 0.0, 1.0 - Double.Epsilon)
        Assert.InRange(indexVal, 0, 9)
        Assert.InRange(ulongVal, 0UL, UInt64.MaxValue)
        Assert.NotEqual(Guid.Empty, guidVal)
        Assert.Equal(44, rng.ByteCount)

    [<Theory>]
    [<InlineData(42UL)>]
    [<InlineData(123456789UL)>]
    let ``randomLcg is deterministic with same seed`` (seed: uint64) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng1 = randomLcg taggedSeed :> IRando
        let rng2 = randomLcg taggedSeed :> IRando
        let values1: list<uint32> = [ for _ in 1..100 -> rng1.NextUInt() ]
        let values2: list<uint32> = [ for _ in 1..100 -> rng2.NextUInt() ]
        Assert.Equal<list<uint32>>(values1, values2)
        Assert.Equal(400, rng1.ByteCount)
        Assert.Equal(400, rng2.ByteCount)

    [<Theory>]
    [<InlineData(42)>]
    [<InlineData(123456789)>]
    let ``randomNet is deterministic with same seed`` (seed: int32) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng1 = randomNet taggedSeed :> IRando
        let rng2 = randomNet taggedSeed :> IRando
        let values1: list<uint32> = [ for _ in 1..100 -> rng1.NextUInt() ]
        let values2: list<uint32> = [ for _ in 1..100 -> rng2.NextUInt() ]
        Assert.Equal<list<uint32>>(values1, values2)
        Assert.Equal(400, rng1.ByteCount)
        Assert.Equal(400, rng2.ByteCount)

    [<Theory>]
    [<InlineData(42UL)>]
    [<InlineData(123456789UL)>]
    let ``randomLcg produces uniform distribution for NextIndex`` (seed: uint64) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng = randomLcg taggedSeed :> IRando
        let n = 100000
        let modulus = 10
        let values = [| for _ in 1..n -> rng.NextIndex modulus |]
        let chi2, criticalValue = chiSquaredTest values modulus
        match criticalValue with
        | Some cv -> Assert.True(chi2 < cv, $"Chi-squared value {chi2} exceeds critical value {cv} for uniform distribution")
        | None -> Assert.Fail("No critical value defined for chi-squared test")
        Assert.Equal(n * 4, rng.ByteCount)

    [<Theory>]
    [<InlineData(42)>]
    [<InlineData(123456789)>]
    let ``randomNet produces uniform distribution for NextIndex`` (seed: int32) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng = randomNet taggedSeed :> IRando
        let n = 100000
        let modulus = 10
        let values = [| for _ in 1..n -> rng.NextIndex modulus |]
        let chi2, criticalValue = chiSquaredTest values modulus
        match criticalValue with
        | Some cv -> Assert.True(chi2 < cv, $"Chi-squared value {chi2} exceeds critical value {cv} for uniform distribution")
        | None -> Assert.Fail("No critical value defined for chi-squared test")
        Assert.Equal(n * 4, rng.ByteCount)

    [<Theory>]
    [<InlineData(42UL)>]
    [<InlineData(123456789UL)>]
    let ``randomLcg generates unique GUIDs`` (seed: uint64) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng = randomLcg taggedSeed :> IRando
        let guids = [ for _ in 1..1000 -> rng.NextGuid() ]
        let distinctGuids = guids |> List.distinct
        Assert.Equal(guids.Length, distinctGuids.Length)
        Assert.Equal(1000 * 16, rng.ByteCount)

    [<Theory>]
    [<InlineData(42)>]
    [<InlineData(123456789)>]
    let ``randomNet generates unique GUIDs`` (seed: int32) =
        let taggedSeed = UMX.tag<randomSeed> seed
        let rng = randomNet taggedSeed :> IRando
        let guids = [ for _ in 1..1000 -> rng.NextGuid() ]
        let distinctGuids = guids |> List.distinct
        Assert.Equal(guids.Length, distinctGuids.Length)
        Assert.Equal(1000 * 16, rng.ByteCount)

    [<Fact>]
    let ``randomLcg and randomNet produce different sequences`` () =
        let seed = 42UL
        let lcg = randomLcg (UMX.tag<randomSeed> seed) :> IRando
        let net = randomNet (UMX.tag<randomSeed> 42) :> IRando
        let lcgValues: list<uint32> = [ for _ in 1..100 -> lcg.NextUInt() ]
        let netValues: list<uint32> = [ for _ in 1..100 -> net.NextUInt() ]
        Assert.NotEqual<list<uint32>>(lcgValues, netValues)