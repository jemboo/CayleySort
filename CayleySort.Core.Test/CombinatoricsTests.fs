namespace CayleySort.Core.Tests

open Xunit
open FsUnit.Xunit
open CayleySort.Core.SequenceProperties
open System
open CayleySort.Core
open FSharp.UMX
open System.Collections
open CayleySort.Core.Combinatorics

type CombinatoricsTests() = 

    // Helper to count elements in a sequence for comparison
    let countElements (seq: seq<'a>) : Map<'a, int> when 'a : equality =
        seq |> Seq.fold (fun map x -> Map.change x (function Some n -> Some (n + 1) | None -> Some 1) map) Map.empty

    [<Fact>]
    let ``fisherYatesShuffle returns Error for null array`` () =
        let getRandomIndex (_: int) = 0
        let result = fisherYatesShuffle getRandomIndex null
        Assert.Equal(Error FisherYatesError.NullArray, result)

    [<Fact>]
    let ``fisherYatesShuffle returns Ok empty sequence for empty array`` () =
        let getRandomIndex (_: int) = 0
        let input: int[] = [||]
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok seq -> Assert.Empty(seq)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")

    [<Fact>]
    let ``fisherYatesShuffle returns Ok with single element for single-element array`` () =
        let getRandomIndex (_: int) = 0
        let input = [|42|]
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok seq ->
            let actual = Array.toList seq
            Assert.Equal<list<int>>([42], actual)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")

    [<Theory>]
    [<InlineData(0)>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    let ``fisherYatesShuffle preserves length and elements`` (seed: int) =
        let rng = Random(seed)
        let getRandomIndex (bound: int) = rng.Next(bound + 1)
        let input = [|1; 2; 3; 4; 5|]
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok seq ->
            let actual = Array.toList seq
            Assert.Equal(5, actual.Length)
            let inputCounts = countElements input
            let actualCounts = countElements actual
            Assert.Equal<Map<int, int>>(inputCounts, actualCounts)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")

    [<Fact>]
    let ``fisherYatesShuffle does not modify input array`` () =
        let getRandomIndex (_: int) = 0
        let input = [|1; 2; 3; 4|]
        let original = Array.copy input
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok _ ->
            Assert.Equal<list<int>>(original |> Array.toList, input |> Array.toList)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")

    [<Fact>]
    let ``fisherYatesShuffle with fixed index zero produces reverse order`` () =
        let getRandomIndex (_: int) = 0
        let input = [|1; 2; 3; 4|]
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok seq ->
            let actual = Seq.toList seq
            Assert.Equal<list<int>>([2; 3; 4; 1], actual)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")

    [<Fact>]
    let ``fisherYatesShuffle with specific indices produces expected permutation`` () =
        let indices = [|2; 1; 0|] // For i=3, return 2; i=2, return 1; i=1, return 0
        let mutable index = 0
        let getRandomIndex (_: int) =
            let j = indices.[index]
            index <- index + 1
            j
        let input = [|1; 2; 3; 4|]
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok seq ->
            let actual = Array.toList seq
            // Expected permutation:
            // i=3: swap 4 with 3 -> [1, 2, 4, 3]
            // i=2: swap 4 with 2 -> [1, 4, 2, 3]
            // i=1: swap 4 with 1 -> [4, 1, 2, 3]
            Assert.Equal<list<int>>([4; 1; 2; 3], actual)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")

    [<Fact>]
    let ``fisherYatesShuffle works with strings`` () =
        let getRandomIndex (_: int) = 0
        let input = [|"a"; "b"; "c"|]
        let result = fisherYatesShuffle getRandomIndex input
        match result with
        | Ok seq ->
            let actual = Array.toList seq
            Assert.Equal<list<string>>(["b"; "c"; "a"], actual)
        | Error e -> Assert.Fail($"Expected Ok, got Error {e}")