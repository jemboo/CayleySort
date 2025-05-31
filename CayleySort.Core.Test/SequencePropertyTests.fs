namespace CayleySort.Core.Tests

open Xunit
open FsUnit.Xunit
open CayleySort.Core.SequenceProperties
open System

type SequencePropertyTests() =

    // Helper to create arrays with predictable values for testing
    let createLongFloatArray n m = Array.init (n * m) (fun i -> float (i + 1))
    let createShortFloatArray n = Array.init n (fun i -> float (i + 1))
    let createLongIntArray n m = Array.init (n * m) (fun i -> (i + 1))
    let createShortIntArray n = Array.init n (fun i -> (i + 1))



    // distance squared tests
    [<Fact>]
    let ``Computes distance squared for int arrays`` () =
        let a = [|1; 2; 3|]
        let b = [|4; 5; 6|]
        let result = distanceSquared a b
        match result with
        | Ok value -> Assert.Equal(27, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Computes distance squared for float arrays`` () =
        let a = [|1.0; 2.0; 3.0|]
        let b = [|4.0; 5.0; 6.0|]
        let result = distanceSquared a b
        match result with
        | Ok value -> Assert.Equal(27.0, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Computes distance squared for decimal arrays`` () =
        let a = [|1m; 2m; 3m|]
        let b = [|4m; 5m; 6m|]
        let result = distanceSquared a b
        match result with
        | Ok value -> Assert.Equal(27m, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Handles empty arrays`` () =
        let a: int[] = [||]
        let b: int[] = [||]
        let result = distanceSquared a b
        match result with
        | Ok value -> Assert.Equal(0, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Returns UnequalLengths for mismatched lengths`` () =
        let a = [|1; 2; 3|]
        let b = [|4; 5|]
        let result = distanceSquared a b
        match result with
        | Error (UnequalLengths (3, 2)) -> ()
        | _ -> Assert.Fail("Expected Error (UnequalLengths (3, 2))")

    [<Fact>]
    let ``Returns NullArrayA for null a`` () =
        let a: int[] = null
        let b = [|1; 2|]
        let result = distanceSquared a b
        match result with
        | Error NullArrayA -> ()
        | _ -> Assert.Fail("Expected Error NullArrayA")

    [<Fact>]
    let ``Returns NullArrayB for null b`` () =
        let a = [|1; 2|]
        let b: int[] = null
        let result = distanceSquared a b
        match result with
        | Error NullArrayB -> ()
        | _ -> Assert.Fail("Expected Error NullArrayB")



    // isSorted Tests



    [<Fact>]
    let ``isSorted: Sorted int array returns Ok true`` () =
        let arr = [|1; 2; 3|]
        match isSorted arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Unsorted int array returns Ok false`` () =
        let arr = [|3; 1; 2|]
        match isSorted arr with
        | Ok result -> Assert.False(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Int array with duplicates returns Ok true`` () =
        let arr = [|1; 1; 2|]
        match isSorted arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Empty int array returns Ok true`` () =
        let arr: int[] = [||]
        match isSorted arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Single element int array returns Ok true`` () =
        let arr = [|1|]
        match isSorted arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Sorted float array returns Ok true`` () =
        let arr = [|1.0; 2.0; 3.0|]
        match isSorted arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Sorted string array returns Ok true`` () =
        let arr = [|"a"; "b"; "c"|]
        match isSorted arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSorted: Null array returns Error NullArray`` () =
        let arr: int[] = null
        match isSorted arr with
        | Error IsSortedError.NullArray -> ()
        | _ -> Assert.Fail("Expected Error NullArray")

    [<Fact>]
    let ``isSortedOffset: Sorted subarray returns Ok true`` () =
        let arr = [|0; 1; 2; 3|]
        match isSortedOffset arr 1 3 with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Unsorted subarray returns Ok false`` () =
        let arr = [|0; 3; 1; 2|]
        match isSortedOffset arr 0 3 with
        | Ok result -> Assert.False(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Subarray with duplicates returns Ok true`` () =
        let arr = [|0; 1; 1; 2|]
        match isSortedOffset arr 1 3 with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Empty subarray returns Ok true`` () =
        let arr = [|1; 2; 3|]
        match isSortedOffset arr 0 0 with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Single element subarray returns Ok true`` () =
        let arr = [|1; 2; 3|]
        match isSortedOffset arr 1 1 with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Sorted float subarray returns Ok true`` () =
        let arr = [|0.0; 1.0; 2.0; 3.0|]
        match isSortedOffset arr 1 3 with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Sorted string subarray returns Ok true`` () =
        let arr = [|"x"; "a"; "b"; "c"|]
        match isSortedOffset arr 1 3 with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isSortedOffset: Null array returns Error NullArray`` () =
        let arr: int[] = null
        match isSortedOffset arr 0 1 with
        | Error IsSortedError.NullArray -> ()
        | _ -> Assert.Fail("Expected Error NullArray")

    [<Fact>]
    let ``isSortedOffset: Negative offset returns Error InvalidOffset`` () =
        let arr = [|1; 2; 3|]
        match isSortedOffset arr -1 1 with
        | Error (InvalidOffset (-1, 3)) -> ()
        | _ -> Assert.Fail("Expected Error InvalidOffset")

    [<Fact>]
    let ``isSortedOffset: Negative length returns Error InvalidLength`` () =
        let arr = [|1; 2; 3|]
        match isSortedOffset arr 0 -1 with
        | Error (InvalidLength -1) -> ()
        | _ -> Assert.Fail("Expected Error InvalidLength")

    [<Fact>]
    let ``isSortedOffset: Offset plus length exceeds array returns Error OffsetPlusLengthExceedsArray`` () =
        let arr = [|1; 2; 3|]
        match isSortedOffset arr 2 2 with
        | Error (OffsetPlusLengthExceedsArray (2, 2, 3)) -> ()
        | _ -> Assert.Fail("Expected Error OffsetPlusLengthExceedsArray")



    //unsortednessSquared tests

    [<Fact>]
    let ``Computes unsortedness for int array`` () =
        let arr = [|3; 1; 2|]
        let result = unsortednessSquared arr
        match result with
        | Ok value -> Assert.Equal(6, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Computes unsortedness for float array`` () =
        let arr = [|3.0; 1.0; 2.0|]
        let result = unsortednessSquared arr
        match result with
        | Ok value -> Assert.Equal(6.0, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Computes unsortedness for decimal array`` () =
        let arr = [|3m; 1m; 2m|]
        let result = unsortednessSquared arr
        match result with
        | Ok value -> Assert.Equal(6m, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Handles already sorted int array`` () =
        let arr = [|1; 2; 3|]
        let result = unsortednessSquared arr
        match result with
        | Ok value -> Assert.Equal(0, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Handles empty int array`` () =
        let arr: int[] = [||]
        let result = unsortednessSquared arr
        match result with
        | Ok value -> Assert.Equal(0, value)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Returns NullArrayA for null array`` () =
        let arr: int[] = null
        let result = unsortednessSquared arr
        match result with
        | Error NullArrayA -> ()
        | _ -> Assert.Fail("Expected Error NullArrayA")



    // Permutation tests

    [<Fact>]
    let ``isPermutationSafe: Valid int permutation returns Ok true`` () =
        let arr = [|1; 0; 2|]
        match isPermutationSafe arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isPermutationSafe: Valid int64 permutation returns Ok true`` () =
        let arr = [|1L; 0L; 2L|]
        match isPermutationSafe arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isPermutationSafe: Empty int array returns Ok true`` () =
        let arr: int[] = [||]
        match isPermutationSafe arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isPermutationSafe: Single valid int element returns Ok true`` () =
        let arr = [|0|]
        match isPermutationSafe arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isPermutationSafe: Invalid permutation returns false`` () =
        let arr = [|0; 0; 1|]
        match isPermutationSafe arr with
        | Ok result -> Assert.False(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isPermutationSafe: Negative index returns Error IndexOutOfBounds`` () =
        let arr = [|-1; 0; 1|]
        match isPermutationSafe arr with
        | Error (IsPermutationError.IndexOutOfBounds -1) -> ()
        | _ -> Assert.Fail("Expected Error NegativeIndex")

    [<Fact>]
    let ``isPermutationSafe: Out-of-bounds index returns false`` () =
        let arr = [|0; 3; 1|]
        match isPermutationSafe arr with
        | Error (IsPermutationError.IndexOutOfBounds (3)) -> ()
        | _ -> Assert.Fail("Expected Error IndexOutOfBounds")

    [<Fact>]
    let ``isPermutationSafe: Single invalid int element returns Error IndexOutOfBounds`` () =
        let arr = [|1|]
        match isPermutationSafe arr with
        | Error (IsPermutationError.IndexOutOfBounds (1)) -> ()
        | _ -> Assert.Fail("Expected Error IndexOutOfBounds")

    [<Fact>]
    let ``isPermutationSafe: Null array returns Error NullArray`` () =
        let arr: int[] = null
        match isPermutationSafe arr with
        | Error IsPermutationError.NullArray -> ()
        | _ -> Assert.Fail("Expected Error NullArray")


    [<Fact>]
    let ``Valid input n=2 m=3 returns correct permutation checks`` () =
        let longArray = [|0.0; 1.0; 1.0; 0.0; 0.0; 1.0|] // [0,1], [1,0], [0,1]
        let result = isPermutationArraySegmentSafe longArray 2
        match result with
        | Ok perms ->
            Assert.Equal(3, perms.Length)
            Assert.True(perms.[0]) // [0,1] is permutation
            Assert.True(perms.[1]) // [1,0] is permutation
            Assert.True(perms.[2]) // [0,1] is permutation
        | Error _ -> Assert.False(true, "Expected Ok result")


    [<Fact>]
    let ``Array of valid permutations`` () =
        let validArrayOfPermuations = [|0;1;2;2;0;1;0;2;1;2;1;0;|]
        let segmentLength = 3
        let result = isPermutationArraySegmentSafe validArrayOfPermuations segmentLength
        match result with
        | Ok perms ->
                Assert.True(perms.[0], $"Segment 0 should be a permutation")
                Assert.True(perms.[1], $"Segment 1 should be a permutation")
                Assert.True(perms.[2], $"Segment 2 should be a permutation")
                Assert.True(perms.[3], $"Segment 3 should be a permutation")
        | Error _ -> Assert.False(true, "Expected Ok result")

    [<Fact>]
    let ``Array of invalid permutations`` () =
        let validArrayOfPermuations = [|0;0;2;2;2;1;0;0;1;2;1;2;|]
        let segmentLength = 3
        let result = isPermutationArraySegmentSafe validArrayOfPermuations segmentLength
        match result with
        | Ok perms ->
                Assert.False(perms.[0], $"Segment 0 should not be a permutation")
                Assert.False(perms.[1], $"Segment 1 should not be a permutation")
                Assert.False(perms.[2], $"Segment 2 should not be a permutation")
                Assert.False(perms.[3], $"Segment 3 should not be a permutation")
        | Error _ -> Assert.False(true, "Expected Ok result")


    [<Fact>]
    let ``Invalid short array length (n=0) returns error`` () =
        let longArray = [|0.0; 1.0|]
        let result = isPermutationArraySegmentSafe longArray 0
        match result with
        | Error (IsPermutationArraySegmentError.InvalidSegmentLength 0) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected InvalidShortArrayLength error")

    [<Fact>]
    let ``Non-whole number segments returns error`` () =
        let longArray = [|0.0; 1.0; 2.0|] // Length=3, n=2
        let result = isPermutationArraySegmentSafe longArray 2
        match result with
        | Error (IsPermutationArraySegmentError.NonWholeNumberSegments (3, 2)) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected NonWholeNumberSegments error")

    [<Fact>]
    let ``Zero-length long array returns error`` () =
        let arrayLength = 0
        let segmentLength = 2
        let longArray = Array.create<int64> arrayLength 0L
        let result = isPermutationArraySegmentSafe longArray segmentLength
        match result with
        | Error (IsPermutationArraySegmentError.InvalidLongArrayLength (arrayLength, segmentLength)) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected InvalidLongArrayLength error")

    [<Fact>]
    let ``Out-of-bounds index returns error`` () =
        let longArray = [|0.0; 1.0; 1.0; 3.0|] // [0,1], [1,3]
        let result = isPermutationArraySegmentSafe longArray 2
        match result with
        | Error (IsPermutationArraySegmentError.IndexOutOfBounds (1, 3)) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected IndexOutOfBounds error")


    //IsTwoCycle tests

    [<Fact>]
    let ``isTwoCycle: Valid int two-cycle with one swap returns Ok true`` () =
        let arr = [|1; 0; 2|]
        let qua = isTwoCycle arr
        match isTwoCycle arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isTwoCycle: Valid int64 two-cycle with all fixed points returns Ok true`` () =
        let arr = [|0L; 1L; 2L|]
        match isTwoCycle arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isTwoCycle: Empty int array returns Ok true`` () =
        let arr: int[] = [||]
        match isTwoCycle arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isTwoCycle: Single valid int element returns Ok true`` () =
        let arr = [|0|]
        match isTwoCycle arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isTwoCycle: Multiple swaps returns Ok true`` () =
        let arr = [|1; 0; 3; 2|]
        match isTwoCycle arr with
        | Ok result -> Assert.True(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isTwoCycle: Negative index returns Error NegativeIndex`` () =
        let arr = [|-1; 0; 1|]
        match isTwoCycle arr with
        | Error (NegativeIndex -1) -> ()
        | _ -> Assert.Fail("Expected Error NegativeIndex")

    [<Fact>]
    let ``isTwoCycle: Out-of-bounds index returns Error IndexOutOfBounds`` () =
        let arr = [|0; 3; 1|]
        match isTwoCycle arr with
        | Error (IndexOutOfBounds (3, 3)) -> ()
        | _ -> Assert.Fail("Expected Error IndexOutOfBounds")

    [<Fact>]
    let ``isTwoCycle: Three cycle returns OK false`` () =
        let arr = [|1; 2; 0|]
        match isTwoCycle arr with
        | Ok result -> Assert.False(result)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``isTwoCycle: Null array returns Error NullArray`` () =
        let arr: int[] = null
        match isTwoCycle arr with
        | Error NullArray -> ()
        | _ -> Assert.Fail("Expected Error NullArray")


    // Helper to compute expected squared distance for a segment
    let expectedDistance (longArray: float[]) (shortArray: float[]) n i =
        let mutable acc = 0.0
        for j = 0 to n - 1 do
            let diff = longArray.[i * n + j] - shortArray.[j]
            acc <- acc + diff * diff
        acc

    [<Fact>]
    let ``Safe: Valid input n=2 m=3 returns correct distances`` () =
        let longArray = createLongFloatArray 2 3 // [1.0; 2.0; 3.0; 4.0; 5.0; 6.0]
        let shortArray = [| 0.0; 0.0 |]
        let result = distanceSquaredArraySegmentSafe longArray shortArray
        match result with
        | Ok distances ->
            Assert.Equal(3, distances.Length)
            Assert.Equal(5.0, distances.[0])  // (1-0)^2 + (2-0)^2 = 1 + 4 = 5
            Assert.Equal(25.0, distances.[1]) // (3-0)^2 + (4-0)^2 = 9 + 16 = 25
            Assert.Equal(61.0, distances.[2]) // (5-0)^2 + (6-0)^2 = 25 + 36 = 61
        | Error _ -> Assert.False(true, "Expected Ok result")

    [<Fact>]
    let ``Unsafe: Valid input n=2 m=3 returns correct distances`` () =
        let longArray = createLongFloatArray 2 3
        let shortArray = [| 0.0; 0.0 |]
        let distances = distanceSquaredArraySegmentUnsafe longArray shortArray
        Assert.Equal(3, distances.Length)
        Assert.Equal(5.0, distances.[0])
        Assert.Equal(25.0, distances.[1])
        Assert.Equal(61.0, distances.[2])

    [<Theory>]
    [<InlineData(1, 2)>]
    [<InlineData(3, 1)>]
    [<InlineData(1, 1)>]
    let ``Safe: Edge cases with small n and m`` (n: int) (m: int) =
        let longArray = createLongFloatArray n m
        let shortArray = createShortFloatArray n
        let result = distanceSquaredArraySegmentSafe longArray shortArray
        match result with
        | Ok distances ->
            Assert.Equal(m, distances.Length)
            for i = 0 to m - 1 do
                let expected = expectedDistance longArray shortArray n i
                Assert.Equal(expected, distances.[i])
        | Error _ -> Assert.False(true, "Expected Ok result")

    [<Theory>]
    [<InlineData(1, 2)>]
    [<InlineData(3, 1)>]
    [<InlineData(1, 1)>]
    let ``Unsafe: Edge cases with small n and m`` (n: int) (m: int) =
        let longArray = createLongFloatArray n m
        let shortArray = createShortFloatArray n
        let distances = distanceSquaredArraySegmentUnsafe longArray shortArray
        Assert.Equal(m, distances.Length)
        for i = 0 to m - 1 do
            let expected = expectedDistance longArray shortArray n i
            Assert.Equal(expected, distances.[i])

    [<Fact>]
    let ``Safe: Null long array returns error`` () =
        let shortArray = [| 1.0; 2.0 |]
        let result = distanceSquaredArraySegmentSafe null shortArray
        match result with
        | Error NullLongArray -> Assert.True(true)
        | _ -> Assert.False(true, "Expected NullLongArray error")

    [<Fact>]
    let ``Safe: Null short array returns error`` () =
        let longArray = createLongFloatArray 2 2
        let result = distanceSquaredArraySegmentSafe longArray null
        match result with
        | Error NullShortArray -> Assert.True(true)
        | _ -> Assert.False(true, "Expected NullShortArray error")

    [<Fact>]
    let ``Safe: Invalid short array length (n=0) returns error`` () =
        let longArray = [| 1.0; 2.0 |]
        let shortArray = [||]
        let result = distanceSquaredArraySegmentSafe longArray shortArray
        match result with
        | Error (InvalidShortArrayLength 0) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected InvalidShortArrayLength error")

    [<Fact>]
    let ``Safe: Non-whole number segments returns error`` () =
        let longArray = [| 1.0; 2.0; 3.0 |] // Length=3, not divisible by n=2
        let shortArray = [| 1.0; 2.0 |]
        let result = distanceSquaredArraySegmentSafe longArray shortArray
        match result with
        | Error (NonWholeNumberSegments (3, 2)) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected NonWholeNumberSegments error")

    [<Fact>]
    let ``Safe: Zero-length long array returns error`` () =
        let longArray = Array.Empty<float>()
        let shortArray = [| 1.0; 2.0 |]
        let result = distanceSquaredArraySegmentSafe longArray shortArray
        match result with
        | Error (InvalidLongArrayLength (2, 0)) -> Assert.True(true)
        | _ -> Assert.False(true, "Expected InvalidLongArrayLength error")