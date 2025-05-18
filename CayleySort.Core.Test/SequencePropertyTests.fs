module SequencePropertyTests

open Xunit
open FsUnit.Xunit
open CayleySort.Core.SequenceProperties

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


// isPermutation tests

[<Fact>]
let ``isPermutation: Valid int permutation returns Ok true`` () =
    let arr = [|1; 0; 2|]
    match isPermutation arr with
    | Ok result -> Assert.True(result)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``isPermutation: Valid int64 permutation returns Ok true`` () =
    let arr = [|1L; 0L; 2L|]
    match isPermutation arr with
    | Ok result -> Assert.True(result)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``isPermutation: Empty int array returns Ok true`` () =
    let arr: int[] = [||]
    match isPermutation arr with
    | Ok result -> Assert.True(result)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``isPermutation: Single valid int element returns Ok true`` () =
    let arr = [|0|]
    match isPermutation arr with
    | Ok result -> Assert.True(result)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``isPermutation: Invalid permutation returns false`` () =
    let arr = [|0; 0; 1|]
    let qua = isPermutation arr
    match isPermutation arr with
    | Ok result -> Assert.False(result)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

[<Fact>]
let ``isPermutation: Negative index returns Error IndexOutOfBounds`` () =
    let arr = [|-1; 0; 1|]
    match isPermutation arr with
    | Error (IsPermutationError.IndexOutOfBounds -1) -> ()
    | _ -> Assert.Fail("Expected Error NegativeIndex")

[<Fact>]
let ``isPermutation: Out-of-bounds index returns false`` () =
    let arr = [|0; 3; 1|]
    match isPermutation arr with
    | Error (IsPermutationError.IndexOutOfBounds (3)) -> ()
    | _ -> Assert.Fail("Expected Error IndexOutOfBounds")

[<Fact>]
let ``isPermutation: Single invalid int element returns Error IndexOutOfBounds`` () =
    let arr = [|1|]
    match isPermutation arr with
    | Error (IsPermutationError.IndexOutOfBounds (1)) -> ()
    | _ -> Assert.Fail("Expected Error IndexOutOfBounds")

[<Fact>]
let ``isPermutation: Null array returns Error NullArray`` () =
    let arr: int[] = null
    match isPermutation arr with
    | Error IsPermutationError.NullArray -> ()
    | _ -> Assert.Fail("Expected Error NullArray")


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