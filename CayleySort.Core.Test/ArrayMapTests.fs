
namespace CayleySort.Core.Tests
open Xunit
open FsUnit.Xunit
open CayleySort.Core.ArrayMap
open CayleySort.Core.CollectionUtils
open System

type ArrayMapTests() =
    // Tests for unsafe version
    [<Fact>]
    let ``Unsafe: Computes array product correctly`` () =
        let lhs = [|10; 20; 30|]
        let rhs = [|2; 0; 1|]
        let result = arrayMapCompositionUnsafe lhs rhs
        Assert.Equal<int list>([30; 10; 20], result |> Seq.toList)

    [<Fact>]
    let ``Unsafe: Handles empty lhs array`` () =
        let lhs: int[] = [||]
        let rhs = [|0; 1|]
        let result = arrayMapCompositionUnsafe lhs rhs
        Assert.Equal<int list>([], result |> Seq.toList)

    [<Fact>] 
    let ``Unsafe: Throws IndexOutOfRangeException for invalid index`` () =
        let lhs = [|10; 20|]
        let rhs = [|2|]
        Assert.Throws<System.IndexOutOfRangeException>(fun () -> arrayMapCompositionUnsafe lhs rhs |> ignore)

    // Tests for safe version
    [<Fact>]
    let ``Safe: Computes array product correctly`` () =
        let lhs = [|10; 20; 30|]
        let rhs = [|2; 0; 1|]
        let result = arrayMapCompositionSafe lhs rhs
        match result with
        | Ok arr -> Assert.Equal<int list>([30; 10; 20], arr |> Seq.toList)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Safe: Handles empty lhs array`` () =
        let lhs: int[] = [||]
        let rhs = [|0; 1|]
        let result = arrayMapCompositionSafe lhs rhs
        match result with
        | Ok arr -> Assert.Equal<int list>([], arr |> Seq.toList)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Safe: Works with int64 type`` () =
        let lhs = [|10L; 20L; 30L|]
        let rhs = [|2L; 0L; 1L|]
        let result = arrayMapCompositionSafe lhs rhs
        match result with
        | Ok arr -> Assert.Equal<int64 list>([30L; 10L; 20L], arr |> Seq.toList)
        | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")

    [<Fact>]
    let ``Safe: Returns NullLeftArray for null lhs`` () =
        let lhs: int[] = null
        let rhs = [|0; 1|]
        let result = arrayMapCompositionSafe lhs rhs
        match result with
        | Error NullLeftArray -> ()
        | _ -> Assert.Fail("Expected Error NullLeftArray")

    [<Fact>]
    let ``Safe: Returns NullRightArray for null rhs`` () =
        let lhs = [|10; 20|]
        let rhs: int[] = null
        let result = arrayMapCompositionSafe lhs rhs
        match result with
        | Error NullRightArray -> ()
        | _ -> Assert.Fail("Expected Error NullRightArray")

    [<Fact>]
    let ``Safe: Returns InsufficientRightLength for short rhs`` () =
        let lhs = [|10; 20; 30|]
        let rhs = [|0; 1|]
        let result = arrayMapCompositionSafe lhs rhs
        match result with
        | Error (InsufficientRightLength (3, 2)) -> ()
        | _ -> Assert.Fail("Expected Error (InsufficientRightLength (3, 2))")

    [<Fact>]
    let ``Safe: Returns IndexOutOfBounds for invalid index`` () =
        let lhs = [|10; 20|]
        let rhs = [|2; 0|]
        let result = arrayMapCompositionSafe lhs rhs
        Assert.Equal(Error (ArrayMapCompositionError.IndexOutOfBounds 2), result)

    [<Fact>]
    let ``Safe: Returns IndexOutOfBounds for negative index`` () =
        let lhs = [|10; 20|]
        let rhs = [|-1; 0|]
        let result = arrayMapCompositionSafe lhs rhs
        Assert.Equal(Error (ArrayMapCompositionError.IndexOutOfBounds -1), result)

    [<Fact>]
    let ``Permutation composition with inverse is identity`` () =
        // Arrange
        let size = 8
        let permutation = [|5; 2; 7; 0; 4; 1; 3; 6|] // Non-trivial permutation of [0..7]

        // Act: Create inverse
        let inverseArray = Array.zeroCreate size
        let inverseResult = invertArrayMapSafe permutation inverseArray
        match inverseResult with
        | Error e -> Assert.False(true, sprintf "Inverse computation failed: %A" e)
        | Ok inverse ->
            // Act: Compose permutation and its inverse
            let compositionResult = arrayMapCompositionSafe permutation inverse
            match compositionResult with
            | Error e -> Assert.False(true, sprintf "Composition failed: %A" e)
            | Ok composition ->
                // Assert: Composition should be identity
                Assert.True(isIdentity composition, "Composition of permutation and its inverse is not identity")

    [<Fact>]
    let ``Rotate permutation has period equal to array length`` () =
        // Arrange
        let n = 5
        let sigma = rotate<int> n // Rotation: [1; 2; 3; 4; 0]
        let identityMap = identity<int> n // Identity: [0; 1; 2; 3; 4]
    
        // Act: Take the entire orbit
        let orbit = arrayMapPowers sigma |> Seq.toList
    
        // Assert
        Assert.Equal(n, orbit.Length) // Orbit should have n elements (sigma^1 to sigma^0)
        Assert.True(isIdentity orbit.[n - 1], "The last element should be the identity")
        for i = 0 to n - 2 do
            Assert.False(isIdentity orbit.[i], $"Power {i + 1} should not be the identity")
    
    [<Fact>]
    let ``Rotate with length 3 has period 3`` () =
        // Arrange
        let n = 3
        let sigma = rotate<int> n // Rotation: [1; 2; 0]
        let identityMap = identity<int> n
    
        // Act
        let orbit = arrayMapPowers sigma |> Seq.toList
    
        // Assert
        Assert.Equal(n, orbit.Length) // Orbit should have 3 elements (sigma^1 to sigma^0)
        Assert.True(isIdentity orbit.[n - 1], "The last element should be the identity")
        for i = 0 to n - 2 do
            Assert.False(isIdentity orbit.[i], $"Power {i + 1} should not be the identity")

    [<Fact>]
    let ``Rotate with length 1 has period 1`` () =
        // Arrange
        let n = 1
        let sigma = rotate<int> n // Rotation: [0]
    
        // Act
        let orbit = arrayMapPowers sigma |> Seq.toList
    
        // Assert
        Assert.Single(orbit) // Orbit should have 1 element (sigma^1 = sigma^0)
        Assert.True(isIdentity orbit.[0], "The only element should be the identity")

    [<Fact>]
    let ``Rotate with invalid length fails`` () =
        // Act & Assert
        Assert.Throws<ArgumentException>(fun () -> rotate<int> -1 |> ignore)
    
    [<Fact>]
    let ``Conjugation with identity sigma returns tau`` () =
        // Arrange
        let size = 5
        let sigma = identity<int> size // Identity: [0->0, 1->1, 2->2, 3->3, 4->4]
        let tau = [|1; 3; 0; 4; 2|]   // Permutation: [0->1, 1->3, 2->0, 3->4, 4->2]
    
        // Act
        let resultArray = Array.zeroCreate size
        let conjugationResult = arrayMapConjugationSafe sigma tau resultArray
    
        // Assert
        match conjugationResult with
        | Error e -> Assert.False(true, sprintf "Conjugation failed: %A" e)
        | Ok result ->
            Assert.Equal<int[]>(tau, result)

    [<Fact>]
    let ``Conjugation with non-trivial permutations produces correct result`` () =
        // Arrange
        let size = 8
        let sigma = [|5; 2; 7; 0; 4; 1; 3; 6|] // Non-trivial permutation
        let tau = [|2; 0; 4; 6; 1; 7; 5; 3|]   // Non-trivial permutation
        let expected = [|3; 6; 5; 1; 2; 7; 0; 4|] // Expected result of sigma * tau * sigma^(-1)
    
        // Act
        let resultArray = Array.zeroCreate size
        let conjugationResult = arrayMapConjugationSafe sigma tau resultArray
    
        // Assert
        match conjugationResult with
        | Error e -> Assert.False(true, sprintf "Conjugation failed: %A" e)
        | Ok result ->
            Assert.Equal<int[]>(expected, result)

    [<Fact>]
    let ``Conjugation with empty arrays returns empty result`` () =
        // Arrange
        let sigma = [||]
        let tau = [||]
        let resultArray = [||]
    
        // Act
        let conjugationResult = arrayMapConjugationSafe sigma tau resultArray
    
        // Assert
        match conjugationResult with
        | Error e -> Assert.False(true, sprintf "Conjugation failed: %A" e)
        | Ok result ->
            Assert.Equal<int[]>([||], result)

    [<Fact>]
    let ``Conjugation fails with null sigma`` () =
        // Arrange
        let sigma: int[] = null
        let tau = [|1; 0|]
        let resultArray = Array.zeroCreate 2
    
        // Act
        let conjugationResult = arrayMapConjugationSafe sigma tau resultArray
    
        // Assert
        match conjugationResult with
        | Error NullSigmaArray -> Assert.True(true)
        | _ -> Assert.False(true, "Expected NullSigmaArray error")

    [<Fact>]
    let ``Conjugation fails with insufficient tau length`` () =
        // Arrange
        let sigma = [|1; 0; 2|]
        let tau = [|1; 0|] // Too short
        let resultArray = Array.zeroCreate 3
    
        // Act
        let conjugationResult = arrayMapConjugationSafe sigma tau resultArray
    
        // Assert
        match conjugationResult with
        | Error (InsufficientTauLength (expected, actual)) ->
            Assert.Equal(3, expected)
            Assert.Equal(2, actual)
        | _ -> Assert.False(true, "Expected InsufficientTauLength error")

    [<Fact>]
    let ``Conjugation fails with invalid index in sigma`` () =
        // Arrange
        let sigma = [|1; 0; 3|] // 3 is out of bounds for length 3
        let tau = [|1; 0; 2|]
        let resultArray = Array.zeroCreate 3
    
        // Act
        let conjugationResult = arrayMapConjugationSafe sigma tau resultArray
    
        // Assert
        match conjugationResult with
        | Error (IndexOutOfBounds idx) ->
            Assert.Equal(3, idx)
        | _ -> Assert.False(true, "Expected IndexOutOfBounds error")