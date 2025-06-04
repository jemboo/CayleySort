
namespace CayleySort.Core.Tests
open Xunit
open FsUnit.Xunit
open CayleySort.Core.ArrayMap
open CayleySort.Core.CollectionUtils
open CayleySort.Core.ArrayMapGroup

type ArrayMapGroupTests() =

    [<Fact>]
    let ``Cube vertex rotation group contains exactly 24 distinct elements`` () =
        // Arrange
        let expectedCount = 24 // Cube rotation group has 24 elements
        let n = 8 // Number of vertices
    
        // Act
        let rotations = cubeVertexRotationGroup() |> Seq.toList
    
        // Assert
        Assert.Equal(expectedCount, rotations.Length) // Verify exactly 24 rotations
    
        // Verify all elements are distinct
        let rotationSet = rotations |> Set.ofList
        Assert.Equal(expectedCount, rotationSet.Count)
    
        // Verify each ArrayMap is a valid permutation of [0..7]
        for rotation in rotations do
            Assert.Equal(n, rotation.Length)
            let values = rotation |> Seq.sort |> Seq.toArray
            let expected = [|0..7|]
            Assert.Equal<int[]>(expected, values) // Each rotation is a permutation of 0 to 7
    
        // Verify identity is included
        let identityMap = identity<int> n
        Assert.Contains(identityMap, rotations)
    
        // Verify generators are included
        let g1 = [|2; 3; 6; 7; 0; 1; 4; 5|] // 90° rotation around x-axis
        let g2 = [|4; 5; 2; 3; 0; 1; 6; 7|] // 180° rotation around front-top to back-bottom edge
        let toArrayMap (perm: int[]) = 
            let arr = Array.zeroCreate n
            for i = 0 to n - 1 do
                arr.[i] <- perm.[i]
            arr
        Assert.Contains(toArrayMap g1, rotations)
        Assert.Contains(toArrayMap g2, rotations)

    [<Fact>]
    let ``Cube vertex rotation group is closed under composition`` () =
        // Arrange
        let rotations = cubeVertexRotationGroup() |> Seq.toList
    
        // Act: Check closure by composing pairs of rotations
        for r1 in rotations do
            for r2 in rotations do
                let composed = arrayMapCompositionUnsafe r1 r2 
                Assert.Contains(composed, rotations) // Every composition must be in the group

    [<Fact>]
    let ``Cube vertex rotation group includes expected rotation orders`` () =
        // Arrange
        let rotations = cubeVertexRotationGroup() |> Seq.toList
        let n = 8
        let identityMap = identity<int> n
    
        // Helper to compute order of a permutation
        let computeOrder (perm: array<int>) =
            if isIdentity perm then
                1 // Identity has order 1
            else
                let powers = arrayMapPowers perm
                powers 
                |> Seq.take 23 // Max order is at most 24, so take up to 23 more
                |> Seq.findIndex (fun p -> isIdentity p)
                |> (+) 1 // Index 0 is sigma^1, so add 1 to get order
    
        // Act: Count rotations by their orders
        let orders = rotations |> List.map computeOrder
        let orderCounts = orders |> List.groupBy id |> List.map (fun (order, group) -> (order, group.Length)) |> Map.ofList
    
        // Assert: Verify expected number of elements of each order
        // Expected orders based on S_4: identity (order 1), transpositions (order 2), 3-cycles (order 3), 4-cycles (order 4)
        Assert.Equal(1, Map.find 1 orderCounts) // 1 identity
        Assert.Equal(9, Map.find 2 orderCounts) // 6 edge + 3 face 180° rotations
        Assert.Equal(8, Map.find 3 orderCounts) // 8 vertex rotations (120°, 240°)
        Assert.Equal(6, Map.find 4 orderCounts) // 6 face rotations (90°, 270°)



    [<Fact>]
    let ``Tesseract vertex rotation group contains exactly 384 distinct elements`` () =
        // Arrange
        let expectedCount = 384 // Tesseract rotation group has 384 elements
        let n = 16 // Number of vertices
    
        // Act
        let rotations = tesseractVertexRotationGroup() |> Seq.toList
    
        // Assert
        Assert.Equal(expectedCount, rotations.Length) // Verify exactly 384 rotations
    
        // Verify all elements are distinct
        let rotationSet = rotations |> Set.ofList
        Assert.Equal(expectedCount, rotationSet.Count)
    
        // Verify each ArrayMap is a valid permutation of [0..15]
        for rotation in rotations do
            Assert.Equal(n, rotation.Length)
            let values = rotation |> Seq.sort |> Seq.toArray
            let expected = [|0..15|]
            Assert.Equal<int[]>(expected, values) // Each rotation is a permutation of 0 to 15
    
        // Verify identity and generators are included
        let identityMap = [|0..15|]
        let g1 = [|1; 0; 3; 2; 5; 4; 7; 6; 9; 8; 11; 10; 13; 12; 15; 14|] // Reflection x_4
        let g2 = [|8; 9; 10; 11; 12; 13; 14; 15; 0; 1; 2; 3; 4; 5; 6; 7|] // Reflection x_1
        let g3 = [|4; 5; 6; 7; 0; 1; 2; 3; 12; 13; 14; 15; 8; 9; 10; 11|] // Reflection x_2
        let g4 = [|2; 3; 0; 1; 6; 7; 4; 5; 10; 11; 8; 9; 14; 15; 12; 13|] // Transposition
        Assert.Contains(identityMap, rotations)
        Assert.Contains(g1, rotations)
        Assert.Contains(g2, rotations)
        Assert.Contains(g3, rotations)
        Assert.Contains(g4, rotations)

    [<Fact>]
    let ``Tesseract vertex rotation group is closed under composition`` () =
        // Arrange
        let rotations = tesseractVertexRotationGroup() |> Seq.toList
    
        // Act: Check closure by composing a subset of rotations
        let sample = rotations |> Seq.take 100 |> Seq.toList
        for r1 in sample do
            for r2 in sample do
                let composed = arrayMapCompositionUnsafe r1 r2
                Assert.Contains(composed, rotations) // Every composition must be in the group

    [<Fact>]
    let ``Tesseract vertex rotation permutation has period equal to array length`` () =
        // Arrange
        let n = 16
        // Use a 16-cycle permutation: (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
        let sigma = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 0|]
        let identityMap = [|0..15|]
    
        // Act: Take the entire orbit
        let orbit = arrayMapPowers sigma |> Seq.toList
    
        // Assert
        Assert.Equal(n, orbit.Length) // Orbit should have n elements (sigma^1 to sigma^0)
        Assert.True(isIdentity orbit.[n - 1], "The last element should be the identity")
        for i = 0 to n - 2 do
            Assert.False(isIdentity orbit.[i], $"Power {i + 1} should not be the identity")