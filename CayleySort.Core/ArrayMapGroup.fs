namespace CayleySort.Core
open System
open CollectionUtils
open Microsoft.FSharp.Core.LanguagePrimitives
open ArrayMap

module ArrayMapGroup = 

    let cubeVertexRotationGroup () : seq<array<int>> =
        let n = 8 // Number of vertices
        let zero = GenericZero< ^a>
        let one = GenericOne< ^a>
        
        // Helper to create an ArrayMap from a permutation array
        let createArrayMap (perm: int[]) : array<^a> =
            let arr = Array.zeroCreate n
            for i = 0 to n - 1 do
                // Convert perm.[i] to type ^a by adding GenericOne< ^a> perm.[i] times
                let mutable value = zero
                for _ = 1 to perm.[i] do
                    value <- value + one
                arr.[i] <- value
            arr

        // Define generators
        // Generator 1: 90° rotation around x-axis: [2; 3; 6; 7; 0; 1; 4; 5]
        // Cycles: (0,2,6,4)(1,3,7,5)
        let g1 = createArrayMap [|2; 3; 6; 7; 0; 1; 4; 5|]
        // Generator 2: 180° rotation around front-top to back-bottom edge: [4; 5; 2; 3; 0; 1; 6; 7]
        // Swaps: (0,4)(1,5)
        let g2 = createArrayMap [|4; 5; 2; 3; 0; 1; 6; 7|]

        // Set to track unique permutations
        let mutable seen = Set.empty.Add(g1)
        let result = ResizeArray<array<^a>>()
        result.Add(g1) // Start with g1

        // Helper to compose two ArrayMaps
        let compose a b =
            arrayMapCompositionUnsafe a b

        // Generate group by applying generators
        let rec generate (current: array<^a>) (count: int) =
            if count < 24 then // Cube rotation group has 24 elements
                let next1 = compose current g1
                let next2 = compose current g2
                for next in [next1; next2] do
                    if not (Set.contains next seen) then
                        seen <- Set.add next seen
                        result.Add(next)
                        generate next (count + 1)

        // Start generation from g1
        generate g1 1

        // Add identity explicitly if not already included
        let identityMap = identity< ^a> n
        if not (Set.contains identityMap seen) then
            result.Add(identityMap)

        // Return as sequence
        seq { for arr in result do yield arr }

    // Generates the rotation group of a tesseract acting on its 16 vertices using int arrays
    let tesseractVertexRotationGroup () : seq<int[]> =
        let n = 16 // Number of vertices
        
        // Helper to create an int array from a permutation
        let createArrayMap (perm: int[]) : int[] =
            Array.copy perm

        // Generators
        // Generator 1: Reflection across x_4=0, swaps vertices by negating x_4
        let g1 = createArrayMap [|1; 0; 3; 2; 5; 4; 7; 6; 9; 8; 11; 10; 13; 12; 15; 14|]
        // Generator 2: Reflection across x_1=0, swaps vertices by negating x_1
        let g2 = createArrayMap [|8; 9; 10; 11; 12; 13; 14; 15; 0; 1; 2; 3; 4; 5; 6; 7|]
        // Generator 3: Reflection across x_2=0, swaps vertices by negating x_2
        let g3 = createArrayMap [|4; 5; 6; 7; 0; 1; 2; 3; 12; 13; 14; 15; 8; 9; 10; 11|]
        // Generator 4: Transposition of x_1 and x_2
        let g4 = createArrayMap [|2; 3; 0; 1; 6; 7; 4; 5; 10; 11; 8; 9; 14; 15; 12; 13|]

        // Set to track unique permutations
        let mutable seen = Set.empty
        let result = ResizeArray<int[]>()
        let identityMap = [|0..15|]
        
        // Initialize with identity and generators
        result.Add(identityMap)
        result.Add(g1)
        result.Add(g2)
        result.Add(g3)
        result.Add(g4)
        seen <- seen.Add(identityMap).Add(g1).Add(g2).Add(g3).Add(g4)

        // Helper to compose two int ArrayMaps
        let compose a b = arrayMapCompositionUnsafe a b

        // Generate group using breadth-first search
        let queue = System.Collections.Generic.Queue<int[]>()
        queue.Enqueue(identityMap)
        queue.Enqueue(g1)
        queue.Enqueue(g2)
        queue.Enqueue(g3)
        queue.Enqueue(g4)
        while queue.Count > 0 && result.Count < 384 do // Tesseract rotation group has 384 elements
            let current = queue.Dequeue()
            let next1 = compose current g1
            let next2 = compose current g2
            let next3 = compose current g3
            let next4 = compose current g4
            for next in [next1; next2; next3; next4] do
                if not (Set.contains next seen) then
                    seen <- Set.add next seen
                    result.Add(next)
                    queue.Enqueue(next)

        // Return as sequence
        seq { for arr in result do yield arr }