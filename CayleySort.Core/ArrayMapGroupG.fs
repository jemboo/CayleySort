namespace CayleySort.Core
open System
open CollectionUtils
open Microsoft.FSharp.Core.LanguagePrimitives
open ArrayMap

module ArrayMapGroupG = 

    let inline cubeVertexRotationGroup< ^a when ^a: (static member Zero: ^a) and
                                       ^a: comparison and
                                       ^a: (static member One: ^a) and
                                       ^a: (static member (+): ^a * ^a -> ^a) and
                                       ^a: (static member op_Explicit: ^a -> int)>
            () : seq<array<^a>> =
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