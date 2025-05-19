namespace CayleySort.Core
open System

// an ArrayMap is a discrete function ^a -> ^a, represented by an A:array<^a>, where
// the input value of the function is the index i of an array, and it's ouput is A[i]
// All short arrays (of length n), or segments of long arrays (of length m*n) in this module 
// are expected to be permuations of [|0 .. (n - 1)|]
module ArrayMap =

    // Error type for safe version
    type InvertArrayMapError =
        | NullInputArray
        | NullOutputArray
        | IndexOutOfBounds of int

    // Helper functions
    let inline zero_of< ^a when ^a: (static member Zero: ^a)> (_: ^a) = (^a : (static member Zero : ^a) ())
    let inline one_of< ^a when ^a: (static member One: ^a)> (_: ^a) = (^a : (static member One : ^a) ())

    // Unsafe version (unchanged)
    let inline invertArrayMapUnsafe< ^a when ^a: (static member Zero: ^a) and
                                  ^a: (static member One: ^a) and
                                  ^a: (static member (+): ^a * ^a -> ^a) and
                                  ^a: (static member op_Explicit: ^a -> int)>
                (ar: array<^a>)
                (inv_out: array<^a>)
                : array<^a> =
        let mutable iv = zero_of ar.[0]
        let incr = one_of ar.[0]
        for i = 0 to ar.Length - 1 do
            inv_out.[int ar.[i]] <- iv
            iv <- iv + incr
        inv_out

    // Safe version with explicit validation
    let inline invertArrayMapSafe< ^a when ^a: (static member Zero: ^a) and
                                    ^a: (static member One: ^a) and
                                    ^a: (static member (+): ^a * ^a -> ^a) and
                                    ^a: (static member op_Explicit: ^a -> int)>
                (ar: array<^a>)
                (inv_out: array<^a>)
                : Result<array<^a>, InvertArrayMapError> =
        // Input validation
        if isNull ar then Error NullInputArray
        elif isNull inv_out then Error NullOutputArray
        elif ar.Length = 0 then Ok inv_out
        else
            // Validate indices
            let mutable allIndicesValid = true
            let mutable invalidIndex = 0
            for i = 0 to ar.Length - 1 do
                let idx = int ar.[i]
                if idx < 0 || idx >= inv_out.Length then
                    allIndicesValid <- false
                    invalidIndex <- idx
            if not allIndicesValid then
                Error (IndexOutOfBounds invalidIndex)
            else
                // Call unsafe version
                Ok (invertArrayMapUnsafe ar inv_out)



    type ArrayMapCompositionError =
        | NullLeftArray
        | NullRightArray
        | NullProductArray
        | InsufficientRightLength of expected: int * actual: int
        | InsufficientProductLength of expected: int * actual: int
        | IndexOutOfBounds of index: int

    // Unsafe version
    let inline arrayMapCompositionUnsafe< ^a when ^a: (static member op_Explicit: ^a -> int)>
                        (lhs: array<^a>) 
                        (rhs: array<^a>) 
                        (prod: array<^a>) 
                        : array<^a> =
        let dmax = lhs.Length
        let mutable curdex = 0
        while curdex < dmax do
            prod.[curdex] <- lhs.[rhs.[curdex] |> int]
            curdex <- curdex + 1
        prod


    // Safe version
    let inline arrayMapCompositionSafe< ^a when ^a: (static member op_Explicit: ^a -> int)>
                        (lhs: array<^a>) 
                        (rhs: array<^a>) 
                        (prod: array<^a>) 
                        : Result<array<^a>, ArrayMapCompositionError> =
        if isNull lhs then Error NullLeftArray
        elif isNull rhs then Error NullRightArray
        elif isNull prod then Error NullProductArray
        elif rhs.Length < lhs.Length then Error (InsufficientRightLength (lhs.Length, rhs.Length))
        elif prod.Length < lhs.Length then Error (InsufficientProductLength (lhs.Length, prod.Length))
        else
            let mutable allIndicesValid = true
            let mutable invalidIndex = 0
            for i = 0 to lhs.Length - 1 do
                let idx = int rhs.[i]
                if idx < 0 || idx >= lhs.Length then
                    allIndicesValid <- false
                    invalidIndex <- idx
            if not allIndicesValid then
                Error (IndexOutOfBounds invalidIndex)
            else
                Ok (arrayMapCompositionUnsafe lhs rhs prod)

