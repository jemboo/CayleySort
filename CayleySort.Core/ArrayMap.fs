namespace CayleySort.Core
open System
open CollectionUtils
open Microsoft.FSharp.Core.LanguagePrimitives

// an ArrayMap is a discrete function ^a -> ^a, represented by an A:array<^a>, where
// the input value of the function is the index i of an array, and it's ouput is A[i]
// ArrayMaps represent permuations of [|0 .. (n - 1)|]

module ArrayMap =

    // Generic identity array: [0 .. order-1]
    let inline identity< ^a when ^a: (static member Zero: ^a)
                            and ^a: (static member One: ^a)
                            and ^a: (static member (+): ^a * ^a -> ^a)>
            (order: int) : ^a[] =
        if order < 0 then raise (ArgumentException("Order must be non-negative"))
        let result = Array.zeroCreate order
        let one = GenericOne<^a>
        let mutable current = GenericZero<^a>
        for i = 0 to order - 1 do
            result.[i] <- current
            current <- current + one
        result

    // Generic isIdentity check
    let inline isIdentity< ^a when ^a: equality
                              and ^a: (static member Zero: ^a)
                              and ^a: (static member One: ^a)
                              and ^a: (static member (+): ^a * ^a -> ^a)>
            (wh: ^a[]) : bool =
        arrayEquals wh (identity wh.Length)


    // Creates an ArrayMap representing a rotation permutation of length n
    // The permutation maps i -> (i + 1) % n, forming a single cycle of length n
    let inline rotate< ^a when ^a: (static member Zero: ^a) and
                      ^a: (static member One: ^a) and
                      ^a: (static member (+): ^a * ^a -> ^a) and
                      ^a: (static member op_Explicit: ^a -> int)>
            (n: int)
            : array<^a> =
        if n < 0 then
            invalidArg "n" "Length must be non-negative"
        let result = Array.zeroCreate n
        let one = GenericOne< ^a>
        let mutable current = GenericZero< ^a>
        for i = 0 to n - 1 do
            // Compute (i + 1) % n
            let nextIndex = if i = n - 1 then GenericZero< ^a> else current + one
            result.[i] <- nextIndex
            current <- nextIndex
        result



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
        | InsufficientRightLength of expected: int * actual: int
        | IndexOutOfBounds of index: int

    // Unsafe version
    let inline arrayMapCompositionUnsafe< ^a when ^a: (static member op_Explicit: ^a -> int)>
                        (lhs: array<^a>) 
                        (rhs: array<^a>)
                   : array<^a> =
        let dmax = lhs.Length
        let prod = Array.zeroCreate dmax
        let mutable curdex = 0
        while curdex < dmax do
            prod.[curdex] <- lhs.[rhs.[curdex] |> int]
            curdex <- curdex + 1
        prod

    // Safe version
    let inline arrayMapCompositionSafe< ^a when ^a: (static member op_Explicit: ^a -> int)>
                        (lhs: array<^a>) 
                        (rhs: array<^a>) 
                        : Result<array<^a>, ArrayMapCompositionError> =
        if isNull lhs then Error NullLeftArray
        elif isNull rhs then Error NullRightArray
        elif rhs.Length < lhs.Length then Error (InsufficientRightLength (lhs.Length, rhs.Length))
        else
            let mutable invalidIndex = None
            for i = 0 to lhs.Length - 1 do
                let idx = int rhs.[i]
                if idx < 0 || idx >= lhs.Length then
                    invalidIndex <- Some idx
            if invalidIndex.IsSome then
                Error (IndexOutOfBounds invalidIndex.Value)
            else
                Ok (arrayMapCompositionUnsafe lhs rhs)



    // Generates the orbit of an ArrayMap: sigma^1, sigma^2, ..., sigma^0 (identity)
    let inline arrayMapPowers< ^a when ^a: (static member Zero: ^a) and
                               ^a: equality and
                               ^a: (static member One: ^a) and
                               ^a: (static member (+): ^a * ^a -> ^a) and
                               ^a: (static member op_Explicit: ^a -> int)>
                (sigma: array<^a>)
                : seq<array<^a>> =
        if isNull sigma then
            invalidArg "sigma" "Input array cannot be null"
        let n = sigma.Length
        let identityMap = identity< ^a> n
        seq {
            if isIdentity sigma then
                yield sigma // If sigma is identity, yield it and stop
            else
                let mutable current = Array.copy sigma // Start with sigma^1
                yield current
                let mutable next = arrayMapCompositionUnsafe sigma current
                while not (isIdentity next) do
                    yield next
                    current <- next
                    next <- arrayMapCompositionUnsafe sigma current
                yield next // Yield the identity (sigma^0)
        }


    type ArrayMapConjugationError =
        | NullSigmaArray
        | NullTauArray
        | NullResultArray
        | InsufficientTauLength of expected: int * actual: int
        | InsufficientResultLength of expected: int * actual: int
        | IndexOutOfBounds of index: int
        | InversionError of InvertArrayMapError
        | CompositionError of ArrayMapCompositionError

    // Unsafe version of conjugation: computes sigma * tau * sigma^(-1)
    let inline arrayMapConjugationUnsafe< ^a when ^a: (static member Zero: ^a) and
                                         ^a: (static member One: ^a) and
                                         ^a: (static member (+): ^a * ^a -> ^a) and
                                         ^a: (static member op_Explicit: ^a -> int)>
                        (sigma: array<^a>) 
                        (tau: array<^a>) 
                        (result: array<^a>) 
                        : array<^a> =
        let sigmaInverse = Array.zeroCreate sigma.Length
        let inverse = invertArrayMapUnsafe sigma sigmaInverse
        let step1 = arrayMapCompositionUnsafe tau inverse
        arrayMapCompositionUnsafe sigma step1

    // Safe version of conjugation
    let inline arrayMapConjugationSafe< ^a when ^a: (static member Zero: ^a) and
                                       ^a: (static member One: ^a) and
                                       ^a: (static member (+): ^a * ^a -> ^a) and
                                       ^a: (static member op_Explicit: ^a -> int)>
                        (sigma: array<^a>) 
                        (tau: array<^a>) 
                        (result: array<^a>) 
                        : Result<array<^a>, ArrayMapConjugationError> =
        if isNull sigma then Error NullSigmaArray
        elif isNull tau then Error NullTauArray
        elif isNull result then Error NullResultArray
        elif tau.Length < sigma.Length then Error (InsufficientTauLength (sigma.Length, tau.Length))
        elif result.Length < sigma.Length then Error (InsufficientResultLength (sigma.Length, result.Length))
        else
            // Validate indices in sigma and tau
            let mutable invalidIndex = None
            for i = 0 to sigma.Length - 1 do
                let idxSigma = int sigma.[i]
                let idxTau = int tau.[i]
                if idxSigma < 0 || idxSigma >= sigma.Length || idxTau < 0 || idxTau >= sigma.Length then
                    invalidIndex <- Some (max idxSigma idxTau)
            if invalidIndex.IsSome then
                Error (IndexOutOfBounds invalidIndex.Value)
            else
                // Compute sigma^(-1)
                let sigmaInverse = Array.zeroCreate sigma.Length
                match invertArrayMapSafe sigma sigmaInverse with
                | Error e -> Error (InversionError e)
                | Ok inverse ->
                    // Compute tau * sigma^(-1)
                    match arrayMapCompositionSafe tau inverse with
                    | Error e -> Error (ArrayMapConjugationError.IndexOutOfBounds 
                            (match e with
                                | ArrayMapCompositionError.IndexOutOfBounds idx -> idx
                                | _ -> 0))
                    | Ok intermediate ->
                        // Compute sigma * (tau * sigma^(-1))
                        match arrayMapCompositionSafe sigma intermediate with
                        | Error e -> Error (CompositionError e)
                        | Ok final -> Ok final