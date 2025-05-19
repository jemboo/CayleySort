namespace CayleySort.Core
open LanguagePrimitives
open System
open CollectionUtils

module SequenceProperties =

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


    let inline distanceSquaredUnsafe< ^a when ^a: (static member Zero: ^a)
                                        and ^a: (static member (+): ^a * ^a -> ^a)
                                        and ^a: (static member (-): ^a * ^a -> ^a)
                                        and ^a: (static member (*): ^a * ^a -> ^a)>
                    (a: ^a[]) (b: ^a[]) : ^a =
        let mutable acc = GenericZero<^a>
        let mutable i = 0
        while i < a.Length do
            acc <- acc + (a.[i] - b.[i]) * (a.[i] - b.[i])
            i <- i + 1
        acc


    type DistanceSquaredError =
        | UnequalLengths of aLength: int * bLength: int
        | NullArrayA
        | NullArrayB


    let inline distanceSquared< ^a when ^a: (static member Zero: ^a)
                                   and ^a: (static member (+): ^a * ^a -> ^a)
                                   and ^a: (static member (-): ^a * ^a -> ^a)
                                   and ^a: (static member (*): ^a * ^a -> ^a)>
                (a: ^a[]) (b: ^a[]) : Result<^a, DistanceSquaredError> =
        if isNull a then Error NullArrayA
        elif isNull b then Error NullArrayB
        elif a.Length <> b.Length then Error (UnequalLengths (a.Length, b.Length))
        else Ok (Array.fold2 (fun acc elem1 elem2 -> acc + (elem1 - elem2) * (elem1 - elem2)) GenericZero<^a> a b)



    let inline unsortednessSquared< ^a when ^a: (static member Zero: ^a)
                                        and ^a: (static member (+): ^a * ^a -> ^a)
                                        and ^a: (static member (-): ^a * ^a -> ^a)
                                        and ^a: (static member (*): ^a * ^a -> ^a)
                                        and ^a: comparison>
                (arr: ^a[]) : Result<^a, DistanceSquaredError> =
        if isNull arr then Error NullArrayA
        elif arr.Length = 0 then Ok GenericZero<^a>
        else
            let sorted = arr |> Array.copy |> Array.sort
            distanceSquared arr sorted


    type IsSortedError =
        | NullArray
        | InvalidOffset of offset: int * arrayLength: int
        | InvalidLength of length: int
        | OffsetPlusLengthExceedsArray of offset: int * length: int * arrayLength: int


    let inline isSorted< ^a when ^a: comparison> (values: ^a[]) : Result<bool, IsSortedError> =
        if isNull values then Error NullArray
        elif values.Length <= 1 then Ok true
        else
            let mutable i = 1
            let mutable isSorted = true
            while (i < values.Length && isSorted) do
                isSorted <- (values.[i - 1] <= values.[i])
                i <- i + 1
            Ok isSorted


    let inline isSortedOffset< ^a when ^a: comparison> (values: ^a[]) offset length : Result<bool, IsSortedError> =
        if isNull values then Error NullArray
        elif offset < 0 then Error (InvalidOffset (offset, values.Length))
        elif length < 0 then Error (InvalidLength length)
        elif offset + length > values.Length then 
            Error (OffsetPlusLengthExceedsArray (offset, length, values.Length))
        elif length <= 1 then Ok true
        else
            let mutable i = 1
            let mutable isSorted = true
            while (i < length && isSorted) do
                isSorted <- (values.[i + offset - 1] <= values.[i + offset])
                i <- i + 1
            Ok isSorted


    type IsPermutationError =
    | NullArray
    | IndexOutOfBounds of value: int

    // returns true if ^a[] is a permutation of [|0 .. (^a.length - 1)|]
    let inline isPermutationSafe< ^a when ^a: (static member op_Explicit: ^a -> int)> (a: ^a[]) 
        : Result<bool, IsPermutationError> =
        if isNull a then Error NullArray
        else
            let n = a.Length
            if n = 0 then Ok (true)
                else    
                    let seen = Array.create n false
                    let mutable valid = true
                    let mutable index = 0
                    let mutable error = None
                    while(index < n && error.IsNone && valid) do
                        let num = int a.[index]
                        if (num < 0) || (num >= n) then
                            error <- Some (IndexOutOfBounds (num))
                        elif not seen.[num] then
                            seen.[num] <- true
                        else
                            valid <- false
                        index <- index + 1

                    match error with
                    | Some e -> Error e
                    | None -> Ok (valid)

    type IsPermutationArraySegmentError =
        | NullLongArray
        | InvalidLongArrayLength of expectedMultiple: int * actual: int
        | InvalidSegmentLength of length: int
        | NonWholeNumberSegments of longLength: int * shortLength: int
        | IndexOutOfBounds of segmentIndex: int * value: int
    // return is true for each index i where longArray[i*segmentLength .. ((i + 1)*segmentLength - 1)]
    // is a permutation
    let inline isPermutationArraySegmentSafe< ^a when ^a: (static member op_Explicit: ^a -> int)>
            (longArray: ^a[]) (segmentLength: int) : Result<bool[], IsPermutationArraySegmentError> =
        if isNull longArray then Error NullLongArray
        elif segmentLength <= 0 then Error (InvalidSegmentLength segmentLength)
        elif longArray.Length = 0 then Error (InvalidLongArrayLength (longArray.Length, 0))
        elif longArray.Length % segmentLength <> 0 then 
            Error (NonWholeNumberSegments (longArray.Length, segmentLength))
        else
            let m = longArray.Length / segmentLength
            let result = Array.zeroCreate m
            let mutable error = None
            let mutable i = 0
            while i < m && error.IsNone do
                let seen = Array.create segmentLength false
                let mutable valid = true
                let mutable j = 0
                while j < segmentLength && valid && error.IsNone do
                    let num = int longArray.[i * segmentLength + j]
                    if num < 0 || num >= segmentLength then
                        error <- Some (IndexOutOfBounds (i, num))
                    elif not seen.[num] then
                        seen.[num] <- true
                    else
                        valid <- false
                    j <- j + 1
                result.[i] <- valid
                i <- i + 1
            match error with
            | Some e -> Error e
            | None -> Ok result



    let isPermutationArraySegmentSafe2
            (longArray: ^a[]) (segmentLength: int) : Result<bool[], IsPermutationArraySegmentError> =
        if isNull longArray then Error NullLongArray
        elif segmentLength <= 0 then Error (InvalidSegmentLength segmentLength)
        elif longArray.Length = 0 then Error (InvalidLongArrayLength (longArray.Length, 0))
        elif longArray.Length % segmentLength <> 0 then 
            Error (NonWholeNumberSegments (longArray.Length, segmentLength))
        else
            let m = longArray.Length / segmentLength
            let result = Array.zeroCreate m
            let mutable error = None
            let mutable i = 0
            while i < m && error.IsNone do
                let seen = Array.create segmentLength false
                let mutable valid = true
                let mutable j = 0
                while j < segmentLength && valid && error.IsNone do
                    let num = int longArray.[i * segmentLength + j]
                    if num < 0 || num >= segmentLength then
                        error <- Some (IndexOutOfBounds (i, num))
                    elif not seen.[num] then
                        seen.[num] <- true
                    else
                        valid <- false
                    j <- j + 1
                result.[i] <- valid
                i <- i + 1
            match error with
            | Some e -> Error e
            | None -> Ok result


    type IsTwoCycleError =
        | NullArray
        | NegativeIndex of value: int
        | IndexOutOfBounds of value: int * arrayLength: int

    let inline isTwoCycle< ^a when ^a: (static member op_Explicit: ^a -> int)> (values: ^a[]) 
                    : Result<bool, IsTwoCycleError> = 
        if isNull values then Error NullArray
        elif values.Length = 0 then Ok true
        else
            let mutable noProblems = true
            let mutable index = 0
            let mutable error = None

            while (index < values.Length && error.IsNone && noProblems) do
                let value = int values.[index]
                if value < 0 then
                    error <- Some (NegativeIndex value)
                elif value >= values.Length then
                    error <- Some (IndexOutOfBounds (value, values.Length))
                elif value = index then
                    index <- index + 1
                else
                    let nextValue = int values.[value]
                    if nextValue = index then
                        index <- index + 1
                    else
                        noProblems <- false

            match error with
            | Some e -> Error e
            | None -> Ok (noProblems)



    type DistanceSquaredArraySegmentError =
        | NullLongArray
        | NullShortArray
        | InvalidLongArrayLength of expectedMultiple: int * actual: int
        | InvalidShortArrayLength of length: int
        | NonWholeNumberSegments of longLength: int * shortLength: int

    // Safe, Non-Span version
    let inline distanceSquaredArraySegmentSafe< ^a when ^a: (static member Zero: ^a)
                                                  and ^a: (static member (+): ^a * ^a -> ^a)
                                                  and ^a: (static member (-): ^a * ^a -> ^a)
                                                  and ^a: (static member (*): ^a * ^a -> ^a)>
            (longArray: ^a[]) (shortArray: ^a[]) : Result<^a[], DistanceSquaredArraySegmentError> =
        if isNull longArray then Error NullLongArray
        elif isNull shortArray then Error NullShortArray
        elif shortArray.Length = 0 then Error (InvalidShortArrayLength shortArray.Length)
        elif longArray.Length = 0 then Error (InvalidLongArrayLength (shortArray.Length, 0))
        elif longArray.Length % shortArray.Length <> 0 then 
            Error (NonWholeNumberSegments (longArray.Length, shortArray.Length))
        else
            let n = shortArray.Length
            let m = longArray.Length / n
            let result = Array.zeroCreate m
            for i = 0 to m - 1 do
                let mutable acc = GenericZero<^a>
                for j = 0 to n - 1 do
                    let diff = longArray.[i * n + j] - shortArray.[j]
                    acc <- acc + diff * diff
                result.[i] <- acc
            Ok result


    // Unsafe, Non-Span version
    let inline distanceSquaredArraySegmentUnsafe< ^a when ^a: (static member Zero: ^a)
                                                    and ^a: (static member (+): ^a * ^a -> ^a)
                                                    and ^a: (static member (-): ^a * ^a -> ^a)
                                                    and ^a: (static member (*): ^a * ^a -> ^a)>
            (longArray: ^a[]) (shortArray: ^a[]) : ^a[] =
        let n = shortArray.Length
        let m = longArray.Length / n
        let result = Array.zeroCreate m
        for i = 0 to m - 1 do
            let mutable acc = GenericZero<^a>
            for j = 0 to n - 1 do
                let diff = longArray.[i * n + j] - shortArray.[j]
                acc <- acc + diff * diff
            result.[i] <- acc
        result
