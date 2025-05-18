
namespace CayleySort.Core
open System
open SequenceProperties
open ArrayMap

type CoreError =
    | InvertArrayMapError of InvertArrayMapError
    | ArrayMapProductError of ArrayMapProductError
    | SortedError of IsSortedError
    | PermutationError of IsPermutationError
    | UnsortednessError of DistanceSquaredError
    | TwoCycleError of IsTwoCycleError

type AnalysisResult< ^a> = {
    IsSorted: bool
    IsSubarraySorted: bool
    IsPermutation: bool
    UnsortednessSquared: ^a
}

module CoreError =

    let inline analyzeArray< ^a when ^a: (static member Zero: ^a)
                             and ^a: (static member (+): ^a * ^a -> ^a)
                             and ^a: (static member (-): ^a * ^a -> ^a)
                             and ^a: (static member (*): ^a * ^a -> ^a)
                             and ^a: comparison
                             and ^a: (static member op_Explicit: ^a -> int)>
            (arr: ^a[]) offset length : Result<AnalysisResult< ^a>, CoreError> =

        let mapSortedError = Result.mapError SortedError
        let mapPermutationError = Result.mapError PermutationError
        let mapUnsortednessError = Result.mapError UnsortednessError

        result {
            let! isSorted = isSorted arr |> mapSortedError
            let! isSubarraySorted = isSortedOffset arr offset length |> mapSortedError
            let! isPermutation = isPermutation arr |> mapPermutationError
            let! unsortedness = unsortednessSquared arr |> mapUnsortednessError
            return {
                IsSorted = isSorted
                IsSubarraySorted = isSubarraySorted
                IsPermutation = isPermutation
                UnsortednessSquared = unsortedness
            }
        }