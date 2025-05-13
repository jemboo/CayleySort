namespace CayleySort.Sorting

open System
open FSharp.UMX

type SorterSet =
        { SorterSetId: Guid<sorterSetId>
          Sorters: Sorter array }


// Core module for SorterSet operations
module SorterSet =
    type SorterSetError =
        | InvalidId of string
        | InvalidSorters of string

    let create (sorterSetId: Guid<sorterSetId>) (sorters: Sorter array) : Result<SorterSet, SorterSetError> =
        if %sorterSetId = Guid.Empty then
            Error (InvalidId "SorterSet ID must not be empty")
        else if Array.isEmpty sorters then
            Error (InvalidSorters "Sorter set must contain at least one sorter")
        else
            Ok { SorterSetId = sorterSetId; Sorters = sorters }

    let createWithNewId (sorters: Sorter array) : Result<SorterSet, SorterSetError> =
        create (UMX.tag<sorterSetId> (Guid.NewGuid())) sorters

    let toString (sorterSet: SorterSet) : string =
        let sortersStr = sorterSet.Sorters |> Array.map Sorter.toString |> String.concat "; "
        sprintf "SorterSet(Id=%A, Sorters=[%s])" (%sorterSet.SorterSetId) sortersStr
