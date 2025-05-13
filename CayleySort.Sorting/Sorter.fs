namespace CayleySort.Sorting

open System
open FSharp.UMX


type Sorter =
        { SorterId: Guid<sorterId>
          Width: int<sorterWidth>
          Ces: Ce array }


// Core module for Sorter operations
module Sorter =
    type SorterError =
        | InvalidId of string
        | InvalidWidth of string
        | InvalidCes of string

    let create (sorterId: Guid<sorterId>) (width: int<sorterWidth>) (ces: Ce array) : Result<Sorter, SorterError> =
        if %sorterId = Guid.Empty then
            Error (InvalidId "Sorter ID must not be empty")
        else if %width < 1 then
            Error (InvalidWidth "Width must be at least 1")
        else if not (Array.forall (fun ce -> ce.Low >= 0 && ce.Low <= ce.Hi && ce.Hi < %width) ces) then
            Error (InvalidCes "Ces must have valid indices within width (0 <= Low <= Hi < width)")
        else
            Ok { SorterId = sorterId; Width = width; Ces = ces }

    let createWithNewId (width: int<sorterWidth>) (ces: Ce array) : Result<Sorter, SorterError> =
        create (UMX.tag<sorterId> (Guid.NewGuid())) width ces

    let toString (sorter: Sorter) : string =
        let cesStr = sorter.Ces |> Array.map Ce.toString |> String.concat "; "
        sprintf "Sorter(Id=%A, Width=%d, Ces=[%s])" (%sorter.SorterId) (%sorter.Width) cesStr