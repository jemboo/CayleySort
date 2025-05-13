namespace CayleySort.Sorting

open System

// Type definitions
[<Struct; CustomEquality; NoComparison>]
type Ce = 
    { Low: int; Hi: int }
    override this.Equals(obj) = 
        match obj with
        | :? Ce as other -> this.Low = other.Low && this.Hi = other.Hi
        | _ -> false
    override this.GetHashCode() = 
        hash (this.Low, this.Hi)
    interface IEquatable<Ce> with
        member this.Equals(other) = 
            this.Low = other.Low && this.Hi = other.Hi

// Core module for Ce operations
module Ce =
    type CeError =
        | InvalidIndices of string
        | InvalidIndex of string

    let create (low: int) (hi: int) : Result<Ce, CeError> =
        if low < 0 || hi < 0 then
            Error (InvalidIndices "Indices must be non-negative")
        else
            Ok { Low = low; Hi = hi }

    let toString (ce: Ce) : string =
        sprintf "(%d, %d)" ce.Low ce.Hi

    let toIndex (ce: Ce) : Result<int, CeError> =
        let i = ce.Low
        let j = ce.Hi
        if i < 0 || j < 0 then
            Error (InvalidIndices "Indices must be non-negative")
        else
            let index = (j * (j + 1)) / 2 + i
            Ok index

    let fromIndex (dex:int) = 
        if dex < 0 then
            Error (InvalidIndex "Index must be non-negative")
        else
        let indexFlt = (dex |> float) + 1.0
        let p = (sqrt (1.0 + 8.0 * indexFlt) - 1.0) / 2.0
        let pfloor = Math.Floor(p)
        if (p = pfloor) then
            Ok {Low= (int pfloor) - 1; Hi = (int pfloor) - 1 }
        else
            let lo = (float dex) - (pfloor * (pfloor + 1.0)) / 2.0 |> int
            let hi = (int pfloor)
            Ok {Low = lo; Hi = hi }

