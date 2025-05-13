namespace CayleySort.Sorting

open System
open System.IO
open System.Text.Json
open FSharp.UMX



// DTOs for serialization
type SorterDto = {
    SorterId: Guid
    Width: int
    Ces: int array
}

type SorterSetDto = {
    SorterSetId: Guid
    Sorters: SorterDto array
}

type SerializationDto = {
    Type: string
    Data: JsonElement
}



// Serialization module for Sorter
module SorterSerialization =
    type SorterSerializationError =
        | FileError of string
        | ParseError of string
        | IndexError of string
        | ValidationError of string

    let toJson (sorter: Sorter) : Result<string, SorterSerializationError> =
        let ceResults = 
            sorter.Ces 
            |> Array.map Ce.toIndex
            |> Array.toList
        let rec combineResults acc = function
            | [] -> Ok (Array.ofList (List.rev acc))
            | Ok index :: rest -> combineResults (index :: acc) rest
            | Error err :: _ -> Error (IndexError $"Failed to compute index: {err}")
        match combineResults [] ceResults with
        | Ok ceIndices ->
            let dto = {
                SorterId = %sorter.SorterId
                Width = %sorter.Width
                Ces = ceIndices
            }
            let wrapper = {
                Type = "Sorter"
                Data = JsonSerializer.SerializeToElement(dto)
            }
            Ok (JsonSerializer.Serialize(wrapper))
        | Error err -> Error err

    let fromFile (filePath: string) : Result<Sorter, SorterSerializationError> =
        try
            let json = File.ReadAllText(filePath)
            let wrapper = JsonSerializer.Deserialize<SerializationDto>(json)
            if wrapper.Type <> "Sorter" then
                Error (ParseError $"Expected type 'Sorter', got '{wrapper.Type}'")
            else
                let dto = JsonSerializer.Deserialize<SorterDto>(wrapper.Data)
                let ceResults = 
                    dto.Ces 
                    |> Array.map Ce.fromIndex
                    |> Array.toList
                let rec combineResults acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok ce :: rest -> combineResults (ce :: acc) rest
                    | Error err :: _ -> Error (IndexError $"Failed to deserialize Ce: {err}")
                match combineResults [] ceResults with
                | Ok ces -> 
                    match Sorter.create (UMX.tag<sorterId> dto.SorterId) (UMX.tag<sorterWidth> dto.Width) ces with
                    | Ok sorter -> Ok sorter
                    | Error err -> Error (ValidationError $"Validation failed: {err}")
                | Error err -> Error err
        with
        | :? IOException as ex -> Error (FileError $"Failed to read file: {ex.Message}")
        | :? JsonException as ex -> Error (ParseError $"Invalid JSON format: {ex.Message}")

// Serialization module for SorterSet
module SorterSetSerialization =
    type SorterSetSerializationError =
        | FileError of string
        | ParseError of string
        | IndexError of string
        | ValidationError of string

    let toJson (sorterSet: SorterSet) : Result<string, SorterSetSerializationError> =
        let sorterResults = 
            sorterSet.Sorters 
            |> Array.map (fun s -> 
                let ceResults = s.Ces |> Array.map Ce.toIndex |> Array.toList
                let rec combineResults acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok index :: rest -> combineResults (index :: acc) rest
                    | Error err :: _ -> Error (IndexError $"Failed to compute index: {err}")
                match combineResults [] ceResults with
                | Ok indices -> 
                    Ok { SorterId = %s.SorterId; Width = %s.Width; Ces = indices }
                | Error err -> Error err)
            |> Array.toList
        let rec combineSorters acc = function
            | [] -> Ok (Array.ofList (List.rev acc))
            | Ok sorter :: rest -> combineSorters (sorter :: acc) rest
            | Error err :: _ -> Error err
        match combineSorters [] sorterResults with
        | Ok sorters ->
            let dto = {
                SorterSetId = %sorterSet.SorterSetId
                Sorters = sorters
            }
            let wrapper = {
                Type = "SorterSet"
                Data = JsonSerializer.SerializeToElement(dto)
            }
            Ok (JsonSerializer.Serialize(wrapper))
        | Error err -> Error err

    let fromFile (filePath: string) : Result<SorterSet, SorterSetSerializationError> =
        try
            let json = File.ReadAllText(filePath)
            let wrapper = JsonSerializer.Deserialize<SerializationDto>(json)
            if wrapper.Type <> "SorterSet" then
                Error (ParseError $"Expected type 'SorterSet', got '{wrapper.Type}'")
            else
                let dto = JsonSerializer.Deserialize<SorterSetDto>(wrapper.Data)
                let sorterResults = 
                    dto.Sorters 
                    |&gt; Array.map (fun s -> 
                        let ceResults = s.Ces |> Array.map Ce.fromIndex |> Array.toList
                        let rec combineResults acc = function
                            | [] -> Ok (Array.ofList (List.rev acc))
                            | Ok ce :: rest -> combineResults (ce :: acc) rest
                            | Error err :: _ -> Error (IndexError $"Failed to deserialize Ce: {err}")
                        match combineResults [] ceResults with
                        | Ok ces -> Sorter.create (UMX.tag<sorterId> s.SorterId) (UMX.tag<sorterWidth> s.Width) ces
                        | Error err -> Error err)
                    |> Array.toList
                let rec combineSorters acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok sorter :: rest -> combineSorters (sorter :: acc) rest
                    | Error err :: _ -> Error (ValidationError $"Failed to create sorter: {err}")
                match combineSorters [] sorterResults with
                | Ok sorters -> 
                    match SorterSet.create (UMX.tag<sorterSetId> dto.SorterSetId) sorters with
                    | Ok sorterSet -> Ok sorterSet
                    | Error err -> Error (ValidationError $"Validation failed: {err}")
                | Error err -> Error err
        with
        | :? IOException as ex -> Error (FileError $"Failed to read file: {ex.Message}")
        | :? JsonException as ex -> Error (ParseError $"Invalid JSON format: {ex.Message}")

// Shared serialization module for generic deserialization
module Serialization =
    type SerializationError =
        | FileError of string
        | ParseError of string
        | UnknownType of string
        | IndexError of string
        | ValidationError of string

    type DeserializedType =
        | Sorter of Sorter
        | SorterSet of SorterSet

    let fromFile (filePath: string) : Result<DeserializedType, SerializationError> =
        try
            let json = File.ReadAllText(filePath)
            let wrapper = JsonSerializer.Deserialize<SerializationDto>(json)
            match wrapper.Type with
            | "Sorter" ->
                let dto = JsonSerializer.Deserialize<SorterDto>(wrapper.Data)
                let ceResults = 
                    dto.Ces 
                    |> Array.map Ce.fromIndex
                    |> Array.toList
                let rec combineResults acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok ce :: rest -> combineResults (ce :: acc) rest
                    | Error err :: _ -> Error (IndexError $"Failed to deserialize Ce: {err}")
                match combineResults [] ceResults with
                | Ok ces -> 
                    match Sorter.create (UMX.tag<sorterId> dto.SorterId) (UMX.tag<sorterWidth> dto.Width) ces with
                    | Ok sorter -> Ok (Sorter sorter)
                    | Error err -> Error (ValidationError $"Failed to create sorter: {err}")
                | Error err -> Error err
            | "SorterSet" ->
                let dto = JsonSerializer.Deserialize<SorterSetDto>(wrapper.Data)
                let sorterResults = 
                    dto.Sorters 
                    |> Array.map (fun s -> 
                        let ceResults = s.Ces |> Array.map Ce.fromIndex |> Array.toList
                        let rec combineResults acc = function
                            | [] -> Ok (Array.ofList (List.rev acc))
                            | Ok ce :: rest -> combineResults (ce :: acc) rest
                            | Error err :: _ -> Error (IndexError $"Failed to deserialize Ce: {err}")
                        match combineResults [] ceResults with
                        | Ok ces -> Sorter.create (UMX.tag<sorterId> s.SorterId) (UMX.tag<sorterWidth> s.Width) ces
                        | Error err -> Error err)
                    |> Array.toList
                let rec combineSorters acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok sorter :: rest -> combineSorters (sorter :: acc) rest
                    | Error err :: _ -> Error (ValidationError $"Failed to create sorter: {err}")
                match combineSorters [] sorterResults with
                | Ok sorters -> 
                    match SorterSet.create (UMX.tag<sorterSetId> dto.SorterSetId) sorters with
                    | Ok sorterSet -> Ok (SorterSet sorterSet)
                    | Error err -> Error (ValidationError $"Failed to create sorter set: {err}")
                | Error err -> Error err
            | unknown -> Error (UnknownType $"Unknown type: {unknown}")
        with
        | :? IOException as ex -> Error (FileError $"Failed to read file: {ex.Message}")
        | :? JsonException as ex -> Error (ParseError $"Invalid JSON format: {ex.Message}")