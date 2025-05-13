namespace CayleySort.Serialization
open MessagePack
open FSharp.UMX
open CayleySort.Sorting

// DTOs for serialization
[<MessagePackObject>]
type SerializationDto = {
    [<Key(0)>] Type: string
    [<Key(1)>] Data: byte array
}

[<MessagePackObject>]
type SorterDto = {
    [<Key(0)>] SorterId: System.Guid
    [<Key(1)>] Width: int
    [<Key(2)>] Ces: int array
}

// Serialization module for Sorter
module SorterSerialization =
    type SorterSerializationError =
        | FileError of string
        | ParseError of string
        | IndexError of string
        | ValidationError of string

    let toBinary (sorter: Sorter) : Result<byte[], SorterSerializationError> =
        try
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
                    Data = MessagePackSerializer.Serialize(dto)
                }
                Ok (MessagePackSerializer.Serialize(wrapper))
            | Error err -> Error err
        with
        | :? MessagePackSerializationException as ex -> Error (ParseError $"Serialization failed: {ex.Message}")

    let fromBinary (data: byte[]) : Result<Sorter, SorterSerializationError> =
        try
            let wrapper = MessagePackSerializer.Deserialize<SerializationDto>(data)
            if wrapper.Type <> "Sorter" then
                Error (ParseError $"Expected type 'Sorter', got '{wrapper.Type}'")
            else
                let dto = MessagePackSerializer.Deserialize<SorterDto>(wrapper.Data)
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
        | :? MessagePackSerializationException as ex -> Error (ParseError $"Deserialization failed: {ex.Message}")


[<MessagePackObject>]
type SorterSetDto = {
    [<Key(0)>] SorterSetId: System.Guid
    [<Key(1)>] Sorters: SorterDto array
}

// Serialization module for SorterSet
module SorterSetSerialization =
    type SorterSetSerializationError =
        | FileError of string
        | ParseError of string
        | IndexError of string
        | ValidationError of string

    let toBinary (sorterSet: SorterSet) : Result<byte[], SorterSetSerializationError> =
        try
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
                    Data = MessagePackSerializer.Serialize(dto)
                }
                Ok (MessagePackSerializer.Serialize(wrapper))
            | Error err -> Error err
        with
        | :? MessagePackSerializationException as ex -> Error (ParseError $"Serialization failed: {ex.Message}")


    let fromBinary (data: byte[]) : Result<SorterSet, SorterSetSerializationError> =
        try
            let wrapper = MessagePackSerializer.Deserialize<SerializationDto>(data)
            if wrapper.Type <> "SorterSet" then
                Error (ParseError $"Expected type 'SorterSet', got '{wrapper.Type}'")
            else
                let dto = MessagePackSerializer.Deserialize<SorterSetDto>(wrapper.Data)
                let sorterResults = 
                    dto.Sorters 
                    |> Array.map (fun s -> 
                        let ceResults = s.Ces |> Array.map Ce.fromIndex |> Array.toList
                        let rec combineResults acc = function
                            | [] -> Ok (Array.ofList (List.rev acc))
                            | Ok ce :: rest -> combineResults (ce :: acc) rest
                            | Error err :: _ -> Error (IndexError $"Failed to deserialize Ce: {err}")
                        match combineResults [] ceResults with
                        | Ok ces -> 
                            match Sorter.create (UMX.tag<sorterId> s.SorterId) (UMX.tag<sorterWidth> s.Width) ces with
                            | Ok sorter -> Ok sorter
                            | Error err -> Error (ValidationError $"Validation failed: {err}")
                        | Error err -> Error err)
                    |> Array.toList
                let rec combineSorters acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok sorter :: rest -> combineSorters (sorter :: acc) rest
                    | Error err :: _ -> Error err
                match combineSorters [] sorterResults with
                | Ok sorters -> 
                    match SorterSet.create (UMX.tag<sorterSetId> dto.SorterSetId) sorters with
                    | Ok sorterSet -> Ok sorterSet
                    | Error err -> Error (ValidationError $"Validation failed: {err}")
                | Error err -> Error err
        with
        | :? MessagePackSerializationException as ex -> Error (ParseError $"Deserialization failed: {ex.Message}")


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

    let fromBinary (data: byte[]) : Result<DeserializedType, SerializationError> =
        try
            let wrapper = MessagePackSerializer.Deserialize<SerializationDto>(data)
            match wrapper.Type with
            | "Sorter" ->
                let dto = MessagePackSerializer.Deserialize<SorterDto>(wrapper.Data)
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
                let dto = MessagePackSerializer.Deserialize<SorterSetDto>(wrapper.Data)
                let sorterResults = 
                    dto.Sorters 
                    |> Array.map (fun s -> 
                        let ceResults = s.Ces |> Array.map Ce.fromIndex |> Array.toList
                        let rec combineResults acc = function
                            | [] -> Ok (Array.ofList (List.rev acc))
                            | Ok ce :: rest -> combineResults (ce :: acc) rest
                            | Error err :: _ -> Error (IndexError $"Failed to deserialize Ce: {err}")
                        match combineResults [] ceResults with
                        | Ok ces -> 
                            match Sorter.create (UMX.tag<sorterId> s.SorterId) (UMX.tag<sorterWidth> s.Width) ces with
                            | Ok sorter -> Ok sorter
                            | Error err -> Error (ValidationError $"Validation failed: {err}")
                        | Error err -> Error err)
                    |> Array.toList
                let rec combineSorters acc = function
                    | [] -> Ok (Array.ofList (List.rev acc))
                    | Ok sorter :: rest -> combineSorters (sorter :: acc) rest
                    | Error err :: _ -> Error err
                match combineSorters [] sorterResults with
                | Ok sorters -> 
                    match SorterSet.create (UMX.tag<sorterSetId> dto.SorterSetId) sorters with
                    | Ok sorterSet -> Ok (SorterSet sorterSet)
                    | Error err -> Error (ValidationError $"Failed to create sorter set: {err}")
                | Error err -> Error err
            | unknown -> Error (UnknownType $"Unknown type: {unknown}")
        with
        | :? MessagePackSerializationException as ex -> Error (ParseError $"Deserialization failed: {ex.Message}")

