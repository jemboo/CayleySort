

namespace CayleySort.Sorting.Tests

open Xunit
open System
open FSharp.UMX
open MessagePack
open CayleySort.Sorting
open CayleySort.Serialization
open CayleySort.Serialization.SorterSerialization


type SerializationTests() =

    [<Fact>]
    let ``Sorter serialization round-trip``() =
        let ces = [| { Low = 0; Hi = 1 }; { Low = 1; Hi = 2 } |]
        match Sorter.createWithNewId (UMX.tag<sorterWidth> 4) ces with
        | Ok sorter ->
            match toBinary sorter with
            | Ok data ->
                match fromBinary data with
                | Ok sorter' ->
                    Assert.Equal(%sorter.SorterId, %sorter'.SorterId)
                    Assert.Equal(%sorter.Width, %sorter'.Width)
                    Assert.Equal(sorter.Ces |> Array.toList, sorter'.Ces)
                | Error err ->
                    Assert.True(false, $"Deserialization failed: {err}")
            | Error err ->
                Assert.True(false, $"Serialization failed: {err}")
        | Error err ->
            Assert.True(false, $"create failed: {err}")


    [<Fact>]
    let ``Sorter serialization rejects out-of-bounds Ces``() =
        let dto = {
            SorterDto.SorterId = Guid.Parse("00000000-0000-0000-0000-000000000001")
            Width = 4
            Ces = [| 2; 7; 100 |]
        }
        let wrapper = {
            Type = "Sorter"
            Data = MessagePackSerializer.Serialize(dto)
        }
        let data = MessagePackSerializer.Serialize(wrapper)
        match fromBinary data with
        | Ok _ -> Assert.True(false, "Expected error for out-of-bounds Ce")
        | Error err -> ()


    [<Fact>]
    let ``SorterSet serialization round-trip``() =
        let ces = [| { Low = 0; Hi = 1 }; { Low = 1; Hi = 2 } |]
        match Sorter.createWithNewId (UMX.tag<sorterWidth> 4) ces with
        | Ok sorter ->
            match SorterSet.createWithNewId [| sorter |] with
            | Ok sorterSet ->
                match SorterSetSerialization.toBinary sorterSet with
                | Ok data ->
                    match SorterSetSerialization.fromBinary data with
                    | Ok sorterSet' ->
                        Assert.Equal(%sorterSet.SorterSetId, %sorterSet'.SorterSetId)
                        Assert.Equal(sorterSet'.Sorters|> Array.length, 1)
                        Assert.Equal(sorterSet.Sorters.[0].Ces |> Array.toList, sorterSet'.Sorters.[0].Ces)
                    | Error err ->
                        Assert.True(false, $"Deserialization failed: {err}")
                | Error err ->
                    Assert.True(false, $"Serialization failed: {err}")
            | Error err ->
                Assert.True(false, $"create failed: {err}")
        | Error err ->
            Assert.True(false, $"create failed: {err}")



// Helper function for writing temporary files
//let private writeTempFile (content: byte[]) : byte[] =
//    let path = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString() + ".bin")
//    File.WriteAllBytes(path, content)
//    File.ReadAllBytes(path)