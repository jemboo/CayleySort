namespace CayleySort.Sorting.Tests

open System
open Xunit
open FSharp.UMX
open CayleySort.Sorting
open CayleySort.Sorting.Ce

type CeTests() =

    [<Fact>]
    let ``toIndex and fromIndex round-trip for valid Ce including Low = Hi``() =
        let ces = [ 
                    { Low = 0; Hi = 0 }; { Low = 0; Hi = 1 }; { Low = 0; Hi = 2 }; { Low = 0; Hi = 3 }; { Low = 0; Hi = 4 }; { Low = 0; Hi = 5 }; 
                    { Low = 1; Hi = 1 }; { Low = 1; Hi = 2 }; { Low = 1; Hi = 3 }; { Low = 1; Hi = 4 }; { Low = 1; Hi = 5 }; { Low = 1; Hi = 6 }; 
                    { Low = 2; Hi = 2 }; { Low = 2; Hi = 3 }; { Low = 2; Hi = 4 }; { Low = 2; Hi = 5 }; { Low = 2; Hi = 6 }; { Low = 2; Hi = 7 }; 
                    { Low = 3; Hi = 3 }; { Low = 3; Hi = 4 }; { Low = 3; Hi = 5 }; { Low = 3; Hi = 6 }; { Low = 3; Hi = 7 }; { Low = 3; Hi = 8 }; 
                    { Low = 4; Hi = 4 }; { Low = 4; Hi = 5 }; { Low = 4; Hi = 6 }; { Low = 4; Hi = 7 }; { Low = 4; Hi = 8 }; { Low = 4; Hi = 9 }; 
                    { Low = 5; Hi = 5 }; { Low = 5; Hi = 6 }; { Low = 5; Hi = 7 }; { Low = 5; Hi = 8 }; { Low = 5; Hi = 9 }; { Low = 5; Hi = 10 }; 
                  ]

        let indexes = ces |> List.map(toIndex >> Result.ExtractOrThrow)
        let cesBack = indexes |> List.map(fromIndex >> Result.ExtractOrThrow)


        for ce in ces do
            match toIndex ce with
            | Ok index ->
                match fromIndex index with
                | Ok ce' ->
                    Assert.Equal(ce, ce')
                | Error err ->
                    Assert.True(false, $"fromIndex failed for index {index}: {err}")
            | Error err ->
                Assert.True(false, $"toIndex failed for {Ce.toString ce}: {err}")

    [<Theory>]
    [<InlineData(-1, 0, "Negative Low")>]
    [<InlineData(0, -1, "Negative Hi")>]
    let ``toIndex handles invalid Ce`` (low: int, hi: int, desc: string) =
        let ce = { Low = low; Hi = hi }
        match toIndex ce with
        | Ok _ -> Assert.True(false, $"{desc}: Expected error for {Ce.toString ce}")
        | Error _ -> ()

    [<Theory>]
    [<InlineData(-1, 1, "Negative Low")>]
    [<InlineData(1, -1, "Negative Hi")>]
    [<InlineData(-1, -1, "Both negative")>]
    let ``Ce.toIndex rejects negative indices`` (low: int, hi: int, desc: string) =
        match Ce.create low hi with
        | Ok ce ->
            match toIndex ce with
            | Ok _ -> Assert.True(false, $"{desc}: Expected InvalidIndices for {Ce.toString ce}")
            | Error (InvalidIndices _) -> ()
            | Error err -> Assert.True(false, $"{desc}: Unexpected error for {Ce.toString ce}: {err}")
        | Error _ -> () // Expected, as create should fail

    [<Theory>]
    [<InlineData(-1)>]
    [<InlineData(-2)>]
    let ``fromIndex handles invalid index`` (index: int) =
        match fromIndex index with
        | Ok _ -> Assert.True(false, $"Expected InvalidIndex for index {index}")
        | Error (InvalidIndex _) -> ()
        | Error err -> Assert.True(false, $"Unexpected error for index {index}: {err}")


    [<Fact>]
    let ``create handles Low = Hi``() =
        let ce = { Low = 1; Hi = 1 }
        match Ce.create 1 1 with
        | Ok ce' ->
            Assert.Equal(ce, ce')
        | Error err ->
            Assert.True(false, $"create failed for (1,1): {err}")


type SorterTests() =

    [<Fact>]
    let ``Sorter.create validates width constraints``() =
        let validCe = { Low = 0; Hi = 1 }
        let noOpCe = { Low = 1; Hi = 1 }
        let outOfBoundsCe = { Low = 0; Hi = 4 }
        let ces = [| validCe; noOpCe; outOfBoundsCe |]
        match Sorter.createWithNewId (UMX.tag<sorterWidth> 4) ces with
        | Ok _ -> Assert.True(false, "Expected error for out-of-bounds Ce")
        | Error err -> ()

    [<Fact>]
    let ``Sorter.create allows Low = Hi within width``() =
        let ces = [| { Low = 0; Hi = 1 }; { Low = 1; Hi = 1 } |]
        match Sorter.createWithNewId (UMX.tag<sorterWidth> 4) ces with
        | Ok sorter ->
            Assert.Equal(2, sorter.Ces.Length)
        | Error err ->
            Assert.True(false, $"create failed: {err}")



type SorterSetTests() =

    [<Fact>]
    let ``SorterSet.create creates valid SorterSet``() =
        let createSorter ces =
            match Sorter.createWithNewId (UMX.tag<sorterWidth> 4) ces with
            | Ok sorter -> sorter
            | Error err -> failwith $"Sorter.create failed: {err}"
        
        let ces1 = [| { Low = 0; Hi = 1 }; { Low = 1; Hi = 1 } |]
        let ces2 = [| { Low = 0; Hi = 2 }; { Low = 1; Hi = 2 } |]
        let sorters = [| createSorter ces1; createSorter ces2 |]
        
        match SorterSet.createWithNewId sorters with
        | Ok sorterSet ->
            Assert.NotEqual(Guid.Empty, %sorterSet.SorterSetId)
            Assert.Equal(2, (%sorterSet.Sorters.Length * 1<sorterCount>))
            Assert.Equal(ces1 |> Array.toList, sorterSet.Sorters.[0].Ces)
            Assert.Equal(ces2 |> Array.toList, sorterSet.Sorters.[1].Ces)
            Assert.Equal(4, %sorterSet.Sorters.[0].Width)
            Assert.Equal(4, %sorterSet.Sorters.[1].Width)
        | Error err ->
            Assert.True(false, $"SorterSet.create failed: {err}")

