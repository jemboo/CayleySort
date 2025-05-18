module ArrayProductTests

open Xunit
open FsUnit.Xunit
open CayleySort.Core.ArrayMap


// Tests for unsafe version
[<Fact>]
let ``Unsafe: Computes array product correctly`` () =
    let lhs = [|10; 20; 30|]
    let rhs = [|2; 0; 1|]
    let prod = Array.zeroCreate<int> 3
    let result = arrayMapProductUnsafe lhs rhs prod
    Assert.Equal<int list>([30; 10; 20], result |> Seq.toList)

[<Fact>]
let ``Unsafe: Handles empty lhs array`` () =
    let lhs: int[] = [||]
    let rhs = [|0; 1|]
    let prod = Array.zeroCreate<int> 0
    let result = arrayMapProductUnsafe lhs rhs prod
    Assert.Equal<int list>([], result |> Seq.toList)

[<Fact>] 
let ``Unsafe: Throws IndexOutOfRangeException for invalid index`` () =
    let lhs = [|10; 20|]
    let rhs = [|2|]
    let prod = Array.zeroCreate<int> 1
    Assert.Throws<System.IndexOutOfRangeException>(fun () -> arrayMapProductUnsafe lhs rhs prod |> ignore)

[<Fact>]
let ``Unsafe: Throws IndexOutOfRangeException for insufficient prod length`` () =
    let lhs = [|10; 20|]
    let rhs = [|0; 1|]
    let prod = Array.zeroCreate<int> 1
    Assert.Throws<System.IndexOutOfRangeException>(fun () -> arrayMapProductUnsafe lhs rhs prod |> ignore)

// Tests for safe version
[<Fact>]
let ``Safe: Computes array product correctly`` () =
    let lhs = [|10; 20; 30|]
    let rhs = [|2; 0; 1|]
    let prod = Array.zeroCreate<int> 3
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Ok arr -> Assert.Equal<int list>([30; 10; 20], arr |> Seq.toList)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    Assert.Equal<int list>([30; 10; 20], prod |> Seq.toList)

[<Fact>]
let ``Safe: Handles empty lhs array`` () =
    let lhs: int[] = [||]
    let rhs = [|0; 1|]
    let prod = Array.zeroCreate<int> 0
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Ok arr -> Assert.Equal<int list>([], arr |> Seq.toList)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    Assert.Equal<int list>([], prod |> Seq.toList)

[<Fact>]
let ``Safe: Works with int64 type`` () =
    let lhs = [|10L; 20L; 30L|]
    let rhs = [|2L; 0L; 1L|]
    let prod = Array.zeroCreate<int64> 3
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Ok arr -> Assert.Equal<int64 list>([30L; 10L; 20L], arr |> Seq.toList)
    | Error e -> Assert.Fail($"Expected Ok, got Error: {e}")
    Assert.Equal<int64 list>([30L; 10L; 20L], prod |> Seq.toList)

[<Fact>]
let ``Safe: Returns NullLeftArray for null lhs`` () =
    let lhs: int[] = null
    let rhs = [|0; 1|]
    let prod = Array.zeroCreate<int> 2
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Error NullLeftArray -> ()
    | _ -> Assert.Fail("Expected Error NullLeftArray")

[<Fact>]
let ``Safe: Returns NullRightArray for null rhs`` () =
    let lhs = [|10; 20|]
    let rhs: int[] = null
    let prod = Array.zeroCreate<int> 2
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Error NullRightArray -> ()
    | _ -> Assert.Fail("Expected Error NullRightArray")

[<Fact>]
let ``Safe: Returns NullProductArray for null prod`` () =
    let lhs = [|10; 20|]
    let rhs = [|0; 1|]
    let prod: int[] = null
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Error NullProductArray -> ()
    | _ -> Assert.Fail("Expected Error NullProductArray")

[<Fact>]
let ``Safe: Returns InsufficientRightLength for short rhs`` () =
    let lhs = [|10; 20; 30|]
    let rhs = [|0; 1|]
    let prod = Array.zeroCreate<int> 3
    let result = arrayMapProductSafe lhs rhs prod
    match result with
    | Error (InsufficientRightLength (3, 2)) -> ()
    | _ -> Assert.Fail("Expected Error (InsufficientRightLength (3, 2))")

[<Fact>]
let ``Safe: Returns InsufficientProductLength for short prod`` () =
    let lhs = [|10; 20; 30|]
    let rhs = [|0; 1; 2|]
    let prod = Array.zeroCreate<int> 2
    let result = arrayMapProductSafe lhs rhs prod
    Assert.Equal(Error (InsufficientProductLength (3, 2)), result)

[<Fact>]
let ``Safe: Returns IndexOutOfBounds for invalid index`` () =
    let lhs = [|10; 20|]
    let rhs = [|2; 0|]
    let prod = Array.zeroCreate<int> 2
    let result = arrayMapProductSafe lhs rhs prod
    Assert.Equal(Error (IndexOutOfBounds 2), result)

[<Fact>]
let ``Safe: Returns IndexOutOfBounds for negative index`` () =
    let lhs = [|10; 20|]
    let rhs = [|-1; 0|]
    let prod = Array.zeroCreate<int> 2
    let result = arrayMapProductSafe lhs rhs prod
    Assert.Equal(Error (IndexOutOfBounds -1), result)
