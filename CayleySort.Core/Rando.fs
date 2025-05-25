namespace CayleySort.Core
open System
open FSharp.UMX

type IRando =
    abstract member NextIndex: int -> int
    abstract member NextUInt: unit -> uint32
    abstract member NextPositiveInt: unit -> int32
    abstract member NextULong: unit -> uint64
    abstract member NextFloat: unit -> float
    abstract member NextGuid: unit -> Guid
    abstract member ByteCount: int

[<Measure>] type randomSeed

/// <summary>
/// A Linear Congruential Generator (LCG) for random number generation.
/// Uses parameters a = 6364136223846793005, c = 1442695040888963407, m = 2^64.
/// </summary>
type randomLcg(seed: uint64<randomSeed>) =
    let _a = 6364136223846793005UL
    let _c = 1442695040888963407UL
    let mutable _last = (_a * (uint64 (seed |> UMX.untag)) + _c)
    let mutable _byteCount = 0
    
    /// <summary>
    /// Gets the initial seed.
    /// </summary>
    member this.Seed = seed
    
    /// <summary>
    /// Gets the number of random bytes generated.
    /// </summary>
    member this.ByteCount = _byteCount
    
    /// <summary>
    /// Generates a random uint32 using the upper 32 bits of the LCG state.
    /// </summary>
    member this.NextUInt =
        _byteCount <- _byteCount + 4
        _last <- (_a * _last + _c)
        uint32 (_last >>> 32)
    
    /// <summary>
    /// Generates a random uint64 by combining two uint32 values.
    /// </summary>
    member this.NextULong =
        let high = uint64 this.NextUInt <<< 32
        let low = uint64 this.NextUInt
        high ||| low
    
    /// <summary>
    /// Generates a random float in [0, 1).
    /// </summary>
    member this.NextFloat =
        (float this.NextUInt) / 4294967295.0
    
    /// <summary>
    /// Returns a function that generates random uint32 values.
    /// </summary>
    member this.AsNextUInt() = fun () -> this.NextUInt
    
    interface IRando with
        member this.NextUInt () = this.NextUInt
        member this.NextPositiveInt () =
            int (this.NextUInt % uint32 Int32.MaxValue)
        member this.NextIndex (modulus: int) =
            if modulus <= 0 then
                int this.NextUInt
            else
                let threshold = UInt32.MaxValue - (UInt32.MaxValue % uint32 modulus)
                let mutable result = None
                while result.IsNone do
                    let r = this.NextUInt
                    if r < threshold then
                        result <- Some (int (r % uint32 modulus))
                result.Value
        member this.NextULong () = this.NextULong
        member this.NextGuid () =
            let b1 = this.NextUInt
            let b2 = this.NextUInt
            let b3 = this.NextUInt
            let b4 = this.NextUInt
            Guid(int b1, int16 b2, int16 (b2 >>> 16), [|
                byte b3; byte (b3 >>> 8); byte (b3 >>> 16); byte (b3 >>> 24);
                byte b4; byte (b4 >>> 8); byte (b4 >>> 16); byte (b4 >>> 24)
            |])
        member this.NextFloat () = this.NextFloat
        member this.ByteCount = this.ByteCount

/// <summary>
/// A random number generator using System.Random.
/// </summary>
type randomNet(seed: int32<randomSeed>) =
    let _random = Random(int (UMX.untag seed))
    let mutable _byteCount = 0
    
    /// <summary>
    /// Gets the initial seed.
    /// </summary>
    member this.Seed = seed
    
    /// <summary>
    /// Gets the number of random bytes generated.
    /// </summary>
    member this.ByteCount = _byteCount
    
    /// <summary>
    /// Generates a random uint32 using System.Random.
    /// </summary>
    member this.NextUInt =
        _byteCount <- _byteCount + 4
        let bytes = Array.zeroCreate<byte> 4
        _random.NextBytes(bytes)
        BitConverter.ToUInt32(bytes, 0)
    
    /// <summary>
    /// Generates a random uint64 using System.Random.
    /// </summary>
    member this.NextULong =
        _byteCount <- _byteCount + 8
        let bytes = Array.zeroCreate<byte> 8
        _random.NextBytes(bytes)
        BitConverter.ToUInt64(bytes, 0)
    
    /// <summary>
    /// Generates a random float in [0, 1) using System.Random.
    /// </summary>
    member this.NextFloat =
        _byteCount <- _byteCount + 8
        let bytes = Array.zeroCreate<byte> 8
        _random.NextBytes(bytes)
        let value = BitConverter.ToDouble(bytes, 0)
        // Scale to [0, 1)
        value / Double.MaxValue
        // Alternatively, could use _random.NextDouble() directly, but using NextBytes for consistency
        // _random.NextDouble()
    
    /// <summary>
    /// Returns a function that generates random uint32 values.
    /// </summary>
    member this.AsNextUInt() = fun () -> this.NextUInt
    
    interface IRando with
        member this.NextUInt () = this.NextUInt
        member this.NextPositiveInt () =
            _byteCount <- _byteCount + 4
            let bytes = Array.zeroCreate<byte> 4
            _random.NextBytes(bytes)
            int (BitConverter.ToUInt32(bytes, 0) % uint32 Int32.MaxValue)
        member this.NextIndex (modulus: int) =
            _byteCount <- _byteCount + 4
            let bytes = Array.zeroCreate<byte> 4
            _random.NextBytes(bytes)
            bytes.[3] <- (bytes.[3] >>> 1)
            let value = BitConverter.ToUInt32(bytes, 0)
            if modulus <= 0 then
                int value
            else
                int value % modulus
        member this.NextULong () = this.NextULong
        member this.NextGuid () =
            _byteCount <- _byteCount + 16
            let bytes = Array.zeroCreate<byte> 16
            _random.NextBytes(bytes)
            Guid(bytes)
        member this.NextFloat () = this.NextFloat
        member this.ByteCount = this.ByteCount

