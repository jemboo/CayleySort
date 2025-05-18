
namespace CayleySort.Core
open System

type IRando =
    abstract member NextInt: int -> int
    abstract member NextUInt: unit -> uint32
    abstract member NextPositiveInt: unit -> int32
    abstract member NextULong: unit -> uint64
    abstract member NextFloat: unit -> float
    abstract member NextGuid: unit -> Guid

module Rando =
    // For making a 2d Gaussian distribution
    let polarBoxMullerDist meanX stdDevX meanY stdDevY (rnd: IRando) =
        let rec getRands () =
            let u = (2.0 * (rnd.NextFloat())) - 1.0
            let v = (2.0 * (rnd.NextFloat())) - 1.0
            let w = u * u + v * v
            if w >= 1.0 then getRands () else u, v, w

        seq {
            while true do
                let u, v, w = getRands ()
                let scale = System.Math.Sqrt(-2.0 * System.Math.Log(w) / w)
                let x = scale * u
                let y = scale * v
                (meanX + x * stdDevX, meanY + y * stdDevY)
            }


    // For making a 1d Gaussian distribution
    let gaussianDistribution meanX stdDevX (rnd: IRando) =
        polarBoxMullerDist meanX stdDevX meanX stdDevX rnd
        |> Seq.collect (fun (x, y) -> [x; y])


    let randOneOrZero (pctOnes: float) (rnd: IRando) (len: int) =
        Seq.init len (fun _ -> if ((rnd.NextFloat ()) > pctOnes) then 0 else 1)


    let randBits (pctTrue: float) (rnd: IRando) (len: int) =
        Seq.init len (fun _ -> if ((rnd.NextFloat ()) > pctTrue) then false else true)

