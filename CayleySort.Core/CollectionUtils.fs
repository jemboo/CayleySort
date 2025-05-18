namespace CayleySort.Core
open LanguagePrimitives

module CollectionUtils =

    let cartesianProduct 
            (seq_a: seq<'a>) 
            (seq_b: seq<'b>) 
        =
        seq {  
            for ae in seq_a do
               for be in seq_b do
                  yield (ae, be)
        }

    let flattenTupleSeq (tuples: seq<'T * 'T>) : seq<'T> =
        tuples |> Seq.collect (fun (x, y) -> [x; y])


    let filterByIndices (indices: int[]) (source: seq<'T>) : seq<'T> =
        source 
        |> Seq.indexed 
        |> Seq.filter (fun (i, _) -> Array.contains i indices)
        |> Seq.map snd


    // returns a sequence of items that occur more than once
    let itemsOccuringMoreThanOnce items =
        seq {
            let d = System.Collections.Generic.Dictionary()
            for i in items do
                match d.TryGetValue(i) with
                | false, _ -> d.[i] <- false // first observance
                | true, false ->
                    d.[i] <- true
                    yield i // second observance
                | true, true -> () // already seen at least twice
        }

    // filters the sequence, blocking the emission of a given value more than max times.
    let getItemsUpToMaxTimes<'k,'v when 'k:equality> 
            (lookup: 'v->'k)
            (max:int) 
            (items:'v seq)  =
        seq {
            let d = System.Collections.Generic.Dictionary()
            for i in items do
                let key = lookup i
                match d.TryGetValue(key) with
                | false, _ ->
                    d.[key] <- 1
                    yield i
                | true, ct ->
                    d.[key] <- ct + 1
                    if (ct < max) then
                        yield i
        }

    // converts a density distr to a cumulative distr.
    let asCumulative (startVal: float) (weights: float[]) =
        weights |> Array.scan (fun cum cur -> cum + cur) startVal
