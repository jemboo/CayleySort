namespace CayleySort.Core
open LanguagePrimitives
open System
open System.Collections
open System.Runtime.CompilerServices

module CollectionUtils =

    let rec compareAny 
                (o1: obj) 
                (o2: obj) 
        =
        match (o1, o2) with
        | (:? IComparable as o1), (:? IComparable as o2) -> Some(compare o1 o2)
        | (:? IEnumerable as arr1), (:? IEnumerable as arr2) ->
            Seq.zip (arr1 |> Seq.cast) (arr2 |> Seq.cast)
            |> Seq.choose (fun (a, b) -> compareAny a b)
            |> Seq.skipWhile ((=) 0)
            |> Seq.tryHead
            |> Option.defaultValue 0
            |> Some
        | (:? ITuple as tup1), (:? ITuple as tup2) ->
            let tupleToSeq (tuple: ITuple) =
                seq {
                    for i in 0 .. tuple.Length do
                        yield tuple.[i]
                }

            compareAny (tupleToSeq tup1) (tupleToSeq tup2)
        | _ -> None

    let areEqual (o1: obj) (o2: obj) =
        match compareAny o1 o2 with
        | Some v -> v = 0
        | None -> false


    // Generic array equality check
    let inline arrayEquals< ^a when ^a: equality> (xs: ^a[]) (ys: ^a[]) : bool =
        if xs.Length <> ys.Length then false
        else Seq.forall2 (fun x y -> x = y) xs ys

    //returns the last n items of the list in the original order
    let rec last n xs =
        if List.length xs <= n then xs else last n xs.Tail

    //returns the first n items of the list in the original order,
    //or all the items if it's shorter than n
    let first n (xs: 'a list) =
        let mn = min n xs.Length
        xs |> List.take mn



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
