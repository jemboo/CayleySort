namespace CayleySort.Core

module Combinatorics =

    let cartesianProduct2d
            (seq_a: seq<'a>) 
            (seq_b: seq<'b>) 
        =
        seq {  
            for ae in seq_a do
               for be in seq_b do
                  yield (ae, be)
        }

    let rec cartesianProduct lists =
        match lists with
        | [] -> [[]]  // Base case: an empty list yields a list with an empty list
        | hd::tl ->
            // Recursive case: compute the cross product of the tail, then combine with the head
            [for x in hd do
                for rest in cartesianProduct tl do
                    yield x :: rest]


    type FisherYatesError = NullArray

    let fisherYatesShuffle (getRandomIndex: int -> int) 
                           (initialList: 'a[]) 
                 : Result<'a[], FisherYatesError> =
        if isNull initialList then Error FisherYatesError.NullArray
        else
            let result = Array.copy initialList
            let swap (arr: 'a[]) i j =
                let temp = arr.[i]
                arr.[i] <- arr.[j]
                arr.[j] <- temp
            for i = initialList.Length - 1 downto 1 do
                let j = getRandomIndex i
                swap result i (int j)
            Ok result