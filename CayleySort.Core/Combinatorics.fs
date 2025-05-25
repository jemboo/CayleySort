namespace CayleySort.Core
open LanguagePrimitives
open System
open System.Collections
open System.Runtime.CompilerServices

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


