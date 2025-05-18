let vowels = set ['a'; 'e'; 'i'; 'o'; 'u']
let isVowel c = Set.contains c vowels
let isConsonant c = not (isVowel c)
let isLetter c = System.Char.IsLetter c
let w = isVowel 'a'
let toPigLatin (word: string) =
    let rec loop acc word =
        match word with
        | "" -> acc
        | _ when isVowel word.[0] -> acc + word + "ay"
        | _ ->
            let firstConsonant = word |> Seq.takeWhile isConsonant |> Seq.toArray |> System.String
            let rest = word.Substring firstConsonant.Length
            loop (acc + rest + firstConsonant + "ay") rest
    loop "" word

let yab = toPigLatin "hello"
let yabb = toPigLatin "apple"
let yabb2 = toPigLatin "banana"
let yabb3 = toPigLatin "string"
