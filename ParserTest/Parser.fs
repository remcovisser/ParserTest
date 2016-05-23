module Parser 

open System
open Dictionary

// Return a string with all the words of the programm, words are characters sepparted by a space
let getWords (text: string) =
    let rawWords = (text.Split (' ','\n')) |> Array.toList
    let words = rawWords |> List.filter(fun words -> words = "" |> not)
    words

// Parse code and execute it
let rec remcode (words: List<string>) (dictionary: List<string>) (next:int) (stack: List<float32>) = 
     match words.Length = next with 
        | true -> "The program has been executed"
        | false ->
            match inDictionary words.[next] dictionary, words.[next] with
                | true, word ->  
                    let stack = action word stack
                    remcode words dictionary (next+1) stack
                | false, word ->
                    match System.Single.TryParse(word) with
                    | (true, number) ->
                        remcode words dictionary (next+1) (number :: stack)
                    | _ ->
                        printfn "Unknown character found: %s" word
                        remcode words dictionary (next+1) stack