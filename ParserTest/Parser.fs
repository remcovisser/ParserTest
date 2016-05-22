module Parser 

open System
open Dictionary

// Return a string with all the words of the programm, words are characters sepparted by a space
let getWords (text: string) =
    (text.Split ' ') |> Array.toList

let rec remcode (words: List<string>) (dictionary: List<string>) (next:int) (stack: List<float32>) = 
     match words.Length = next with 
        | true -> "Program has been executed"
        | false ->
            match inDictionary words.[next] dictionary, words.[next] with
                | true, word ->  
                    let stack = action1 word stack
                    remcode words dictionary (next+1) stack
                | false, word -> 
                    remcode words dictionary (next+1) (System.Single.Parse(word) :: stack)
 

