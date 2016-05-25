module Parser 

open System
open Dictionary

// Return a string with all the words of the programm, words are characters sepparted by a space
let getWords (text: string) =
    let rawWords = (text.Split (' ','\n')) |> Array.toList
    let words = rawWords |> List.filter(fun words -> words = "" |> not)
    words

// Parse code and execute it
let rec remcode (words: List<string>) (dictionary: Map<string, int>) (next:int) (stack: Map<int, float32>) = 
     match words.Length = next with 
        | true -> stack //"The program has been executed"
        | false ->
            match dictionary |> Map.containsKey words.[next], words.[next] with
                | true, "var" ->
                    let key = dictionary.Count
                    let dictionary' = dictionary.Add (words.[next+1], (key+1))
                    remcode words dictionary' (next+2) stack
                | true, word ->  
                    let stack' = action word stack dictionary
                    remcode words dictionary (next+1) stack'
                | false, word -> 
                    match System.Single.TryParse(word) with
                    | (true, number) ->
                        let position = 
                            match stack.ContainsKey 0 with
                            | true -> 1
                            | false -> 0
                        remcode words dictionary (next+1) (stack.Add (position, number))
                    | _ ->
                        printfn "Unknown character found: %s" word
                        remcode words dictionary (next+1) stack