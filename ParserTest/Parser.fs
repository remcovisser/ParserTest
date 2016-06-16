module Parser 

open System

// Return a string with all the words of the programm, words are characters sepparted by a space
let getWords (text: string) =
    let rawWords = (text.Split (' ','\n','\r')) |> Array.toList
    let words = rawWords |> List.filter(fun words -> words = "" |> not)
    words

// Define the Dictionary
let dictionary:Map<string,int> = [] |> Map.ofList

// Parse code and execute it
let rec remcode (words: List<string>) (dictionary: Map<string, int>) (next:int) (stack: Map<int, float32>) = 
     match words.Length = next with 
        | true -> stack //"The program has been executed"
        | false ->
            match dictionary |> Map.containsKey words.[next], words.[next] with
                | true, word ->  
                    remcode words dictionary (next+1) stack
                | false, "var" ->
                    let dictionary' = dictionary.Add (words.[next+1], 0)
                    remcode words dictionary' (next+2) stack
                | false, "store" ->
                    let value = stack.Item 0
                    let stack' = stack.Remove 0
                    let key = stack.Count
                    let stack' = stack'.Add(key, value)
                    let dictionary' = dictionary.Add (words.[next-1], key) 
                    remcode words dictionary' (next+1) stack'
                | false, "print" ->
                    let printingWord = words.[next-1]
                    match dictionary |> Map.containsKey printingWord with
                    | true -> 
                        let dictionaryKey = dictionary.Item printingWord
                        let itemFromStack = stack.Item dictionaryKey
                        printfn "%f" itemFromStack
                    | false -> printfn "%f"  (stack |> Seq.last).Value
                    remcode words dictionary (next+1) stack
                | false, "+" -> 
                    let key = (stack |> Seq.last).Key
                    let stack' = stack.Add ((key+1), (stack.Item 0 + stack.Item 1))
                    let stack' = stack'.Remove 0
                    let stack' = stack'.Remove 1
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