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
                    let key = stack.Count
                    let value = stack.Item key
                    let stack' = stack.Remove key
                    let stack' = stack'.Add(key, value)
                    let dictionary' = dictionary.Add (words.[next-1], key) 
                    remcode words dictionary' (next+1) stack'
                | false, "print" ->
                    let printingWord = words.[next-1]
                    let stack' = match dictionary |> Map.containsKey printingWord with
                    | true -> 
                        let dictionaryKey = dictionary.Item printingWord
                        let itemFromStack = stack.Item dictionaryKey
                        printfn "%f" itemFromStack
                        stack
                    | false -> 
                        let key =  (stack |> Seq.last).Key
                        let stack' = stack.Remove key
                        printfn "%f"  (stack |> Seq.last).Value
                        stack'
                    remcode words dictionary (next+1) stack'
                | false, "+" -> 
                    let word1 = words.[next-1]
                    let word2 = words.[next-2]
                    let key = match stack.Count with
                        | 0 -> 0
                        | _ -> (stack |> Seq.last).Key
                    let stack' = match System.Single.TryParse(word1), System.Single.TryParse(word2) with
                    | (false, value1), (false, value2) ->
                        let dictionaryKey1 = dictionary.Item word1
                        let itemFromStack1 = stack.Item dictionaryKey1
                        let dictionaryKey2 = dictionary.Item word2
                        let itemFromStack2 = stack.Item dictionaryKey2
                        let stack' = stack.Add ((key+1), (itemFromStack1 + itemFromStack2))
                        stack'
                    | (true, value1), (true, value2) ->
                        let stack' = stack.Remove key
                        let stack' = stack'.Remove (key-1)
                        let stack' = stack'.Add ((key-1), (value1 + value2))
                        stack'
                    remcode words dictionary (next+1) stack'
                | false, word -> 
                    match System.Single.TryParse(word) with
                    | (true, number) ->
                        let key = stack.Count+1
                        let stack' = stack.Add (key, number)
                        remcode words dictionary (next+1) stack'
                    | _ ->
                        printfn "Unknown character found: %s" word
                        remcode words dictionary (next+1) stack