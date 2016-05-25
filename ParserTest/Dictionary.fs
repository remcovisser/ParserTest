module Dictionary
open System


let dictionary:Map<string,int> = [("+", 0); ("print", 0); ("var", 0)] |> Map.ofList
   

// Execute action based on current word, returns updated stack
let action (word: string) (stack: Map<int, float32>) (dictionary: Map<string, int>) =
    match word with
    | "+" -> 
        let key = (stack |> Seq.last).Key
        let stack' = stack.Add ((key+1), (stack.Item 0 + stack.Item 1))
        let stack' = stack'.Remove 0
        let stack' = stack'.Remove 1
        stack'
    | "print" ->
        printfn "%f" (stack.Item ((stack |> Seq.last).Key))
        let stack' = stack.Remove ((stack |> Seq.last).Key)
        stack'
    | _ -> match dictionary.ContainsKey word with
            | true -> 
                let totalKeys = stack.Count
                let value = stack.Item 0
                let key = dictionary.Item word
                let a = stack.Remove 0
                let b = a.Remove 1
                let c = b.Add (key, value)

                c
            | _ -> failwith "Unknown character"