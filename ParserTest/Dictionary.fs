module Dictionary
open System


let dictionary:Map<string,string> = [("+", ""); ("print", ""); ("var", "")] |> Map.ofList
   

// Execute action based on current word, returns updated stack
let action (word: string) (stack: Map<int, float32>) =
    match word with
    | "+" -> 
        let key = stack.Count
        let stack' = stack.Add ((key+2), (stack.Item 0 + stack.Item 1))
        let stack' = stack.Remove 0
        let stack' = stack.Remove 1
        stack'
    | "print" ->
        printfn "%f" (stack.Item ((stack |> Seq.head).Key))
        let stack' = stack.Remove ((stack |> Seq.head).Key)
        stack'
    match dictionary.ContainsKey word with
        | true -> 
            let key = stack.Count
            let stack' = stack.Add ((key+1), (stack.Item 1))
            stack'
        | false -> failwith "Unknown character"