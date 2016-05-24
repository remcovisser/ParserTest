module Dictionary
open System


let dictionary:Map<string,string> = [("+", ""); ("print", "")] |> Map.ofList
   

// Execute action based on current word, returns updated stack
let action (word: string) (stack: Map<int, float32>) =
    match word with
    | "+" -> 
        let key = stack.Count
        let mutable stack = stack.Add ((key+2), (stack.Item 0 + stack.Item 1))
        stack <- stack.Remove 0
        stack <- stack.Remove 1
        stack
    | "print" ->
        printfn "%f" (stack.Item ((stack |> Seq.head).Key))
        let mutable stack = stack.Remove ((stack |> Seq.head).Key)
        stack
    | _ -> failwith "Unknown character"