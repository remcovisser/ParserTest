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
        let key = (stack |> Seq.head).Key
        printfn "%f" (stack.Item key)
        stack
    | _ -> failwith "Unknown character"
