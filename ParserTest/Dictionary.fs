module Dictionary
open System

let dictionary = ["+"; "-"]

let rec inDictionary value dictionary =
    match dictionary with
    | head::tail when head = value -> true
    | head::tail -> inDictionary value tail
    | [] -> false
    | _ -> false


let action1 (word: string) (stack: List<float32>) =
    match word, stack with
    | "+", head::tail -> 
        printfn "%f" (head + tail.Head)
        tail.Head :: tail

let action2 (dictionary: List<string>) (stack: List<float32>) =
    match dictionary, stack with
    | headDictionary::tailDictionary, headStack::tailStack when headDictionary = "+" ->
        printfn "%f" (headStack + tailStack.Head)
        tailStack.Head :: tailStack
 
     
