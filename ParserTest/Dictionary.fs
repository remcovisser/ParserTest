module Dictionary
open System

let dictionary = ["+"; "-"; "*"]

let rec inDictionary value dictionary =
    match dictionary with
    | head::tail when head = value -> true
    | head::tail -> inDictionary value tail
    | [] -> false


let action (word: string) (stack: List<float32>) =
    match word, stack with
    | "+", head::tail -> 
        printfn "%f" (tail.Head + head)
        tail.Head :: tail
    | "-", head::tail -> 
        printfn "%f" (tail.Head - head)
        tail.Head :: tail
    | "*", head::tail -> 
        printfn "%f" (tail.Head * head)
        tail.Head :: tail
    | _ , head::tail -> 
        printfn "Unknow character"
        tail.Head :: tail
    | _, [] ->
        printfn "Stack is empty"
        []