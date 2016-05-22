module Dictionary
open System

let dictionary = ["+"; "-"; "*"; "print"]

// Check if word is in the dictionary, returns true of false
let rec inDictionary value dictionary =
    match dictionary with
    | head::tail when head = value -> true
    | head::tail -> inDictionary value tail
    | [] -> false

// Remove element from list specified by index
let rec remove i l =
    match i, l with
    | 1, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | i, [] -> failwith "index out of range"

// Execute action based on current word, returns updated stack
let action (word: string) (stack: List<float32>) =
    match word, stack with
    | "+", head::tail -> 
        let stack = remove stack.Length stack
        let stack = remove stack.Length stack
        tail.Head + head::stack
    | "-", head::tail -> 
        printfn "%f" (tail.Head - head)
        tail.Head :: tail
    | "*", head::tail -> 
        printfn "%f" (tail.Head * head)
        tail.Head :: tail
    | "print", head::tail ->
        printfn "%f" head
        head::tail
    | _ , head::tail -> 
        printfn "Unknow character"
        tail.Head :: tail
    | _, [] ->
        printfn "Stack is empty"
        []