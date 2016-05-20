module Parser 

open System


let multiply (stack: List<float32>) =
    let sum = stack.[0] + stack.[1]
    printfn "%f" sum
    ()

// Return a string with all the words of the programm, words are characters sepparted by a space
let getWords (text: string) =
    (text.Split ' ') |> Array.toList

let rec remcode (words: List<string>) (dictionary: List<string>) (next:int) (stack: List<float32>) = 
     match words.Length = next with 
        | true -> stack
        | false ->
            match words.[next] with
                | x when x = "1" -> remcode words dictionary (next+1) (System.Single.Parse(x) :: stack)
                | x when x = "+" -> 
                    multiply stack
                    remcode words dictionary (next+1) stack 
                | _ -> remcode words dictionary (next+1) stack
 