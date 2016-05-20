module Program

open System
open Parser

let words = getWords "1 1 +"
let dictionary = ["+"; "-"]

let parsedCode = remcode words dictionary 0 []

printf("test")