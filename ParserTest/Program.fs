module Program

open System
open Parser
open Dictionary


let programInput =  "8 5 + print"




let program = getWords programInput
let programOutput = remcode program dictionary 0 []
printf "%s" programOutput