module Program

open System
open Parser
open Dictionary

let program = getWords "2 1 + 9 7 - 5 5 *"

let programOutput = remcode program dictionary 0 []

printf "%s" programOutput