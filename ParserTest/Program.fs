module Program

open System
open Parser
open Dictionary
open System.IO

// Read program from file, just because
let programInput = File.ReadAllText("SomeProgram.remcode");


let program = getWords programInput
let defaultStack:Map<int, float32> = [(0, 0.f); (0, 0.f)] |> Map.ofList

let programOutput = remcode program dictionary 0 Map.empty

printf "test"
//printf "%s" programOutput