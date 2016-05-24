module Program

open System
open Parser
open Dictionary
open System.IO

// Read program from file, just because
let programInput = File.ReadAllText("SomeProgram.remcode");


let program = getWords programInput
let programOutput = remcode program dictionary 0 Map.empty

//printf "%s" programOutput