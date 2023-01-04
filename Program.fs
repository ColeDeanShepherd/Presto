open System.IO
open Lexer

let fileName = "../../../BootstrappedCompiler.pst"
let sourceCode = File.ReadAllText fileName
let tokenizeOutput = tokenize sourceCode

let x = 3