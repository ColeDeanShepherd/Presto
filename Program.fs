open System.IO
open Lexer
open Parser

let fileName = "../../../BootstrappedCompiler.pst"

let sourceCode = File.ReadAllText fileName
let tokenizeOutput = tokenize sourceCode

if not tokenizeOutput.Errors.IsEmpty then
    for error in tokenizeOutput.Errors do
         printfn $"{error}"
else
    let parseOutput = parse tokenizeOutput.Tokens

    if not parseOutput.Errors.IsEmpty then
        for error in parseOutput.Errors do
             printfn $"{error}"
    else
        printfn "Parsing succeeded."