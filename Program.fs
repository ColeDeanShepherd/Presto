open System.IO
open Lexer
open Parser
open ASTBuilder

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
        let buildAstOutput = buildAst parseOutput.Program

        if not buildAstOutput.Errors.IsEmpty then
            for error in buildAstOutput.Errors do
                 printfn $"{error}"
        else
            printfn "Building the AST succeeded."