open System.IO
open Lexer
open Parser
open ASTBuilder
open CodeGenerator

let fileName = "../../../BootstrappedCompiler.pst"

let sourceCode = File.ReadAllText fileName

// tokenize
let tokenizeOutput = tokenize sourceCode

if not tokenizeOutput.Errors.IsEmpty then
    for error in tokenizeOutput.Errors do
         printfn $"{error}"
else
    // parse
    let parseOutput = parse tokenizeOutput.Tokens

    if not parseOutput.Errors.IsEmpty then
        for error in parseOutput.Errors do
             printfn $"{error}"
    else
        // build AST
        let buildAstOutput = buildAst parseOutput.Program

        if not buildAstOutput.Errors.IsEmpty then
            for error in buildAstOutput.Errors do
                 printfn $"{error}"
        else
            // generate code
            let codeGeneratorOutput = generateCode buildAstOutput.Program

            if not codeGeneratorOutput.Errors.IsEmpty then
                for error in codeGeneratorOutput.Errors do
                     printfn $"{error}"
            else
                printfn "Compile succeeded!"
                printfn $"{codeGeneratorOutput.GeneratedCode}"