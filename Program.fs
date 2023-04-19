open System.IO
open Lexer
open Parser
open ASTBuilder
open TypeChecker
open CodeGenerator

let commandLineArgs = System.Environment.GetCommandLineArgs()

if commandLineArgs.Length < 2 then
    failwith "Invalid file."

let filePaths = Array.skip 1 commandLineArgs

let compileFile (filePath: string) =
    let dirPath = Path.GetDirectoryName(filePath)
    let fileName = Path.GetFileName(filePath)
    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(filePath)

    let sourceCode = File.ReadAllText filePath

    // tokenize
    let tokenizeOutput = tokenize sourceCode

    if not tokenizeOutput.Errors.IsEmpty then
        for error in tokenizeOutput.Errors do
             printfn $"{error}"
    else
        printfn "Done lexing!"

        // parse
        let parseOutput = parse tokenizeOutput.Tokens

        if not parseOutput.Errors.IsEmpty then
            for error in parseOutput.Errors do
                 printfn $"{error}"
        else
            printfn "Done parsing!"

            // build AST
            let buildAstOutput = buildAst parseOutput.Program

            if not buildAstOutput.Errors.IsEmpty then
                for error in buildAstOutput.Errors do
                     printfn $"{error}"
            else
                printfn "Done building the AST!"

                // generate code
                let (program, typeCheckingErrors) = checkTypes buildAstOutput.Program

                if not typeCheckingErrors.IsEmpty then
                    for error in typeCheckingErrors do
                         printfn $"{error}"
                else
                    printfn "Done type checking!"

                    // generate code
                    let codeGeneratorOutput = generateCode program

                    if not codeGeneratorOutput.Errors.IsEmpty then
                        for error in codeGeneratorOutput.Errors do
                             printfn $"{error}"
                    else
                        printfn "Compile succeeded!"
                        printfn ""
                        printfn $"{codeGeneratorOutput.GeneratedCode}"

                        File.WriteAllText(Path.Combine(dirPath, fileNameWithoutExtension + ".g.cs"), codeGeneratorOutput.GeneratedCode)

for filePath in filePaths do
    compileFile filePath