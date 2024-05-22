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

let (scopeId, scopesById) = getInitialScopesById
let mutable program = {
    Bindings = List.empty
    ScopeId = scopeId
    ScopesById = scopesById
    TypesByExpressionId = Map.empty
    ResolvedSymbolsByExpressionId = Map.empty
    TypeCanonicalNamesByScopeId = Map.empty
}

let compileFile (program: Program) (filePath: string): Program =
    let dirPath = Path.GetDirectoryName(filePath)
    let fileName = Path.GetFileName(filePath)
    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(filePath)

    printfn $"Compiling {fileName}"

    let sourceCode = File.ReadAllText filePath

    // tokenize
    let tokenizeOutput = tokenize filePath sourceCode

    if tokenizeOutput.errors.Count > 0 then
        for error in tokenizeOutput.errors do
            printfn $"{error}"

        program
    else
        printfn "Done lexing!"

        // parse
        let parseOutput = parse (List.ofSeq tokenizeOutput.tokens)

        if not parseOutput.Errors.IsEmpty then
            for error in parseOutput.Errors do
                printfn $"{error}"

            program
        else
            printfn "Done parsing!"

            // build AST
            let buildAstOutput = buildAst parseOutput.CompilationUnit program

            if not buildAstOutput.Errors.IsEmpty then
                for error in buildAstOutput.Errors do
                    printfn $"{error}"

                program
            else
                printfn "Done building the AST!"

                // generate code
                let (program, typeCheckingErrors) = checkTypes buildAstOutput.Program

                if not typeCheckingErrors.IsEmpty then
                    for error in typeCheckingErrors do
                        printfn $"{error}"

                    program
                else
                    printfn "Done type checking!"

                    // generate code
                    let codeGeneratorOutput = generateCode program

                    if not codeGeneratorOutput.Errors.IsEmpty then
                        for error in codeGeneratorOutput.Errors do
                            printfn $"{error}"

                        program
                    else
                        printfn "Compile succeeded!"
                        printfn ""
                        printfn $"{codeGeneratorOutput.GeneratedCode}"

                        File.WriteAllText(Path.Combine(dirPath, fileNameWithoutExtension + ".g.cs"), codeGeneratorOutput.GeneratedCode)

                        program

for filePath in filePaths do
    program <- compileFile program filePath