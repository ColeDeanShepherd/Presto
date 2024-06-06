open System.IO
open Lexer
open Parser
open ASTBuilder
open TypeChecker
open CodeGenerator
open CSharpCompiler

type CompileOptions = {
    FilePaths: string list
    OutputPath: string
    CompileGeneratedCSharp: bool
    CompileLibrary: bool
    RunExe: bool
}

let rec parseCommandLineArgsHelper (args: string array) (options: CompileOptions): string array * CompileOptions =
    if args.Length = 0 then
        (args, options)
    else
        match args[0] with
        | "--compile-generated-csharp" ->
            if args.Length = 0 then
                failwith "No value was specified for option \"--compile-generated-csharp\""
            else
                parseCommandLineArgsHelper args[2..] { options with CompileGeneratedCSharp = (args[1] = "true") }
        | "--compile-library" ->
            if args.Length = 0 then
                failwith "No value was specified for option \"--compile-library\""
            else
                parseCommandLineArgsHelper args[2..] { options with CompileLibrary = (args[1] = "true") }
        | "--run-exe" ->
            if args.Length = 0 then
                failwith "No value was specified for option \"--run-exe\""
            else
                let value = args[1] = "true"
                parseCommandLineArgsHelper args[2..] { options with RunExe = value; CompileGeneratedCSharp = value || options.CompileGeneratedCSharp }
        | "--output-dir" ->
            if args.Length = 0 then
                failwith "No value was specified for option \"--output-dir\""
            else
                parseCommandLineArgsHelper args[2..] { options with OutputPath = args[1] }
        | _ -> parseCommandLineArgsHelper args[1..] { options with FilePaths = (options.FilePaths @ [args[0]])}

let parseCommandLineArgs (args: string array): CompileOptions =
    let compileOptions: CompileOptions = {
        FilePaths = []
        OutputPath = System.Environment.CurrentDirectory
        CompileGeneratedCSharp = false
        CompileLibrary = false
        RunExe = false
    }

    let (args, compileOptions) = parseCommandLineArgsHelper (Array.skip 1 args) compileOptions

    compileOptions

let commandLineArgs = System.Environment.GetCommandLineArgs()
let compileOptions = parseCommandLineArgs commandLineArgs

if compileOptions.FilePaths.IsEmpty then
    failwith "No files specified."

let (scopeId, scopesById) = getInitialScopesById
let mutable program = {
    Bindings = List.empty
    ScopeId = scopeId
    ScopesById = scopesById
    TypesByExpressionId = Map.empty
    ResolvedSymbolsByExpressionId = Map.empty
    TypeCanonicalNamesByScopeId = Map.empty
}
let mutable generatedFilePaths: string list = []

let compileFile (program: Program) (filePath: string): string option * Program =
    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(filePath)

    printfn $"Compiling {filePath}"

    let sourceCode = File.ReadAllText filePath

    // tokenize
    let tokenizeOutput = tokenize filePath sourceCode

    if tokenizeOutput.errors.Count > 0 then
        for error in tokenizeOutput.errors do
            printfn $"{error}"

        (None, program)
    else
        printfn "Done lexing!"

        // parse
        let parseOutput = parse (List.ofSeq tokenizeOutput.tokens)

        if not parseOutput.Errors.IsEmpty then
            for error in parseOutput.Errors do
                printfn $"{error}"

            (None, program)
        else
            printfn "Done parsing!"

            // build AST
            let buildAstOutput = buildAst parseOutput.CompilationUnit program

            if not buildAstOutput.Errors.IsEmpty then
                for error in buildAstOutput.Errors do
                    printfn $"{error}"

                (None, program)
            else
                printfn "Done building the AST!"

                // generate code
                let (program, typeCheckingErrors) = checkTypes buildAstOutput.Program

                if not typeCheckingErrors.IsEmpty then
                    for error in typeCheckingErrors do
                        printfn $"{error}"

                    (None, program)
                else
                    printfn "Done type checking!"

                    // generate code
                    let codeGeneratorOutput = generateCode program compileOptions.CompileLibrary

                    if not codeGeneratorOutput.Errors.IsEmpty then
                        for error in codeGeneratorOutput.Errors do
                            printfn $"{error}"

                        (None, program)
                    else
                        printfn "Compile succeeded!"
                        printfn ""
                        printfn $"{codeGeneratorOutput.GeneratedCode}"

                        let generatedFilePath = Path.Combine(compileOptions.OutputPath, (fileNameWithoutExtension + ".g.cs"))
                        File.WriteAllText(generatedFilePath, codeGeneratorOutput.GeneratedCode)

                        (Some generatedFilePath, program)

for filePath in compileOptions.FilePaths do
    let (maybeGeneratedFilePath, newProgram) = compileFile program filePath

    match maybeGeneratedFilePath with
    | Some generatedFilePath ->
        generatedFilePaths <- generatedFilePaths @ [generatedFilePath]
    | None ->
    
    program <- newProgram

let prestoCompilerSucceeded = generatedFilePaths.Length = compileOptions.FilePaths.Length

if prestoCompilerSucceeded && compileOptions.CompileGeneratedCSharp then
    let runtimeCSharpFilePaths = Directory.GetFiles("../../../Presto.Runtime", "*.cs")
    let filePathsToCompile = Set.ofList (generatedFilePaths @ (List.ofArray runtimeCSharpFilePaths))
    let csProjName = compileCSharpFiles filePathsToCompile compileOptions.OutputPath

    runCommandInNewWindow (Path.Combine(compileOptions.OutputPath, csProjName + ".exe"))