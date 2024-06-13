open System.IO
open Lexer
open Parser
open ASTBuilder
open TypeChecker
open CodeGenerator
open CSharpCompiler

let stdLibFilePath = "./Presto.StandardLibrary/StdLib.pst"

type CompileOptions = {
    FilePaths: string list
    OutputPath: string
    CompileGeneratedCSharp: bool
    CompileLibrary: bool
    RunExe: bool
    NoStdLib: bool
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
        | "--no-std-lib" ->
            if args.Length = 0 then
                failwith "No value was specified for option \"--no-std-lib\""
            else
                parseCommandLineArgsHelper args[2..] { options with NoStdLib = true }
        | _ -> parseCommandLineArgsHelper args[1..] { options with FilePaths = (options.FilePaths @ [args[0]])}

let parseCommandLineArgs (args: string array): CompileOptions =
    let compileOptions: CompileOptions = {
        FilePaths = [stdLibFilePath]
        OutputPath = System.Environment.CurrentDirectory
        CompileGeneratedCSharp = false
        CompileLibrary = false
        RunExe = false
        NoStdLib = false
    }

    let (args, compileOptions) = parseCommandLineArgsHelper (Array.skip 1 args) compileOptions

    if compileOptions.NoStdLib then
        { compileOptions with FilePaths = compileOptions.FilePaths.Tail }
    else
        compileOptions

let commandLineArgs = System.Environment.GetCommandLineArgs()
let compileOptions = parseCommandLineArgs commandLineArgs

if compileOptions.FilePaths.IsEmpty then
    failwith "No files specified."

let (scopeId, scopesById) = getInitialScopesById

let programHasMain (program: Program): bool =
    List.exists<Binding> (fun b -> b.NameToken._text = "main") program.Bindings

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
            let hadMain = programHasMain program
            let buildAstOutput = buildAst parseOutput.CompilationUnit program

            if not buildAstOutput.Errors.IsEmpty then
                for error in buildAstOutput.Errors do
                    printfn $"{error}"

                (None, program)
            else
                printfn "Done building the AST!"
                
                let hasMain = programHasMain buildAstOutput.Program

                // check types
                let (program, typeCheckingErrors) = checkTypes buildAstOutput.Program

                if not typeCheckingErrors.IsEmpty then
                    for error in typeCheckingErrors do
                        printfn $"{error}"

                    (None, program)
                else
                    printfn "Done type checking!"

                    // generate code
                    let generateMain = (not hadMain) && hasMain
                    let codeGeneratorOutput = generateCode program compileOptions.CompileLibrary generateMain

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

let rec compileFiles (filePaths: string list) (program: Program) (generatedFilePaths: string list): Program * string list =
    if filePaths.IsEmpty then (program, generatedFilePaths)
    else

    let (maybeGeneratedFilePath, newProgram) = compileFile program filePaths.Head

    let generatedFilePaths =
        match maybeGeneratedFilePath with
        | Some generatedFilePath ->
            generatedFilePaths @ [generatedFilePath]
        | None -> generatedFilePaths
    
    compileFiles filePaths.Tail newProgram generatedFilePaths

let initialProgram = {
    Bindings = List.empty
    ScopeId = scopeId
    ScopesById = scopesById
    TypesByExpressionId = Map.empty
    ResolvedSymbolsByExpressionId = Map.empty
    TypeCanonicalNamesByScopeId = Map.empty
}

let (program, generatedFilePaths) = compileFiles compileOptions.FilePaths initialProgram []

let prestoCompilerSucceeded = generatedFilePaths.Length = compileOptions.FilePaths.Length

if prestoCompilerSucceeded && compileOptions.CompileGeneratedCSharp then
    let runtimeCSharpFilePaths = Directory.GetFiles("../../../Presto.Runtime", "*.cs")
    let filePathsToCompile = Set.ofList (generatedFilePaths @ (List.ofArray runtimeCSharpFilePaths))
    let csProjName = compileCSharpFiles filePathsToCompile compileOptions.OutputPath

    runCommandInNewWindow (Path.Combine(compileOptions.OutputPath, csProjName + ".exe"))