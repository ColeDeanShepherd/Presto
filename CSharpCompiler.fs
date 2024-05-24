module CSharpCompiler

open System.IO
open System.Diagnostics

let runProcess (exePath: string) (args: string) =
    let startInfo = new ProcessStartInfo()
    startInfo.FileName <- exePath
    startInfo.Arguments <- args
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
        
    use proc = new Process()
    proc.StartInfo <- startInfo

    proc.OutputDataReceived.Add(fun args -> printfn "%s" args.Data)
    proc.ErrorDataReceived.Add(fun args -> printfn "ERROR: %s" args.Data)

    proc.Start() |> ignore
    proc.BeginOutputReadLine()
    proc.BeginErrorReadLine()

    proc.WaitForExit()

    if proc.ExitCode <> 0 then
        failwithf "Process exited with code %d" proc.ExitCode

let compileCSharpFiles (csharpFilePaths: string Set) (outputDir: string) =
    let csProjName = Path.GetRandomFileName()

    // Create temporary directory to hold the C# project.
    let tempDirPath = Path.Combine(Path.GetTempPath(), csProjName)
    Directory.CreateDirectory(tempDirPath) |> ignore

    // Create C# project.
    runProcess "dotnet" $"new console -o {tempDirPath} --force"

    // Delete the auto-generated "Program.cs" file.
    File.Delete(Path.Combine(tempDirPath, "Program.cs"))

    // Copy generated C# files into project.
    for filePath in csharpFilePaths do
        let fileName = Path.GetFileName(filePath)
        let dstFilePath = Path.Combine(tempDirPath, fileName)
        File.Copy(filePath, dstFilePath, overwrite = true)

    // Build C# project.
    let csProjFileName = csProjName + ".csproj"
    let csprojPath = Path.Combine(tempDirPath, csProjFileName)
    runProcess "dotnet" $"build {csprojPath} -o {outputDir}"

    // Delete the temporary directory.
    Directory.Delete(tempDirPath, recursive = true)