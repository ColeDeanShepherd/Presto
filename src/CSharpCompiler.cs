using System.IO;

// TODO: handle process.start output?
// TODO: async wrappers?

namespace Presto
{
    public static class CSharpCompiler
    {
        public const string TmpDirectoryPath = "bin/tmp";

        public static void CompileCSharpProgram(string sourceCode)
        {
            CleanTmpDirectory();
            GenerateProjectFiles(sourceCode);
            CompileProject();
        }

        public static void CleanTmpDirectory()
        {
            // Delete the tmp directory so we can recreate it from scratch.
            if (Directory.Exists(TmpDirectoryPath))
            {
                Directory.Delete(TmpDirectoryPath, recursive: true);
            }

            // Create an empty tmp directory.
            Directory.CreateDirectory(TmpDirectoryPath);
        }

        public static void GenerateProjectFiles(string sourceCode)
        {
            // Generate a C# project file.
            var genProjectProcess = System.Diagnostics.Process.Start(
                "cmd.exe", $"/C dotnet new console -n PrestoProgram -o {TmpDirectoryPath}");
            genProjectProcess.WaitForExit();

            File.WriteAllText(Path.Combine(TmpDirectoryPath, "Program.cs"), sourceCode);
        }

        public static void CompileProject()
        {
            var compileProjectProcess = System.Diagnostics.Process.Start(
                "cmd.exe", $"/C cd {TmpDirectoryPath} && dotnet publish -o ../");
            compileProjectProcess.WaitForExit();
        }
    }
}
