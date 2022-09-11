using System.Diagnostics;

namespace Presto
{
    public class CSharpCodeRunner
    {
        public async Task RunCode(string cSharpCode)
        {
            // Generate executable.
            string executablePath = await GenerateExecutable(cSharpCode);

            // Run the executable.
            await RunExe(executablePath);
        }

        private async Task<string> GenerateExecutable(string cSharpCode)
        {
            if (Directory.Exists("tmp"))
            {
                Directory.Delete("tmp", recursive: true);
            }

            Directory.CreateDirectory("tmp");

            await GenerateTmpCsProject();

            await File.WriteAllTextAsync("tmp/Program.cs", cSharpCode);

            await CompileTmpCsProject();

            return "tmp/bin/Debug/net6.0/tmp.exe";
        }

        private async Task GenerateTmpCsProject()
        {
            await RunExe("dotnet", "new console", "tmp");
        }

        private async Task CompileTmpCsProject()
        {
            await RunExe("dotnet", "build", "tmp");
        }

        private async Task RunExe(string exeFilePath, string args = "", string? workingDirectory = null)
        {
            ProcessStartInfo processStartInfo = new();
            processStartInfo.FileName = exeFilePath;
            processStartInfo.Arguments = args;

            if (workingDirectory != null)
            {
                processStartInfo.WorkingDirectory = workingDirectory;
            }

            processStartInfo.CreateNoWindow = true;
            processStartInfo.RedirectStandardOutput = true;
            processStartInfo.RedirectStandardError = true;
            processStartInfo.UseShellExecute = false;

            Process process = new();
            process.StartInfo = processStartInfo;
            process.OutputDataReceived += (s, e) => Console.WriteLine(e.Data);
            process.ErrorDataReceived += (s, e) => Console.WriteLine(e.Data);

            process.Start();
            process.BeginOutputReadLine();
            process.BeginErrorReadLine();

            await process.WaitForExitAsync();
        }
    }
}
