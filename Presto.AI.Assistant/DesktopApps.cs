using System.Diagnostics;

namespace Presto.AI.Assistant;

public static class DesktopApps
{
    public static void OpenApp(string fileName, string arguments = "")
    {
        ProcessStartInfo processStartInfo = new(fileName, arguments);
        Process.Start(processStartInfo);
    }

    public static string GetChromeExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), @"Google\Chrome\Application", "chrome.exe");
    public static void OpenChrome(string profileName = "Default") =>
        OpenApp(
            GetChromeExecutablePath(),
            !string.IsNullOrWhiteSpace(profileName)
                ? $"--profile-directory=\"{profileName}\""
                : "");

    public static string GetSlackExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "slack", "slack.exe");
    public static void OpenSlack() => OpenApp(GetSlackExecutablePath());

    public static string GetNotepadPlusPlusExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "Notepad++", "notepad++.exe");
    public static void OpenNotepadPlusPlus() => OpenApp(GetNotepadPlusPlusExecutablePath());

    public static string GetDockerDesktopExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "Docker", "Docker", "Docker Desktop.exe");
    public static void OpenDockerDesktop() => OpenApp(GetDockerDesktopExecutablePath());

    public static string GetVisualStudioCodeExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "Programs", "Microsoft VS Code", "Code.exe");
    public static void OpenVisualStudioCode() => OpenApp(GetVisualStudioCodeExecutablePath());

    public static string GetSteamExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), "Steam", "steam.exe");
    public static void OpenSteam() => OpenApp(GetSteamExecutablePath());
}