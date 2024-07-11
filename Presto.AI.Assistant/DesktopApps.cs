using System.Diagnostics;

namespace Presto.AI.Assistant;

using static FileSystem;

public static class DesktopApps
{
    public static void OpenApp(string fileName, string arguments = "")
    {
        ProcessStartInfo processStartInfo = new(fileName, arguments);
        Process.Start(processStartInfo);
    }

    public static string GetGoogleChromeExecutablePath() =>
        Path.Combine(GetProgramFilesFolderPath(), @"Google\Chrome\Application\chrome.exe");
    public static void OpenGoogleChrome() => OpenGoogleChrome("Default");
    public static void OpenGoogleChrome(string profileName) =>
        OpenApp(
            GetGoogleChromeExecutablePath(),
            arguments: (profileName != null)
                ? $"--profile-directory=\"{profileName}\""
                : "");

    public static string GetSlackExecutablePath() =>
        Path.Combine(GetLocalAppDataFolderPath(), "slack/slack.exe");
    public static void OpenSlack() => OpenApp(GetSlackExecutablePath());

    public static string GetNotepadPlusPlusExecutablePath() =>
        Path.Combine(GetProgramFilesFolderPath(), "Notepad++/notepad++.exe");
    public static void OpenNotepadPlusPlus() => OpenApp(GetNotepadPlusPlusExecutablePath());

    public static string GetDockerDesktopExecutablePath() =>
        Path.Combine(GetProgramFilesFolderPath(), "Docker/Docker/Docker Desktop.exe");
    public static void OpenDockerDesktop() => OpenApp(GetDockerDesktopExecutablePath());

    public static string GetVisualStudioCodeExecutablePath() =>
        Path.Combine(GetLocalAppDataFolderPath(), "Programs/Microsoft VS Code/Code.exe");
    public static void OpenVisualStudioCode() => OpenApp(GetVisualStudioCodeExecutablePath());

    public static string GetSteamExecutablePath() =>
        Path.Combine(GetProgramFilesX86FolderPath(), "Steam/steam.exe");
    public static void OpenSteam() => OpenApp(GetSteamExecutablePath());
}