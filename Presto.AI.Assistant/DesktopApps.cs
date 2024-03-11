using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Presto.AI.Assistant;

public interface IDesktopApps
{
    void OpenApp(string fileName, string arguments = "");
    void OpenUrlInBrowser(string url);

    public string GetChromeExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), @"Google\Chrome\Application", "chrome.exe");
    public void OpenChrome(string profileName = "Default") =>
        OpenApp(
            GetChromeExecutablePath(),
            !string.IsNullOrWhiteSpace(profileName)
                ? $"--profile-directory=\"{profileName}\""
                : "");

    public string GetSlackExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "slack", "slack.exe");
    public void OpenSlack() => OpenApp(GetSlackExecutablePath());

    public string GetNotepadPlusPlusExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "Notepad++", "notepad++.exe");
    public void OpenNotepadPlusPlus() => OpenApp(GetNotepadPlusPlusExecutablePath());

    public void OpenYouTube() =>
        OpenUrlInBrowser("https://youtube.com/");

    public void OpenGmail() =>
        OpenUrlInBrowser("https://mail.google.com/");

    public void OpenGoogleCalendar() =>
        OpenUrlInBrowser("https://calendar.google.com/");

    public void OpenTodoist() =>
        OpenUrlInBrowser("https://todoist.com");

    public void OpenChatGPT() =>
        OpenUrlInBrowser("https://chat.openai.com");

    public string GetDockerDesktopExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "Docker", "Docker", "Docker Desktop.exe");
    public void OpenDockerDesktop() => OpenApp(GetDockerDesktopExecutablePath());

    public string GetVisualStudioCodeExecutablePath() =>
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "Programs", "Microsoft VS Code", "Code.exe");
    public void OpenVisualStudioCode() => OpenApp(GetVisualStudioCodeExecutablePath());
}

public class DesktopApps : IDesktopApps
{
    public void OpenApp(string fileName, string arguments = "")
    {
        ProcessStartInfo processStartInfo = new(fileName, arguments);
        Process.Start(processStartInfo);
    }

    public void OpenUrlInBrowser(string url)
    {
        try
        {
            Process.Start(url);
        }
        catch
        {
            // hack because of this: https://github.com/dotnet/corefx/issues/10361
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            {
                url = url.Replace("&", "^&");
                Process.Start(new ProcessStartInfo("cmd", $"/c start {url}") { CreateNoWindow = true });
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
            {
                Process.Start("xdg-open", url);
            }
            else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            {
                Process.Start("open", url);
            }
            else
            {
                throw;
            }
        }
    }
}