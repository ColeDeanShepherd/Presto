using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Presto.AI.Assistant;

// TODO:
// - non-singleton service lifetimes (scoped, transient?)
// - thread-safety
public struct Injected<TInterface>
{
    public static void Register(TInterface implementation)
    {
        _value = implementation;
        _hasValue = true;
    }

    private static TInterface? _value;
    private static bool _hasValue;

    public TInterface Value =>
        _hasValue
            ? _value!
            : throw new InvalidOperationException();
}

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

public static class ColesCommands
{
    public static void OpenRecRoomDesktopEnvironment(Injected<IDesktopApps> desktopApps = default)
    {
        desktopApps.Value.OpenChrome();
        desktopApps.Value.OpenYouTube();
        OpenColesNotesDoc();
        desktopApps.Value.OpenGmail();
        desktopApps.Value.OpenGoogleCalendar();
        OpenRecRoomAdoPullRequests();
        desktopApps.Value.OpenSlack();
        desktopApps.Value.OpenApp(GetRecNetDevEnvPath());
        desktopApps.Value.OpenNotepadPlusPlus();
    }

    public static void OpenColesNotesDoc(Injected<IDesktopApps> desktopApps = default) =>
        desktopApps.Value.OpenUrlInBrowser("https://docs.google.com/document/d/1IxN-PAExj77bW0WR1PcX_9LkDB193_sGGL0fAkwrYQ4/edit#");

    public static void OpenRecRoomAdoPullRequests(Injected<IDesktopApps> desktopApps = default) =>
        desktopApps.Value.OpenUrlInBrowser("https://dev.azure.com/recroom/_pulls");

    public static string GetRecNetDevEnvPath() =>
        Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
            @"dev\DevTools\RecNetDevEnv\bin\Release\RecNetDevEnv.exe");
}

internal class Program
{
    static void Main(string[] args)
    {
        var desktopApps = new DesktopApps();
        Injected<IDesktopApps>.Register(desktopApps);

        Run();
    }

    static void Run(Injected<IDesktopApps> desktopApps = default)
    {
        ColesCommands.OpenRecRoomDesktopEnvironment();
    }
}