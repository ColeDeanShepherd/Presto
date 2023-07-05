using Presto.Core;

namespace Presto.AI.Assistant;

public static class ColesCommands
{
    public static void OpenRecRoomDesktopEnvironment(Injected<IDesktopApps> desktopApps = default)
    {
        desktopApps.Value.OpenYouTube();
        OpenColesNotesDoc();
        desktopApps.Value.OpenGmail();
        desktopApps.Value.OpenGoogleCalendar();
        OpenRecRoomAdoPullRequests();
        desktopApps.Value.OpenSlack();
        desktopApps.Value.OpenApp(GetRecNetDevEnvPath());
        desktopApps.Value.OpenVisualStudioCode();
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
