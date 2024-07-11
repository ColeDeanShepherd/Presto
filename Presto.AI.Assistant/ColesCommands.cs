namespace Presto.AI.Assistant;

public static class ColesCommands
{
    public static async Task OpenRecRoomDesktopEnvironment()
    {
        #region Web Pages

        WebBrowser.OpenYouTube();

        // Wait to give the web browser a moment to open.
        await Task.Delay(5000);

        WebBrowser.OpenTodoist();
        OpenColesNotesDoc();
        WebBrowser.OpenGmail();
        WebBrowser.OpenGoogleCalendar();
        WebBrowser.OpenChatGPT();
        OpenRecRoomAdoPullRequests();

        #endregion Web Pages

        #region Desktop Apps

        DesktopApps.OpenSlack();
        DesktopApps.OpenApp(GetRecNetDevEnvPath());
        DesktopApps.OpenVisualStudioCode();
        DesktopApps.OpenDockerDesktop();
        DesktopApps.OpenSteam();

        #endregion Desktop Apps
    }

    public static void OpenColesNotesDoc() =>
        WebBrowser.OpenUrl("https://docs.google.com/document/d/1IxN-PAExj77bW0WR1PcX_9LkDB193_sGGL0fAkwrYQ4/edit#");

    public static void OpenRecRoomAdoPullRequests() =>
        WebBrowser.OpenUrl("https://dev.azure.com/recroom/_pulls");

    public static string GetRecNetDevEnvPath() =>
        Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
            @"dev\DevTools\RecNetDevEnv\bin\Release\RecNetDevEnv.exe");
}
