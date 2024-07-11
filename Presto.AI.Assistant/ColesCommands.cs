namespace Presto.AI.Assistant;

using static Task;
using static FileSystem;
using static DesktopApps;
using static WebBrowser;

public static class ColesCommands
{
    public static async Task OpenRecRoomDesktopEnvironment()
    {
        #region Web Pages

        OpenYouTube();

        // Wait to give the web browser a moment to open.
        await Delay(5000);

        OpenTodoist();
        OpenColesNotesDoc();
        OpenGmail();
        OpenGoogleCalendar();
        OpenChatGPT();
        OpenRecRoomAdoPullRequests();

        #endregion Web Pages

        #region Desktop Apps

        OpenSlack();
        OpenRecNetDevEnv();
        OpenVisualStudioCode();
        OpenDockerDesktop();
        OpenSteam();

        #endregion Desktop Apps
    }

    public static void OpenColesNotesDoc() =>
        OpenUrl("https://docs.google.com/document/d/1IxN-PAExj77bW0WR1PcX_9LkDB193_sGGL0fAkwrYQ4");

    public static void OpenRecRoomAdoPullRequests() =>
        OpenUrl("https://dev.azure.com/recroom/_pulls");

    public static void OpenRecNetDevEnv() => OpenApp(GetRecNetDevEnvPath());

    public static string GetRecNetDevEnvPath() =>
        Path.Combine(
            GetUserProfileFolderPath(),
            @"dev\DevTools\RecNetDevEnv\bin\Release\RecNetDevEnv.exe");
}
