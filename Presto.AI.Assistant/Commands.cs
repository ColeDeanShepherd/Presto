namespace Presto.AI.Assistant;

public static class Commands
{
    public static readonly Command[] All = [
        #region Desktop Apps
        
        Command.Create("Open Firefox", Desktop.OpenFirefox),
        Command.Create("Open Google Chrome", Desktop.OpenGoogleChrome),
        Command.Create("Open Slack", Desktop.OpenSlack),
        Command.Create("Open Notepad++", Desktop.OpenNotepadPlusPlus),
        Command.Create("Open Docker Desktop", Desktop.OpenDockerDesktop),
        Command.Create("Open Visual Studio Code", Desktop.OpenVisualStudioCode),
        Command.Create("Open Steam", Desktop.OpenSteam),
        Command.Create("Open Snipping Tool", Desktop.OpenSnippingTool),

        #endregion Desktop Apps

        #region Web Browser

        Command.Create("Open YouTube", WebBrowser.OpenYouTube),
        Command.Create("Open Gmail", WebBrowser.OpenGmail),
        Command.Create("Open Google Calendar", WebBrowser.OpenGoogleCalendar),
        Command.Create("Open Todoist", WebBrowser.OpenTodoist),
        Command.Create("Open ChatGPT", WebBrowser.OpenChatGPT),
        Command.Create("Open Statsig Console", WebBrowser.OpenStatsigConsole),

        #endregion Web Browser

        #region Rec Room

        Command.Create(
            "Open Cole Shepherd's RR Dev Desktop",
            () =>
            {
                // Open web pages.
                Desktop.OpenFirefox();
                WebBrowser.OpenUrl("https://app.todoist.com");
                WebBrowser.OpenYouTube();
                WebBrowser.OpenGmail();
                WebBrowser.OpenGoogleCalendar();
                ADO.OpenPullRequestsWebPage();
                
                // Open "Cole's Notes".
                WebBrowser.OpenUrl("https://docs.google.com/document/d/1IxN-PAExj77bW0WR1PcX_9LkDB193_sGGL0fAkwrYQ4/edit#");

                // Open desktop applications.
                Desktop.OpenSlack();
                RecNet.OpenRecNetDevEnv();
                Desktop.OpenNotepadPlusPlus();
            }),

        #endregion Rec Room
    ];
}
