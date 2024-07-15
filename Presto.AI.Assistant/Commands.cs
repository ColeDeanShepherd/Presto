namespace Presto.AI.Assistant;

using static Presto.Core.Util;

public static class Commands
{
    public static readonly Command[] All = [
        #region Desktop Apps

        new Command("Open Google Chrome", ToAsync(DesktopApps.OpenGoogleChrome)),
        new Command("Open Slack", ToAsync(DesktopApps.OpenSlack)),
        new Command("Open Notepad++", ToAsync(DesktopApps.OpenNotepadPlusPlus)),
        new Command("Open Docker Desktop", ToAsync(DesktopApps.OpenDockerDesktop)),
        new Command("Open Visual Studio Code", ToAsync(DesktopApps.OpenVisualStudioCode)),
        new Command("Open Steam", ToAsync(DesktopApps.OpenSteam)),
        new Command("Open Snipping Tool", ToAsync(DesktopApps.OpenSnippingTool)),

        #endregion Desktop Apps

        #region Web Browser

        new Command("Open YouTube", ToAsync(WebBrowser.OpenYouTube)),
        new Command("Open Gmail", ToAsync(WebBrowser.OpenGmail)),
        new Command("Open Google Calendar", ToAsync(WebBrowser.OpenGoogleCalendar)),
        new Command("Open Todoist", ToAsync(WebBrowser.OpenTodoist)),
        new Command("Open ChatGPT", ToAsync(WebBrowser.OpenChatGPT)),
        new Command("Open Statsig Console", ToAsync(WebBrowser.OpenStatsigConsole)),

        #endregion Web Browser
    ];
}
