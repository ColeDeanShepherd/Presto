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
        // convert mp4 to gif

        #endregion Desktop Apps

        #region Web Browser

        new Command("Open YouTube", ToAsync(WebBrowser.OpenYouTube)),
        new Command("Open Gmail", ToAsync(WebBrowser.OpenGmail)),
        new Command("Open Google Calendar", ToAsync(WebBrowser.OpenGoogleCalendar)),
        new Command("Open Todoist", ToAsync(WebBrowser.OpenTodoist)),
        new Command("Open ChatGPT", ToAsync(WebBrowser.OpenChatGPT)),
        new Command("Open Statsig Console", ToAsync(WebBrowser.OpenStatsigConsole)),

        #endregion Web Browser

        #region Rec Room
        
        new Command("Open RR Pull Requests", ToAsync(ColesCommands.OpenRecRoomAdoPullRequests)),
        new Command("Open Rec Room", ToAsync(ColesCommands.OpenRecRoom)),
        new Command("Open RecNetDevEnv", ToAsync(ColesCommands.OpenRecNetDevEnv)),
        new Command("Open RR Production Environment Dashboard", ToAsync(() => WebBrowser.OpenUrl("https://portal.azure.com/#@recroom.com/dashboard/arm/subscriptions/580e38c5-ddc0-4f3a-9725-512e5d12bfbd/resourcegroups/dashboards/providers/microsoft.portal/dashboards/production-environment"))),
        new Command("Open RR Grafana", ToAsync(() => WebBrowser.OpenUrl("https://grafana-prod-eastus-htczbac4c7gtchej.eus.grafana.azure.com/d/tkzWGMOVz/recnet-health?orgId=1&from=now-1h&to=now&refresh=5m"))),

        #endregion Rec Room

        #region Cole's Commands

        new Command("Open Cole's Notes", ToAsync(ColesCommands.OpenColesNotesDoc)),
        new Command("Open Cole's RR Desktop Environment", ColesCommands.OpenRecRoomDesktopEnvironment),
        // dashboard

        #endregion Cole's Commands
    ];
}
