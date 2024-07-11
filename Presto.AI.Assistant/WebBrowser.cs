using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Presto.AI.Assistant;

public static class WebBrowser
{
    public static void OpenUrl(string url)
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

    public static void OpenYouTube() =>
        OpenUrl("https://youtube.com/");

    public static void OpenGmail() =>
        OpenUrl("https://mail.google.com/");

    public static void OpenGoogleCalendar() =>
        OpenUrl("https://calendar.google.com/");

    public static void OpenTodoist() =>
        OpenUrl("https://todoist.com");

    public static void OpenChatGPT() =>
        OpenUrl("https://chat.openai.com");
}
