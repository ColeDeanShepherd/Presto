namespace Presto.AI.Assistant;

public static class ADO
{
    public static void OpenPullRequestsWebPage() => WebBrowser.OpenUrl("https://dev.azure.com/recroom/_pulls");
}