namespace Presto.AI.Assistant;

public static class RecNet
{
    public static string GetRecNetDevEnvPath() =>
        Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
            @"dev\DevTools\RecNetDevEnv\bin\Release\RecNetDevEnv.exe");

    public static void OpenRecNetDevEnv() =>
        Desktop.OpenApp(GetRecNetDevEnvPath());
}
