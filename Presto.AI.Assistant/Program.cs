using Presto.Core;

namespace Presto.AI.Assistant;

internal class Program
{
    static async Task Main(string[] args)
    {
        SetupDependencyInjection();
        await Run();
    }

    static void SetupDependencyInjection()
    {
        var desktopApps = new DesktopApps();
        Injected<IDesktopApps>.Register(desktopApps);
    }

    static async Task Run(Injected<IDesktopApps> desktopApps = default)
    {
        await ColesCommands.OpenRecRoomDesktopEnvironment();
    }
}