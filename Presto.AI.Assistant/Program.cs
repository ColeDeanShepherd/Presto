using Presto.Core;

namespace Presto.AI.Assistant;

internal class Program
{
    static void Main(string[] args)
    {
        SetupDependencyInjection();
        Run();
    }

    static void SetupDependencyInjection()
    {
        var desktopApps = new DesktopApps();
        Injected<IDesktopApps>.Register(desktopApps);
    }

    static void Run(Injected<IDesktopApps> desktopApps = default)
    {
        ColesCommands.OpenRecRoomDesktopEnvironment();
    }
}