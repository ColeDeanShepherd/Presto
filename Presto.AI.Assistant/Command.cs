namespace Presto.AI.Assistant;

using static Presto.Core.Util;

public record Command(string Name, Func<Task> RunFunc, string? Description = null)
{
    public static Command Create(string name, Action runFunc, string? description = null) =>
        new Command(name, ToAsync(runFunc), description);

    public static Command Create(string name, Func<Task> runFunc, string? description = null) =>
        new Command(name, runFunc, description);
}