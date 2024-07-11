namespace Presto.AI.Assistant;

public record Command(string Name, Func<Task> RunFunc, string? Description = null);