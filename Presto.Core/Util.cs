namespace Presto.Core;

public static class Util
{
    public static Func<Task> ToAsync(Action func) =>
        () =>
        {
            func();
            return Task.CompletedTask;
        };
}
