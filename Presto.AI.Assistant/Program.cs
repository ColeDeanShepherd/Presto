namespace Presto.AI.Assistant;

public static class Program
{
    public static async Task Main(string[] args)
    {
        await ColesCommands.OpenRecRoomDesktopEnvironment();
    }
}

/*
C# Language Improvements I'd Like
* No classes
* Green threads instead of async/await
* No need for semicolons


Random Ideas
* What if calling no-arg functions didn't require parenthesis?
  There's an ambiguity when we want to reference a function though.
  Maybe we can reference a function with &FuncName? Or ref(FuncName)?
*/