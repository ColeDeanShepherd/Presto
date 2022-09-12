namespace Presto;

public static class DBC
{
    public static void Assert(bool condition, string? errorMessage = null)
    {
        if (!condition)
        {
            throw new Exception(errorMessage);
        }
    }
}