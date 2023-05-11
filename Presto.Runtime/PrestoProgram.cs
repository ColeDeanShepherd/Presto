namespace Presto.Runtime;

public static partial class PrestoProgram
{
    public static bool eq<T>(T a, T b) => (a != null) ? a.Equals(b) : (b == null);
    public static bool not(bool x) => !x;
    public static uint length(string x) => (uint)x.Length;
}