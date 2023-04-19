namespace Presto.Runtime;

public static partial class PrestoProgram
{
    static bool eq<T>(T a, T b) => (a != null) ? a.Equals(b) : (b == null);
    static bool not(bool x) => !x;
}