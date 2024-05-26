namespace Presto.Runtime;

using nat = System.UInt32;

public class Unit
{
    public static readonly Unit Instance = new();
}

public class Console
{
}

public static partial class PrestoProgram
{
    public static bool eq<T>(T a, T b) => (a != null) ? a.Equals(b) : (b == null);
    public static bool not(bool x) => !x;
    public static uint length(string x) => (uint)x.Length;
    public static T last_or_default<T>(IEnumerable<T> x, T @default) => x.LastOrDefault(@default);
    public static Unit print(Console console, string text) { System.Console.Write(text); return Unit.Instance; }
    public static Unit print_line(Console console, string text) { System.Console.WriteLine(text); return Unit.Instance; }
    public static string? read_line(Console console) => System.Console.ReadLine();
    public static string concatenate(string a, string b ) => a + b;
    public static nat sum(nat a, nat b) => a + b;
    public static string to_string<T>(T x) => x.ToString();
    public static double parse_real(string s) => double.Parse(s);
}