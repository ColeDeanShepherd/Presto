namespace Presto.Runtime;

using System.Runtime.CompilerServices;
using static System.Net.Mime.MediaTypeNames;
using nat = System.UInt32;
using real = System.Decimal;

public class Unit
{
    public static readonly Unit Instance = new();
}

public class Console
{
}

public record Result<T, E>(bool IsOk, T? Value, E? Error)
{
    public bool IsErr => !IsOk;

    public static implicit operator Result<T, E>(Result<Unit, E> r) =>
        new Result<T, E>(r.IsOk, default, r.Error);
}

public static class Result
{
    public static Result<T, E> Ok<T, E>(T value) => new Result<T, E>(true, value, default);
    public static Result<T, E> Err<T, E>(E error) => new Result<T, E>(false, default, error);
    public static Result<Unit, E> ErrUnit<E>(E error) => new Result<Unit, E>(false, Unit.Instance, error);
}

public static partial class PrestoProgram
{
    public static TOut __exec<TOut>(Func<TOut> func) => func();

    public static bool eq<T>(T a, T b) => (a != null) ? a.Equals(b) : (b == null);
    public static bool not(bool x) => !x;
    public static uint length(string x) => (uint)x.Length;
    public static T last_or_default<T>(IEnumerable<T> x, T @default) => x.LastOrDefault(@default);

    public static Result<Unit, string> print(Console console, string text)
    {
        try
        {
            System.Console.Write(text);
        }
        catch (Exception ex)
        {
            return Result.Err<Unit, string>(ex.ToString());
        }

        return Result.Ok<Unit, string>(Unit.Instance);
    }

    public static Result<Unit, string> print_line(Console console, string text)
    {
        try
        {
            System.Console.WriteLine(text);
        }
        catch (Exception ex)
        {
            return Result.Err<Unit, string>(ex.ToString());
        }

        return Result.Ok<Unit, string>(Unit.Instance);
    }

    public static Result<string?, string> read_line(Console console)
    {
        try
        {
            return Result.Ok<string?, string>(System.Console.ReadLine());
        }
        catch (Exception ex)
        {
            return Result.Err<string?, string>(ex.ToString());
        }
    }

    public static string concatenate(string a, string b ) => a + b;
    public static nat sum(nat a, nat b) => a + b;
    public static string to_string<T>(T x) => x.ToString()!;
    public static Result<real, string> parse_real(string s)
    {
        try
        {
            return Result.Ok<real, string>(real.Parse(s));
        }
        catch (Exception ex)
        {
            return Result.Err<real, string>(ex.ToString());
        }
    }
    public static string uppercase(string s) => s.ToUpperInvariant();
}