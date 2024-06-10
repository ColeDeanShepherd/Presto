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

public class FileSystem { }

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

    public static Result<real, string> read_real(Console console)
    {
        var readResult = read_line(console);
        if (readResult.IsErr)
        {
            return Result.ErrUnit(readResult.Error!);
        }

        string str = readResult.Value!;

        return parse_real(str);
    }

    public static string uppercase(string s) => s.ToUpperInvariant();

    public static Result<IEnumerable<char>, string> read_text_file(FileSystem fs, string fileName)
    {
        try
        {
            return Result.Ok<IEnumerable<char>, string>(File.ReadAllText(fileName));
        }
        catch (Exception e)
        {
            return Result.Err<IEnumerable<char>, string>(e.ToString());
        }
    }

    public static IEnumerable<string> get_lines(IEnumerable<char> textStream) =>
        new string(textStream.ToArray()).Split(new[] { "\r\n", "\r", "\n" }, StringSplitOptions.None);

    public static IEnumerable<TResult> map<TSource, TResult>(
        IEnumerable<TSource> source,
        Func<TSource, TResult> selector
    ) =>
        source.Select(selector);

    public static IEnumerable<IGrouping<TKey, TSource>> group_by<TSource, TKey>(
        IEnumerable<TSource> source,
        Func<TSource, TKey> keySelector
    ) =>
        source.GroupBy(keySelector);

    public static IOrderedEnumerable<TSource> sort_by<TSource, TKey>(
        IEnumerable<TSource> source,
        Func<TSource, TKey> keySelector
    ) =>
        source.OrderBy(keySelector);

    public static Func<T, TResult> ReverseCompose<T, TIntermediate, TResult>(
        Func<T, TIntermediate> f,
        Func<TIntermediate, TResult> g)
    {
        return x => g(f(x));
    }

    public static IEnumerable<string> split_by(string str, string separator) =>
        str.Split(separator);

    public static T1 t4_1st<T1, T2, T3, T4>(Tuple<T1, T2, T3, T4> t) => t.Item1;
    public static T2 t4_2nd<T1, T2, T3, T4>(Tuple<T1, T2, T3, T4> t) => t.Item2;
    public static T3 t4_3rd<T1, T2, T3, T4>(Tuple<T1, T2, T3, T4> t) => t.Item3;
    public static T4 t4_4th<T1, T2, T3, T4>(Tuple<T1, T2, T3, T4> t) => t.Item4;

    public static T unwrap<T, E>(Result<T, E> result) =>
        result.IsOk
            ? result.Value!
            : throw new Exception($"Failed to unwrap result error: {result.Error}");
}