namespace Presto.Runtime;

using Nat = System.UInt32;
using Real = System.Decimal;
using Text = System.String;

public class Unit
{
    public static readonly Unit Instance = new();
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
    public static Nat length(Text x) => (Nat)x.Length;
    public static T last_or_default<T>(IEnumerable<T> x, T @default) => x.LastOrDefault(@default);

    public static Result<Unit, Text> __unsafe_print(Text text)
    {
        try
        {
            System.Console.Write(text);
        }
        catch (Exception ex)
        {
            return Result.Err<Unit, Text>(ex.ToString());
        }

        return Result.Ok<Unit, Text>(Unit.Instance);
    }

    public static Result<Unit, Text> __unsafe_print_line(Text text)
    {
        try
        {
            System.Console.WriteLine(text);
        }
        catch (Exception ex)
        {
            return Result.Err<Unit, Text>(ex.ToString());
        }

        return Result.Ok<Unit, Text>(Unit.Instance);
    }

    public static Result<Text?, Text> __unsafe_read_line()
    {
        try
        {
            return Result.Ok<Text?, Text>(System.Console.ReadLine());
        }
        catch (Exception ex)
        {
            return Result.Err<Text?, Text>(ex.ToString());
        }
    }

    public static Text concatenate(Text a, Text b ) => a + b;
    public static Nat sum(Nat a, Nat b) => a + b;
    public static Text to_text<T>(T x) => x!.ToString()!;
    public static Result<Real, Text> parse_real(Text s)
    {
        try
        {
            return Result.Ok<Real, Text>(Real.Parse(s));
        }
        catch (Exception ex)
        {
            return Result.Err<Real, Text>(ex.ToString());
        }
    }

    public static Result<Real, Text> __unsafe__read_real()
    {
        var readResult = __unsafe_read_line();
        if (readResult.IsErr)
        {
            return Result.ErrUnit(readResult.Error!);
        }

        Text str = readResult.Value!;

        return parse_real(str);
    }

    public static Text uppercase(Text s) => s.ToUpperInvariant();

    public static Result<IEnumerable<char>, Text> __unsafe_read_text_file(Text fileName)
    {
        try
        {
            return Result.Ok<IEnumerable<char>, Text>(File.ReadAllText(fileName));
        }
        catch (Exception e)
        {
            return Result.Err<IEnumerable<char>, Text>(e.ToString());
        }
    }

    public static IEnumerable<Text> get_lines(IEnumerable<char> textStream) =>
        new Text(textStream.ToArray()).Split(new[] { "\r\n", "\r", "\n" }, StringSplitOptions.None);

    public static IEnumerable<TResult> map<TSource, TResult>(
        IEnumerable<TSource> source,
        Func<TSource, TResult> selector
    ) =>
        source.Select(selector);

    public static Unit for_each<TSource, TResult>(
        IEnumerable<TSource> source,
        Func<TSource, TResult> selector
    )
    {
        foreach (var item in source)
        {
            selector(item);
        }

        return Unit.Instance;
    }

    public static IEnumerable<IGrouping<TKey, TResultElement>> group_by<TKey, TElement, TResultElement>(
        IEnumerable<TElement> source,
        Func<TElement, TKey> keySelector,
        Func<TElement, TResultElement> elementSelector
    ) =>
        source.GroupBy(keySelector, elementSelector);

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

    public static IEnumerable<Text> split_by(Text str, Text separator) =>
        str.Split(separator);

    public static T1 _1st<T1, T2>((T1, T2) t) => t.Item1;
    public static T2 _2nd<T1, T2>((T1, T2) t) => t.Item2;

    public static T1 t4_1st<T1, T2, T3, T4>((T1, T2, T3, T4) t) => t.Item1;
    public static T2 t4_2nd<T1, T2, T3, T4>((T1, T2, T3, T4) t) => t.Item2;
    public static T3 t4_3rd<T1, T2, T3, T4>((T1, T2, T3, T4) t) => t.Item3;
    public static T4 t4_4th<T1, T2, T3, T4>((T1, T2, T3, T4) t) => t.Item4;

    public static T list_1st<T>(IEnumerable<T> l) => l.First();
    public static T list_2nd<T>(IEnumerable<T> l) => l.Skip(1).First();

    public static TKey key<TKey, T>(IGrouping<TKey, T> grouping) => grouping.Key;
    public static IEnumerable<T> values<TKey, T>(IGrouping<TKey, T> grouping) => grouping;

    public static decimal min(IEnumerable<Real> d) => d.Min();
    public static decimal max(IEnumerable<Real> d) => d.Max();
    public static decimal mean(IEnumerable<Real> d) => d.Average();
    public static decimal round(Real d, Nat decimals) => Math.Round(d, (int)decimals);

    public static T unwrap<T, E>(Result<T, E> result) =>
        result.IsOk
            ? result.Value!
            : throw new Exception($"Failed to unwrap result error: {result.Error}");
}