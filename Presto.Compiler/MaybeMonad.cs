namespace System;

public static class MaybeMonad
{
    public static T2? SelectMany<T1, TI, T2>(
        this T1? a,
        Func<T1, TI?> mapper,
        Func<T1, TI, T2> getResult)
        where T1 : class
        where T2 : class
    {
        if (a == null)
        {
            return null;
        }

        var intermediate = mapper(a);
        return (intermediate != null)
            ? getResult(a, intermediate)
            : null;
    }

    public static T2? Select<T1, T2>(this T1? a, Func<T1, T2?> fn)
        where T1 : struct
        where T2 : struct =>
        Bind(a, fn);

    public static T2? Select<T1, T2>(this T1? a, Func<T1, T2?> fn)
        where T1 : class
        where T2 : class =>
        Bind(a, fn);

    public static T2? Bind<T1, T2>(this T1? a, Func<T1, T2?> fn)
        where T1 : struct
        where T2 : struct =>
        a.HasValue
            ? fn(a.Value)
            : null;

    public static T2? Bind<T1, T2>(this T1? a, Func<T1, T2?> fn)
        where T1 : class
        where T2 : class =>
        (a != null)
            ? fn(a)
            : null;
}
