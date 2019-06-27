using System.Collections.Generic;
using System.Linq;

public static class IEnumerableExtensions
{
    public static IEnumerable<T> AsEnumerable<T>(T t)
    {
        yield return t;
    }
    public static IEnumerable<T> Concat<T>(params IEnumerable<T>[] enumerables)
    {
        if (enumerables.Length == 0) { return Enumerable.Empty<T>(); }

        var result = enumerables[0];

        foreach (var enumerable in enumerables.Skip(1))
        {
            result = result.Concat(enumerable);
        }

        return result;
    }
}