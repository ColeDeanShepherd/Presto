namespace System
{
    public static class PrestoStringExtensions
    {
        public static string Substring(this string str, uint startIndex, uint length) =>
            str.Substring((int)startIndex, (int)length);

        public static uint? IndexOfAny(this string str, char[] anyOf, uint startIndex)
        {
            int resultAsInt = str.IndexOfAny(anyOf, (int)startIndex);

            return resultAsInt switch
            {
                -1 => null,
                _ => (uint)resultAsInt
            };
        }

        public static uint? LastIndexOf(this string str, char value, uint startIndex)
        {
            int resultAsInt = str.LastIndexOf(value, (int)startIndex);

            return resultAsInt switch
            {
                -1 => null,
                _ => (uint)resultAsInt
            };
        }

        public static ReadOnlySpan<char> AsSpan(this string text, uint start) =>
            text.AsSpan((int)start);

        public static ReadOnlySpan<char> AsSpan(this string text, uint start, uint length) =>
            text.AsSpan((int)start, (int)length);
    }
}

namespace System.Collections.Generic
{
    public static class PrestoIEnumerableExtensions
    {
        public static IEnumerable<TSource> Skip<TSource>(this IEnumerable<TSource> source, uint count)
            => source.Skip((int)count);

        public static IEnumerable<TResult> Map<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector) =>
            source.Select(selector);
    }
}
