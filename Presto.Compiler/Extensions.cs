namespace System.Collections.Generic
{
    public static class PrestoIEnumerableExtensions
    {
        public static IEnumerable<TResult> Map<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector) =>
            source.Select(selector);
    }
}
