using System.Collections.Generic;

namespace Presto
{
    public static class ExtensionMethods
    {
        public static int GetSequenceHashCode<T>(this IReadOnlyCollection<T> collection)
        {
            int hashCode = 486187739;
            
            foreach (var element in collection)
            {
                hashCode = (hashCode * 16777619) ^ element.GetHashCode();
            }

            return hashCode;
        }
    }
}
