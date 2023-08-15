using System.Collections.Immutable;
using static Presto.Experiments.Program;

namespace Presto.Experiments
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello, World!");
        }

        static int? First(int[] x) => x.Any() ? x[0] : null;

        public class NonEmptyArray<T>
        {
            public static NonEmptyArray<T>? TryCreate(ImmutableArray<T> x) =>
                x.Any()
                    ? new NonEmptyArray<T>(x)
                    : null;

            public static NonEmptyArray<T> CreateOrThrow(ImmutableArray<T> x) =>
                x.Any()
                    ? new NonEmptyArray<T>(x)
                    : throw new Exception();

            public readonly ImmutableArray<T> Value;

            private NonEmptyArray(ImmutableArray<T> value)
            {
                Value = value;
            }
        }

        public struct MutableNonEmptyArray<T>
        {
            public static MutableNonEmptyArray<T>? TryCreate(T[] x) =>
                x.Any()
                    ? new MutableNonEmptyArray<T>(x)
                    : null;

            public static MutableNonEmptyArray<T> CreateOrThrow(T[] x) =>
                x.Any()
                    ? new MutableNonEmptyArray<T>(x)
                    : throw new Exception();

            public readonly T[] Value;

            public MutableNonEmptyArray(MutableNonEmptyArray<T> other)
            {
                if (!other.Value.Any())
                {
                    throw new Exception();
                }

                Value = other.Value;
            }

            private MutableNonEmptyArray(T[] value)
            {
                Value = value;
            }
        }

        public interface TypeRefinement<T>
        {
            TypeRefinement<T> Create();
            bool Refine(T value);
        }

        public class RefinedType<TValue, TRefinement> where TRefinement : TypeRefinement<TValue>
        {
            public static RefinedType<TValue, TRefinement>? TryCreate(TValue x) =>
                x.Any()
                    ? new RefinedType<TValue, TRefinement>(x)
            : null;

            public static NRefinedType<TValue, TRefinement> CreateOrThrow(TValue x) =>
                x.Any()
                    ? new RefinedType<TValue, TRefinement>(x)
                    : throw new Exception();

            public readonly TValue Value;

            private RefinedType(TValue value)
            {
                Value = value;
            }
        }

        // From weak type safety to strong type safety:

        // ------------------------------

        // def First(x): return x[0]

        // Dynamically typing without return type annotations (ex: Python).

        // What does the compiler ensure for us?
        // - The function must be called with one argument.

        // What do unit tests ensure for us?
        // - The function throws when it's passed a non-indexable type.
        // - The function throws when it's passed an empty indexable type.
        // - The function returns the first element of the argument when it's passed a non-empty indexable type.

        // Note that we cannot exhaustively test all possible arguments (all types, all lengths).

        // What runtime errors can we get?
        // - An exception, if it's passed a non-indexable type.
        // - An exception, if it's passed an empty indexable type.

        // ------------------------------

        //static dynamic First(dynamic x) => x[0];

        // Dynamic typing with return value specification.

        // What does the compiler ensure for us?
        // - The function must be called with one argument.
        // - The function returns something.

        // What do unit tests ensure for us?
        // - The function throws when it's passed a non-indexable type.
        // - The function throws when it's passed an empty indexable type.
        // - The function returns the first element of the argument when it's passed a non-empty indexable type.

        // What runtime errors can we get?
        // - An exception, if it's passed a non-indexable type.
        // - An exception, if it's passed an empty indexable type.

        // ------------------------------
    }
}