namespace Presto.Core;

// TODO:
// - non-singleton service lifetimes (scoped, transient?)
// - thread-safety
public struct Injected<TInterface>
{
    public static void Register(TInterface implementation)
    {
        _value = implementation;
        _hasValue = true;
    }

    private static TInterface? _value;
    private static bool _hasValue;

    public TInterface Value =>
        _hasValue
            ? _value!
            : throw new InvalidOperationException();
}