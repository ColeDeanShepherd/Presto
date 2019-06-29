namespace Presto
{
    public struct Result<T, E>
    {
        public static Result<T, E> Ok(T value)
        {
            return new Result<T, E>(value, default(E), succeeded: true);
        }
        public static Result<T, E> Err(E error)
        {
            return new Result<T, E>(default(T), error, succeeded: false);
        }

        public readonly T Value;
        public readonly E Error;
        public readonly bool Succeeded;

        private Result(T value, E error, bool succeeded)
        {
            Value = value;
            Error = error;
            Succeeded = succeeded;
        }
    }
}
