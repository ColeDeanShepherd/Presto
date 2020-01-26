using System;

namespace Presto
{
    public static class Dbc
    {
        public static void Precondition(bool condition)
        {
            if (!condition)
            {
                throw new Exception("Failed precondition.");
            }
        }
        public static void Postcondition(bool condition)
        {
            if (!condition)
            {
                throw new Exception("Failed postcondition.");
            }
        }
    }
}
