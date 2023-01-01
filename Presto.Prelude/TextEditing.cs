using System.Text.RegularExpressions;

namespace Presto.Prelude
{
    public static class TextEditing
    {
        public static bool IsLineTerminatorCharacter(char c) =>
            (c == '\r') || (c == '\n');

        public static string DetectLineTerminator(string text, string? defaultLineTerminator = null)
        {
            if (text.Contains("\r\n"))
            {
                return "\r\n";
            }
            else if (text.Contains("\n"))
            {
                return "\n";
            }
            else
            {
                return defaultLineTerminator ?? Environment.NewLine;
            }
        }

        public static string DuplicateAndTransformLines(
            string text,
            Regex regex,
            Func<string, Match, string> transformDuplicateLine)
        {
            string lineTerminator = DetectLineTerminator(text);

            uint charIndex = 0;

            while (true)
            {
                Match match = regex.Match(text, (int)charIndex);
                if (!match.Success)
                {
                    break;
                }

                (_, string line) = GetCurrentLine(text, (uint)match.Index);
                string newLine = transformDuplicateLine(line, match);
                (text, charIndex) = AddLineAfterCurrentLine(text, (uint)match.Index, newLine, lineTerminator);
            }

            return text;
        }

        public static uint GetIndexOfStartOfLine(string text, uint charIndex)
        {
            var indexOfLastLineTerminatorChar = text.LastIndexOf('\n', charIndex);

            return (indexOfLastLineTerminatorChar != null)
                ? indexOfLastLineTerminatorChar.Value + 1
                : 0;
        }

        public static uint GetIndexOfStartOfNextLine(string text, uint charIndex)
        {
            uint indexOfLineTerminator = GetIndexOfLineTerminator(text, charIndex);
            uint lineTerminatorLength =
                (uint)text
                    .Skip(indexOfLineTerminator)
                    .TakeWhile(IsLineTerminatorCharacter)
                    .Count();

            return indexOfLineTerminator + lineTerminatorLength;
        }

        public static uint GetIndexOfLineTerminator(string text, uint charIndex) =>
            text.IndexOfAny(new[] { '\r', '\n' }, charIndex) ?? (uint)text.Length;

        public static (uint, string) GetCurrentLine(string text, uint charIndex)
        {
            uint indexOfStartOfLine = GetIndexOfStartOfLine(text, charIndex);
            uint indexOfEndLineTerminator = GetIndexOfLineTerminator(text, charIndex);
            uint lineLength = indexOfEndLineTerminator - indexOfStartOfLine;

            return (indexOfStartOfLine, text.Substring(indexOfStartOfLine, lineLength));
        }

        public static (string, uint) AddLineAfterCurrentLine(string text, uint charIndex, string newLine, string lineTerminator)
        {
            uint indexOfStartOfNextLine = GetIndexOfStartOfNextLine(text, charIndex);
            uint indexOfStartOfLineAfterNewLine = indexOfStartOfNextLine + (uint)newLine.Length + (uint)lineTerminator.Length;

            return (
                $"{text.AsSpan(0, indexOfStartOfNextLine)}{newLine}{lineTerminator}{text.AsSpan(indexOfStartOfNextLine)}",
                indexOfStartOfLineAfterNewLine
            );
        }

        public static async Task TransformTextFile(string filePath, Func<string, string> transformText)
        {
            string fileText = await File.ReadAllTextAsync(filePath);
            fileText = transformText(fileText);
            await File.WriteAllTextAsync(filePath, fileText);
        }
    }
}