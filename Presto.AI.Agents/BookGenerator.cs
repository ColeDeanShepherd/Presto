using System.Text;

namespace Presto.AI.Agents;

class Book
{
    public string Title { get; set; } = null!;
    public List<BookChapter> Chapters { get; set; } = null!;
}

class BookChapter
{
    public string Title { get; set; } = null!;
    public List<BookSection> Sections { get; set; } = null!;
}

class BookSection
{
    public string Title { get; set; } = null!;
    public List<BookSubsection> Subsections { get; set; } = null!;
}

class BookSubsection
{
    public string Title { get; set; } = null!;
    public string Content { get; set; } = null!;
}

public static class BookGenerator
{
    public static async Task<string> ComposeABook(string apiKey, string title, string description, bool onlyOutline)
    {
        Agent agent = new(apiKey, "gpt-3.5-turbo");
        await agent.SendSystemMessage("You are a helpful assistant.");

        async Task<T> DoUntilNoExceptions<T>(Func<Task<T>> asyncFunc)
        {
            while (true)
            {
                try
                {
                    return await asyncFunc();
                }
                catch (Exception e)
                {
                    Console.WriteLine($"Failed writing. Retrying.");
                    continue;
                }
            }
        }

        async Task<Book> WriteBook()
        {
            var book = new Book { Title = title };

            var chapterTitles = await WriteChapterTitles();

            book.Chapters =
                (
                    await Task.WhenAll(
                        chapterTitles
                            .Select(x =>
                                Task.Run(() => WriteChapter(x, agent.Fork()))))
                ).ToList();

            return book;

            async Task<List<string>> WriteChapterTitles()
            {
                var chapterTitles = await DoUntilNoExceptions(async () =>
                {
                    var response = await agent.SendMessageAndGetResponse($"You are writing a book titled \"{title}\", which will be about the following: {description} \n\nWrite \"BEGIN\", then write a bullet point list of chapter titles in the book, then write \"END\".");
                    List<string> chapterTitles = ParseBulletPointList(ExtractBeginEndDelimitedText(response));
                    return chapterTitles;
                });

                return chapterTitles;
            }

            async Task<BookChapter> WriteChapter(string chapterTitle, Agent agent)
            {
                var chapter = new BookChapter { Title = chapterTitle };

                var sectionTitles = await WriteSectionTitles(chapter, agent);

                chapter.Sections =
                    (
                        await Task.WhenAll(
                            sectionTitles
                                .Select(x =>
                                    Task.Run(() => WriteSection(chapter, x, agent.Fork()))))
                    ).ToList();

                return chapter;
            }

            async Task<List<string>> WriteSectionTitles(BookChapter chapter, Agent agent)
            {
                var sectionTitles = await DoUntilNoExceptions(async () =>
                {
                    var response = await agent.SendMessageAndGetResponse($"Write \"BEGIN\", then write a bullet point list of sections in chapter \"{chapter.Title}\", then write \"END\".");
                    List<string> sectionTitles = ParseBulletPointList(ExtractBeginEndDelimitedText(response));
                    return sectionTitles;
                });

                return sectionTitles;
            }

            async Task<BookSection> WriteSection(BookChapter chapter, string sectionTitle, Agent agent)
            {
                var section = new BookSection { Title = sectionTitle };

                var subsectionTitles = await WriteSubsectionTitles(agent, chapter, section);

                section.Subsections =
                    (
                        await Task.WhenAll(
                            subsectionTitles
                                .Select(x =>
                                    Task.Run(() => WriteSubsection(chapter, section, x, agent.Fork()))))
                    ).ToList();

                return section;
            }

            async Task<List<string>> WriteSubsectionTitles(Agent agent, BookChapter chapter, BookSection section)
            {


                var subsectionTitles = await DoUntilNoExceptions(async () =>
                {
                    var response = await agent.SendMessageAndGetResponse($"Write \"BEGIN\", then write a bullet point list of subsections in section \"{section.Title}\" in chapter \"{chapter.Title}\", then write \"END\".");
                    List<string> subsectionTitles = ParseBulletPointList(ExtractBeginEndDelimitedText(response));
                    return subsectionTitles;
                });

                return subsectionTitles;
            }

            async Task<BookSubsection> WriteSubsection(BookChapter chapter, BookSection section, string subsectionTitle, Agent agent)
            {
                var subsection = new BookSubsection { Title = subsectionTitle };

                if (!onlyOutline)
                {
                    subsection.Content = await DoUntilNoExceptions(async () =>
                    {
                        var response = await agent.SendMessageAndGetResponse($"Write \"BEGIN\", then write the contents of subsection (in full sentences, not as a bulleted list) \"{subsection.Title}\" in section \"{section.Title}\" in chapter \"{chapter.Title}\", then write \"END\".");
                        return ExtractBeginEndDelimitedText(response);
                    });
                }

                return subsection;
            }
        }

        string? ExtractBeginEndDelimitedText(string text)
        {
            var indexOfBegin = text.IndexOf("BEGIN");
            if (indexOfBegin < 0)
            {
                return null;
            }

            var indexOfEnd = text.LastIndexOf("END");
            if (indexOfEnd < 0)
            {
                return null;
            }

            var contentsLength = indexOfEnd - indexOfBegin - 5;
            var contents = text.Substring(indexOfBegin + 5, contentsLength).Trim();
            return contents;
        }

        List<string> ParseBulletPointList(string text) =>
            text
                .Split(new[] { "\r\n", "\r", "\n" }, StringSplitOptions.RemoveEmptyEntries)
                .Select(x => x.TrimStart('-').Trim())
                .ToList();

        var book = await WriteBook();

        StringBuilder stringBuilder = new();

        stringBuilder.AppendLine($"# {book.Title}");
        stringBuilder.AppendLine();
        stringBuilder.AppendLine();

        stringBuilder.AppendLine("## Table of Contents");

        foreach (var (chapter, chapterIndex) in book.Chapters.Select((c, i) => (c, i)))
        {
            stringBuilder.AppendLine($"{chapterIndex + 1}. {chapter.Title}");

            foreach (var (section, sectionIndex) in chapter.Sections.Select((s, i) => (s, i)))
            {
                stringBuilder.AppendLine($"    {sectionIndex + 1}. {section.Title}");

                foreach (var (subsection, subsectionIndex) in section.Subsections.Select((ss, i) => (ss, i)))
                {
                    stringBuilder.AppendLine($"        {subsectionIndex + 1}. {subsection.Title}");
                }
            }
        }

        stringBuilder.AppendLine();
        stringBuilder.AppendLine();
        stringBuilder.AppendLine();

        foreach (var (chapter, chapterIndex) in book.Chapters.Select((c, i) => (c, i)))
        {
            stringBuilder.AppendLine($"## {chapterIndex + 1}: {chapter.Title}");
            stringBuilder.AppendLine();

            foreach (var (section, sectionIndex) in chapter.Sections.Select((s, i) => (s, i)))
            {
                stringBuilder.AppendLine($"### {sectionIndex + 1}: {section.Title}");
                stringBuilder.AppendLine();

                foreach (var (subsection, subsectionIndex) in section.Subsections.Select((ss, i) => (ss, i)))
                {
                    stringBuilder.AppendLine($"#### {subsectionIndex + 1}: {subsection.Title}");
                    stringBuilder.AppendLine();
                    stringBuilder.AppendLine(subsection.Content);
                    stringBuilder.AppendLine();
                }
            }
        }

        var bookText = stringBuilder.ToString();
        return bookText;
    }
}
