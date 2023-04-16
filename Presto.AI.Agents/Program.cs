using Microsoft.Extensions.Configuration;
using OpenAI;
using System.Text;

namespace Presto.AI.Agents;

class Book
{
    public string Title { get; set; }
    public List<BookChapter> Chapters { get; set; }
}

class BookChapter
{
    public string Title { get; set; }
    public List<BookSection> Sections { get; set; }
}

class BookSection
{
    public string Title { get; set; }
    public List<BookSubsection> Subsections { get; set; }
}

class BookSubsection
{
    public string Title { get; set; }
    public string Content { get; set; }
}

internal class Program
{
    static async Task Main(string[] args)
    {
        var configuration = new ConfigurationBuilder()
            .AddUserSecrets<Program>()
            .Build();

        string apiKey = configuration["OpenAIAPIKey"] ?? "";

        var bookTitle = "Improving Rec Room";
        var book = await ComposeABook(
            apiKey,
            bookTitle,
            "How the video game \"Rec Room\" can be improved, and grow its market share.",
            onlyOutline: false);

        File.WriteAllText($"{bookTitle}.md", book);

        Console.WriteLine(book);
    }

    static async Task TwoConversingAgents(string apiKey)
    {
        string model = "gpt-3.5-turbo";
        string systemMessage = "You are a conversation partner.";

        List<ChatMessage> agent1ChatMessages = new()
        {
            new ChatMessage("system", systemMessage)
        };

        List<ChatMessage> agent2ChatMessages = new()
        {
            new ChatMessage("system", systemMessage)
        };

        int speakingAgent = 1;
        string message = "Let's chat in detail about how the video game \"Rec Room\" can be improved, and overtake its competitors Roblox and Fortnite!";

        while (true)
        {
            Console.WriteLine($"Agent {speakingAgent}:");
            Console.WriteLine("========");
            Console.WriteLine(message);
            Console.ReadLine();

            message = message + "\n\nPlease respond with an answer to my questions, and end your response with a question.";

            var speakingAgentChatMessages = (speakingAgent == 1) ? agent1ChatMessages : agent2ChatMessages;
            var listeningAgentChatMessages = (speakingAgent == 1) ? agent2ChatMessages : agent1ChatMessages;

            speakingAgentChatMessages.Add(new ChatMessage("assistant", message));
            listeningAgentChatMessages.Add(new ChatMessage("user", message));

            var request = new ChatCompletionRequest(
                model,
                listeningAgentChatMessages.ToArray());

            ChatCompletionResponse response = await API.CreateChatCompletion(apiKey, request);
            ChatMessage responseChatMessage = response.Choices[0].Message;

            listeningAgentChatMessages.Add(responseChatMessage);

            message = responseChatMessage.Content;
            speakingAgent = (speakingAgent == 1) ? 2 : 1;
        }
    }

    static async Task<string> ComposeABook(string apiKey, string title, string description, bool onlyOutline)
    {
        string model = "gpt-3.5-turbo";
        string systemMessage = "You are a helpful assistant.";

        List<ChatMessage> _chatMessages = new()
        {
            new ChatMessage("system", systemMessage)
        };

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
                                Task.Run(() => WriteChapter(x, new List<ChatMessage>(_chatMessages)))))
                ).ToList();

            
            return book;

            async Task<List<string>> WriteChapterTitles()
            {
                _chatMessages.Add(new ChatMessage("user", $"You are writing a book titled \"{title}\", which will be about the following: {description} \n\nWrite \"BEGIN\", then write a bullet point list of chapter titles in the book, then write \"END\"."));

                var chapterTitles = await DoUntilNoExceptions(async () =>
                {
                    ChatMessage responseMessage = await API.CreateSingleChoiceChatCompletion(
                        apiKey,
                        new ChatCompletionRequest(model, _chatMessages.ToArray()));
                    List<string> chapterTitles = ParseBulletPointList(ExtractBeginEndDelimitedText(responseMessage.Content));
                    _chatMessages.Add(responseMessage);
                    return chapterTitles;
                });

                return chapterTitles;
            }

            async Task<BookChapter> WriteChapter(string chapterTitle, List<ChatMessage> chatMessages)
            {
                var chapter = new BookChapter { Title = chapterTitle };

                var sectionTitles = await WriteSectionTitles(chapter, chatMessages);

                chapter.Sections =
                    (
                        await Task.WhenAll(
                            sectionTitles
                                .Select(x =>
                                    Task.Run(() => WriteSection(chapter, x, new List<ChatMessage>(chatMessages)))))
                    ).ToList();

                return chapter;
            }

            async Task<List<string>> WriteSectionTitles(BookChapter chapter, List<ChatMessage> chatMessages)
            {
                chatMessages.Add(new ChatMessage("user", $"Write \"BEGIN\", then write a bullet point list of sections in chapter \"{chapter.Title}\", then write \"END\"."));

                var sectionTitles = await DoUntilNoExceptions(async () =>
                {
                    var responseMessage = await API.CreateSingleChoiceChatCompletion(apiKey, new ChatCompletionRequest(model, chatMessages.ToArray()));
                    List<string> sectionTitles = ParseBulletPointList(ExtractBeginEndDelimitedText(responseMessage.Content));
                    chatMessages.Add(responseMessage);
                    return sectionTitles;
                });

                return sectionTitles;
            }

            async Task<BookSection> WriteSection(BookChapter chapter, string sectionTitle, List<ChatMessage> chatMessages)
            {
                var section = new BookSection { Title = sectionTitle };

                var subsectionTitles = await WriteSubsectionTitles(chatMessages, chapter, section);

                section.Subsections =
                    (
                        await Task.WhenAll(
                            subsectionTitles
                                .Select(x =>
                                    Task.Run(() => WriteSubsection(chapter, section, x, new List<ChatMessage>(chatMessages)))))
                    ).ToList();

                return section;
            }

            async Task<List<string>> WriteSubsectionTitles(List<ChatMessage> chatMessages, BookChapter chapter, BookSection section)
            {
                chatMessages.Add(new ChatMessage("user", $"Write \"BEGIN\", then write a bullet point list of subsections in section \"{section.Title}\" in chapter \"{chapter.Title}\", then write \"END\"."));

                var subsectionTitles = await DoUntilNoExceptions(async () =>
                {
                    var responseMessage = await API.CreateSingleChoiceChatCompletion(apiKey, new ChatCompletionRequest(model, chatMessages.ToArray()));
                    List<string> subsectionTitles = ParseBulletPointList(ExtractBeginEndDelimitedText(responseMessage.Content));
                    chatMessages.Add(responseMessage);
                    return subsectionTitles;
                });

                return subsectionTitles;
            }

            async Task<BookSubsection> WriteSubsection(BookChapter chapter, BookSection section, string subsectionTitle, List<ChatMessage> chatMessages)
            {
                var subsection = new BookSubsection { Title = subsectionTitle };

                if (!onlyOutline)
                {
                    chatMessages.Add(new ChatMessage("user", $"Write \"BEGIN\", then write the contents of subsection \"{subsection.Title}\" in section \"{section.Title}\" in chapter \"{chapter.Title}\", then write \"END\"."));

                    subsection.Content = await DoUntilNoExceptions(async () =>
                    {
                        var responseMessage = await API.CreateSingleChoiceChatCompletion(apiKey, new ChatCompletionRequest(model, chatMessages.ToArray()));
                        chatMessages.Add(responseMessage);

                        return ExtractBeginEndDelimitedText(responseMessage.Content);
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