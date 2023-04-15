using Microsoft.Extensions.Configuration;
using OpenAI;
using System.Text;
using System.Text.Json;

namespace Presto.AI.Agents;

/*
BEGIN

I. The Basics of Cooking
   A. The essentials of a well-stocked kitchen
   B. Understanding cooking terminology
   C. Measuring ingredients correctly
   D. Cooking methods

II. Appetizers and Snacks
   A. Quick and easy appetizers
   B. Healthy snacks
   C. Party platters

III. Soups and Salads
   A. Classic soup recipes
   B. Unique and flavorful salads
   C. Dressings and toppings

IV. Main Courses
   A. Meat dishes
   B. Poultry dishes
   C. Vegetarian dishes
   D. Seafood dishes

V. Sides and Sweets
   A. Side dish recipes
   B. Dessert recipes

END
 */
class BookOutline
{
    public string title { get; set; }
    public List<BookOutlineChapter> chapters { get; set; }
}

class BookOutlineChapter
{
    public string title { get; set; }
    public List<BookOutlineSection> sections { get; set; }
}

class BookOutlineSection
{
    public string title { get; set; }
    public string content { get; set; }
}

internal class Program
{
    static async Task Main(string[] args)
    {
        var configuration = new ConfigurationBuilder()
            .AddUserSecrets<Program>()
            .Build();

        string apiKey = configuration["OpenAIAPIKey"] ?? "";

        var book = await ComposeABook(apiKey, "How to Play Classical Guitar for Dummies");

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

    static async Task<string> ComposeABook(string apiKey, string title)
    {
        string model = "gpt-3.5-turbo";
        string systemMessage = "You are a helpful assistant.";

        string outlineJsonFormat = @"{
    ""title"": ""book title here"",
    ""chapters"": [
        {
            ""title"": ""chapter title here"",
            ""sections"": [
                {
                    ""title"": ""section title here"",
                    ""subsections"": [
                        ""describe the contents of the subsection here""
                    ],
                }
            ]
        }
    ]
}";
        List<ChatMessage> chatMessages = new()
        {
            new ChatMessage("system", systemMessage),
            new ChatMessage("user", $"Please write a three-level outline for a book titled \"{title}\", formatted in JSON structured as follows: \n\n" + outlineJsonFormat)
            //new ChatMessage("user", $"Please write an outline for a book about cooking in a two-level list of bullet points. Please write \"BEGIN\" before your outline, and \"END\" after your outline.")
        };

        async Task<(ChatMessage, BookOutline)> WriteOutline()
        {
            var request = new ChatCompletionRequest(
                model,
                chatMessages.ToArray());

            ChatCompletionResponse response = await API.CreateChatCompletion(apiKey, request);
            ChatMessage responseChatMessage = response.Choices[0].Message;

            var indexOfFirstLeftCurlyBrace = responseChatMessage.Content.IndexOf('{');
            var indexOfLastRightCurlyBrace = responseChatMessage.Content.LastIndexOf('}');
            var jsonStringLength = indexOfLastRightCurlyBrace - indexOfFirstLeftCurlyBrace + 1;
            var jsonString = responseChatMessage.Content.Substring(indexOfFirstLeftCurlyBrace, jsonStringLength);
            var outline = JsonSerializer.Deserialize<BookOutline>(jsonString);

            return (responseChatMessage, outline!);
        }

        // Write outline
        var (outlineMessage, outline) = await WriteOutline();

        chatMessages.Add(outlineMessage);

        async Task<string> WriteSection(BookOutlineChapter chapter, BookOutlineSection section)
        {
            var request = new ChatCompletionRequest(
                model,
                chatMessages
                    .Concat(new[]
                    {
                        new ChatMessage("user", $"Can you please write a few pages for chapter \"{chapter.title}\" section \"{section.title}\", which you summarized as \"{section.content}\"? Please write the contents of section in plaintext, beginning with \"BEGIN\" and ending with \"END\".")
                    })
                    .ToArray());

            ChatCompletionResponse response = await API.CreateChatCompletion(apiKey, request);
            ChatMessage responseChatMessage = response.Choices[0].Message;

            var indexOfBegin = responseChatMessage.Content.IndexOf("BEGIN");
            var indexOfEnd = responseChatMessage.Content.LastIndexOf("END");
            var contentsLength = indexOfEnd - indexOfBegin - 5;
            var contents = responseChatMessage.Content.Substring(indexOfBegin + 5, contentsLength).Trim();

            return contents;
        }

        StringBuilder stringBuilder = new();

        stringBuilder.AppendLine(outline.title);
        stringBuilder.AppendLine();
        stringBuilder.AppendLine();

        foreach (var chapter in outline.chapters)
        {
            stringBuilder.AppendLine("Chapter: " + chapter.title);
            stringBuilder.AppendLine();

            foreach (var section in chapter.sections)
            {
                stringBuilder.AppendLine("Section: " + section.title);
                stringBuilder.AppendLine();

                stringBuilder.AppendLine(await WriteSection(chapter, section));
                stringBuilder.AppendLine();
            }
        }

        return stringBuilder.ToString();
    }
}