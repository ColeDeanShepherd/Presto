using Microsoft.Extensions.Configuration;
using OpenAI;

namespace Presto.AI.Agents;

internal class Program
{
    static async Task Main(string[] args)
    {
        var configuration = new ConfigurationBuilder()
            .AddUserSecrets<Program>()
            .Build();

        string apiKey = configuration["OpenAIAPIKey"] ?? "";

        var bookTitle = "Music Theory";
        var book = await BookGenerator.ComposeABook(
            apiKey,
            bookTitle,
            "An informational book about music theory and its applications.",
            onlyOutline: true);

        File.WriteAllText($"{bookTitle}.md", book);
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
}