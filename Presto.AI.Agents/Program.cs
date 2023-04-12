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

        var request = new ChatCompletionRequest(
            "gpt-3.5-turbo",
            new[]
            {
                new ChatMessage("user", "Hello!")
            });

        ChatCompletionResponse response = await API.CreateChatCompletion(apiKey, request);
        ChatMessage responseChatMessage = response.Choices[0].Message;
    }
}