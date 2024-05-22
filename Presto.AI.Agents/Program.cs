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

        await WriteSectionOfWhatEveryoneShouldKnowBook(apiKey);
    }

    static async Task WriteSectionOfWhatEveryoneShouldKnowBook(string apiKey)
    {
        string model = "gpt-3.5-turbo";
        string systemMessage = "You are a conversation partner.";
        string sectionName = "Addition & subtraction";
        string prompt = $"I'm writing a book called \"What Everyone Should Know\". This book contains all the information that I believe every human should know to be a happy, healthy, good adult. I'm currently writing a section called \"{sectionName}\". I want the section to contain both an explanation of the most important relevant pieces of information, and why it's important to know this. Can you write this section for me? Please keep the section brief. Keep the writing casual, simple, and brief. Prefer writing with prose over outputting lists. Please output in markdown. Here's an example of the writing style I'm looking for: \"There's a lot we need to know to be happy, healthy, good people in today's society, and unfortunately we learn very little of it in school. This book is an attempt to cover **all** of this information at a high level in an unbiased way, and fill in the gaps left by our school system.\"";
        
        //prompt = "I'm writing a book called \"What Everyone Should Know\". This book contains all the information that I believe every human should know to be a happy, healthy, good adult. Can you please write a very brief introduction? Keep the writing casual, simple, and brief. Here's an example of the writing style I'm looking for: \"There's a lot we need to know to be happy, healthy, good people in today's society, and unfortunately we learn very little of it in school. This book is an attempt to cover **all** of this information at a high level in an unbiased way, and fill in the gaps left by our school system.\"";

        List<ChatMessage> chatMessages = new()
        {
            new ChatMessage("system", systemMessage),
            new ChatMessage("user", prompt)
        };

        var request = new ChatCompletionRequest(
            model,
            chatMessages.ToArray());

        ChatCompletionResponse response = await API.CreateChatCompletion(apiKey, request);
        ChatMessage responseChatMessage = response.Choices[0].Message;

        Console.WriteLine(responseChatMessage.Content);
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