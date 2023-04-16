using System.Net.Http.Json;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace OpenAI
{
    public enum Model
    {
        GPT_3_5_Turbo
    }

    public static class Constants
    {
        public static readonly Dictionary<Model, string> IdStringsByModel = new()
        {
            { Model.GPT_3_5_Turbo, "gpt-3.5-turbo" }
        };
    }

    public record ChatMessage(
        [property: JsonPropertyName("role")]
        string Role,

        [property: JsonPropertyName("content")]
        string Content);

    public record ChatCompletionRequest(
        [property: JsonPropertyName("model")]
        string Model,

        [property: JsonPropertyName("messages")]
        ChatMessage[] Messages);

    public record ChatCompletionChoice(
        [property: JsonPropertyName("message")]
        ChatMessage Message);

    public record ChatCompletionResponse(
        [property: JsonPropertyName("choices")]
        ChatCompletionChoice[] Choices);

    public static class API
    {
        private static SemaphoreSlim semaphore = new SemaphoreSlim(8);

        public static async Task<ChatCompletionResponse> CreateChatCompletion(string apiKey, ChatCompletionRequest request)
        {
            await semaphore.WaitAsync();

            try
            {
                using var httpClient = new HttpClient();

                var dataJson = JsonSerializer.Serialize(request);
                var content = new StringContent(dataJson, Encoding.UTF8, "application/json");
                httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {apiKey}");
                var responseMessage = await httpClient.PostAsync("https://api.openai.com/v1/chat/completions", content);

                var response = await responseMessage.Content.ReadFromJsonAsync<ChatCompletionResponse>();

                if (response == null)
                {
                    throw new Exception();
                }

                return response;
            }
            finally
            {
                semaphore.Release();
            }
        }

        public static async Task<ChatMessage> CreateSingleChoiceChatCompletion(string apiKey, ChatCompletionRequest request)
        {
            Console.WriteLine(request.Messages[request.Messages.Length - 1].Content);

            var response = await CreateChatCompletion(apiKey, request);

            var responseMessage = response.Choices[0].Message;

            Console.WriteLine(responseMessage);

            return responseMessage;
        }
    }
}