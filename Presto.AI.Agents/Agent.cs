using OpenAI;

namespace Presto.AI.Agents
{
    public class Agent
    {
        private readonly string _apiKey;
        private readonly string _model;
        private List<ChatMessage> _chatMessages;

        public Agent(string apiKey, string model)
            : this(apiKey, model, new List<ChatMessage>())
        {
        }

        private Agent(string apiKey, string model, List<ChatMessage> chatMessages)
        {
            _apiKey = apiKey;
            _model = model;
            _chatMessages = chatMessages;
        }

        public async Task SendSystemMessage(string message)
        {
            _chatMessages.Add(new ChatMessage("system", message));
        }

        public async Task<string> SendMessageAndGetResponse(string message, string role = "user")
        {
            var newChatMessages = _chatMessages.Concat(new[] { new ChatMessage(role, message) });
            ChatMessage responseMessage =
                await API.CreateSingleChoiceChatCompletion(
                    _apiKey,
                    new ChatCompletionRequest(_model, newChatMessages.ToArray()));
            _chatMessages = newChatMessages.ToList();
            _chatMessages.Add(responseMessage);
            return responseMessage.Content;
        }

        public Agent Fork() =>
            new Agent(_apiKey, _model, new List<ChatMessage>(_chatMessages));
    }
}
