using System.Text.Json.Nodes;

namespace Presto.Prelude
{
    public static class JsonUtil
    {
        public static void SortJsonArrayValuesWithName(JsonNode jsonDoc, string name)
        {
            var props = GetGrandchildPropertiesWithName(name, jsonDoc)
                .Where(prop => prop.Value is JsonArray)
                .ToList();

            foreach (KeyValuePair<string, JsonNode?> resourceIdsProperty in props)
            {
                JsonArray resourceIds = (resourceIdsProperty.Value as JsonArray)!;

                resourceIds.Parent![name] =
                    new JsonArray(
                        resourceIds
                            .Where(n => n != null)
                            .Cast<JsonNode>()
                            .Select(n => n.AsValue().GetValue<string>())
                            .OrderBy(s => s)
                            .Map(s => JsonValue.Create(s))
                            .ToArray());
            }
        }

        public static IEnumerable<KeyValuePair<string, JsonNode?>> GetGrandchildPropertiesWithName(string name, JsonNode node)
        {
            if (node is JsonObject obj)
            {
                foreach (KeyValuePair<string, JsonNode?> prop in obj)
                {
                    if (prop.Key == name)
                    {
                        yield return prop;
                    }

                    if (prop.Value != null)
                    {
                        foreach (var grandchild in GetGrandchildPropertiesWithName(name, prop.Value))
                        {
                            yield return grandchild;
                        }
                    }
                }
            }
            else if (node is JsonArray arr)
            {
                foreach (JsonNode elem in arr.Where(e => e != null).Cast<JsonNode>())
                {
                    foreach (var grandchild in GetGrandchildPropertiesWithName(name, elem))
                    {
                        yield return grandchild;
                    }
                }
            }
        }
    }
}
