using System;
using System.Threading.Tasks;

namespace Presto.AI.Assistant.GUI;

public record Command(string Name, string Description, Func<Task> RunFunc);