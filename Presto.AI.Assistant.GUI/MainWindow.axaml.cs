using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using System.Linq;

namespace Presto.AI.Assistant.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
        commandList.ItemsSource = Commands.All;
    }

    private async void OnRunClicked(object? sender, RoutedEventArgs args)
    {
        if (sender is Button button &&
            button.DataContext is Command command)
        {
            await command.RunFunc();
            // TODO: handle errors
        }
    }

    private void OnSearchBarTextChanged(object? sender, TextChangedEventArgs e)
    {
        if (sender is TextBox textBox)
        {
            commandList.ItemsSource = SearchCommands(textBox.Text);
        }
    }

    private Command[] SearchCommands(string? searchQuery)
    {
        if (string.IsNullOrWhiteSpace(searchQuery))
        {
            return Commands.All;
        }

        return Commands.All
            .Where(x => x.Name.Contains(searchQuery, System.StringComparison.InvariantCultureIgnoreCase))
            .ToArray();
    }
}