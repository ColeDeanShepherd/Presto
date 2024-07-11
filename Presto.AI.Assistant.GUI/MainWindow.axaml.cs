using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using System.Threading.Tasks;

namespace Presto.AI.Assistant.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
        commandBar.ItemsSource = new[]
        {
            new Command("Open YouTube", "Open YouTube in the web browser.", async () => WebBrowser.OpenYouTube())
        };
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
}