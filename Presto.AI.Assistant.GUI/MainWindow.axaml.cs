using Avalonia.Controls;
using Avalonia.Interactivity;

namespace Presto.AI.Assistant.GUI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
        commandBar.ItemsSource = Commands.All;
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