namespace Presto.AI.Assistant;

public static class FileSystem
{
    public static string GetUserProfileFolderPath() =>
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);

    public static string GetLocalAppDataFolderPath() =>
        Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);

    public static string GetProgramFilesFolderPath() =>
        Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);

    public static string GetProgramFilesX86FolderPath() =>
        Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86);
}
