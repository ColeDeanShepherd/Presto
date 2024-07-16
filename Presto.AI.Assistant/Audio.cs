using NAudio.Wave;

namespace Presto.AI.Assistant;

public static class Audio
{
    public static async Task RecordAudioTest()
    {
        // Specify the output file path
        string outputFilePath = "output.wav";

        // Set up the capture device (WasapiLoopbackCapture captures audio output)
        using (var capture = new WasapiLoopbackCapture())
        {
            // Define the format of the audio to be captured
            var waveFormat = capture.WaveFormat;

            // Create a writer to write the captured audio to a file
            using (var writer = new WaveFileWriter(outputFilePath, waveFormat))
            {
                // Set up an event handler to write audio data as it's captured
                capture.DataAvailable += (s, a) =>
                {
                    writer.Write(a.Buffer, 0, a.BytesRecorded);
                    writer.Flush();
                };

                // Start capturing audio
                capture.StartRecording();

                await Task.Delay(5 * 1000);

                // Stop capturing audio
                capture.StopRecording();
            }
        }
    }
}
