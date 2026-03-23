Imports System
Imports System.Diagnostics
Imports System.IO
Imports System.Text
Imports System.Threading
Imports System.Threading.Tasks

Namespace Global.QBLib.Audio

  Friend NotInheritable Class LinuxAudio

    Private Shared ReadOnly s_tempDir As String
    Private Shared ReadOnly s_audioPlayer As String

    Shared Sub New()
      s_tempDir = Path.Combine(Path.GetTempPath(), "qbasic_audio")
      If Not Directory.Exists(s_tempDir) Then
        Directory.CreateDirectory(s_tempDir)
      End If

      s_audioPlayer = FindAudioPlayer()
    End Sub

    Private Shared Function FindAudioPlayer() As String
      Dim candidates = New String() {"aplay", "paplay", "play", "ffplay"}
      Dim pathEnv = Environment.GetEnvironmentVariable("PATH")

      For Each candidate In candidates
        For Each pathDir In pathEnv.Split(Path.PathSeparator)
          Dim fullPath = Path.Combine(pathDir, candidate)
          If File.Exists(fullPath) Then
            Return candidate
          End If
        Next
      Next

      Return Nothing
    End Function

    Public Shared Sub Beep()
      Beep(800, 250)
    End Sub

    Public Shared Sub Beep(frequency As Integer, durationMs As Integer)
      If frequency < 37 OrElse frequency > 32767 Then
        Return
      End If
      If durationMs < 0 Then Return

      If String.IsNullOrEmpty(s_audioPlayer) Then
        Return
      End If

      Dim wavFile = Path.Combine(s_tempDir, Guid.NewGuid().ToString("N") & ".wav")
      Try
        GenerateWavFile(wavFile, frequency, durationMs)
        PlayAudioFile(wavFile)
      Finally
        Try
          If File.Exists(wavFile) Then
            File.Delete(wavFile)
          End If
        Catch
        End Try
      End Try
    End Sub

    Public Shared Sub SoundAsync(frequency As Integer, duration As Integer, token As CancellationToken)
      If frequency < 37 OrElse frequency > 32767 Then
        AudioDevice.OnSoundFinished()
        Return
      End If
      If duration < 0 Then
        AudioDevice.OnSoundFinished()
        Return
      End If

      Dim durationMs = CInt(duration * 1000.0 / 18.2)
      If durationMs < 1 Then durationMs = 1

      Task.Run(Sub()
                 Try
                   Beep(frequency, durationMs)
                 Catch
                 Finally
                   If Not token.IsCancellationRequested Then
                     AudioDevice.OnSoundFinished()
                   End If
                 End Try
               End Sub, token)
    End Sub

    Private Shared s_currentProcess As Process = Nothing
    Private Shared ReadOnly s_processLock As New Object()

    Public Shared Sub StopTone()
      SyncLock s_processLock
        If s_currentProcess IsNot Nothing AndAlso Not s_currentProcess.HasExited Then
          Try
            s_currentProcess.Kill()
          Catch
          End Try
          s_currentProcess = Nothing
        End If
      End SyncLock
    End Sub

    ' Keep synchronous version for compatibility
    Public Shared Sub Sound(frequency As Integer, duration As Integer)
      If frequency < 37 OrElse frequency > 32767 Then
        Return
      End If
      If duration < 0 Then Return

      Dim durationMs = CInt(duration * 1000.0 / 18.2)
      If durationMs < 1 Then durationMs = 1

      Beep(frequency, durationMs)
    End Sub

    Private Shared Sub GenerateWavFile(filePath As String, frequency As Integer, durationMs As Integer)
      Dim sampleRate = 44100
      Dim bitsPerSample = 16
      Dim channels = 1

      Dim numSamples = CULng(sampleRate) * CULng(durationMs) \ 1000UL
      Dim dataSize = numSamples * CULng(bitsPerSample \ 8) * CULng(channels)
      Dim fileSize = 36UL + dataSize

      Using fs = New FileStream(filePath, FileMode.Create, FileAccess.Write)
        Using writer = New BinaryWriter(fs, Encoding.ASCII)

          writer.Write(Encoding.ASCII.GetBytes("RIFF"))
          writer.Write(CUInt(fileSize))
          writer.Write(Encoding.ASCII.GetBytes("WAVE"))

          writer.Write(Encoding.ASCII.GetBytes("fmt "))
          writer.Write(CUInt(16))
          writer.Write(CUShort(1))
          writer.Write(CUShort(channels))
          writer.Write(CUInt(sampleRate))
          writer.Write(CUInt(sampleRate * channels * bitsPerSample \ 8))
          writer.Write(CUShort(channels * bitsPerSample \ 8))
          writer.Write(CUShort(bitsPerSample))

          writer.Write(Encoding.ASCII.GetBytes("data"))
          writer.Write(CUInt(dataSize))

          Dim amplitude As Double = 0.7 * Short.MaxValue
          Dim twoPiF As Double = 2.0 * Math.PI * frequency
          Dim maxIndex = CInt(numSamples)

          For i As Integer = 0 To maxIndex - 1
            Dim t As Double = CDbl(i) / sampleRate
            Dim sample As Double = amplitude * Math.Sin(twoPiF * t)
            Dim sampleShort As Short = CShort(Math.Max(Short.MinValue, Math.Min(Short.MaxValue, sample)))
            writer.Write(sampleShort)
          Next
        End Using
      End Using
    End Sub

    Private Shared Sub PlayAudioFile(filePath As String)
      If String.IsNullOrEmpty(s_audioPlayer) Then Return

      Dim args As String
      Select Case s_audioPlayer
        Case "aplay"
          args = $"-q ""{filePath}"""
        Case "paplay"
          args = $"-q ""{filePath}"""
        Case "play"
          args = $"-q ""{filePath}"""
        Case Else
          args = $"-i ""{filePath}"""
      End Select

      Try
        Dim psi = New ProcessStartInfo(s_audioPlayer, args) With {
          .UseShellExecute = False,
          .CreateNoWindow = True,
          .RedirectStandardOutput = True,
          .RedirectStandardError = True
        }

        Using proc = Process.Start(psi)
          If proc IsNot Nothing Then
            proc.WaitForExit(30000)
          End If
        End Using
      Catch
      End Try
    End Sub

  End Class

End Namespace