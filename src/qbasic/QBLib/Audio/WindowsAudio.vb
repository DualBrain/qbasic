Imports System
Imports System.Runtime.InteropServices
Imports System.Threading
Imports System.Threading.Tasks

Namespace Global.QBLib.Audio

  Friend NotInheritable Class WindowsAudio

#Region "Win32 API Declarations"

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function Beep(dwFreq As UInteger, dwDuration As UInteger) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    Private Declare Auto Sub MessageBeep Lib "user32.dll" (uType As UInteger)

#End Region

#Region "Waveform Audio API"

    Private Enum MMRESULT As UInteger
      MMSYSERR_NOERROR = 0
      MMSYSERR_ERROR = 1
      MMSYSERR_BADDEVICEID = 2
      MMSYSERR_NOTENABLED = 3
      MMSYSERR_ALLOCATED = 4
      MMSYSERR_INVALHANDLE = 5
      MMSYSERR_NODRIVER = 6
      MMSYSERR_NOMEM = 7
      MMSYSERR_NOTSUPPORTED = 8
      MMSYSERR_ERRVAL = 9
      MMSYSERR_HANDLEBUSY = 12
    End Enum

    Private Const WAVE_FORMAT_PCM As Integer = 1

    <StructLayout(LayoutKind.Sequential)>
    Private Structure WAVEFORMATEX
      Public wFormatTag As Short
      Public nChannels As Short
      Public nSamplesPerSec As Integer
      Public nAvgBytesPerSec As Integer
      Public nBlockAlign As Short
      Public wBitsPerSample As Short
      Public cbSize As Short
    End Structure

    <DllImport("winmm.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Private Shared Function waveOutOpen(
        ByRef phwo As IntPtr,
        uDeviceID As UInteger,
        ByRef pwfx As WAVEFORMATEX,
        dwCallback As IntPtr,
        dwInstance As IntPtr,
        fdwOpen As UInteger) As MMRESULT
    End Function

    <DllImport("winmm.dll", SetLastError:=True)>
    Private Shared Function waveOutClose(hwo As IntPtr) As MMRESULT
    End Function

    <DllImport("winmm.dll", SetLastError:=True)>
    Private Shared Function waveOutPrepareHeader(hwo As IntPtr, ByRef pwh As WAVEHDR, cbwh As UInteger) As MMRESULT
    End Function

    <DllImport("winmm.dll", SetLastError:=True)>
    Private Shared Function waveOutUnprepareHeader(hwo As IntPtr, ByRef pwh As WAVEHDR, cbwh As UInteger) As MMRESULT
    End Function

    <DllImport("winmm.dll", SetLastError:=True)>
    Private Shared Function waveOutWrite(hwo As IntPtr, ByRef pwh As WAVEHDR, cbwh As UInteger) As MMRESULT
    End Function

    <DllImport("winmm.dll", SetLastError:=True)>
    Private Shared Function waveOutReset(hwo As IntPtr) As MMRESULT
    End Function

    <DllImport("winmm.dll", SetLastError:=True)>
    Private Shared Function waveOutGetPosition(hwo As IntPtr, pmmt As IntPtr, cbmmt As UInteger) As MMRESULT
    End Function

    <StructLayout(LayoutKind.Sequential)>
    Private Structure WAVEHDR
      Public lpData As IntPtr
      Public dwBufferLength As UInteger
      Public dwBytesRecorded As UInteger
      Public dwUser As IntPtr
      Public dwFlags As UInteger
      Public dwLoops As UInteger
      Public lpNext As IntPtr
      Public reserved As IntPtr
    End Structure

    Private Const WHDR_DONE As UInteger = &H1UI

    <DllImport("kernel32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
    Private Shared Function HeapAlloc(hHeap As IntPtr, dwFlags As UInteger, dwBytes As IntPtr) As IntPtr
    End Function

    Private Const HEAP_ZERO_MEMORY As UInteger = &H8UI

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function HeapFree(hHeap As IntPtr, dwFlags As UInteger, lpMem As IntPtr) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function GetProcessHeap() As IntPtr
    End Function

#End Region

#Region "Streaming Audio"

    Private Const BUFFER_COUNT As Integer = 4
    Private Const BUFFER_DURATION_MS As Integer = 50
    Private Shared s_hWaveOut As IntPtr = IntPtr.Zero
    Private Shared s_buffers(BUFFER_COUNT - 1) As WAVEHDR
    Private Shared s_bufferData(BUFFER_COUNT - 1) As IntPtr
    Private Shared s_isStreaming As Boolean = False
    Private Shared s_streamThread As Thread = Nothing
    Private Shared s_cts As CancellationTokenSource = Nothing

    Private Shared s_currentFrequency As Integer = 0
    Private Shared s_targetFrequency As Integer = 0
    Private Shared s_samplesRemaining As Integer = 0
    Private Shared s_samplePhase As Double = 0.0
    Private Shared s_currentAmplitude As Double = 0.0
    Private Shared s_streamLock As New Object()

    Private Shared s_debugEnabled As Boolean = False
    Private Shared s_debugStream As System.IO.FileStream = Nothing
    Private Shared s_debugSamples As Integer = 0

    Public Shared Sub EnableDebugOutput(filePath As String)
      s_debugEnabled = True
      s_debugStream = New System.IO.FileStream(filePath, System.IO.FileMode.Create, System.IO.FileAccess.Write)
      s_debugSamples = 0
      WriteWavHeader(s_debugStream, 44100, 1, 16)
    End Sub

    Public Shared Sub DisableDebugOutput()
      s_debugEnabled = False
      If s_debugStream IsNot Nothing Then
        s_debugStream.Flush()
        Dim dataSize = s_debugSamples * 2
        s_debugStream.Seek(4, System.IO.SeekOrigin.Begin)
        s_debugStream.Write(BitConverter.GetBytes(dataSize), 0, 4)
        s_debugStream.Seek(40, System.IO.SeekOrigin.Begin)
        s_debugStream.Write(BitConverter.GetBytes(dataSize), 0, 4)
        s_debugStream.Flush()
        s_debugStream.Close()
        s_debugStream = Nothing
      End If
    End Sub

    Private Shared Sub WriteWavHeader(stream As System.IO.FileStream, sampleRate As Integer, channels As Short, bitsPerSample As Short)
      Dim totalDataLen = 0
      Dim totalAudioLen = 0
      Dim byteRate = sampleRate * channels * bitsPerSample \ 8
      Dim blockAlign = channels * bitsPerSample \ 8

      stream.Write(System.Text.Encoding.ASCII.GetBytes("RIFF"), 0, 4)
      stream.Write(BitConverter.GetBytes(totalDataLen), 0, 4)
      stream.Write(System.Text.Encoding.ASCII.GetBytes("WAVE"), 0, 4)
      stream.Write(System.Text.Encoding.ASCII.GetBytes("fmt "), 0, 4)
      stream.Write(BitConverter.GetBytes(16), 0, 4)
      stream.Write(BitConverter.GetBytes(CShort(1)), 0, 2)
      stream.Write(BitConverter.GetBytes(channels), 0, 2)
      stream.Write(BitConverter.GetBytes(sampleRate), 0, 4)
      stream.Write(BitConverter.GetBytes(byteRate), 0, 4)
      stream.Write(BitConverter.GetBytes(blockAlign), 0, 2)
      stream.Write(BitConverter.GetBytes(bitsPerSample), 0, 2)
      stream.Write(System.Text.Encoding.ASCII.GetBytes("data"), 0, 4)
      stream.Write(BitConverter.GetBytes(totalAudioLen), 0, 4)
    End Sub

    Public Shared Sub StartStream()
      If s_isStreaming AndAlso s_hWaveOut <> IntPtr.Zero AndAlso s_streamThread IsNot Nothing AndAlso s_streamThread.IsAlive Then Return

      If s_isStreaming Then
        CleanupStream()
      End If

      Dim waveFormat = New WAVEFORMATEX() With {
        .wFormatTag = CShort(WAVE_FORMAT_PCM),
        .nChannels = CShort(AudioConstants.CHANNELS),
        .nSamplesPerSec = AudioConstants.SAMPLE_RATE,
        .wBitsPerSample = CShort(AudioConstants.BITS_PER_SAMPLE),
        .cbSize = 0
      }
      waveFormat.nBlockAlign = CShort(waveFormat.nChannels * waveFormat.wBitsPerSample / 8)
      waveFormat.nAvgBytesPerSec = waveFormat.nSamplesPerSec * waveFormat.nBlockAlign

      Dim result = waveOutOpen(s_hWaveOut, UInt32.MaxValue, waveFormat, IntPtr.Zero, IntPtr.Zero, 0)
      If result <> MMRESULT.MMSYSERR_NOERROR Then
        s_hWaveOut = IntPtr.Zero
        Return
      End If

      Dim bufferSize = AudioConstants.SAMPLE_RATE * (AudioConstants.BITS_PER_SAMPLE \ 8) * BUFFER_DURATION_MS \ 1000
      Dim heap = GetProcessHeap()

      For i As Integer = 0 To BUFFER_COUNT - 1
        s_bufferData(i) = HeapAlloc(heap, HEAP_ZERO_MEMORY, New IntPtr(bufferSize))
        s_buffers(i).lpData = s_bufferData(i)
        s_buffers(i).dwBufferLength = CUInt(bufferSize)

        result = waveOutPrepareHeader(s_hWaveOut, s_buffers(i), CUInt(Marshal.SizeOf(s_buffers(i))))
        If result <> MMRESULT.MMSYSERR_NOERROR Then
          StopStream()
          Return
        End If
      Next

      s_cts = New CancellationTokenSource()
      s_isStreaming = True
      s_currentFrequency = 0
      s_targetFrequency = 0
      s_samplesRemaining = 0
      s_samplePhase = 0.0
      s_currentAmplitude = 0.0

      s_streamThread = New Thread(AddressOf StreamingLoop)
      s_streamThread.IsBackground = True
      s_streamThread.Start()
    End Sub

    Private Shared Sub CleanupStream()
      If s_hWaveOut <> IntPtr.Zero Then
        waveOutReset(s_hWaveOut)

        For i As Integer = 0 To BUFFER_COUNT - 1
          If s_bufferData(i) <> IntPtr.Zero Then
            waveOutUnprepareHeader(s_hWaveOut, s_buffers(i), CUInt(Marshal.SizeOf(s_buffers(i))))
            HeapFree(GetProcessHeap(), 0, s_bufferData(i))
            s_bufferData(i) = IntPtr.Zero
          End If
        Next

        waveOutClose(s_hWaveOut)
        s_hWaveOut = IntPtr.Zero
      End If
      s_isStreaming = False
    End Sub

    Public Shared Sub StopStream()
      If Not s_isStreaming Then Return

      If s_cts IsNot Nothing Then
        s_cts.Cancel()
        s_cts.Dispose()
        s_cts = Nothing
      End If

      If s_streamThread IsNot Nothing AndAlso s_streamThread.IsAlive Then
        s_streamThread.Join(1000)
      End If
      s_streamThread = Nothing

      If s_hWaveOut <> IntPtr.Zero Then
        waveOutReset(s_hWaveOut)

        For i As Integer = 0 To BUFFER_COUNT - 1
          If s_bufferData(i) <> IntPtr.Zero Then
            waveOutUnprepareHeader(s_hWaveOut, s_buffers(i), CUInt(Marshal.SizeOf(s_buffers(i))))
            HeapFree(GetProcessHeap(), 0, s_bufferData(i))
            s_bufferData(i) = IntPtr.Zero
          End If
        Next

        waveOutClose(s_hWaveOut)
        s_hWaveOut = IntPtr.Zero
      End If

      s_isStreaming = False
    End Sub

    Private Shared Sub StreamingLoop()
      Dim bufferSize = CInt(s_buffers(0).dwBufferLength)
      Dim samplesPerBuffer = bufferSize \ 2
      Dim audioStarted As Boolean = False

      Do
        If s_cts.IsCancellationRequested Then Exit Do

        Dim samplesToGenerate As Integer
        Dim hasAudio As Boolean = False

        For i As Integer = 0 To BUFFER_COUNT - 1
          If s_cts.IsCancellationRequested Then Exit Do

          SyncLock s_streamLock
            samplesToGenerate = samplesPerBuffer

            If s_targetFrequency > 0 AndAlso s_samplesRemaining > 0 Then
              s_currentFrequency = s_targetFrequency
              If s_samplesRemaining < samplesToGenerate Then
                samplesToGenerate = s_samplesRemaining
              End If
              hasAudio = True
              audioStarted = True
            ElseIf s_samplesRemaining > 0 Then
              s_currentFrequency = s_targetFrequency
              hasAudio = True
            Else
              s_currentFrequency = 0
            End If
          End SyncLock

          GenerateBuffer(s_bufferData(i), samplesToGenerate, s_currentFrequency)

          Dim result = waveOutWrite(s_hWaveOut, s_buffers(i), CUInt(bufferSize))
          If result <> MMRESULT.MMSYSERR_NOERROR Then Exit Do
        Next

        If audioStarted AndAlso Not hasAudio Then
          Exit Do
        End If

        Thread.Sleep(BUFFER_DURATION_MS \ 2)
      Loop While s_isStreaming AndAlso Not s_cts.IsCancellationRequested
    End Sub

    Private Shared Sub GenerateBuffer(buffer As IntPtr, numSamples As Integer, frequency As Integer)
      Const MAX_AMPLITUDE As Double = 0.7 * Short.MaxValue
      Const RAMP_SAMPLES As Integer = 50

      Dim phase = s_samplePhase
      Dim currentAmp = s_currentAmplitude
      Dim twoPiF As Double = 2.0 * Math.PI * frequency
      Dim targetAmp As Double = 0.0
      Dim noteEndSamples As Integer = 0

      If frequency > 0 AndAlso s_samplesRemaining > 0 Then
        targetAmp = MAX_AMPLITUDE
        noteEndSamples = Math.Min(RAMP_SAMPLES, s_samplesRemaining \ 4)
      End If

      For j As Integer = 0 To numSamples - 1
        Dim env As Double = 1.0
        Dim samplesRemaining = s_samplesRemaining - j

        If samplesRemaining <= 0 Then
          env = 0.0
        Else
          If currentAmp < MAX_AMPLITUDE * 0.9 Then
            env = Math.Min(1.0, currentAmp / MAX_AMPLITUDE + 0.1)
          End If
          If samplesRemaining <= noteEndSamples Then
            env = CDbl(samplesRemaining) / noteEndSamples
          End If
        End If

        Dim sample As Double = MAX_AMPLITUDE * env * Math.Sin(phase)
        Dim sampleShort As Short = CShort(Math.Max(Short.MinValue, Math.Min(Short.MaxValue, sample)))
        Marshal.WriteInt16(buffer, j * 2, sampleShort)

        If s_debugEnabled AndAlso s_debugStream IsNot Nothing Then
          Dim sampleBytes = BitConverter.GetBytes(sampleShort)
          s_debugStream.Write(sampleBytes, 0, 2)
          s_debugSamples += 1
        End If

        phase += twoPiF / AudioConstants.SAMPLE_RATE
        If phase >= 2.0 * Math.PI Then phase -= 2.0 * Math.PI
      Next

      s_samplePhase = phase
      s_currentAmplitude = targetAmp
      s_samplesRemaining = Math.Max(0, s_samplesRemaining - numSamples)
    End Sub

    Public Shared Sub PlayTone(frequency As Integer, durationTicks As Integer, token As CancellationToken)
      If Not s_isStreaming OrElse s_streamThread Is Nothing OrElse Not s_streamThread.IsAlive Then
        StartStream()
      End If

      If frequency < AudioConstants.MIN_FREQUENCY OrElse frequency > AudioConstants.MAX_FREQUENCY Then
        AudioDevice.OnSoundFinished()
        Return
      End If
      If durationTicks < 0 Then
        AudioDevice.OnSoundFinished()
        Return
      End If

      Dim durationMs = CInt(durationTicks * 1000.0 / 18.2)
      If durationMs < BUFFER_DURATION_MS Then
        durationMs = BUFFER_DURATION_MS
      End If
      Dim totalDurationMs = CInt(durationTicks * 1000.0 / 18.2)

      SyncLock s_streamLock
        s_targetFrequency = frequency
        s_samplesRemaining = AudioConstants.SAMPLE_RATE * durationMs \ 1000
      End SyncLock

      Thread.Sleep(totalDurationMs)

      If Not token.IsCancellationRequested Then
        SyncLock s_streamLock
          s_samplesRemaining = 0
          s_targetFrequency = 0
          s_currentFrequency = 0
        End SyncLock
        AudioDevice.OnSoundFinished()
      End If
    End Sub

    Public Shared Sub PlayToneAsync(frequency As Integer, durationTicks As Integer, token As CancellationToken)
      Task.Run(Sub()
        Try
          PlayTone(frequency, durationTicks, token)
        Catch
        End Try
      End Sub, token)
    End Sub

    Public Shared Sub StopTone()
      SyncLock s_streamLock
        s_samplesRemaining = 0
        s_targetFrequency = 0
        s_currentFrequency = 0
        s_samplePhase = 0.0
        s_currentAmplitude = 0.0
      End SyncLock
    End Sub

#End Region

#Region "Legacy Beep (for simple beeps)"

    Public Shared Sub Beep()
      Beep(AudioConstants.DEFAULT_BEEP_FREQUENCY, AudioConstants.DEFAULT_BEEP_DURATION_MS)
    End Sub

    Public Shared Sub Beep(frequency As Integer, durationMs As Integer)
      If frequency < AudioConstants.MIN_FREQUENCY OrElse frequency > AudioConstants.MAX_FREQUENCY Then
        Return
      End If
      If durationMs < 0 Then Return

      If frequency <= 65535 AndAlso durationMs <= 65535 Then
        Beep(CUInt(frequency), CUInt(durationMs))
      Else
        BeepWithWaveOut(frequency, durationMs)
      End If
    End Sub

    Private Shared Sub BeepWithWaveOut(frequency As Integer, durationMs As Integer)
      Try
        Dim waveFormat = New WAVEFORMATEX() With {
          .wFormatTag = CShort(WAVE_FORMAT_PCM),
          .nChannels = CShort(AudioConstants.CHANNELS),
          .nSamplesPerSec = AudioConstants.SAMPLE_RATE,
          .wBitsPerSample = CShort(AudioConstants.BITS_PER_SAMPLE),
          .cbSize = 0
        }
        waveFormat.nBlockAlign = CShort(waveFormat.nChannels * waveFormat.wBitsPerSample / 8)
        waveFormat.nAvgBytesPerSec = waveFormat.nSamplesPerSec * waveFormat.nBlockAlign

        Dim hWaveOut As IntPtr = IntPtr.Zero
        Dim result = waveOutOpen(hWaveOut, UInt32.MaxValue, waveFormat, IntPtr.Zero, IntPtr.Zero, 0)
        If result <> MMRESULT.MMSYSERR_NOERROR Then Return

        Try
          Dim numSamples = CULng(AudioConstants.SAMPLE_RATE) * CULng(durationMs) \ 1000UL
          Dim bufferSizeBytes = CULng(AudioConstants.BITS_PER_SAMPLE \ 8) * numSamples
          Dim bufferSizeInt = CInt(Math.Min(bufferSizeBytes, CUInt(Integer.MaxValue)))

          Dim waveHeader = New WAVEHDR()
          waveHeader.lpData = HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, New IntPtr(bufferSizeInt))
          If waveHeader.lpData = IntPtr.Zero Then Return

          Try
            GenerateSineWave(waveHeader.lpData, frequency, numSamples)

            waveHeader.dwBufferLength = CUInt(bufferSizeInt)
            result = waveOutPrepareHeader(hWaveOut, waveHeader, CUInt(Marshal.SizeOf(waveHeader)))
            If result <> MMRESULT.MMSYSERR_NOERROR Then Return

            Try
              result = waveOutWrite(hWaveOut, waveHeader, CUInt(Marshal.SizeOf(waveHeader)))
              If result = MMRESULT.MMSYSERR_NOERROR Then
                WaitForWaveDone(hWaveOut)
              End If
            Finally
              waveOutUnprepareHeader(hWaveOut, waveHeader, CUInt(Marshal.SizeOf(waveHeader)))
            End Try
          Finally
            HeapFree(GetProcessHeap(), 0, waveHeader.lpData)
          End Try
        Finally
          waveOutClose(hWaveOut)
        End Try
      Catch ex As Exception
      End Try
    End Sub

    Private Shared Sub WaitForWaveDone(hWaveOut As IntPtr)
      Dim waveHeader = New WAVEHDR()
      Dim timeout = Environment.TickCount + 30000

      While Environment.TickCount < timeout
        Dim result = waveOutWrite(hWaveOut, waveHeader, 0)
        If result <> MMRESULT.MMSYSERR_NOERROR Then Exit While
        If (waveHeader.dwFlags And WHDR_DONE) <> 0 Then Exit While
        Threading.Thread.Sleep(10)
      End While

      waveOutReset(hWaveOut)
    End Sub

    Private Shared Sub GenerateSineWave(buffer As IntPtr, frequency As Integer, numSamples As ULong)
      Dim amplitude As Double = 0.7 * Short.MaxValue
      Dim twoPiF As Double = 2.0 * Math.PI * frequency
      Dim offset As Integer = 0

      For i As Integer = 0 To CInt(Math.Min(numSamples, CUInt(Integer.MaxValue))) - 1
        Dim t As Double = CDbl(i) / AudioConstants.SAMPLE_RATE
        Dim sample As Double = amplitude * Math.Sin(twoPiF * t)
        Dim sampleShort As Short = CShort(Math.Max(Short.MinValue, Math.Min(Short.MaxValue, sample)))
        Marshal.WriteInt16(buffer, offset, sampleShort)
        offset += 2

        If s_debugEnabled AndAlso s_debugStream IsNot Nothing Then
          Dim sampleBytes = BitConverter.GetBytes(sampleShort)
          s_debugStream.Write(sampleBytes, 0, 2)
          s_debugSamples += 1
        End If
      Next
    End Sub

    Public Shared Sub Sound(frequency As Integer, duration As Integer)
      If frequency < AudioConstants.MIN_FREQUENCY OrElse frequency > AudioConstants.MAX_FREQUENCY Then
        Return
      End If
      If duration < 0 Then Return

      Dim durationMs = CInt(duration * 1000.0 / 18.2)
      If durationMs < 1 Then durationMs = 1


      Beep(frequency, durationMs)
    End Sub

#End Region

  End Class

End Namespace