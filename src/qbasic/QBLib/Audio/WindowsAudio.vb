Imports System.Runtime.InteropServices
Imports System.Threading

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

    Private Shared ReadOnly s_buffers(BUFFER_COUNT - 1) As WAVEHDR
    Private Shared ReadOnly s_bufferData(BUFFER_COUNT - 1) As IntPtr

    Private Shared s_isStreaming As Boolean = False
    Private Shared s_streamThread As Thread = Nothing
    Private Shared s_cts As CancellationTokenSource = Nothing
    'Private Shared s_currentFrequency As Integer = 0
    'Private Shared s_targetFrequency As Integer = 0
    Private Shared s_samplesRemaining As Integer = 0
    Private Shared s_samplePhase As Double = 0.0
    Private Shared ReadOnly s_streamLock As New Object()

    ' queue-based audio buffer system
    ' each entry: (audioData As Byte(), totalSamples As Integer)
    Private Shared ReadOnly s_audioQueue As New System.Collections.Concurrent.ConcurrentQueue(Of Tuple(Of Byte(), Integer))
    Private Shared s_queuedSamples As Integer = 0

    ' track samples pending completion for current SOUND
    'Private Shared s_pendingSamples As Integer = 0
    'Private Shared s_currentAudioSamples As Integer = 0

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

      ' first disable debug mode - this signals the streaming loop to stop gracefully
      s_debugEnabled = False

      ' wait for streaming loop to finish naturally (with timeout)
      Dim timeout = Environment.TickCount + 2000
      Do While s_isStreaming AndAlso s_streamThread IsNot Nothing AndAlso s_streamThread.IsAlive AndAlso Environment.TickCount < timeout
        Threading.Thread.Sleep(10)
      Loop

      ' then close the file and stop the stream if needed
      If s_debugStream IsNot Nothing Then
        s_debugStream.Flush()
        Dim dataSize = s_debugSamples * 2
        Dim riffSize = 36 + dataSize
        s_debugStream.Seek(4, System.IO.SeekOrigin.Begin)
        s_debugStream.Write(BitConverter.GetBytes(riffSize), 0, 4)
        s_debugStream.Seek(40, System.IO.SeekOrigin.Begin)
        s_debugStream.Write(BitConverter.GetBytes(dataSize), 0, 4)
        s_debugStream.Flush()
        s_debugStream.Close()
        s_debugStream = Nothing
      End If

      ' now stop the stream if still running
      If s_isStreaming Then StopStream()

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

      ' if already streaming and valid, do nothing
      If s_isStreaming AndAlso s_hWaveOut <> IntPtr.Zero AndAlso s_streamThread IsNot Nothing AndAlso s_streamThread.IsAlive Then Return

      ' if partially initialized, cleanup
      If s_isStreaming OrElse s_hWaveOut <> IntPtr.Zero Then StopStream() : Thread.Sleep(50)

      Dim waveFormat = New WAVEFORMATEX() With {
        .wFormatTag = CShort(WAVE_FORMAT_PCM),
        .nChannels = CShort(AudioConstants.CHANNELS),
        .nSamplesPerSec = AudioConstants.SAMPLE_RATE,
        .wBitsPerSample = CShort(AudioConstants.BITS_PER_SAMPLE),
        .cbSize = 0}
      waveFormat.nBlockAlign = CShort(waveFormat.nChannels * waveFormat.wBitsPerSample / 8)
      waveFormat.nAvgBytesPerSec = waveFormat.nSamplesPerSec * waveFormat.nBlockAlign

      Dim hWaveOut = IntPtr.Zero
      Dim result = waveOutOpen(hWaveOut, UInt32.MaxValue, waveFormat, IntPtr.Zero, IntPtr.Zero, 0)
      If result <> MMRESULT.MMSYSERR_NOERROR Then Return
      s_hWaveOut = hWaveOut

      Dim bufferSize = AudioConstants.SAMPLE_RATE * (AudioConstants.BITS_PER_SAMPLE \ 8) * BUFFER_DURATION_MS \ 1000
      Dim heap = GetProcessHeap()

      For i = 0 To BUFFER_COUNT - 1
        s_bufferData(i) = HeapAlloc(heap, HEAP_ZERO_MEMORY, New IntPtr(bufferSize))
        s_buffers(i).lpData = s_bufferData(i)
        s_buffers(i).dwBufferLength = CUInt(bufferSize)
        result = waveOutPrepareHeader(s_hWaveOut, s_buffers(i), CUInt(Marshal.SizeOf(s_buffers(i))))
        If result <> MMRESULT.MMSYSERR_NOERROR Then StopStream() : Return
      Next

      s_cts = New CancellationTokenSource()
      s_isStreaming = True
      's_currentFrequency = 0
      's_targetFrequency = 0
      s_samplesRemaining = 0
      s_samplePhase = 0.0

      s_streamThread = New Thread(AddressOf StreamingLoop) With {.IsBackground = True}
      s_streamThread.Start()

    End Sub

    Private Shared Sub CleanupStream()

      If s_hWaveOut <> IntPtr.Zero Then

        waveOutReset(s_hWaveOut)

        For i = 0 To BUFFER_COUNT - 1
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

      ' Just stop the thread - don't close the device
      If s_cts IsNot Nothing Then
        s_cts.Cancel()
        s_cts.Dispose()
        s_cts = Nothing
      End If

      If s_streamThread IsNot Nothing AndAlso s_streamThread.IsAlive Then
        s_streamThread.Join(500)
      End If
      s_streamThread = Nothing

      ' Keep the device open but reset it
      If s_hWaveOut <> IntPtr.Zero Then
        waveOutReset(s_hWaveOut)
      End If

      s_isStreaming = False

    End Sub

    Private Shared Sub StreamingLoop()

      Dim bufferSize = CInt(s_buffers(0).dwBufferLength)
      Dim samplesPerBuffer = bufferSize \ 2

      Do

        If s_cts.IsCancellationRequested Then Exit Do

        ' Try to get audio from queue
        Dim audioEntry As Tuple(Of Byte(), Integer) = Nothing
        Dim hasAudio = False

        SyncLock s_streamLock
          'If s_audioQueue.TryDequeue(audioEntry) Then
          '  hasAudio = True
          '  's_currentAudioSamples = audioEntry.Item2
          'End If
          hasAudio = s_audioQueue.TryDequeue(audioEntry)
        End SyncLock

        'Dim audioData() As Byte = Nothing

        If hasAudio Then

          Dim audioData = audioEntry.Item1
          Dim offset = 0
          Dim remaining = audioData.Length

          ' write all available buffers first (without waiting)
          For i = 0 To BUFFER_COUNT - 1
            If remaining <= 0 Then Exit For
            Dim copySize = Math.Min(samplesPerBuffer * 2, remaining)
            Marshal.Copy(audioData, offset, s_bufferData(i), copySize)
            WriteAudioToDebug(audioData, offset, copySize)
            waveOutWrite(s_hWaveOut, s_buffers(i), CUInt(copySize))
            offset += copySize
            remaining -= copySize
          Next

          ' then write remaining audio - wait for buffer to become free
          Do While remaining > 0
            Thread.Sleep(BUFFER_DURATION_MS)
            For i = 0 To BUFFER_COUNT - 1
              If remaining <= 0 Then Exit Do
              If (s_buffers(i).dwFlags And WHDR_DONE) <> 0 Then
                Dim copySize = Math.Min(samplesPerBuffer * 2, remaining)
                Marshal.Copy(audioData, offset, s_bufferData(i), copySize)
                WriteAudioToDebug(audioData, offset, copySize)
                waveOutWrite(s_hWaveOut, s_buffers(i), CUInt(copySize))
                offset += copySize
                remaining -= copySize
              End If
            Next
          Loop

        Else
          ' no audio in queue - don't write anything
          Thread.Sleep(10)
        End If

        ' keep streaming alive while debug recording is enabled
        If Not s_debugEnabled Then

          ' check if all audio is done and we should exit
          Dim queueEmpty = False
          Dim noPending = False
          SyncLock s_streamLock
            queueEmpty = (s_audioQueue.Count = 0)
            noPending = (s_queuedSamples = 0)
          End SyncLock

          If queueEmpty AndAlso noPending Then
            Dim allDone = True
            For i = 0 To BUFFER_COUNT - 1
              If s_bufferData(i) <> IntPtr.Zero AndAlso (s_buffers(i).dwFlags And WHDR_DONE) = 0 Then allDone = False : Exit For
            Next
            If allDone Then Exit Do
          End If

        End If

      Loop While s_isStreaming AndAlso (s_cts IsNot Nothing AndAlso Not s_cts.IsCancellationRequested)

    End Sub

    Private Shared Sub GenerateBuffer(buffer As IntPtr, numSamples As Integer, frequency As Integer)

      Const MAX_AMPLITUDE As Double = 0.85 * Short.MaxValue
      Const ATTACK_SAMPLES As Integer = 20
      Const DECAY_SAMPLES As Integer = 20

      Dim phase = s_samplePhase
      Dim twoPiF = 2.0 * Math.PI * frequency
      Dim samplesRemaining = s_samplesRemaining
      Dim inNote = (frequency > 0 AndAlso samplesRemaining > 0)
      Dim decayStart = Math.Max(0, samplesRemaining - DECAY_SAMPLES)

      For j = 0 To numSamples - 1

        Dim env = 1.0

        If Not inNote Then
          env = 0.0
        Else
          Dim sampleIndex = samplesRemaining - j
          If sampleIndex > samplesRemaining - ATTACK_SAMPLES Then
            env = CDbl(samplesRemaining - sampleIndex) / ATTACK_SAMPLES
          ElseIf sampleIndex <= decayStart Then
            env = CDbl(sampleIndex) / decayStart
          End If
        End If

        Dim squareValue = If(Math.Sin(phase) >= 0.0, MAX_AMPLITUDE, -MAX_AMPLITUDE)
        Dim sample = squareValue * env
        Dim sampleShort = CShort(Math.Max(Short.MinValue, Math.Min(Short.MaxValue, sample)))
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
      s_samplesRemaining = Math.Max(0, s_samplesRemaining - numSamples)

    End Sub

    ' generate audio data as byte array (for queue-based playback)
    Private Shared Function GenerateAudioData(numSamples As Integer, frequency As Integer) As Byte()

      Const MAX_AMPLITUDE As Double = 0.85 * Short.MaxValue

      Dim twoPiF = 2.0 * Math.PI * frequency
      Dim audioData(numSamples * 2 - 1) As Byte

      Dim phase = s_samplePhase
      For j = 0 To numSamples - 1
        ' constant amplitude - no attack/decay
        Dim squareValue = If(Math.Sin(phase) >= 0.0, MAX_AMPLITUDE, -MAX_AMPLITUDE)
        Dim sampleShort = CShort(squareValue)
        Dim bytes = BitConverter.GetBytes(sampleShort)
        audioData(j * 2) = bytes(0)
        audioData(j * 2 + 1) = bytes(1)
        phase += twoPiF / AudioConstants.SAMPLE_RATE
        If phase >= 2.0 * Math.PI Then phase -= 2.0 * Math.PI
      Next

      s_samplePhase = phase
      Return audioData

    End Function

    ' write audio data to debug stream
    Private Shared Sub WriteAudioToDebug(data() As Byte, offset As Integer, length As Integer)
      If s_debugEnabled AndAlso s_debugStream IsNot Nothing Then
        s_debugStream.Write(data, offset, length)
        s_debugSamples += length \ 2
      End If
    End Sub

    ' write silence to debug stream
    Private Shared Sub WriteSilenceToDebug(numBytes As Integer)
      If s_debugEnabled AndAlso s_debugStream IsNot Nothing Then
        Dim silence(numBytes - 1) As Byte
        s_debugStream.Write(silence, 0, numBytes)
        s_debugSamples += numBytes \ 2
      End If
    End Sub

    Public Shared Sub PlayTone(frequency As Integer, durationTicks As Integer, token As CancellationToken)

      If Not s_isStreaming OrElse s_streamThread Is Nothing OrElse Not s_streamThread.IsAlive Then StartStream()
      If frequency > AudioConstants.MAX_FREQUENCY Then AudioDevice.OnSoundFinished() : Return
      If frequency < 1 Then frequency = 1
      If durationTicks < 0 Then AudioDevice.OnSoundFinished() : Return

      Dim durationMs = CInt(durationTicks * 1000.0 / 18.2)
      If durationMs < BUFFER_DURATION_MS Then durationMs = BUFFER_DURATION_MS

      Dim samplesToPlay = AudioConstants.SAMPLE_RATE * durationMs \ 1000

      ' generate all audio data upfront and queue it as tuple (data, sampleCount)
      Dim audioData = GenerateAudioData(samplesToPlay, frequency)
      s_audioQueue.Enqueue(Tuple.Create(audioData, samplesToPlay))
      SyncLock s_streamLock
        s_queuedSamples += samplesToPlay
        's_pendingSamples = samplesToPlay
      End SyncLock

    End Sub

    Public Shared Sub PlayToneAsync(frequency As Integer, durationTicks As Integer, token As CancellationToken)
      ' calculate duration upfront and set end time BEFORE async task starts
      Dim durationMs = CInt(durationTicks * 1000.0 / 18.2)
      If durationMs < BUFFER_DURATION_MS Then durationMs = BUFFER_DURATION_MS
      Dim playTimeMs = durationMs + 20
      Interlocked.Exchange(s_lastSoundEndTime, Environment.TickCount + playTimeMs)
      Task.Run(Sub()
                 Try
                   PlayTone(frequency, durationTicks, token)
                 Catch ex As Exception
                 End Try
               End Sub, token)
    End Sub

    Public Shared Sub StopTone()
      SyncLock s_streamLock
        s_samplesRemaining = 0
        's_targetFrequency = 0
        's_currentFrequency = 0
        s_samplePhase = 0.0
        s_queuedSamples = 0
        's_pendingSamples = 0
      End SyncLock
      ' Clear the queue
      While s_audioQueue.TryDequeue(Nothing)
      End While
    End Sub

    Public Shared Sub ResetAudioState()
      ' only clear pending audio queue, keep current playback
      While s_audioQueue.TryDequeue(Nothing)
      End While
      's_pendingSamples = 0
    End Sub

    Public Shared Sub ClearAudioQueue()
      ' clear pending audio queue
      While s_audioQueue.TryDequeue(Nothing)
      End While
      's_pendingSamples = 0
    End Sub

    Public Shared Function HasQueuedAudio() As Boolean
      ' check if there are buffers still playing or audio in queue
      Dim hasPlaying = False
      For i = 0 To BUFFER_COUNT - 1
        If s_bufferData(i) <> IntPtr.Zero AndAlso (s_buffers(i).dwFlags And WHDR_DONE) = 0 Then
          hasPlaying = True
          Exit For
        End If
      Next
      Dim hasQueued = False
      SyncLock s_streamLock
        hasQueued = s_audioQueue.Count > 0 OrElse s_queuedSamples > 0 OrElse hasPlaying
      End SyncLock
      Return hasQueued
    End Function

    Private Shared s_lastSoundEndTime As Long = 0

    Public Shared Function IsStreaming() As Boolean
      Return s_isStreaming
    End Function

    Public Shared Sub WaitForPendingAudio()
      ' simply wait a reasonable time for the previous sound to finish
      ' the duration of the last sound was stored, so just wait for that
      Dim endTime = Interlocked.Read(s_lastSoundEndTime)
      If endTime > Environment.TickCount Then
        Do While endTime > Environment.TickCount
          Thread.Sleep(10)
        Loop
      End If
    End Sub

#End Region

#Region "Legacy Beep (for simple beeps)"

    Public Shared Sub Beep()
      Beep(AudioConstants.DEFAULT_BEEP_FREQUENCY, AudioConstants.DEFAULT_BEEP_DURATION_MS)
    End Sub

    Public Shared Sub Beep(frequency As Integer, durationMs As Integer)
      Sound(frequency, CInt(durationMs * 18.2 / 1000.0))
    End Sub

    Private Shared Sub BeepWithWaveOut(frequency As Integer, durationMs As Integer)
      If (Not s_isStreaming) OrElse (s_streamThread Is Nothing) OrElse (Not s_streamThread.IsAlive) Then
        StartStream()
      End If

      Dim numSamples = CULng(AudioConstants.SAMPLE_RATE) * CULng(durationMs) \ 1000UL
      Dim audioData As Byte() = GenerateAudioDataForDuration(CInt(numSamples), frequency)

      ' Queue the audio for the streaming system
      s_audioQueue.Enqueue(Tuple.Create(audioData, CInt(numSamples)))
      SyncLock s_streamLock
        s_queuedSamples += CInt(numSamples)
      End SyncLock

      ' Set when this sound will finish playing
      ' We estimate playback time based on samples / sample rate + buffer
      Dim playTimeMs As Integer = CInt(numSamples) * 1000 \ AudioConstants.SAMPLE_RATE + 100
      Interlocked.Exchange(s_lastSoundEndTime, Environment.TickCount + playTimeMs + 100)
      ' Return immediately - sound plays asynchronously
    End Sub

    Private Shared Function GenerateAudioDataForDuration(numSamples As Integer, frequency As Integer) As Byte()
      Const MAX_AMPLITUDE As Double = 0.85 * Short.MaxValue
      Const ATTACK_SAMPLES As Integer = 20
      Const DECAY_SAMPLES As Integer = 20

      Dim twoPiF As Double = 2.0 * Math.PI * frequency
      Dim audioData(numSamples * 2 - 1) As Byte
      Dim decayStart As Integer = Math.Max(0, numSamples - DECAY_SAMPLES)

      For i As Integer = 0 To numSamples - 1
        Dim env As Double = 1.0
        If i < ATTACK_SAMPLES Then
          env = CDbl(i) / ATTACK_SAMPLES
        ElseIf i >= decayStart Then
          env = CDbl(numSamples - i) / DECAY_SAMPLES
        End If

        Dim squareValue As Double = If(Math.Sin(twoPiF * CDbl(i) / AudioConstants.SAMPLE_RATE) >= 0.0, MAX_AMPLITUDE, -MAX_AMPLITUDE)
        Dim sample As Double = squareValue * env
        Dim sampleShort As Short = CShort(Math.Max(Short.MinValue, Math.Min(Short.MaxValue, sample)))
        Dim bytes() As Byte = BitConverter.GetBytes(sampleShort)
        audioData(i * 2) = bytes(0)
        audioData(i * 2 + 1) = bytes(1)
      Next

      Return audioData
    End Function

    Private Shared Sub GenerateSquareWave(buffer As IntPtr, frequency As Integer, numSamples As ULong)
      Const MAX_AMPLITUDE As Double = 0.85 * Short.MaxValue
      Const ATTACK_SAMPLES As Integer = 20
      Const DECAY_SAMPLES As Integer = 20

      Dim twoPiF As Double = 2.0 * Math.PI * frequency
      Dim offset As Integer = 0
      Dim totalSamples = CInt(Math.Min(numSamples, CUInt(Integer.MaxValue)))
      Dim decayStart As Integer = Math.Max(0, totalSamples - DECAY_SAMPLES)

      For i As Integer = 0 To totalSamples - 1
        Dim env As Double = 1.0
        If i < ATTACK_SAMPLES Then
          env = CDbl(i) / ATTACK_SAMPLES
        ElseIf i >= decayStart Then
          env = CDbl(totalSamples - i) / DECAY_SAMPLES
        End If

        Dim squareValue As Double = If(Math.Sin(twoPiF * CDbl(i) / AudioConstants.SAMPLE_RATE) >= 0.0, MAX_AMPLITUDE, -MAX_AMPLITUDE)
        Dim sample As Double = squareValue * env
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

      BeepWithWaveOut(frequency, durationMs)
    End Sub

#End Region

  End Class

End Namespace