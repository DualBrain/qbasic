Imports System
Imports System.Runtime.InteropServices

Namespace Global.QBLib.Audio

  Friend NotInheritable Class WindowsAudio

#Region "Win32 API Declarations"

    <DllImport("kernel32.dll", SetLastError:=True)>
    Private Shared Function Beep(dwFreq As UInteger, dwDuration As UInteger) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    Private Declare Auto Sub MessageBeep Lib "user32.dll" (uType As UInteger)

#End Region

#Region "Waveform Audio API for longer durations"

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

#End Region

#Region "Beep"

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
          waveHeader.lpData = HeapAlloc(Diagnostics.Process.GetCurrentProcess().Handle, HEAP_ZERO_MEMORY, New IntPtr(bufferSizeInt))
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
            HeapFree(Diagnostics.Process.GetCurrentProcess().Handle, 0, waveHeader.lpData)
          End Try
        Finally
          waveOutClose(hWaveOut)
        End Try
      Catch
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
      Next
    End Sub

#End Region

#Region "Sound (ticks-based duration)"

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
