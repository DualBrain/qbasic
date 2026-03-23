Imports System.Runtime.InteropServices
Imports System
Imports System.Threading

Namespace Global.QBLib.Audio

  Public Module AudioDevice

    Private ReadOnly s_isWindows As Boolean = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    Private ReadOnly s_isLinux As Boolean = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)

    Private m_soundPlaying As Boolean = False
    Private m_currentSoundCts As CancellationTokenSource = Nothing

    Public Property IsSoundPlaying As Boolean
      Get
        Return m_soundPlaying
      End Get
      Private Set(value As Boolean)
        m_soundPlaying = value
      End Set
    End Property

    Public Sub Beep()
      If s_isWindows Then
        WindowsAudio.Beep()
      ElseIf s_isLinux Then
        LinuxAudio.Beep()
      End If
    End Sub

    Public Sub Beep(frequency As Integer, durationMs As Integer)
      If s_isWindows Then
        WindowsAudio.Beep(frequency, durationMs)
      ElseIf s_isLinux Then
        LinuxAudio.Beep(frequency, durationMs)
      End If
    End Sub

    Public Sub Sound(frequency As Integer, duration As Integer)
      SyncLock GetType(AudioDevice)
        If duration = 0 Then
          If m_soundPlaying Then
            If s_isWindows Then
              WindowsAudio.StopTone()
            ElseIf s_isLinux Then
              LinuxAudio.StopTone()
            End If
            CancelCurrentSound()
          End If
          Return
        End If

        If m_soundPlaying Then
          WaitForSound()
        End If

        m_currentSoundCts = New CancellationTokenSource()
        Dim token = m_currentSoundCts.Token
        m_soundPlaying = True

        If s_isWindows Then
          WindowsAudio.PlayToneAsync(frequency, duration, token)
        ElseIf s_isLinux Then
          LinuxAudio.SoundAsync(frequency, duration, token)
        End If
      End SyncLock
    End Sub

    Public Sub WaitForSound()
      SyncLock GetType(AudioDevice)
        While m_soundPlaying
          Monitor.Wait(GetType(AudioDevice))
        End While
      End SyncLock
    End Sub

    Private Sub CancelCurrentSound()
      SyncLock GetType(AudioDevice)
        If m_currentSoundCts IsNot Nothing Then
          m_currentSoundCts.Cancel()
          m_currentSoundCts.Dispose()
          m_currentSoundCts = Nothing
        End If
        m_soundPlaying = False
        Monitor.PulseAll(GetType(AudioDevice))
      End SyncLock
    End Sub

    Friend Sub OnSoundFinished()
      SyncLock GetType(AudioDevice)
        m_soundPlaying = False
        If m_currentSoundCts IsNot Nothing Then
          m_currentSoundCts.Dispose()
          m_currentSoundCts = Nothing
        End If
        Monitor.PulseAll(GetType(AudioDevice))
      End SyncLock
    End Sub

    Friend Sub StopAudio()
      SyncLock GetType(AudioDevice)
        If s_isWindows Then
          WindowsAudio.StopTone()
        ElseIf s_isLinux Then
          LinuxAudio.StopTone()
        End If
        If m_currentSoundCts IsNot Nothing Then
          m_currentSoundCts.Cancel()
          m_currentSoundCts.Dispose()
          m_currentSoundCts = Nothing
        End If
        m_soundPlaying = False
        Monitor.PulseAll(GetType(AudioDevice))
      End SyncLock
    End Sub

    Public Sub EnableAudioDebug(filePath As String)
      If s_isWindows Then
        WindowsAudio.EnableDebugOutput(filePath)
      End If
    End Sub

    Public Sub DisableAudioDebug()
      If s_isWindows Then
        WindowsAudio.DisableDebugOutput()
      End If
    End Sub

  End Module

End Namespace