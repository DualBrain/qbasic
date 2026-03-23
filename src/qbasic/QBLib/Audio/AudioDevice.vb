Imports System
Imports System.Collections.Generic
Imports System.Runtime.InteropServices
Imports System.Threading

Namespace Global.QBLib.Audio

  Public Module AudioDevice

    Private ReadOnly s_isWindows As Boolean = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    Private ReadOnly s_isLinux As Boolean = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)

    Private m_soundPlaying As Boolean = False
    Private m_currentSoundCts As CancellationTokenSource = Nothing

    Private Enum MusicMode
      Foreground
      Background
    End Enum

    Private m_musicMode As MusicMode = MusicMode.Foreground
    Private ReadOnly m_musicQueue As New List(Of (Frequency As Integer, Duration As Integer))
    Private ReadOnly m_musicQueueLock As New Object()
    Private m_backgroundPlayerThread As Thread = Nothing
    Private m_backgroundPlayerCts As CancellationTokenSource = Nothing
    Private m_playEventHandler As Action = Nothing
    Private m_playEventThreshold As Integer = 32
    Private m_playEventEnabled As Boolean = False
    Private m_playEventStopped As Boolean = False
    Private m_playEventPending As Boolean = False

    Public Property IsSoundPlaying As Boolean
      Get
        Return m_soundPlaying
      End Get
      Private Set(value As Boolean)
        m_soundPlaying = value
      End Set
    End Property

    Public ReadOnly Property MusicQueueDepth As Integer
      Get
        SyncLock m_musicQueueLock
          Return m_musicQueue.Count
        End SyncLock
      End Get
    End Property

    Public Sub SetMusicModeForeground()
      m_musicMode = MusicMode.Foreground
    End Sub

    Public Sub SetMusicModeBackground()
      m_musicMode = MusicMode.Background
      StartBackgroundPlayer()
    End Sub

    Public Sub SetPlayEventHandler(handler As Action, threshold As Integer)
      m_playEventHandler = handler
      m_playEventThreshold = threshold
    End Sub

    Public Sub EnablePlayEvents()
      m_playEventEnabled = True
      m_playEventStopped = False
      CheckPlayEvent()
    End Sub

    Public Sub DisablePlayEvents()
      m_playEventEnabled = False
      m_playEventStopped = False
      m_playEventPending = False
    End Sub

    Public Sub StopPlayEvents()
      m_playEventStopped = True
    End Sub

    Friend Sub CheckPlayEvent()
      If m_playEventEnabled AndAlso Not m_playEventStopped AndAlso m_playEventHandler IsNot Nothing Then
        Dim queueDepth As Integer
        SyncLock m_musicQueueLock
          queueDepth = m_musicQueue.Count
        End SyncLock

        If queueDepth < m_playEventThreshold AndAlso Not m_playEventPending Then
          m_playEventPending = True
          m_playEventHandler()
        End If
      End If
    End Sub

    Friend Sub OnPlayEventHandled()
      m_playEventPending = False
      If m_playEventStopped Then
        m_playEventEnabled = True
        m_playEventStopped = False
      End If
    End Sub

    Private Sub StartBackgroundPlayer()
      If m_backgroundPlayerThread IsNot Nothing AndAlso m_backgroundPlayerThread.IsAlive Then
        Return
      End If

      m_backgroundPlayerCts = New CancellationTokenSource()
      m_backgroundPlayerThread = New Thread(AddressOf BackgroundPlayerLoop)
      m_backgroundPlayerThread.IsBackground = True
      m_backgroundPlayerThread.Start()
    End Sub

    Private Sub BackgroundPlayerLoop()
      Do
        If m_backgroundPlayerCts.Token.IsCancellationRequested Then
          Exit Do
        End If

        Dim note As (Frequency As Integer, Duration As Integer)?
        SyncLock m_musicQueueLock
          If m_musicQueue.Count > 0 Then
            note = m_musicQueue(0)
            m_musicQueue.RemoveAt(0)
          End If
        End SyncLock

        If note IsNot Nothing Then
          CheckPlayEvent()
          PlayNoteForeground(note.Value.Frequency, note.Value.Duration)
        Else
          Thread.Sleep(10)
        End If
      Loop While Not m_backgroundPlayerCts.Token.IsCancellationRequested OrElse m_musicQueue.Count > 0
    End Sub

    Private Sub PlayNoteForeground(frequency As Integer, durationTicks As Integer)
      If frequency = 0 Then
        Return
      End If

      If frequency < AudioConstants.MIN_FREQUENCY OrElse frequency > AudioConstants.MAX_FREQUENCY Then
        Return
      End If

      m_currentSoundCts = New CancellationTokenSource()
      Dim token = m_currentSoundCts.Token

      SyncLock GetType(AudioDevice)
        m_soundPlaying = True
      End SyncLock

      If s_isWindows Then
        WindowsAudio.PlayToneAsync(frequency, durationTicks, token)
      ElseIf s_isLinux Then
        LinuxAudio.SoundAsync(frequency, durationTicks, token)
      End If

      WaitForSound()
    End Sub

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
          SyncLock m_musicQueueLock
            m_musicQueue.Clear()
          End SyncLock
          Return
        End If

        If m_musicMode = MusicMode.Background Then
          SyncLock m_musicQueueLock
            If m_musicQueue.Count < 32 Then
              m_musicQueue.Add((frequency, duration))
            End If
          End SyncLock
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

      If m_backgroundPlayerCts IsNot Nothing Then
        m_backgroundPlayerCts.Cancel()
        m_backgroundPlayerCts.Dispose()
        m_backgroundPlayerCts = Nothing
      End If
      SyncLock m_musicQueueLock
        m_musicQueue.Clear()
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