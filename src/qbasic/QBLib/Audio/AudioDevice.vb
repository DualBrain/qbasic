Imports System
Imports System.Collections.Generic
Imports System.Runtime.InteropServices
Imports System.Threading

Namespace Global.QBLib.Audio

  Friend Class TimedQueueItem
    Public ReadOnly Frequency As Integer
    Public ReadOnly DurationMs As Integer
    Public ReadOnly Volume As Integer
    Public ReadOnly IsGap As Boolean
    Public ReadOnly IsLooping As Boolean
    Public Expiry As DateTime

    Public Sub New(frequency As Integer, durationMs As Integer, volume As Integer, isGap As Boolean, isLooping As Boolean)
      Me.Frequency = frequency
      Me.DurationMs = durationMs
      Me.Volume = volume
      Me.IsGap = isGap
      Me.IsLooping = isLooping
    End Sub
  End Class

  Friend NotInheritable Class TimedQueue
    Private ReadOnly m_items As List(Of TimedQueueItem) = New List(Of TimedQueueItem)()
    Private m_balloonPopped As Boolean = False

    Public Sub Add(frequency As Integer, durationMs As Integer, volume As Integer, isGap As Boolean, isLooping As Boolean)
      RemoveExpired()

      If isLooping AndAlso m_items.Count > 0 AndAlso m_items(m_items.Count - 1).IsLooping Then
        m_items.RemoveAt(m_items.Count - 1)
      End If

      Dim expiry As DateTime
      If durationMs < 0 Then
        expiry = DateTime.MaxValue
      ElseIf m_items.Count = 0 Then
        expiry = DateTime.Now.AddMilliseconds(durationMs)
      Else
        Dim lastExpiry = m_items(m_items.Count - 1).Expiry
        expiry = If(lastExpiry > DateTime.Now, lastExpiry, DateTime.Now).AddMilliseconds(durationMs)
      End If

      Dim item = New TimedQueueItem(frequency, durationMs, volume, isGap, isLooping) With {.Expiry = expiry}
      m_items.Add(item)
    End Sub

    Public Sub Clear()
      m_items.Clear()
      m_balloonPopped = False
    End Sub

    Public ReadOnly Property Count As Integer
      Get
        RemoveExpired()
        Return m_items.Count
      End Get
    End Property

    Public ReadOnly Property TonesWaiting As Integer
      Get
        RemoveExpired()
        Dim waiting = 0
        For i = 0 To m_items.Count - 1
          If m_items(i).IsGap = False AndAlso i > 0 Then
            waiting += 1
          End If
        Next
        waiting += If(m_balloonPopped, 1, 0)
        m_balloonPopped = False
        Return waiting
      End Get
    End Property

    Public Function GetExpiry() As DateTime
      RemoveExpired()
      If m_items.Count = 0 Then Return DateTime.Now
      If m_items(m_items.Count - 1).IsLooping Then Return DateTime.Now
      Return m_items(m_items.Count - 1).Expiry
    End Function

    Public Function PeekItems() As IEnumerable(Of TimedQueueItem)
      RemoveExpired()
      Return m_items
    End Function

    Private Sub RemoveExpired()
      Dim now = DateTime.Now
      While m_items.Count > 0 AndAlso m_items(0).Expiry <= now
        Dim popped = m_items(0)
        m_items.RemoveAt(0)
        If popped.IsLooping Then
          m_balloonPopped = True
        End If
      End While
    End Sub
  End Class

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

    Private m_soundOn As Boolean = True
    Private m_beepOn As Boolean = True
    Private m_multivoice As Boolean = False
    Private ReadOnly m_voices As TimedQueue() = {
      New TimedQueue(),
      New TimedQueue(),
      New TimedQueue(),
      New TimedQueue()
    }
    Private m_noiseFrequencies As Double() = {
      3579545.0 / 1024.0,
      3579545.0 / 1024.0 / 2.0,
      3579545.0 / 1024.0 / 4.0,
      0.0,
      3579545.0 / 1024.0,
      3579545.0 / 1024.0 / 2.0,
      3579545.0 / 1024.0 / 4.0,
      0.0
    }

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

      For Each voice In m_voices
        voice.Clear()
      Next
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

    Public Sub SetSoundOn(isOn As Boolean)
      m_soundOn = isOn
      If Not isOn Then
        For Each voice In m_voices
          voice.Clear()
        Next
        If s_isWindows Then
          WindowsAudio.StopTone()
        ElseIf s_isLinux Then
          LinuxAudio.StopTone()
        End If
      End If
    End Sub

    Public Sub SetBeepOn(isOn As Boolean)
      m_beepOn = isOn
    End Sub

    Public Sub EmitTone(voice As Integer, frequency As Integer, durationSec As Double, looped As Boolean, volume As Integer)
      If voice < 0 OrElse voice > 3 Then Return

      If Not (m_beepOn OrElse m_soundOn) Then
        volume = 0
      End If

      If frequency = 0 OrElse volume = 0 Then
        m_voices(voice).Add(0, 0, 0, True, False)
        Return
      End If

      Dim durationTicks = CInt(durationSec * 18.2)
      m_voices(voice).Add(frequency, CInt(durationSec * 1000.0), volume, False, looped)

      If m_soundOn Then
        m_currentSoundCts = New CancellationTokenSource()
        m_soundPlaying = True
        If s_isWindows Then
          WindowsAudio.PlayToneAsync(frequency, durationTicks, m_currentSoundCts.Token)
        ElseIf s_isLinux Then
          LinuxAudio.SoundAsync(frequency, durationTicks, m_currentSoundCts.Token)
        End If
      End If

      If voice = 2 AndAlso frequency <> 0 AndAlso m_multivoice Then
        m_noiseFrequencies(3) = frequency / 2.0
        m_noiseFrequencies(7) = frequency / 2.0
      End If
    End Sub

    Public Sub EmitGap(voice As Integer, durationSec As Double)
      If voice < 0 OrElse voice > 3 Then Return
      m_voices(voice).Add(0, CInt(durationSec * 1000.0), 0, True, False)
    End Sub

    Public Sub EmitNoise(source As Integer, volume As Integer, durationSec As Double, looped As Boolean)
      If Not m_soundOn Then Return
      If source < 0 OrElse source > 7 Then Return
      Dim frequency = CInt(m_noiseFrequencies(source))
      Dim durationTicks = CInt(durationSec * 18.2)
      m_voices(3).Add(frequency, CInt(durationSec * 1000.0), volume, False, looped)

      If s_isWindows Then
        m_currentSoundCts = New CancellationTokenSource()
        m_soundPlaying = True
        WindowsAudio.PlayToneAsync(frequency, durationTicks, m_currentSoundCts.Token)
      End If
    End Sub

    Public Sub StopAllSound()
      For Each voice In m_voices
        voice.Clear()
      Next
      If s_isWindows Then
        WindowsAudio.StopTone()
      ElseIf s_isLinux Then
        LinuxAudio.StopTone()
      End If
      CancelCurrentSound()
    End Sub

    Public Function GetQueueDepth(voice As Integer) As Integer
      If voice < 0 OrElse voice > 3 Then Return 0
      Return m_voices(voice).Count
    End Function

    Public Function GetMaxQueueDepth() As Integer
      Dim maxDepth = 0
      For i = 0 To 3
        maxDepth = Math.Max(maxDepth, m_voices(i).Count)
      Next
      Return maxDepth
    End Function

    Public Function TonesWaiting() As Integer
      Dim maxWaiting = 0
      For i = 0 To 2
        maxWaiting = Math.Max(maxWaiting, m_voices(i).TonesWaiting)
      Next
      Return maxWaiting
    End Function

    Public Sub WaitForBackground(maxQueue As Integer)
      While GetMaxQueueDepth() > maxQueue
        Thread.Sleep(10)
      End While
    End Sub

    Public Sub SetMultiVoice(multivoice As Boolean)
      m_multivoice = multivoice
      m_soundOn = Not multivoice
    End Sub

    Public ReadOnly Property IsMultiVoice As Boolean
      Get
        Return m_multivoice
      End Get
    End Property

  End Module

End Namespace
