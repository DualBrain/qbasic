Imports System.Runtime.InteropServices
Imports System

Namespace Global.QBLib.Audio

  Public Module AudioDevice

    Private ReadOnly s_isWindows As Boolean = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    Private ReadOnly s_isLinux As Boolean = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)

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
      If s_isWindows Then
        WindowsAudio.Sound(frequency, duration)
      ElseIf s_isLinux Then
        LinuxAudio.Sound(frequency, duration)
      End If
    End Sub

  End Module

End Namespace
