Namespace Global.Basic.Printer

  Public Class Printer

    ' + IOCTL
    ' + IOCTL$
    ' + LLIST
    ' + LPRINT
    ' + LPRINT USING
    ' + LPOS
    ' + PRINT #
    ' + PRINT USING #
    ' + WIDTH

    Private ReadOnly m_deviceName As String
    Private ReadOnly m_lines As New List(Of String)
    Private m_width As Integer = 80
    Private m_pageLength As Integer = -1

    Private ReadOnly m_ioCtlCommand As String

    Public Sub New(deviceName As String)
      m_deviceName = deviceName
      m_lines.Add("")
    End Sub

    Public ReadOnly Property Document() As String
      Get
        Dim result As String = Nothing
        For Each line In m_lines
          If result IsNot Nothing Then
            result &= String.Format("{0}{1}", vbCrLf, line)
          Else
            result = line
          End If
        Next
        Return result
      End Get
    End Property

    Public ReadOnly Property DeviceName As String
      Get
        Return m_deviceName
      End Get
    End Property

    Public Function IoCtl() As String
      If m_ioCtlCommand = "GW" Then
        Return m_width.ToString
      End If
      Return ""
    End Function

    Public Sub IoCtl(command As String)
      Dim list As String() = command.Split(";")
      For Each cmd In list
        If cmd.StartsWith("PL") Then ' Page Length
          If IsNumeric(cmd.Substring(2), False) Then
            m_pageLength = CInt(cmd.Substring(2))
          End If
        End If
      Next
      If m_pageLength = 0 Then
      End If
    End Sub

    Public ReadOnly Property LPos() As Integer
      Get
        Return m_lines(m_lines.Count - 1).Length
      End Get
    End Property

    Public Property Width() As Integer
      Get
        Return m_width
      End Get
      Set(value As Integer)
        m_width = value
      End Set
    End Property

    Public Function Print() As Boolean
      Return Print("", True)
    End Function

    Public Function Print(text As String, lineFeed As Boolean) As Boolean

      If m_lines(m_lines.Count - 1).Length + text.Length <= m_width Then
        m_lines(m_lines.Count - 1) &= text ' fits on the current line...
      Else
        Dim split As Integer = m_width - m_lines(m_lines.Count - 1).Length
        m_lines(m_lines.Count - 1) &= text.Substring(0, split)
        m_lines.Add(text.Substring(split))
      End If

      If lineFeed Then
        m_lines.Add("")
      End If

      Return True

    End Function

    Public Sub Clear()
      m_lines.Clear()
    End Sub

  End Class

End Namespace