Namespace Global.QBLib

  Public NotInheritable Class KeyboardEmulator

    Private Shared m_memory(65535) As Byte
    Private Shared m_initialized As Boolean = False

    Private Shared ReadOnly ImplementedBiosAddresses As New Dictionary(Of Integer, String) From {
      {1047, "Keyboard flag byte 0 (BIOS Data Area 0x417)"}
    }

    Private Shared ReadOnly BiosDataAreaAddresses As HashSet(Of Integer)

    Shared Sub New()
      BiosDataAreaAddresses = New HashSet(Of Integer)
      For i As Integer = 0 To 255
        BiosDataAreaAddresses.Add(i)
      Next
      BiosDataAreaAddresses.Add(1040)
      BiosDataAreaAddresses.Add(1041)
      BiosDataAreaAddresses.Add(1047)
    End Sub

    Private Sub New()
    End Sub

    Public Shared Sub Initialize()
      For i As Integer = 0 To 65535
        m_memory(i) = 0
      Next
      m_memory(1047) = &H20
      m_initialized = True
    End Sub

    Public Shared Sub Reset()
      Initialize()
    End Sub

    Public Shared Function GetKeyboardFlags(address As Integer) As Byte
      If Not m_initialized Then Initialize()
      If address < 0 OrElse address > 65535 Then Return 0
      Return m_memory(address)
    End Function

    Public Shared Sub SetKeyboardFlags(address As Integer, value As Byte)
      If Not m_initialized Then Initialize()
      If address >= 0 AndAlso address <= 65535 Then
        m_memory(address) = value
      End If
    End Sub

    Public Shared Function Peek(address As Integer) As Byte
      If Not m_initialized Then Initialize()
      If address < 0 OrElse address > 65535 Then Return 0
      Return m_memory(address)
    End Function

    Public Shared Sub Poke(address As Integer, value As Byte)
      If Not m_initialized Then Initialize()
      If address >= 0 AndAlso address <= 65535 Then
        m_memory(address) = value
      End If
    End Sub

    Public Shared Function IsNumLockOn() As Boolean
      Return (GetKeyboardFlags(1047) And &H20) <> 0
    End Function

    Public Shared Function IsCapsLockOn() As Boolean
      Return (GetKeyboardFlags(1047) And &H40) <> 0
    End Function

    Public Shared Function IsScrollLockOn() As Boolean
      Return (GetKeyboardFlags(1047) And &H10) <> 0
    End Function

    Public Shared Function IsKnownBiosAddress(address As Integer) As Boolean
      Return address >= 0 AndAlso address <= 65535 AndAlso BiosDataAreaAddresses.Contains(address)
    End Function

    Public Shared Function IsImplementedBiosAddress(address As Integer) As Boolean
      Return ImplementedBiosAddresses.ContainsKey(address)
    End Function

    Public Shared Function GetAddressDescription(address As Integer) As String
      If ImplementedBiosAddresses.ContainsKey(address) Then
        Return ImplementedBiosAddresses(address)
      ElseIf BiosDataAreaAddresses.Contains(address) Then
        Return "BIOS Data Area"
      End If
      Return "Unknown"
    End Function

  End Class

End Namespace
