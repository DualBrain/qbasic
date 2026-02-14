Namespace Global.QB.CodeAnalysis

  Public NotInheritable Class PenService

    Private Shared m_state As New PenState
    Private Shared m_isEnabled As Boolean = False
    Private Shared m_isStopped As Boolean = False
    Private Shared m_handlerTarget As Object = Nothing

    Public Shared ReadOnly Property State As PenState
      Get
        Return m_state
      End Get
    End Property

    Public Shared Property Enabled As Boolean
      Get
        Return m_isEnabled
      End Get
      Set(value As Boolean)
        m_isEnabled = value
      End Set
    End Property

    Public Shared Property Stopped As Boolean
      Get
        Return m_isStopped
      End Get
      Set(value As Boolean)
        m_isStopped = value
      End Set
    End Property

    Public Shared Property HandlerTarget As Object
      Get
        Return m_handlerTarget
      End Get
      Set(value As Object)
        m_handlerTarget = value
      End Set
    End Property

    Public Shared ReadOnly Property IsActive As Boolean
      Get
        Return m_isEnabled AndAlso Not m_isStopped
      End Get
    End Property

    Public Shared ReadOnly Property HasHandler As Boolean
      Get
        Return m_handlerTarget IsNot Nothing
      End Get
    End Property

    Public Shared Sub OnPenDown(pixelX As Integer, pixelY As Integer, characterWidth As Integer, characterHeight As Integer)
      m_state.WasPressed = True
      m_state.IsPressed = True
      m_state.PressedX = pixelX
      m_state.PressedY = pixelY
      m_state.CurrentX = pixelX
      m_state.CurrentY = pixelY

      Dim row = (pixelY \ characterHeight) + 1
      Dim col = (pixelX \ characterWidth) + 1
      m_state.PressedRow = Math.Max(1, Math.Min(row, 24))
      m_state.PressedColumn = Math.Max(1, Math.Min(col, If(QBLib.Video.m_textW = 8, 80, 40)))
      m_state.CurrentRow = m_state.PressedRow
      m_state.CurrentColumn = m_state.PressedColumn
    End Sub

    Public Shared Sub OnPenUp()
      m_state.IsPressed = False
    End Sub

    Public Shared Sub OnPenMove(pixelX As Integer, pixelY As Integer, characterWidth As Integer, characterHeight As Integer)
      m_state.CurrentX = pixelX
      m_state.CurrentY = pixelY

      Dim row = (pixelY \ characterHeight) + 1
      Dim col = (pixelX \ characterWidth) + 1
      m_state.CurrentRow = Math.Max(1, Math.Min(row, 24))
      m_state.CurrentColumn = Math.Max(1, Math.Min(col, If(QBLib.Video.m_textW = 8, 80, 40)))
    End Sub

    Public Shared Function PollWasPressed() As Boolean
      Dim result = m_state.WasPressed
      m_state.WasPressed = False
      Return result
    End Function

    Public Shared Sub Reset()
      m_state = New PenState
      m_isEnabled = False
      m_isStopped = False
      m_handlerTarget = Nothing
    End Sub

  End Class

  Public Structure PenState

    Public WasPressed As Boolean
    Public PressedX As Integer
    Public PressedY As Integer
    Public IsPressed As Boolean
    Public CurrentX As Integer
    Public CurrentY As Integer
    Public PressedRow As Integer
    Public PressedColumn As Integer
    Public CurrentRow As Integer
    Public CurrentColumn As Integer

  End Structure

End Namespace
