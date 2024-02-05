Namespace Global.Basic.Input

  Public Class PositionChangedEventArgs
    Inherits EventArgs

    Private ReadOnly m_position As New Drawing.Point(0, 0)

    Public Sub New()
    End Sub

    Public Sub New(x As Integer, y As Integer)
      m_position = New Drawing.Point(x, y)
    End Sub

    Public Sub New(position As Drawing.Point)
      m_position = position
    End Sub

    Public ReadOnly Property Position As Drawing.Point
      Get
        Return m_position
      End Get
    End Property

  End Class

End Namespace