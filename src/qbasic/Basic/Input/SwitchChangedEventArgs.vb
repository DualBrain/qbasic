Namespace Global.Basic.Input

  Public Class SwitchChangedEventArgs
    Inherits EventArgs

    Private ReadOnly m_pressed As Boolean = False

    Public Sub New()
    End Sub

    Public Sub New(pressed As Boolean)
      m_pressed = pressed
    End Sub

    Public ReadOnly Property Pressed As Boolean
      Get
        Return m_pressed
      End Get
    End Property

  End Class

End Namespace