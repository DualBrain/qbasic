Public MustInherit Class Control

  Public Event AutoSizeChanged As EventHandler
  Public Event BackColorChanged As EventHandler
  Public Event BackgroundImageChanged As EventHandler
  Public Event BackgroundImageLayoutChanged As EventHandler
  Public Event BindingContextChanged As EventHandler
  Public Event CausesValidationChanged As EventHandler
  Public Event ChangeUICues As EventHandler
  Public Event Click As EventHandler
  Public Event ClientSizeChanged As EventHandler
  Public Event ContextMenuStripChanged As EventHandler
  Public Event ControlAdded As EventHandler
  Public Event ControlRemoved As EventHandler
  Public Event CursorChanged As EventHandler
  Public Event DataContextChanged As EventHandler
  Public Event Disposed As EventHandler
  Public Event DockChanged As EventHandler
  Public Event DoubleClick As EventHandler
  Public Event DpiChangedAfterParent As EventHandler
  Public Event DpiChangedBeforeParent As EventHandler
  Public Event DragDrop As EventHandler
  Public Event DragEnter As EventHandler
  Public Event DragLeave As EventHandler
  Public Event DragOver As EventHandler
  Public Event EnabledChanged As EventHandler
  Public Event Enter As EventHandler
  Public Event FontChanged As EventHandler
  Public Event ForeColorChanged As EventHandler
  Public Event GiveFeedback As EventHandler
  Public Event GotFocus As EventHandler
  Public Event HandleCreated As EventHandler
  Public Event HandleDestroyed As EventHandler
  Public Event HelpRequested As EventHandler
  Public Event ImeModeChanged As EventHandler
  Public Event Invalidated As EventHandler
  Public Event KeyDown As EventHandler
  Public Event KeyPress As EventHandler
  Public Event KeyUp As EventHandler
  Public Event Layout As EventHandler
  Public Event Leave As EventHandler
  Public Event LocationChanged As EventHandler
  Public Event LostFocus As EventHandler
  Public Event MarginChanged As EventHandler
  Public Event MouseCaptureChanged As EventHandler
  Public Event MouseClick As EventHandler
  Public Event MouseDoubleClick As EventHandler
  Public Event MouseDown As EventHandler
  Public Event MouseEnter As EventHandler
  Public Event MouseHover As EventHandler
  Public Event MouseLeave As EventHandler
  Public Event MouseMove As EventHandler
  Public Event MouseUp As EventHandler
  Public Event MouseWheel As EventHandler
  Public Event Move As EventHandler
  Public Event PaddingChanged As EventHandler
  Public Event Paint As EventHandler
  Public Event ParentChanged As EventHandler
  Public Event PreviewKeyDown As EventHandler
  Public Event QueryAccessibilityHelp As EventHandler
  Public Event QueryContinueDrag As EventHandler
  Public Event RegionChanged As EventHandler
  Public Event Resize As EventHandler
  Public Event RightToLeftChanged As EventHandler
  Public Event SizeChanged As EventHandler
  Public Event StyleChanged As EventHandler
  Public Event SystemColorChanged As EventHandler
  Public Event TabIndexChanged As EventHandler
  Public Event TabStopChanged As EventHandler
  Public Event Validated As EventHandler
  Public Event Validating As EventHandler
  Public Event VisibleChanged As EventHandler

  Private m_backColor As Integer = 8 'Forms.DefaultBackColor
  Private m_foreColor As Integer = 0 'Forms.DefaultForeColor

  Public Property BackColor As Integer
    Get
      Return m_backColor
    End Get
    Set(value As Integer)
      m_backColor = value
      RaiseEvent BackColorChanged(Me, EventArgs.Empty)
    End Set
  End Property
  Public Property BackgroundImage As Object
  Public Property BackgroundImageLayout As Object
  Public Property BindingContext As Object
  Public Property CausesValidation As Object
  Public Property ClientSize As Object
  Public Property ContextMenuStrip As Object
  Public Property ControlCollection As New List(Of Control)
  Public Property Cursor As Object
  Public Property DataContext As Object
  Public Property Dock As Object
  Public Property Enabled As Boolean
  Public Property Font As Object = Forms.DefaultFont
  Public Property ForeColor As Integer
    Get
      Return m_foreColor
    End Get
    Set(value As Integer)
      m_foreColor = value
      RaiseEvent ForeColorChanged(Me, EventArgs.Empty)
    End Set
  End Property
  Public Property ImeMode As Object
  Public Property Location As New Location(1, 1)
  Public Property Parent As Control
  Public Property Region As Object
  Public Property RightToLeft As Object
  Public Property Size As New Size(1, 1)
  Public Property TabIndex As Integer
  Public Property TabStop As Boolean = True
  'Public Property Text As String
  Public Property Visible As Boolean = True

  Public Property Focused As Boolean

  Public Function MouseHit(mRow As Integer, mCol As Integer) As Boolean

    Dim top = If(Parent?.Location.Row, 1) + Location.Row - 1
    Dim left = If(Parent?.Location.Col, 1) + Location.Col - 1

    If mRow >= top AndAlso mRow <= top + Size.Rows - 1 Then
      If mCol >= left AndAlso mCol <= left + Size.Cols - 1 Then
        Return True
      End If
    End If

    Return False

  End Function

  Public Property CursorVisible As Boolean
  Protected m_cursorRow As Integer
  Public ReadOnly Property CursorRow As Integer
    Get
      Return If(m_cursorRow = 0, 1, m_cursorRow)
    End Get
  End Property
  Protected m_cursorCol As Integer
  Public ReadOnly Property CursorCol As Integer
    Get
      Return If(m_cursorCol = 0, 1, m_cursorCol)
    End Get
  End Property

  'Cursor Size?

  Public Overridable Sub OnMouse(e As MouseEventArgs)

    Dim top = If(Parent?.Location.Row, 1) + Location.Row - 1
    Dim left = If(Parent?.Location.Col, 1) + Location.Col - 1

    If e.Row >= top AndAlso e.Row <= top + Size.Rows - 1 Then
      If e.Column >= left AndAlso e.Column <= left + Size.Cols - 1 Then
        If Not Focused Then
          RaiseEvent GotFocus(Me, EventArgs.Empty)
        End If
        e.Handled = True
      End If
    End If

  End Sub

  Public MustOverride Sub OnKeyPress(e As KeyPressEventArgs)

  Public MustOverride Sub OnDraw()

End Class
