Imports QB.Video
Imports VbPixelGameEngine

Public Class HelpPanel
  Inherits PgeX
  Implements IContext

  Public Property Title As String
  Public Property Content As String

  Public Property Visible As Boolean = False

  Public Property EditorTop As Integer = 2
  Public Property EditorLeft As Integer = 1
  Public Property EditorWidth As Integer = 80
  Public Property EditorHeight As Integer = 20

  Public Property ScrollBars As Boolean = False
  Public Property Focused As Boolean = False

  Public Property TextRows As Integer
  Public Property TextColumns As Integer
  Public Property TopScreenRow As Integer = 3
  Public Property TopTextLine As Integer = 1
  Public Property CurrentColumn As Integer = 1
  Public Property CurrentLine As Integer = 1
  Public Property LeftScreenColumn As Integer = 2
  Public Property LeftTextColumn As Integer = 1

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
    Get
      Return TopScreenRow + CurrentLine - TopTextLine
    End Get
  End Property

  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol
    Get
      Return LeftScreenColumn + (CurrentColumn - LeftTextColumn)
    End Get
  End Property

  Public ReadOnly Property DocumentRow As Integer
    Get
      Return CurrentLine
    End Get
  End Property

  Public ReadOnly Property DocumentCol As Integer
    Get
      Return CurrentColumn
    End Get
  End Property

  Sub New()
    TextRows = EditorHeight - 2
    TextColumns = EditorWidth - 2
  End Sub

  Public Sub Render() Implements IContext.Render

    If Visible Then

      TextRows = EditorHeight - 2
      TextColumns = EditorWidth - 2

      Dim lrRow = EditorTop + EditorHeight - 1
      Dim lrCol = EditorLeft + EditorWidth - 1

      ' Box
      Box0(EditorTop, EditorLeft, EditorTop + EditorHeight - 1, lrCol, 1, OneColor(8, 0))

      If EditorTop > 2 Then
        HLine(EditorTop, EditorLeft, lrCol, 1, OneColor(8, 0))
      End If

      ' Content Area
      ClearScr0(EditorTop + 1, EditorLeft + 1, lrRow - 1, lrCol - 1, OneColor(8, 0))

      Dim windowColor = OneColor(8, 0)

      LOCATE(TopScreenRow, LeftScreenColumn, 0)
      Call HideCursor()
      Dim document = Split(Me.Content, vbLf)
      Call APrint0(document,
                   TopTextLine - 1,
                   TextRows,
                   LeftTextColumn,
                   TextColumns,
                   windowColor)
      LOCATE(TopScreenRow + CurrentLine - TopTextLine, LeftScreenColumn + (CurrentColumn - LeftTextColumn), 1)
      Call ShowCursor()

      ' Expand/Collapse Tool
      QPrintRC(ChrW(180), EditorTop, lrCol - 4, OneColor(8, 0))
      If EditorHeight = 24 Then
        QPrintRC(ChrW(18), EditorTop, lrCol - 3, OneColor(0, 8))
      Else
        QPrintRC(ChrW(24), EditorTop, lrCol - 3, OneColor(0, 8))
      End If
      QPrintRC(ChrW(195), EditorTop, lrCol - 2, OneColor(8, 0))

      ' Scrollbars
      If ScrollBars Then
        'TODO: Determine current position within the scrollbars...
        If EditorHeight > 5 Then VScrollBar(0, EditorTop + 1, lrCol, lrRow - 1, 1)
        'If EditorHeight > 3 Then HScrollBar(lrRow - 1, EditorLeft + 1, lrCol - 1, 1)
      End If

      ' Title
      Dim title = $" HELP: {Me.Title} "
      Dim titleOffset = (EditorWidth - title.Length) \ 2
      QPrintRC(title, EditorTop, EditorLeft + titleOffset, OneColor(0, 8))

    End If

  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If keys?.Count > 0 Then
      For Each key In keys
        Select Case key
          Case ConsoleKey.Escape
            ' Close help...
            Return False
          Case ConsoleKey.Enter
            ' If on a "link", navigate...
            ' otherwise, beep.
          Case ConsoleKey.F1
            ' Seems to do "something", need to zero in on the specifics here.
          Case ConsoleKey.A To ConsoleKey.Z
            ' "tab" to next "link" with a matching first character to that was typed (from current cursor location).
          Case ConsoleKey.Tab
            ' "tab" to the next availalble "link" (from current cursor location)
          Case ConsoleKey.LeftArrow, ConsoleKey.RightArrow, ConsoleKey.UpArrow, ConsoleKey.DownArrow
            ' move the cursor anywhere on the document
          Case Else
        End Select
      Next
    End If

    Return True

  End Function

End Class
