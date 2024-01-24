Imports VbPixelGameEngine

Public Class ImmediatePanel
  Inherits PgeX
  Implements IContext

  Public Property Visible As Boolean = True

  Public Property Row As Integer = 22
  Public Property Col As Integer = 1
  Public Property Width As Integer = 80
  Public Property Height As Integer = 4

  Public Property Focused As Boolean = False

  Public ReadOnly Property CursorRow As Integer
    Get
      Return 1
    End Get
  End Property

  Public ReadOnly Property CursorCol As Integer
    Get
      Return 1
    End Get
  End Property

  Public ReadOnly Property DocumentRow As Integer
    Get
      Return 1
    End Get
  End Property

  Public ReadOnly Property DocumentCol As Integer
    Get
      Return 1
    End Get
  End Property

  Public Sub Render() Implements IContext.Render

    If Visible Then

      Dim lrRow = Row + Height - 1
      Dim lrCol = Col + Width - 1

      ClearScr0(Row + 1, Col + 1, lrRow - 1, lrCol - 1, OneColor(8, 1))
      Box0(Row, Col, lrRow, lrCol, 1, OneColor(8, 1))
      HLine(Row, Col, lrCol, 1, OneColor(8, 1))

      Dim title = " Immediate "
      Dim titleOffset = (Width - title.Length) \ 2
      QPrintRC(title, Row, Col + titleOffset, OneColor(8, 1))

    End If

  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys
    Return True
  End Function

End Class
