Imports System.Drawing
Imports Basic

Public Class Display
  Inherits Basic.Display.Display

  'Private m_bitmap As WriteableBitmap
  Private m_invalidated As Boolean = True

  Private ReadOnly m_debug As Boolean = False

  'Private m_cursorColor As Integer = ConvertToARGB32(Colors.Gray)

  Public Sub New() 'timer As System.Timers.Timer)
    MyBase.New() 'Timer)
  End Sub

  'Public ReadOnly Property Bitmap As WriteableBitmap
  '  Get
  '    Return m_bitmap
  '  End Get
  'End Property

  Public ReadOnly Property IsUpdated As Boolean
    Get
      Dim invalidated = m_invalidated
      m_invalidated = False
      Return invalidated
    End Get
  End Property

  Public Overrides Sub Paint()

    Static cursorCounter As Integer

    Dim invalidate = MyBase.IsInvalidated OrElse m_debug

    If cursorCounter > 7 Then
      cursorCounter = 0
      invalidate = True
    Else
      cursorCounter += 1
    End If

    'If m_bitmap Is Nothing Then
    '  m_bitmap = New WriteableBitmap(MyBase.ScreenWidth, MyBase.ScreenHeight)
    '  invalidate = True
    'Else
    '  If m_bitmap.PixelWidth <> MyBase.ScreenWidth OrElse
    '     m_bitmap.PixelHeight <> MyBase.ScreenHeight Then
    '    m_bitmap = New WriteableBitmap(MyBase.ScreenWidth, MyBase.ScreenHeight)
    '    invalidate = True
    '  End If
    'End If

    ' Copy all of the pixels to the bitmap from the active page.

    'If invalidate Then
    '  MyBase.Pixels(MyBase.VisualPage).CopyTo(m_bitmap.Pixels, 0)
    'End If

    If invalidate Then
      If MyBase.Location < MyBase.ColumnCount * MyBase.RowCount Then

        If MyBase.CursorToggle AndAlso MyBase.CursorVisible Then

          Dim r As Integer = MyBase.Location \ MyBase.ColumnCount
          Dim c As Integer = MyBase.Location Mod MyBase.ColumnCount

          Dim x As Integer = c * MyBase.CharacterWidth
          Dim y As Integer = r * MyBase.CharacterHeight

          MyBase.CursorThick = MyBase.Insert

          For h = 0 To If(MyBase.CursorThick, 4, 0)
            For w As Integer = 0 To MyBase.CharacterWidth - 1
              'm_bitmap.Pixels(((y + MyBase.CharacterHeight - 1 - h) * MyBase.ScreenWidth) + (x + w)) = CInt(m_cursorColor)
            Next
          Next

          'cursor.Add(New Point(x, y))

          MyBase.CursorLocation = MyBase.Location

        End If

        MyBase.CursorToggle = Not MyBase.CursorToggle

      End If

    End If

    If invalidate AndAlso m_debug Then

      For Each s In MyBase.Segments

        Dim r As Short = s.LocationBegin \ MyBase.ColumnCount
        Dim c As Short = s.LocationBegin Mod MyBase.ColumnCount

        Dim x = c * MyBase.CharacterWidth
        Dim y = r * MyBase.CharacterHeight

        'Dim cc = ConvertToARGB32(Colors.Green)

        If s.LocationBegin < MyBase.ColumnCount * MyBase.RowCount Then
          If Not (MyBase.CursorVisible AndAlso MyBase.CursorLocation = s.LocationBegin) Then
            For h = 0 To 1
              'For w As Integer = 0 To MyBase.CharacterWidth - 1
              '  m_bitmap.Pixels(((y + MyBase.CharacterHeight - 1 - h) * MyBase.ScreenWidth) + (x + w)) = cc
              'Next
            Next
          End If
        End If

        r = s.LocationEnd \ MyBase.ColumnCount
        c = s.LocationEnd Mod MyBase.ColumnCount

        x = c * MyBase.CharacterWidth
        y = r * MyBase.CharacterHeight

        'cc = ConvertToARGB32(Colors.Red)

        If s.LocationEnd < MyBase.ColumnCount * MyBase.RowCount Then
          If Not (MyBase.CursorVisible AndAlso MyBase.CursorLocation = s.LocationEnd) Then
            For h = 0 To 1
              For w As Integer = 0 To MyBase.CharacterWidth - 1
                'm_bitmap.Pixels(((y + MyBase.CharacterHeight - 1 - h) * MyBase.ScreenWidth) + (x + w)) = cc
              Next
            Next
          End If
        End If

      Next

    End If

    m_invalidated = invalidate

  End Sub

  'Private Shared Function ConvertToARGB32(ByVal color As Color) As Integer
  '  Return (CInt(color.A) << 24) Or (CInt(color.R) << 16) Or (CInt(color.G) << 8) Or (CInt(color.B))
  'End Function

End Class