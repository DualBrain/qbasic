Imports System.Diagnostics.Tracing
Imports VbPixelGameEngine
Imports VbPixelGameEngine.PixelGameEngine

Public Class MainMenu
  Inherits PgeX
  Implements IContext

  Public Property Items As List(Of MenuItem)

  Public RightAlignLast As Boolean = True

  Public Property ShowAccelerators As Boolean = False
  Public Property Focused As Boolean = False

  Private m_selected As Integer = 0

  Public Event OnClick As EventHandler(Of MenuClickEventArgs)

  Public Property AltPressed As Boolean

  Public ReadOnly Property CursorRow As Integer Implements IContext.CursorRow
  Public ReadOnly Property CursorCol As Integer Implements IContext.CursorCol

  Public Property Selected As Integer
    Get
      Return m_selected
    End Get
    Set(value As Integer)
      If value > Items.Count - 1 Then
        m_selected = 0
      ElseIf value < 0 Then
        m_selected = Items.Count - 1
      Else
        m_selected = value
      End If
    End Set
  End Property

  Private m_subSelected As Integer = 0

  Public Sub Reset()
    AltPressed = False
    ShowAccelerators = False
    Focused = False
    Selected = 0
    SubSelected = 0
    Expanded = False
  End Sub

  Public Sub Render() Implements IContext.Render

    ' Draw the background...
    QPrintRC(Space(80), 1, 1, OneColor(0, 8))

    If Items Is Nothing Then Exit Sub

    ' Draw the base main menu items....
    For Each entry In Items
      QPrintRC(entry.Text, 1, entry.Offset + 1, OneColor(0, 8))
    Next
    ' Show accel letters...
    If ShowAccelerators AndAlso Not Expanded Then
      For Each entry In Items
        If entry.AccelPosition > -1 Then
          QPrintRC(entry.Text(entry.AccelPosition), 1, entry.Offset + entry.AccelPosition + 1, OneColor(15, 8))
        End If
      Next
    End If
    ' Highlight the currently selected item...
    If Focused Then
      Dim selected = Items(Me.Selected)
      QPrintRC($" {selected.Text} ", 1, selected.Offset, OneColor(8, 0))
      If ShowAccelerators AndAlso Not Expanded Then
        QPrintRC($"{selected.Text(selected.AccelPosition)}", 1, selected.Offset + selected.AccelPosition + 1, OneColor(15, 0))
      End If
    End If

    If Expanded Then
      Dim subMenu = Items(Selected)
      Dim adjust = 0
      Dim width = 15
      For Each entry In subMenu.Items
        Dim tw = entry.Text.Length
        Dim hk = If(entry.Hotkey?.Length + 6, 0)
        If tw + 1 + hk > width Then
          width = tw + 1 + hk
        End If
      Next
      If RightAlignLast AndAlso Me.Selected = Items.Count - 1 Then
        adjust = -(width - 3)
      End If

      Dim ulRow = 2
      Dim ulCol = subMenu.Offset - 1 + adjust
      Dim lrRow = subMenu.Items.Count + 3
      Dim lrCol = subMenu.Offset + width + 1 + adjust

      PaintBox0(ulRow + 1, ulCol + 2, lrRow + 1, lrCol + 2, OneColor(7, 0))
      ClearScr0(ulRow, ulCol, lrRow, lrCol, OneColor(0, 8))
      Box0(ulRow, ulCol, lrRow, lrCol, 1, OneColor(0, 8))

      ' Draw menu items
      For index = 0 To subMenu.Items.Count - 1
        Dim entry = subMenu.Items(index)
        If entry.Text = "-" Then
          HLine(3 + index, subMenu.Offset - 1 + adjust, subMenu.Offset + width + 1 + adjust, 1, OneColor(0, 8))
        Else
          Dim fg = 0 : Dim bg = 8
          If SubSelected = index Then
            fg = 8 : bg = 0
            QPrintRC(Space(width + 1), 3 + index, subMenu.Offset + adjust, OneColor(fg, bg))
          ElseIf Not entry.Enabled Then
            fg = 7
          End If
          If entry.Checked Then
            QPrintRC(ChrW(249), 3 + index, subMenu.Offset + adjust, OneColor(fg, bg))
          End If
          QPrintRC(entry.Text, 3 + index, subMenu.Offset + 1 + adjust, OneColor(fg, bg))
          If entry.AccelPosition > -1 AndAlso entry.Enabled Then
            QPrintRC(entry.Text(entry.AccelPosition), 3 + index, subMenu.Offset + 1 + entry.Offset + entry.AccelPosition + 1 + adjust, OneColor(15, bg))
          End If
          If entry.Hotkey IsNot Nothing Then
            QPrintRC(entry.Hotkey, 3 + index, subMenu.Offset + width - entry.Hotkey.Length + adjust, OneColor(fg, bg))
          End If
        End If
      Next

    End If

  End Sub

  Public Function ProcessKeys(keys As List(Of ConsoleKey), capsLock As Boolean, ctrl As Boolean, alt As Boolean, shift As Boolean) As Boolean Implements IContext.ProcessKeys

    If Focused Then
      If keys IsNot Nothing Then
        For Each key In keys
          Select Case key
            Case ConsoleKey.Escape
              Reset()
              Return False
            Case ConsoleKey.Enter
              If Not Expanded Then
                Expanded = True
              Else
                If Items(Selected).Items(SubSelected).Enabled Then
                  Dim s = Selected
                  Dim ss = SubSelected
                  Reset()
                  RaiseEvent OnClick(Me, New MenuClickEventArgs(Items(s).Items(ss)))
                  Return False
                Else
                  ' beep
                End If
              End If
            Case ConsoleKey.UpArrow
              If Not Expanded Then
                Expanded = True
              Else
                SubSelected -= 1
                If SubSelected > 0 Then
                  If Items(Selected).Items(SubSelected).Text = "-" Then
                    SubSelected -= 1
                  End If
                End If
                If SubSelected < 0 Then
                  SubSelected = Items(Selected).Items.Count - 1
                End If
              End If
            Case ConsoleKey.DownArrow
              If Not Expanded Then
                Expanded = True
              Else
                SubSelected += 1
                If SubSelected < Items(Selected).Items.Count Then
                  If Items(Selected).Items(SubSelected).Text = "-" Then
                    SubSelected += 1
                  End If
                End If
                If SubSelected > Items(Selected).Items.Count - 1 Then
                  SubSelected = 0
                End If
              End If
            Case ConsoleKey.RightArrow
              Selected += 1 : SubSelected = 0
            Case ConsoleKey.LeftArrow
              Selected -= 1 : SubSelected = 0
            Case Else
              If Not Expanded Then
                For index = 0 To Items.Count - 1
                  If Items(index).Enabled AndAlso Items(index).AcceleratorKey > ChrW(0) Then
                    Dim k = AscW(Items(index).AcceleratorKey)
                    If k = key Then
                      Selected = index : Expanded = True : Exit For
                    End If
                  End If
                Next
              Else
                For index = 0 To Items(Selected).Items.Count - 1
                  If Items(Selected).Items(index).Enabled AndAlso Items(Selected).Items(index).AcceleratorKey > ChrW(0) Then
                    Dim k = AscW(Items(Selected).Items(index).AcceleratorKey)
                    If k = key Then
                      Dim s = Selected
                      Reset()
                      'DrawScreen()
                      RaiseEvent OnClick(Me, New MenuClickEventArgs(Items(s).Items(index)))
                      Return False
                    End If
                  End If
                Next
              End If
          End Select
        Next
      End If

    Else

      If AltPressed AndAlso Not ShowAccelerators Then
        ShowAccelerators = True
      ElseIf AltPressed AndAlso keys IsNot Nothing Then
        For index = 0 To Items.Count - 1
          Dim found = False
          For Each key In keys
            If key = ConsoleKey.Escape Then
              Reset() : Return False
            End If
            If Items(index).Enabled AndAlso Items(index).AcceleratorKey > ChrW(0) Then
              Dim k = AscW(Items(index).AcceleratorKey)
              If k = key Then
                found = True
                Selected = index : Expanded = True
                Focused = True
                Exit For
              End If
            End If
          Next
          If found Then Exit For
        Next
      ElseIf Not AltPressed AndAlso ShowAccelerators Then
        Focused = True
      End If

    End If

    Return True

  End Function

  Public Property SubSelected As Integer
    Get
      Return m_subSelected
    End Get
    Set(value As Integer)
      If value > Items(m_selected).Items.Count - 1 Then
        m_subSelected = 0
      ElseIf value < 0 Then
        m_subSelected = Items(m_selected).Items.Count - 1
      Else
        m_subSelected = value
      End If
    End Set
  End Property

  Public Property Expanded As Boolean = False

  Public Sub CalculateOffsets()

    Dim cOffset = 3
    For index = 0 To Items.Count - If(RightAlignLast, 2, 1)
      Items(index).Offset = cOffset
      cOffset += Items(index).Text.Length + 2
    Next
    If RightAlignLast Then
      Items.Last.Offset = (80 - Items.Last.Text.Length) - 2
    End If

  End Sub

End Class

Public Class MenuItem

  Private ReadOnly m_text As String

  Public Sub New(text As String, Optional help As String = Nothing)
    If text.Contains("&"c) Then
      AccelPosition = text.IndexOf("&"c)
      m_text = text.Replace("&"c, "")
      AcceleratorKey = m_text(AccelPosition)
      If AcceleratorKey >= "a"c AndAlso AcceleratorKey <= "z"c Then
        Dim b = AscW(AcceleratorKey)
        b -= 32
        AcceleratorKey = ChrW(b)
      End If
    Else
      m_text = text
    End If
    Me.Help = help
  End Sub

  Public ReadOnly Property Text As String
    Get
      Return m_text
    End Get
  End Property

  Public Property AcceleratorKey As Char
  Public Property Hotkey As String

  Public Property Offset As Integer = -1
  Public Property Expanded As Boolean = False
  Public Property AccelPosition As Integer = -1
  Public Property Enabled As Boolean = True
  Public Property Checked As Boolean = False

  Public Property Help As String

  Public Property Items As List(Of MenuItem)

End Class

Public Class MenuClickEventArgs
  Inherits EventArgs

  Public ReadOnly Property Item As MenuItem

  Public Sub New(item As MenuItem)
    Me.Item = item
  End Sub

End Class