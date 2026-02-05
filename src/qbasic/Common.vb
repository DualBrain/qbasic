Imports System.Threading

Imports QBLib.Video

Friend Module Forms

  Public Property DefaultBackColor As Drawing.Color = Drawing.Color.Gray ' Integer = 8
  Public Property DefaultFont As Object
  Public Property DefaultForeColor As Drawing.Color = Drawing.Color.Black 'Integer = 0

End Module

Friend Module Common

  Friend m_running As Boolean
  Friend m_abort As Boolean

  Friend g_display As Display

  Friend Property Insert As Boolean

  Friend s_cancelTokenSource As New CancellationTokenSource()
  Friend s_cancelToken As CancellationToken

  Friend Function DetermineCopyright() As String
    Dim r As New Random()
    Dim n = r.Next(1, 20)
    Select Case n
      Case 1, 5, 9 : Return "The Kid In Us All"
      Case 2 : Return "Inconceivable!"
      Case 4 : Return "Shall We Play A Game?"
      Case 6 : Return "Late Night Coders"
      Case 3, 8 : Return "NOT Microsoft Corp."
      Case 10 : Return "Cliffs of Insanity"
      Case 12 : Return "All Out Of Bubblegum"
      Case 14 : Return "We Don't Need Roads"
      Case 16 : Return "Wax On, Wax Off"
      Case 18 : Return "Yippee Ki-yay"
      Case Else : Return "Dartmouth Didn't"
    End Select
  End Function

  Friend Sub ToggleInsertMode()
    Insert = Not Insert
    CsrSize()
  End Sub

  Friend Sub CsrSize()
    Dim BScan = 12
    ' The below has been modified to match behavior in EDIT.COM (MS-DOS).
    If Insert Then                          'If in insert mode,
      'LOCATE , , , BScan \ 2, BScan            '  make large size cursor
      LOCATE(, , , BScan - 2, BScan)            '  use 2 scan line cursor.
    Else                                        'Overtype mode,
      'LOCATE , , , BScan - 1, BScan            '  use 2 scan line cursor.
      LOCATE(, , , 0, BScan)                     '  make large size cursor
    End If
  End Sub

  Friend Function GetChar(key As ConsoleKey, capsLock As Boolean, shift As Boolean) As String
    Dim result = "?"c
    Select Case key

      Case ConsoleKey.Oem1 ' ;: (186)
        If shift Then result = ":"c Else result = ";"c
      Case ConsoleKey.OemPlus ' =+ (187)
        If shift Then result = "+"c Else result = "="c
      Case ConsoleKey.OemComma ' ,< (188)
        If shift Then result = "<"c Else result = ","c
      Case ConsoleKey.OemMinus ' -_ (189)
        If shift Then result = "_"c Else result = "-"c
      Case ConsoleKey.OemPeriod ' .> (190)
        If shift Then result = ">"c Else result = "."c
      Case ConsoleKey.Oem2 ' /?
        If shift Then result = "?"c Else result = "/"c
      Case ConsoleKey.Oem3 ' `~
        If shift Then result = "~"c Else result = "`"c
      Case ConsoleKey.Oem4 ' [{ (219)
        If shift Then result = "{"c Else result = "["c
      Case ConsoleKey.Oem5 ' \| (220)
        If shift Then result = "|"c Else result = "\"c
      Case ConsoleKey.Oem6 ' ]} (221)
        If shift Then result = "}"c Else result = "]"c
      Case ConsoleKey.Oem7 ' '" (222)
        If shift Then result = """"c Else result = "'"c

      Case ConsoleKey.Spacebar
        result = " "c

      Case ConsoleKey.A To ConsoleKey.Z

        result = ChrW(key)
        If capsLock Then
          If Not shift Then result = CChar($"{result}".ToLower)
        Else
          If shift Then result = CChar($"{result}".ToLower)
        End If

      Case ConsoleKey.Add : Return "+"c
      Case ConsoleKey.Multiply : Return "*"c
      Case ConsoleKey.Divide : Return "/"c
      Case ConsoleKey.Subtract : Return "-"c

      Case ConsoleKey.D1 : If shift Then Return "!"c Else Return "1"c
      Case ConsoleKey.D2 : If shift Then Return "@"c Else Return "2"c
      Case ConsoleKey.D3 : If shift Then Return "#"c Else Return "3"c
      Case ConsoleKey.D4 : If shift Then Return "$"c Else Return "4"c
      Case ConsoleKey.D5 : If shift Then Return "%"c Else Return "5"c
      Case ConsoleKey.D6 : If shift Then Return "^"c Else Return "6"c
      Case ConsoleKey.D7 : If shift Then Return "&"c Else Return "7"c
      Case ConsoleKey.D8 : If shift Then Return "*"c Else Return "8"c
      Case ConsoleKey.D9 : If shift Then Return "("c Else Return "9"c
      Case ConsoleKey.D0 : If shift Then Return ")"c Else Return "0"c

      Case ConsoleKey.Escape : Return ChrW(27)
      Case ConsoleKey.Enter : Return ChrW(13)
      Case ConsoleKey.Backspace : Return ChrW(8)
      Case ConsoleKey.Tab : Return ChrW(9)

      Case ConsoleKey.Delete : Return ChrW(0) & ChrW(83)
      Case ConsoleKey.Home : Return ChrW(0) & ChrW(71)
      Case ConsoleKey.UpArrow : Return ChrW(0) & ChrW(72)
      Case ConsoleKey.PageUp : Return ChrW(0) & ChrW(73)
      Case ConsoleKey.End : Return ChrW(0) & ChrW(79)
      Case ConsoleKey.DownArrow : Return ChrW(0) & ChrW(80)
      Case ConsoleKey.PageDown : Return ChrW(0) & ChrW(81)

      Case ConsoleKey.Insert : Return ChrW(0) & ChrW(82)
      Case ConsoleKey.LeftArrow : Return ChrW(0) & ChrW(75)
      Case ConsoleKey.RightArrow : Return ChrW(0) & ChrW(77)

      Case ConsoleKey.F1 : Return ChrW(0) & ChrW(59) 'If(shift OrElse ctrl OrElse alt, ChrW(84 + adder), ChrW(59))
      Case ConsoleKey.F2 : Return ChrW(0) & ChrW(60) 'If(shift OrElse ctrl OrElse alt, ChrW(85 + adder), ChrW(60))
      Case ConsoleKey.F3 : Return ChrW(0) & ChrW(61) 'If(shift OrElse ctrl OrElse alt, ChrW(86 + adder), ChrW(61))
      Case ConsoleKey.F4 : Return ChrW(0) & ChrW(62) 'If(shift OrElse ctrl OrElse alt, ChrW(87 + adder), ChrW(62))
      Case ConsoleKey.F5 : Return ChrW(0) & ChrW(63) 'If(shift OrElse ctrl OrElse alt, ChrW(88 + adder), ChrW(63))
      Case ConsoleKey.F6 : Return ChrW(0) & ChrW(64) 'If(shift OrElse ctrl OrElse alt, ChrW(89 + adder), ChrW(64))
      Case ConsoleKey.F7 : Return ChrW(0) & ChrW(65) 'If(shift OrElse ctrl OrElse alt, ChrW(80 + adder), ChrW(65))
      Case ConsoleKey.F8 : Return ChrW(0) & ChrW(66) 'If(shift OrElse ctrl OrElse alt, ChrW(91 + adder), ChrW(66))
      Case ConsoleKey.F9 : Return ChrW(0) & ChrW(67) 'If(shift OrElse ctrl OrElse alt, ChrW(92 + adder), ChrW(67))
      Case ConsoleKey.F10 : Return ChrW(0) & ChrW(68) 'If(shift OrElse ctrl OrElse alt, ChrW(93 + adder), ChrW(68))

      Case ConsoleKey.F11 : Return ChrW(0) & ChrW(133)
      Case ConsoleKey.F12 : Return ChrW(0) & ChrW(134)

        'Case ConsoleKey.F11
        '  If shift Then
        '    Return ChrW(0) & ChrW(135)
        '  ElseIf ctrl Then
        '    Return ChrW(0) & ChrW(137)
        '  ElseIf alt Then
        '    Return ChrW(0) & ChrW(139)
        '  Else
        '    Return ChrW(0) & ChrW(133)
        '  End If
        'Case ConsoleKey.F12
        '  If shift Then
        '    Return ChrW(0) & ChrW(136)
        '  ElseIf ctrl Then
        '    Return ChrW(0) & ChrW(138)
        '  ElseIf alt Then
        '    Return ChrW(0) & ChrW(140)
        '  Else
        '    Return ChrW(0) & ChrW(134)
        '  End If

      Case Else
        Stop
    End Select
    Return result
  End Function

#Region "Helper Methods"

#Region "QuickPak Professional"

  Friend Sub APrint(array$(), first As Integer, numEls As Integer, firstChar As Integer, numChars As Integer, colr As Integer, page As Integer)
    'BUG: The VERTMENU.BAS file makes a call where colr is -1;
    '     however, it appears to be not using the current "basic" fg/bg but
    '     the value from "somewhere else". Testing combined with COLOR, PRINT
    '     MQPrint, QPrintRC, etc. and -1 for colr does not seem to be affected
    '     by any of these. COLOR, for example, seems to be completely ignored 
    '     which doesn't seem to match what the documentation states.
    ' NOTE: page is currently not implemented.
    ' If page is -1, use current page.
    If page <> 0 Then
    End If
    'If page > -1 Then SCREEN(, , page, page)
    Dim row = CSRLIN()
    Dim col = POS(0)
    For element = first To first + numEls - 1
      If element > UBound(array$) Then Continue For
      Dim value$ = Mid(array$(element), firstChar, numChars)
      QPrintRC(value$ + Space(numChars - Len(value$)), row, col, colr)
      row += 1
    Next
  End Sub

  Friend Sub APrint0(array$(), first As Integer, numEls As Integer, firstChar As Integer, numChars As Integer, colr As Integer)
    APrint(array$, first, numEls, firstChar, numChars, colr, 0)
  End Sub

  Friend Function ArraySize(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer) As Integer
    Return (lrRow - ulRow + 1) * (lrCol - ulCol + 1)
  End Function

  Friend Function ASCII(ch As String) As Integer
    If ch?.Length > 0 Then ASCII = Asc(ch) Else ASCII = -1
  End Function

  Friend Function Blanks(value As String) As Integer
    If String.IsNullOrEmpty(value) Then
      Return 0
    Else
      Dim result = value.Length
      For index = 1 To value.Length
        Select Case Asc(Mid(value, index, 1))
          Case 0, 32
          Case Else
            result = index - 1 : Exit For
        End Select
      Next
      Return result
    End If
  End Function

  Friend Sub Box0(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, ch As Integer, colr As Integer)

    Dim ulc = ChrW(218), urc = ChrW(191)
    Dim hc = ChrW(196)
    Dim vc = ChrW(179)
    Dim llc = ChrW(192), lrc = ChrW(217)

    If ch = 2 Then
      ulc = ChrW(201) : urc = ChrW(187)
      hc = ChrW(205)
      vc = ChrW(186)
      llc = ChrW(200) : lrc = ChrW(188)
    End If

    If ch = 3 Then
      ulc = ChrW(213) : urc = ChrW(184)
      hc = ChrW(205)
      vc = ChrW(179)
      llc = ChrW(212) : lrc = ChrW(190)
    End If

    If ch = 4 Then
      ulc = ChrW(214) : urc = ChrW(183)
      hc = ChrW(196)
      vc = ChrW(186)
      llc = ChrW(211) : lrc = ChrW(189)
    End If

    If ch > 4 OrElse ch < 1 Then
      ulc = ChrW(ch) : urc = ChrW(ch)
      hc = ChrW(ch)
      vc = ChrW(ch)
      llc = ChrW(ch) : lrc = ChrW(ch)
    End If

    Dim w = lrCol - ulCol
    Dim h = lrRow - ulRow

    QPrintRC(ulc, ulRow, ulCol, colr)
    QPrintRC(urc, ulRow, lrCol, colr)
    For c = 1 To w - 1
      QPrintRC(hc, ulRow, ulCol + c, colr)
      QPrintRC(hc, lrRow, ulCol + c, colr)
    Next
    QPrintRC(llc, lrRow, ulCol, colr)
    QPrintRC(lrc, lrRow, lrCol, colr)
    For r = 1 To h - 1
      QPrintRC(vc, ulRow + r, ulCol, colr)
      QPrintRC(vc, ulRow + r, lrCol, colr)
    Next

  End Sub

  Friend Sub ButtonPress(ByRef button As Integer, ByRef status As Integer, ByRef count As Integer, ByRef x As Integer, ByRef y As Integer)
    If button <> 0 OrElse status <> 0 OrElse count <> 0 OrElse x <> 0 OrElse y <> 0 Then
    End If
  End Sub

  Friend Sub ClearScr(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, colr As Integer, page As Integer)
    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If
    For r = ulRow To lrRow
      For c = ulCol To lrCol
        QPrintRC(" "c, r, c, colr)
      Next
    Next
  End Sub

  Friend Sub ClearScr0(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, colr As Integer)
    For r = ulRow To lrRow
      For c = ulCol To lrCol
        QPrintRC(" "c, r, c, colr)
      Next
    Next
  End Sub

  Friend Sub DeleteStr(array$(), element As Integer, numEls As Integer)
    Dim entries = element + numEls
    If entries >= UBound(array$) Then entries = UBound(array$) - 1
    For entry = element To entries
      QBLib.Core.SWAP(array$(entry), array$(entry + 1))
    Next
  End Sub

  Friend Function DOSVer() As Integer
    Return 611
  End Function

  Friend Sub FillScrn0(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, ch As Integer, colr As Integer)
    For r = ulRow To lrRow
      For c = ulCol To lrCol
        QPrintRC(ChrW(ch), r, c, colr)
      Next
    Next
  End Sub

  Friend Function FindLast(array() As String, last As Integer, size As Integer) As Integer
    For index = last To last - size + 1 Step -1
      If array(index) <> "" Then
        Return index
      End If
    Next
    Return -1
  End Function

  Friend Sub GetCursor(ByRef x As Integer, ByRef y As Integer, ByRef button As Integer)
    If x <> 0 OrElse y <> 0 OrElse button <> 0 Then
    End If
  End Sub

  Friend Sub GetVMode(ByRef mode As Integer, ByRef page As Integer, ByRef pageSize As Integer, ByRef rows As Integer, ByRef columns As Integer)
    mode = 3
    page = 0
    pageSize = 4096
    rows = 25
    columns = 80
  End Sub

  Friend Sub HideCursor()
    m_cursorVisible = False
  End Sub

  Friend Sub InsertStr(array$(), startElement As Integer, insertVAlue As String, numEls As Integer)
    For entry = startElement + numEls - 1 To startElement + 1 Step -1
      QBLib.Core.SWAP(array(entry), array(entry - 1))
    Next
    QBLib.Core.SWAP(array(startElement), insertVAlue)
  End Sub

  Friend Function MaxInt(value1 As Integer, value2 As Integer) As Integer
    If value1 > value2 Then Return value1 Else Return value2
  End Function

  Friend Function MinInt(value1 As Integer, value2 As Integer) As Integer
    If value1 < value2 Then Return value1 Else Return value2
  End Function

  Friend Function Monitor() As Integer
    Return 7
  End Function

  Friend Sub MPaintBox(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, colr As Integer)
    PaintBox0(ulRow, ulCol, lrRow, lrCol, colr)
  End Sub

  Friend Sub MPRestore(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, originalWidth As Integer, elements() As UShort, start As Integer)

    Dim w = lrCol - ulCol + 1
    Dim h = lrRow - ulRow + 1

    Dim index = start '+ LBOUND(element)  ' not really sure why the +1 is needed???
    ' but I've verified that index of 1 is the correct starting
    ' element for a saved array.
    Dim r = 0
    Do
      For c = 0 To w - 1
        ' clr = element(index + c) AND &H00FF ' split color
        ' ch = ((element(index + c) AND &HFF00) \ 256) AND &H00FF ' split character
        Dim clr = elements(index + c) And &HFF ' split color
        Dim ch = ((elements(index + c) And &HFF00) \ 256) And &HFF ' split character
        QPrintRC(ChrW(ch), ulRow + r, ulCol + c, clr)
      Next
      r += 1 : index += originalWidth
    Loop Until r >= h Or index >= UBound(elements)

  End Sub

  Friend Sub MQPrint(txt As String, colr As Integer)
    Dim ofg = -1, obg = -1
    Dim fg, bg As Integer
    If colr = -1 Then
      ' Maintain existing color...
    Else
      ofg = QBLib.Video.m_fgColor : obg = QBLib.Video.m_bgColor
      SplitColor(colr, fg, bg)
      COLOR(fg, bg)
    End If
    Dim pr = CSRLIN()
    Dim pc = POS(0)
    PRINT(txt$, True)
    LOCATE(pr, pc)
    If ofg <> -1 OrElse obg <> -1 Then
      COLOR(ofg, obg)
    End If
  End Sub

  Friend Sub MScrnRest(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, elements() As UShort)
    ScrnRest0(ulRow, ulCol, lrRow, lrCol, elements)
  End Sub

  Friend Sub MScrnSave(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, elements() As UShort)
    ScrnSave0(ulRow, ulCol, lrRow, lrCol, elements)
  End Sub

  Friend Function Null(value As String) As Integer
    Dim result = -1
    If value <> "" Then
      For index = 1 To Len(value)
        Select Case Asc(Mid(value, index, 1))
          Case 0 ', 32
          Case Else
            result = 0 : Exit For
        End Select
      Next
    End If
    Return result
  End Function

  Friend Function OneColor(fg As Integer, bg As Integer) As Integer
    '  Dim f = fg
    '  Dim b = bg
    '  Return (f And 16) * 8 + ((b And 7) * 16) + (f And 15)
    Return CByte((fg << 4) Or bg)
  End Function

  Friend Sub PaintBox(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, colr As Integer, page As Integer)
    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If
    For r = ulRow To lrRow
      For c = ulCol To lrCol
        Dim ch = SCREEN(r, c)
        QPrintRC(ChrW(ch), r, c, colr)
      Next
    Next
  End Sub

  Friend Sub PaintBox0(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, colr As Integer)
    Call PaintBox(ulRow, ulCol, lrRow, lrCol, colr, 0)
  End Sub

  Friend Function QInstrB(startPosition As Integer, sourceValue As String, searchValue As String) As Integer

    ' Setting position to 0 will return 0; matching QB behavior.
    If startPosition = 0 Then Return 0
    Dim position = startPosition
    Do
      Dim segment = Mid(sourceValue, position, Len(searchValue))
      If Len(segment) <> Len(searchValue) Then Exit Do
      Dim isMatch = True
      For index = 1 To Len(searchValue)
        Dim character = Mid(searchValue, index, 1)
        If character <> "?" Then
          If Mid(segment, index, 1) <> character Then isMatch = False : Exit For
        End If
      Next
      If isMatch Then Return position
      position -= 1
      If position < 1 Then Exit Do
    Loop

    Return 0

  End Function

  Friend Function QPLen(value As String) As Integer
    If String.IsNullOrEmpty(value) Then
      Return 0
    Else
      Return Len(value)
    End If
  End Function

  Friend Sub QPrintRC(text As String, row As Integer, col As Integer, colr As Integer)
    Dim rr = QBLib.Video.CursorRow, rc = QBLib.Video.CursorCol
    Dim rfg = m_fgColor, rbg = m_bgColor
    QBLib.Video.CursorRow = row : QBLib.Video.CursorCol = col
    Dim fg, bg As Integer
    SplitColor(colr, fg, bg)
    If fg <> m_fgColor OrElse bg <> m_bgColor Then
      m_fgColor = fg : m_bgColor = bg
    End If
    PRINT(text, True, True)
    If m_fgColor <> rfg OrElse m_bgColor <> rbg Then
      m_fgColor = rfg : m_bgColor = rbg
    End If
    If QBLib.Video.CursorRow <> rr OrElse QBLib.Video.CursorCol <> rc Then
      QBLib.Video.CursorRow = rr : QBLib.Video.CursorCol = rc
    End If
  End Sub

  Friend Sub ScrnRest(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, elements() As UShort, page As Integer)

    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If

    Dim rows = (lrRow - ulRow) + 1
    Dim cols = (lrCol - ulCol) + 1
    For r = 0 To rows - 1
      For c = 0 To cols - 1
        Dim index = LBound(elements) + ((r * cols) + c)
        ' clr = element(index) AND &H00FF ' split color
        ' ch = ((element(index) AND &HFF00) \ 256) AND &H00FF ' split character
        Dim ch = elements(index) And &HFF ' split color
        Dim clr = ((elements(index) And &HFF00) \ 256) And &HFF ' split character
        QPrintRC(ChrW(ch), ulRow + r, ulCol + c, clr)
      Next
    Next

  End Sub

  Friend Sub ScrnRest0(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, elements() As UShort)
    ScrnRest(ulRow, ulCol, lrRow, lrCol, elements, 0)
  End Sub

  Friend Sub ScrnSave(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, elements() As UShort, page As Integer)

    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If

    Dim rows = (lrRow - ulRow) + 1
    Dim cols = (lrCol - ulCol) + 1
    For r = 0 To rows - 1
      For c = 0 To cols - 1
        Dim index = LBound(elements) + ((r * cols) + c)
        Dim clr = SCREEN(ulRow + r, ulCol + c, 0)
        Dim ch = SCREEN(ulRow + r, ulCol + c, 1)
        'elements(index) = (ch * 256) + clr ' combine
        'elements(index) = CShort((clr * 256) + ch)
        elements(index) = CUShort((clr * 256) + ch)
      Next
    Next

  End Sub

  Friend Sub ScrnSave0(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, elements() As UShort)
    ScrnSave(ulRow, ulCol, lrRow, lrCol, elements, 0)
  End Sub

  Friend Sub ScrollD(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, lines As Integer, page As Integer)

    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If

    Dim tr = (lrRow - ulRow) + 1
    If tr < lines Then
      ' If the number of lines to scroll is greater than the area to scroll, just clear.
      ClearScr(ulRow, ulCol, lrRow, lrCol, -1, page)
    Else
      Dim array(ArraySize(ulRow + lines, ulCol, lrRow, lrCol)) As UShort
      ScrnSave(ulRow, ulCol, lrRow - lines, lrCol, array, page)
      ScrnRest(ulRow + lines, ulCol, lrRow, lrCol, array, page)
      ClearScr(ulRow, ulCol, ulRow + (lines - 1), lrCol, -1, page)
    End If

  End Sub

  Friend Sub ScrollL(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, columns As Integer, page As Integer)

    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If

    Dim tc = (lrCol - ulCol) + 1
    If tc < columns Then
      ' If the number of columns to scroll is greater than the area to scroll, just clear.
      ClearScr(ulRow, ulCol, lrRow, lrCol, -1, page)
    Else
      Dim array(ArraySize(ulRow, ulCol + columns, lrRow, lrCol)) As UShort
      ScrnSave(ulRow, ulCol + columns, lrRow, lrCol, array, page)
      ScrnRest(ulRow, ulCol, lrRow, lrCol - columns, array, page)
      ClearScr(ulRow, lrCol - (columns - 1), lrRow, lrCol, -1, page)
    End If

  End Sub

  Friend Sub ScrollR(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, columns As Integer, page As Integer)

    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If

    Dim tc = (lrCol - ulCol) + 1
    If tc < columns Then
      ' If the number of columns to scroll is greater than the area to scroll, just clear.
      ClearScr(ulRow, ulCol, lrRow, lrCol, -1, page)
    Else
      Dim array(ArraySize(ulRow, ulCol, lrRow, lrCol - columns)) As UShort
      ScrnSave(ulRow, ulCol, lrRow, lrCol - columns, array, page)
      ScrnRest(ulRow, ulCol + columns, lrRow, lrCol, array, page)
      ClearScr(ulRow, ulCol, lrRow, ulCol + (columns - 1), -1, page)
    End If

  End Sub

  Friend Sub ScrollU(ulRow As Integer, ulCol As Integer, lrRow As Integer, lrCol As Integer, lines As Integer, page As Integer)

    ' NOTE: page is currently not implemented.
    If page = -1 Then
      ' page of -1 is "current page"
    End If

    Dim tr = (lrRow - ulRow) + 1
    If tr < lines Then
      ' If the number of lines to scroll is greater than the area to scroll, just clear.
      ClearScr(ulRow, ulCol, lrRow, lrCol, -1, page)
    Else
      Dim array(ArraySize(ulRow, ulCol, lrRow - lines, lrCol)) As UShort
      ScrnSave(ulRow + lines, ulCol, lrRow, lrCol, array, page)
      ScrnRest(ulRow, ulCol, lrRow - lines, lrCol, array, page)
      ClearScr(lrRow - (lines - 1), ulCol, lrRow, lrCol, -1, page)
    End If

  End Sub

  Friend Sub ShowCursor()
    m_cursorVisible = True
  End Sub

  Friend Sub SplitColor(colr As Integer, ByRef fg As Integer, ByRef bg As Integer)
    '  fg = (colr And 128) \ 8 + (colr And 15)
    '  bg = (colr And 112) \ 16
    fg = colr >> 4
    bg = colr And &HF
  End Sub

  Friend Function StringSize(array() As String, start As Integer, numStringEls As Integer) As Long
    Dim c = numStringEls * 2 ' At least 2 bytes per array element (CRLF)...
    For entry = start To start + numStringEls - 1
      c += Len(array(entry)) ' add the number of bytes that make of length for each string element
    Next
    Return c
  End Function

#End Region

#Region "Other"

  Friend Sub Button(text As String, row As Integer, col As Integer, Optional selected As Boolean = False)
    QPrintRC(text, row, col, OneColor(0, 8))
    If selected Then
      QPrintRC("<", row, col, OneColor(15, 8))
      QPrintRC(">", row, col + text.Length - 1, OneColor(15, 8))
    End If
  End Sub

  Friend Sub HLine(row As Integer, ulCol As Integer, lrCol As Integer, ch As Integer, colr As Integer)

    Dim lc = ChrW(195), rc = ChrW(180)
    Dim hc = ChrW(196)

    If ch = 2 Then
      lc = ChrW(204) : rc = ChrW(185)
      hc = ChrW(205)
    End If

    If ch = 3 Then
      lc = ChrW(198) : rc = ChrW(181)
      hc = ChrW(205)
    End If

    If ch = 4 Then
      lc = ChrW(199) : rc = ChrW(182)
      hc = ChrW(196)
    End If

    If ch > 4 OrElse ch < 1 Then
      lc = ChrW(ch) : rc = ChrW(ch)
      hc = ChrW(ch)
    End If

    Dim w = lrCol - ulCol

    QPrintRC(lc, row, ulCol, colr)
    QPrintRC(rc, row, lrCol, colr)
    For c = 1 To w - 1
      QPrintRC(hc, row, ulCol + c, colr)
    Next

  End Sub

  Friend Sub VScrollBar(pct As Double, ulRow As Integer, col As Integer, lrRow As Integer, arrowFg As Integer)
    QPrintRC(ChrW(24), ulRow, col, OneColor(arrowFg, 8))
    For r = ulRow + 1 To lrRow - 1
      QPrintRC(ChrW(176), r, col, OneColor(0, 8))
    Next
    QPrintRC(ChrW(25), lrRow, col, OneColor(arrowFg, 8))
    ' position...
    Dim range = (lrRow - 1) - (ulRow + 1)
    Dim current = CInt(Fix(pct * range)) + 1
    QPrintRC(ChrW(32), ulRow + current, col, OneColor(0, 0))
  End Sub

  Friend Sub HScrollBar(pct As Double, row As Integer, ulCol As Integer, lrCol As Integer, arrowFg As Integer)
    QPrintRC(ChrW(27), row, ulCol, OneColor(arrowFg, 8))
    For c = ulCol + 1 To lrCol - 1
      QPrintRC(ChrW(176), row, c, OneColor(0, 8))
    Next
    QPrintRC(ChrW(26), row, lrCol, OneColor(arrowFg, 8))
    ' position...
    Dim range = (lrCol - 1) - (ulCol + 1)
    Dim current = CInt(Fix(pct * range)) + 1
    QPrintRC(ChrW(32), row, ulCol + current, OneColor(0, 0))
  End Sub

#End Region

#End Region

End Module