Namespace Global.Basic.Input

  Public NotInheritable Class KeyEventArgs

    Friend Sub New()

    End Sub

    'Public Sub New(ByVal key As Key)

    '  Me.Key = key
    '  Me.KeyScanCode = ToKeyScanCode(key)
    '  Me.PlatformKeyCode = ToPlatformKeyCode(key)

    'End Sub

    Public Sub New(scanCode As Input.ScanCode, asScanCode As Boolean)
      If asScanCode Then
      End If
      'Me.OppositeShiftState = scancode < 0
      PlatformKeyCode = 0
      Key = ToKey(scanCode, True)
      KeyScanCode = scanCode
    End Sub

    Public Sub New(platformKeyCode As Integer)
      OppositeShiftState = platformKeyCode < 0
      Me.PlatformKeyCode = Math.Abs(platformKeyCode)
      Key = ToKey(Math.Abs(platformKeyCode))
      KeyScanCode = ToKeyScanCode(Math.Abs(platformKeyCode))
    End Sub

    'Public Sub New(ByVal keyScanCode As Integer)
    '  Me.KeyScanCode = keyScanCode
    '  Me.Key = ToKey(keyScanCode)
    '  Me.PlatformKeyCode = ToPlatformKeyCode(keyScanCode)
    'End Sub

    Public Sub New(key As Key, platformKeyCode As Integer)
      Me.Key = key
      Me.PlatformKeyCode = platformKeyCode
      KeyScanCode = ToKeyScanCode(platformKeyCode)
    End Sub

    Public Property Key As Key = Key.Unknown
    Public Property PlatformKeyCode As Integer = 0
    Public Property KeyScanCode As Integer = 0
    Public Property OppositeShiftState As Boolean = False

    Public Shared Function ToKeyScanCode(key As Key) As Integer

      Dim result As Integer = 0

      Select Case key

        Case Input.Key.D1 : result = &H2
        Case Input.Key.D2 : result = &H3
        Case Input.Key.D3 : result = &H4
        Case Input.Key.D4 : result = &H5
        Case Input.Key.D5 : result = &H6
        Case Input.Key.D6 : result = &H7
        Case Input.Key.D7 : result = &H8
        Case Input.Key.D8 : result = &H9
        Case Input.Key.D9 : result = &HA
        Case Input.Key.D0 : result = &HB

        Case Input.Key.A : result = &H1E
        Case Input.Key.B : result = &H30
        Case Input.Key.C : result = &H2E
        Case Input.Key.D : result = &H20
        Case Input.Key.E : result = &H12
        Case Input.Key.F : result = &H21
        Case Input.Key.G : result = &H22
        Case Input.Key.H : result = &H23
        Case Input.Key.I : result = &H17
        Case Input.Key.J : result = &H24
        Case Input.Key.K : result = &H25
        Case Input.Key.L : result = &H26
        Case Input.Key.M : result = &H32
        Case Input.Key.N : result = &H31
        Case Input.Key.O : result = &H18
        Case Input.Key.P : result = &H19
        Case Input.Key.Q : result = &H10
        Case Input.Key.R : result = &H13
        Case Input.Key.S : result = &H1F
        Case Input.Key.T : result = &H14
        Case Input.Key.U : result = &H16
        Case Input.Key.V : result = &H2F
        Case Input.Key.W : result = &H11
        Case Input.Key.X : result = &H2D
        Case Input.Key.Y : result = &H15
        Case Input.Key.Z : result = &H2C

        Case Input.Key.F1 : result = &H3B
        Case Input.Key.F2 : result = &H3C
        Case Input.Key.F3 : result = &H3D
        Case Input.Key.F4 : result = &H3E
        Case Input.Key.F5 : result = &H3F
        Case Input.Key.F6 : result = &H40
        Case Input.Key.F7 : result = &H41
        Case Input.Key.F8 : result = &H42
        Case Input.Key.F9 : result = &H43
        Case Input.Key.F10 : result = &H44
          'Case Input.Key.F11
          'Case Input.Key.F12

        Case Input.Key.Insert : result = &H52
        Case Input.Key.Delete : result = &H53

        Case Input.Key.Home : result = &H47
        Case Input.Key.End : result = &H4F

        Case Input.Key.PageUp : result = &H49
        Case Input.Key.PageDown : result = &H51

        Case Input.Key.Add : result = &HD
        Case Input.Key.Subtract : result = &HC
        Case Input.Key.Multiply : result = &H37
        Case Input.Key.Divide

        Case Input.Key.Decimal : result = &H53

        Case Input.Key.Up : result = &H48
        Case Input.Key.Down : result = &H50
        Case Input.Key.Left : result = &H4B
        Case Input.Key.Right : result = &H4D

        Case Input.Key.NumPad0 : result = &H52
        Case Input.Key.NumPad1 : result = &H4F
        Case Input.Key.NumPad2 : result = &H50
        Case Input.Key.NumPad3 : result = &H51
        Case Input.Key.NumPad4 : result = &H4B
        Case Input.Key.NumPad5 : result = &H4C
        Case Input.Key.NumPad6 : result = &H4D
        Case Input.Key.NumPad7 : result = &H47
        Case Input.Key.NumPad8 : result = &H48
        Case Input.Key.NumPad9 : result = &H49

        Case Input.Key.Escape : result = &H1
        Case Input.Key.Enter : result = &H1C
        Case Input.Key.Space : result = &H39
        Case Input.Key.Tab : result = &HF
        Case Input.Key.Back : result = &HE
        Case Input.Key.CapsLock : result = &H3A
        Case Input.Key.NumLock : result = &H45
        Case Input.Key.ScrLk : result = &H46

        Case Input.Key.Ctrl : result = &H1D
        Case Input.Key.Alt : result = &H38
        Case Input.Key.Shift : result = &H2A

        Case Input.Key.None : result = 0
        Case Input.Key.Unknown

          result = 0

        Case Else

      End Select

      Return result

    End Function

    Public Shared Function ToKeyScanCode(platformKeyCode As Integer) As Integer

      ' Takes PlatformKeyCode as a parameter and converts it to a KeyScanCode (GW-BASIC).

      Dim keyScanCode As Integer = 0

      Select Case Math.Abs(platformKeyCode)

        Case 3 ' CTRL+BREAK?

        Case 27 : keyScanCode = ScanCode.Escape ' &H1 ' Esc
        Case 112 : keyScanCode = ScanCode.F1 ' &H3B ' F1
        Case 113 : keyScanCode = &H3C ' F2
        Case 114 : keyScanCode = &H3D ' F3
        Case 115 : keyScanCode = &H3E ' F4
        Case 116 : keyScanCode = &H3F ' F5
        Case 117 : keyScanCode = &H40 ' F6
        Case 118 : keyScanCode = &H41 ' F7
        Case 119 : keyScanCode = &H42 ' F8
        Case 120 : keyScanCode = &H43 ' F9
        Case 121 : keyScanCode = &H44 ' F10
        Case 122 ' F11
        Case 123 ' F12

        Case 192 : keyScanCode = &H29 ' `
        Case 49 : keyScanCode = &H2 ' 1
        Case 50 : keyScanCode = &H3 ' 2
        Case 51 : keyScanCode = &H4 ' 3
        Case 52 : keyScanCode = &H5 ' 4
        Case 53 : keyScanCode = &H6 ' 5
        Case 54 : keyScanCode = &H7 ' 6
        Case 55 : keyScanCode = &H8 ' 7
        Case 56 : keyScanCode = &H9 ' 8
        Case 57 : keyScanCode = &HA ' 9
        Case 48 : keyScanCode = &HB ' 0
        Case 189 : keyScanCode = &HC ' -
        Case 187 : keyScanCode = &HD ' =
        Case 8 : keyScanCode = &HE ' Backspace

        Case 9 : keyScanCode = &HF ' Tab
        Case 81 : keyScanCode = &H10 ' q
        Case 87 : keyScanCode = &H11 ' w
        Case 69 : keyScanCode = &H12 ' e
        Case 82 : keyScanCode = &H13 ' r
        Case 84 : keyScanCode = &H14 ' t
        Case 89 : keyScanCode = &H15 ' y
        Case 85 : keyScanCode = &H16 ' u
        Case 73 : keyScanCode = &H17 ' i
        Case 79 : keyScanCode = &H18 ' o
        Case 80 : keyScanCode = &H19 ' p
        Case 219 : keyScanCode = &H1A ' [
        Case 221 : keyScanCode = &H1B ' ]
        Case 220 : keyScanCode = &H2B ' \

        Case 20 : keyScanCode = &H3A ' Capslock
        Case 65 : keyScanCode = &H1E ' a
        Case 83 : keyScanCode = &H1F ' s
        Case 68 : keyScanCode = &H20 ' d
        Case 70 : keyScanCode = &H21 ' f
        Case 71 : keyScanCode = &H22 ' g
        Case 72 : keyScanCode = &H23 ' h
        Case 74 : keyScanCode = &H24 ' j
        Case 75 : keyScanCode = &H25 ' k
        Case 76 : keyScanCode = &H26 ' l
        Case 186 : keyScanCode = &H27 ' ;
        Case 222 : keyScanCode = &H28 ' '
        Case 13 : keyScanCode = &H1C ' Enter

        Case 16 : keyScanCode = &H2A ' SHIFT (Technically, left shift)
        Case 90 : keyScanCode = &H2C ' z
        Case 88 : keyScanCode = &H2D ' x
        Case 67 : keyScanCode = &H2E ' c
        Case 86 : keyScanCode = &H2F ' v
        Case 66 : keyScanCode = &H30 ' b
        Case 78 : keyScanCode = &H31 ' n
        Case 77 : keyScanCode = &H32 ' m
        Case 188 : keyScanCode = &H33 ' ,
        Case 190 : keyScanCode = &H34 ' .
        Case 191 : keyScanCode = &H35 ' /

        Case 17 : keyScanCode = &H1D ' CTRL
        Case 91 ' Windows
        Case -1 : keyScanCode = &H38 ' ALT
        Case 32 : keyScanCode = &H39 ' Space
        Case 93 ' Menu

        Case 44 : keyScanCode = &H37 ' SysRq
        Case 145 : keyScanCode = &H46 ' ScrLk
        Case 19 ' Break

        Case 45 ' Insert
        Case 36 ' Home
        Case 33 ' Page Up

        Case 46 ' Delete
        Case 35 ' End
        Case 34 ' Page Down

        Case 37 ' Left
        Case 38 ' Up
        Case 39 ' Right
        Case 40 ' Down

        Case 187 ' =
        Case -1 ' (
        Case -1 ' )

          ' Number pad...

        Case 144 : keyScanCode = &H45 ' Numlock
        Case 111 ' / 
        Case 106 : keyScanCode = &H37 ' *  (????)
        Case 109 : keyScanCode = &H4A ' -
        Case 103 : keyScanCode = &H47 ' 7 (NumLock On)
        Case 104 : keyScanCode = &H48 ' 8 (NumLock On)
        Case 105 : keyScanCode = &H49 ' 9 (NumLock On)
        Case 100 : keyScanCode = &H4B ' 4 (NumLock On)
        Case 101 : keyScanCode = &H4C ' 5 (NumLock On)
        Case 102 : keyScanCode = &H4D ' 6 (NumLock On)
        Case 97 : keyScanCode = &H4F ' 1 (NumLock On)
        Case 98 : keyScanCode = &H50 ' 2 (NumLock On)
        Case 99 : keyScanCode = &H51 ' 3 (NumLock On)
        Case 96 : keyScanCode = &H52 ' 0 (NumLock On)
        Case 110 : keyScanCode = &H53 ' . (NumLock On)
        Case 107 : keyScanCode = &H4E ' +
        Case 12 : keyScanCode = &H4C ' 5 key in Numlock Off mode.

        Case Else
          keyScanCode = 0
      End Select

      Return keyScanCode

    End Function

    Public Shared Function ToKey(scancode As ScanCode, asScanCode As Boolean) As Input.Key

      If asScanCode Then
      End If

      Dim result As Input.Key '= Input.Key.None

      Select Case CInt(scancode)

        Case Input.ScanCode.Escape : result = Input.Key.Escape
        Case Input.ScanCode.F1 : result = Input.Key.F1
        Case Input.ScanCode.F2 : result = Input.Key.F2
        Case Input.ScanCode.F3 : result = Input.Key.F3
        Case Input.ScanCode.F4 : result = Input.Key.F4
        Case Input.ScanCode.F5 : result = Input.Key.F5
        Case Input.ScanCode.F6 : result = Input.Key.F6
        Case Input.ScanCode.F7 : result = Input.Key.F7
        Case Input.ScanCode.F8 : result = Input.Key.F8
        Case Input.ScanCode.F9 : result = Input.Key.F9
        Case Input.ScanCode.F10 : result = Input.Key.F10
        Case Input.ScanCode.F11 : result = Input.Key.F11
        Case Input.ScanCode.F12 : result = Input.Key.F12

        Case Input.ScanCode.Tilde : result = Input.Key.Tilde ' `
        Case Input.ScanCode.D1 : result = Input.Key.D1 ' 1
        Case Input.ScanCode.D2 : result = Input.Key.D2 ' 2
        Case Input.ScanCode.D3 : result = Input.Key.D3 ' 3
        Case Input.ScanCode.D4 : result = Input.Key.D4 ' 4
        Case Input.ScanCode.D5 : result = Input.Key.D5 ' 5
        Case Input.ScanCode.D6 : result = Input.Key.D6 ' 6
        Case Input.ScanCode.D7 : result = Input.Key.D7 ' 7
        Case Input.ScanCode.D8 : result = Input.Key.D8 ' 8
        Case Input.ScanCode.D9 : result = Input.Key.D9 ' 9
        Case Input.ScanCode.D0 : result = Input.Key.D0 ' 0
        Case Input.ScanCode.Minus : result = Input.Key.Minus ' -
        Case Input.ScanCode.Plus : result = Input.Key.Plus ' =
        Case Input.ScanCode.Backspace : result = Input.Key.Back ' Backspace

        Case Input.ScanCode.Tab : result = Input.Key.Tab ' Tab
        Case Input.ScanCode.Q : result = Input.Key.Q ' q
        Case Input.ScanCode.W : result = Input.Key.W ' w
        Case Input.ScanCode.E : result = Input.Key.E ' e
        Case Input.ScanCode.R : result = Input.Key.R ' r
        Case Input.ScanCode.T : result = Input.Key.T ' t
        Case Input.ScanCode.Y : result = Input.Key.Y ' y
        Case Input.ScanCode.U : result = Input.Key.U ' u
        Case Input.ScanCode.I : result = Input.Key.I ' i
        Case Input.ScanCode.O : result = Input.Key.O ' o
        Case Input.ScanCode.P : result = Input.Key.P ' p
        Case Input.ScanCode.BracketLeft : result = Input.Key.BracketLeft ' [
        Case Input.ScanCode.BracketRight : result = Input.Key.BracketRight ' ]
        Case Input.ScanCode.Backslash : result = Input.Key.Backslash ' \

        Case -1 : result = Input.Key.CapsLock ' Capslock
        Case Input.ScanCode.A : result = Input.Key.A ' a
        Case Input.ScanCode.S : result = Input.Key.S ' s
        Case Input.ScanCode.D : result = Input.Key.D ' d
        Case Input.ScanCode.F : result = Input.Key.F ' f
        Case Input.ScanCode.G : result = Input.Key.G ' g
        Case Input.ScanCode.H : result = Input.Key.H ' h
        Case Input.ScanCode.J : result = Input.Key.J ' j
        Case Input.ScanCode.K : result = Input.Key.K ' k
        Case Input.ScanCode.L : result = Input.Key.L ' l
        Case Input.ScanCode.SemiColon : result = Input.Key.SemiColon ' ;
        Case Input.ScanCode.Apostrophe : result = Input.Key.Apostrophe ' '
        Case Input.ScanCode.Enter : result = Input.Key.Enter ' Enter

        Case -1 : result = Input.Key.Shift ' SHIFT (Technically, left shift)
        Case Input.ScanCode.Z : result = Input.Key.Z ' z
        Case Input.ScanCode.X : result = Input.Key.X ' x
        Case Input.ScanCode.C : result = Input.Key.C ' c
        Case Input.ScanCode.V : result = Input.Key.V ' v
        Case Input.ScanCode.B : result = Input.Key.B ' b
        Case Input.ScanCode.N : result = Input.Key.N ' n
        Case Input.ScanCode.M : result = Input.Key.M ' m
        Case Input.ScanCode.Comma : result = Input.Key.Comma ' ,
        Case Input.ScanCode.Period : result = Input.Key.Period ' .
        Case Input.ScanCode.Slash : result = Input.Key.Slash ' /

        Case -1 : result = Input.Key.Ctrl ' CTRL
        Case Input.ScanCode.Windows : result = Input.Key.Windows ' Windows
        Case -1 : result = Input.Key.Alt ' ALT
        Case Input.ScanCode.Space : result = Input.Key.Space ' Space
        Case Input.ScanCode.Menu : result = Input.Key.Menu ' Menu

        Case Input.ScanCode.SysRq : result = Input.Key.SysRq ' SysRq
        Case Input.ScanCode.ScrLk : result = Input.Key.ScrLk ' ScrLk
        Case Input.ScanCode.Break : result = Input.Key.Break ' Break

        Case Input.ScanCode.Insert : result = Input.Key.Insert ' Insert
        Case Input.ScanCode.Home : result = Input.Key.Home ' Home
        Case Input.ScanCode.PageUp : result = Input.Key.PageUp ' Page Up

        Case Input.ScanCode.Delete : result = Input.Key.Delete ' Delete
        Case Input.ScanCode.End : result = Input.Key.End ' End
        Case Input.ScanCode.PageDown : result = Input.Key.PageDown ' Page Down

        Case Input.ScanCode.LeftArrow : result = Input.Key.Left ' Left
        Case Input.ScanCode.UpArrow : result = Input.Key.Up ' Up
        Case Input.ScanCode.RightArrow : result = Input.Key.Right ' Right
        Case Input.ScanCode.DownArrow : result = Input.Key.Down ' Down

        Case -1 : result = Input.Key.Unknown ' =
          'Case -1 ' (
          'Case -1 ' )

          ' Number pad...

        Case -1 : result = Input.Key.NumLock ' Numlock
        Case Input.ScanCode.Divide : result = Input.Key.Divide ' / 
        Case Input.ScanCode.Multiply : result = Input.Key.Multiply ' *  (????)
        Case Input.ScanCode.Subtract : result = Input.Key.Subtract ' -
        Case Input.ScanCode.NumPad7 : result = Input.Key.D7 ' 7 (NumLock On)
        Case Input.ScanCode.NumPad8 : result = Input.Key.D8 ' 8 (NumLock On)
        Case Input.ScanCode.NumPad9 : result = Input.Key.D9 ' 9 (NumLock On)
        Case Input.ScanCode.NumPad4 : result = Input.Key.D4 ' 4 (NumLock On)
        Case Input.ScanCode.NumPad5 : result = Input.Key.D5 ' 5 (NumLock On)
        Case Input.ScanCode.NumPad6 : result = Input.Key.D6 ' 6 (NumLock On)
        Case Input.ScanCode.NumPad1 : result = Input.Key.D1 ' 1 (NumLock On)
        Case Input.ScanCode.NumPad2 : result = Input.Key.D2 ' 2 (NumLock On)
        Case Input.ScanCode.NumPad3 : result = Input.Key.D3 ' 3 (NumLock On)
        Case Input.ScanCode.NumPad0 : result = Input.Key.D0 ' 0 (NumLock On)
        Case Input.ScanCode.Decimal : result = Input.Key.Decimal ' . (NumLock On)
        Case Input.ScanCode.Add : result = Input.Key.Add ' +
        Case Input.ScanCode.NumPad5 : result = Input.Key.D5 ' 5 key in Numlock Off mode.

        Case Else
          result = Input.Key.Unknown
      End Select

      Return result

    End Function

    Public Shared Function ToKey(platformKeyCode As Integer) As Input.Key

      Dim result As Input.Key '= Input.Key.None

      Select Case platformKeyCode

        Case 27 : result = Input.Key.Escape
        Case 112 : result = Input.Key.F1
        Case 113 : result = Input.Key.F2
        Case 114 : result = Input.Key.F3
        Case 115 : result = Input.Key.F4
        Case 116 : result = Input.Key.F5
        Case 117 : result = Input.Key.F6
        Case 118 : result = Input.Key.F7
        Case 119 : result = Input.Key.F8
        Case 120 : result = Input.Key.F9
        Case 121 : result = Input.Key.F10
        Case 122 : result = Input.Key.F11
        Case 123 : result = Input.Key.F12

        Case 192 : result = Input.Key.Unknown ' `
        Case 49 : result = Input.Key.D1 ' 1
        Case 50 : result = Input.Key.D2 ' 2
        Case 51 : result = Input.Key.D3 ' 3
        Case 52 : result = Input.Key.D4 ' 4
        Case 53 : result = Input.Key.D5 ' 5
        Case 54 : result = Input.Key.D6 ' 6
        Case 55 : result = Input.Key.D7 ' 7
        Case 56 : result = Input.Key.D8 ' 8
        Case 57 : result = Input.Key.D9 ' 9
        Case 48 : result = Input.Key.D0 ' 0
        Case 189 : result = Input.Key.Unknown ' -
        Case 187 : result = Input.Key.Unknown ' =
        Case 8 : result = Input.Key.Back ' Backspace

        Case 9 : result = Input.Key.Tab ' Tab
        Case 81 : result = Input.Key.Q ' q
        Case 87 : result = Input.Key.W ' w
        Case 69 : result = Input.Key.E ' e
        Case 82 : result = Input.Key.R ' r
        Case 84 : result = Input.Key.T ' t
        Case 89 : result = Input.Key.Y ' y
        Case 85 : result = Input.Key.U ' u
        Case 73 : result = Input.Key.I ' i
        Case 79 : result = Input.Key.O ' o
        Case 80 : result = Input.Key.P ' p
        Case 219 : result = Input.Key.Unknown ' [
        Case 221 : result = Input.Key.Unknown ' ]
        Case 220 : result = Input.Key.Unknown ' \

        Case 20 : result = Input.Key.CapsLock ' Capslock
        Case 65 : result = Input.Key.A ' a
        Case 83 : result = Input.Key.S ' s
        Case 68 : result = Input.Key.D ' d
        Case 70 : result = Input.Key.F ' f
        Case 71 : result = Input.Key.G ' g
        Case 72 : result = Input.Key.H ' h
        Case 74 : result = Input.Key.J ' j
        Case 75 : result = Input.Key.K ' k
        Case 76 : result = Input.Key.L ' l
        Case 186 : result = Input.Key.Unknown ' ;
        Case 222 : result = Input.Key.Unknown ' '
        Case 13 : result = Input.Key.Enter ' Enter

        Case 16 : result = Input.Key.Shift ' SHIFT (Technically, left shift)
        Case 90 : result = Input.Key.Z ' z
        Case 88 : result = Input.Key.X ' x
        Case 67 : result = Input.Key.C ' c
        Case 86 : result = Input.Key.V ' v
        Case 66 : result = Input.Key.B ' b
        Case 78 : result = Input.Key.N ' n
        Case 77 : result = Input.Key.M ' m
        Case 188 : result = Input.Key.Unknown ' ,
        Case 190 : result = Input.Key.Unknown ' .
        Case 191 : result = Input.Key.Unknown ' /

        Case 17 : result = Input.Key.Ctrl ' CTRL
        Case 91 : result = Input.Key.Unknown ' Windows
        Case -1 : result = Input.Key.Alt ' ALT
        Case 32 : result = Input.Key.Space ' Space
        Case 93 : result = Input.Key.Unknown ' Menu

        Case 44 : result = Input.Key.Unknown ' SysRq
        Case 145 : result = Input.Key.ScrLk ' ScrLk
        Case 19 : result = Input.Key.Unknown ' Break

        Case 45 : result = Input.Key.Insert ' Insert
        Case 36 : result = Input.Key.Home ' Home
        Case 33 : result = Input.Key.PageUp ' Page Up

        Case 46 : result = Input.Key.Delete ' Delete
        Case 35 : result = Input.Key.End ' End
        Case 34 : result = Input.Key.PageDown ' Page Down

        Case 37 : result = Input.Key.Left ' Left
        Case 38 : result = Input.Key.Up ' Up
        Case 39 : result = Input.Key.Right ' Right
        Case 40 : result = Input.Key.Down ' Down

        Case 187 : result = Input.Key.Unknown ' =
          'Case -1 ' (
          'Case -1 ' )

          ' Number pad...

        Case 144 : result = Input.Key.NumLock ' Numlock
        Case 111 : result = Input.Key.Unknown ' / 
        Case 106 : result = Input.Key.Multiply ' *  (????)
        Case 109 : result = Input.Key.Subtract ' -
        Case 103 : result = Input.Key.D7 ' 7 (NumLock On)
        Case 104 : result = Input.Key.D8 ' 8 (NumLock On)
        Case 105 : result = Input.Key.D9 ' 9 (NumLock On)
        Case 100 : result = Input.Key.D4 ' 4 (NumLock On)
        Case 101 : result = Input.Key.D5 ' 5 (NumLock On)
        Case 102 : result = Input.Key.D6 ' 6 (NumLock On)
        Case 97 : result = Input.Key.D1 ' 1 (NumLock On)
        Case 98 : result = Input.Key.D2 ' 2 (NumLock On)
        Case 99 : result = Input.Key.D3 ' 3 (NumLock On)
        Case 96 : result = Input.Key.D0 ' 0 (NumLock On)
        Case 110 : result = Input.Key.Decimal ' . (NumLock On)
        Case 107 : result = Input.Key.Add ' +
        Case 12 : result = Input.Key.D5 ' 5 key in Numlock Off mode.

        Case Else
          result = Input.Key.Unknown
      End Select

      Return result

    End Function

  End Class

End Namespace