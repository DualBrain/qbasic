Namespace Global.Basic.Input

  ' Notes:
  '   Standard buffer is 15 keys.

  Public Enum ShiftState
    None = 0 '&H0 ' None
    Left = 1 '&H1 ' Left Shift Key
    Right = 2 '&H2 ' Right Shift Key
    Both = 3 '&H3 ' Both Shift Keys
    Ctrl = 4 '&H4 ' Ctrl
    Alt = 8 '&H8 ' Alt
    NumLock = 32 '&H20 ' Num Lock
    CapsLock = 64 '&H40 ' Caps Lock
    Extended = 128 '&H80 ' Extended keys on the 101-key keyboard
  End Enum

  ' Covers the standard 83 keys of the keyboard on the original IBM PC.
  Public Enum ScanCode
    None = 0
    Escape = 1
    D1 = 2
    D2 = 3
    D3 = 4
    D4 = 5
    D5 = 6
    D6 = 7
    D7 = 8
    D8 = 9
    D9 = 10
    D0 = 11
    Minus = 12
    Plus = 13
    Left = 14
    Tab = 15
    Q = 16
    W = 17
    E = 18
    R = 19
    T = 20
    Y = 21
    U = 22
    I = 23
    O = 24
    P = 25
    BracketLeft = 26
    BracketRight = 27
    Enter = 28
    Control = 29
    A = 30
    S = 31
    D = 32
    F = 33
    G = 34
    H = 35
    J = 36
    K = 37
    L = 38
    SemiColon = 39
    Apostrophe = 40
    Tilde = 41
    ShiftLeft = 42
    Backslash = 43
    Z = 44
    X = 45
    C = 46
    V = 47
    B = 48
    N = 49
    M = 50
    Comma = 51
    Period = 52
    Slash = 53
    ShiftRight = 54
    SysRq = 55
    Alt = 56
    Space = 57
    CapsLock = 58
    F1 = 59
    F2 = 60
    F3 = 61
    F4 = 62
    F5 = 63
    F6 = 64
    F7 = 65
    F8 = 66
    F9 = 67
    F10 = 68
    NumLock = 69
    ScrLk = 70
    NumPad7 = 71
    NumPad8 = 72
    NumPad9 = 73
    Subtract = 74
    NumPad4 = 75
    NumPad5 = 76
    NumPad6 = 77
    Add = 78
    NumPad1 = 79
    NumPad2 = 80
    NumPad3 = 81
    NumPad0 = 82
    [Decimal] = 83

    Break
    Divide
    Multiply
    LeftArrow
    RightArrow
    UpArrow
    DownArrow
    Insert
    Delete
    Home
    PageUp
    PageDown
    [End]
    Backspace
    Menu
    Windows
    F11
    F12

  End Enum

  ' The combination of these keys results in the following scan codes
  ' when used with the INKEY$; the results will be CHR$(0) + CHR$(scancode).
  ' In other words, a two character string result.
  Public Enum ExtendedScanCodes
    AltA = 30
    AltB = 48
    AltC = 46
    AltD = 32
    AltE = 18
    AltF = 33
    AltG = 34
    AltH = 35
    AltI = 23
    AltJ = 36
    AltK = 37
    AltL = 38
    AltM = 50
    AltN = 49
    AltO = 24
    AltP = 25
    AltQ = 16
    AltR = 19
    AltS = 31
    AltT = 20
    AltU = 22
    AltV = 47
    AltW = 17
    AltX = 45
    AltY = 21
    AltZ = 44
    Alt1 = 120
    Alt2 = 121
    Alt3 = 122
    Alt4 = 123
    Alt5 = 124
    Alt6 = 125
    Alt7 = 126
    Alt8 = 127
    Alt9 = 128
    Alt0 = 129
    AltMinus = 130
    AltEqual = 131
    F1 = 59
    F2 = 60
    F3 = 61
    F4 = 62
    F5 = 63
    F6 = 64
    F7 = 65
    F8 = 66
    F9 = 67
    F10 = 68
    F11 = 133
    F12 = 134
    ShiftF1 = 84
    ShiftF2 = 85
    ShiftF3 = 86
    ShiftF4 = 87
    ShiftF5 = 88
    ShiftF6 = 89
    ShiftF7 = 90
    ShiftF8 = 91
    ShiftF9 = 92
    ShiftF10 = 93
    ShiftF11 = 135
    ShiftF12 = 136
    ControlF1 = 94
    ControlF2 = 95
    ControlF3 = 96
    ControlF4 = 97
    ControlF5 = 98
    ControlF6 = 99
    ControlF7 = 100
    ControlF8 = 101
    ControlF9 = 102
    ControlF10 = 103
    ControlF11 = 137
    ControlF12 = 138
    AltF1 = 104
    AltF2 = 105
    AltF3 = 106
    AltF4 = 107
    AltF5 = 108
    AltF6 = 109
    AltF7 = 110
    AltF8 = 111
    AltF9 = 112
    AltF10 = 113
    AltF11 = 139
    AltF12 = 140
    Home = 71
    Up = 72
    PageUp = 73
    Left = 75
    Right = 77
    [End] = 79
    Down = 80
    PageDown = 81
    Insert = 82
    Delete = 83
    ControlPrintScreen = 114
    ControlLeft = 115
    ControlRight = 116
    ControlEnd = 117
    ControlPageDown = 118
    ControlHome = 119
    ControlPageUp = 132
  End Enum

  Public Enum Key
    None = 0
    Back = 1
    Tab = 2
    Enter = 3
    Shift = 4
    Ctrl = 5
    Alt = 6
    CapsLock = 7
    Escape = 8
    Space = 9
    PageUp = 10
    PageDown = 11
    [End] = 12
    Home = 13
    Left = 14
    Up = 15
    Right = 16
    Down = 17
    Insert = 18
    Delete = 19
    D0 = 20
    D1 = 21
    D2 = 22
    D3 = 23
    D4 = 24
    D5 = 25
    D6 = 26
    D7 = 27
    D8 = 28
    D9 = 29
    A = 30
    B = 31
    C = 32
    D = 33
    E = 34
    F = 35
    G = 36
    H = 37
    I = 38
    J = 39
    K = 40
    L = 41
    M = 42
    N = 43
    O = 44
    P = 45
    Q = 46
    R = 47
    S = 48
    T = 49
    U = 50
    V = 51
    W = 52
    X = 53
    Y = 54
    Z = 55
    F1 = 56
    F2 = 57
    F3 = 58
    F4 = 59
    F5 = 60
    F6 = 61
    F7 = 62
    F8 = 63
    F9 = 64
    F10 = 65
    F11 = 66
    F12 = 67
    NumPad0 = 68
    NumPad1 = 69
    NumPad2 = 70
    NumPad3 = 71
    NumPad4 = 72
    NumPad5 = 73
    NumPad6 = 74
    NumPad7 = 75
    NumPad8 = 76
    NumPad9 = 77
    Multiply = 78
    Add = 79
    Subtract = 80
    [Decimal] = 81
    Divide = 82

    Window = 91

    Period = 92
    Minus = 93
    Menu = 94
    Windows = 95
    Comma = 96
    Plus = 97
    Break = 98
    BracketRight = 99
    BracketLeft = 100
    Backslash = 101
    Apostrophe = 102
    SemiColon = 103
    Slash = 104
    Tilde = 105
    SysRq = 106

    NumLock = 253
    ScrLk = 254
    Unknown = 255
  End Enum

End Namespace