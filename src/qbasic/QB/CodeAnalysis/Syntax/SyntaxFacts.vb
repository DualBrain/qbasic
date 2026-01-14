' Presedence
' -------------------------
' 14 ()
' 13 ^
' 12 - (negation "unary")
' 11 */
' 10 \
' 09 MOD
' 08 +-
' 07 = > >= < <= <>
' 06 NOT
' 05 AND, AndAlso
' 04 OR, OrElse
' 03 XOR
' 02 EQV
' 01 IMP

'TODO: Consider additional operators...

'  << and >>?
'  +=, -=, *=, /=, \=, ^=
'  & and &=?
'  for ++ and --?

Imports System.Runtime.CompilerServices

Namespace Global.QB.CodeAnalysis.Syntax

  Public Module SyntaxFacts

    <Extension()>
    Public Function GetUnaryOperatorPrecedence(kind As SyntaxKind) As Integer
      Select Case kind
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken : Return 12
        Case SyntaxKind.NotKeyword : Return 6
        Case Else
          Return 0
      End Select
    End Function

    <Extension()>
    Public Function GetBinaryOperatorPrecedence(kind As SyntaxKind) As Integer
      Select Case kind
        Case SyntaxKind.HatToken : Return 13
        Case SyntaxKind.StarToken, SyntaxKind.SlashToken : Return 11
        Case SyntaxKind.BackslashToken : Return 10
        Case SyntaxKind.ModKeyword : Return 9
        Case SyntaxKind.PlusToken, SyntaxKind.MinusToken : Return 8
        Case SyntaxKind.EqualToken, SyntaxKind.GreaterThanToken, SyntaxKind.GreaterThanEqualToken, SyntaxKind.LessThanToken, SyntaxKind.LessThanEqualToken, SyntaxKind.LessThanGreaterThanToken : Return 7
        Case SyntaxKind.AndKeyword, SyntaxKind.AndKeyword : Return 5
        Case SyntaxKind.OrKeyword, SyntaxKind.OrKeyword : Return 4
        Case SyntaxKind.XorKeyword : Return 3
        Case SyntaxKind.EqvKeyword : Return 2
        Case SyntaxKind.ImpKeyword : Return 1
        Case Else
          Return 0
      End Select
    End Function

    Public Iterator Function GetUnaryOperatorKinds() As IEnumerable(Of SyntaxKind)
      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())
      For Each kind In kinds
        If GetUnaryOperatorPrecedence(kind) > 0 Then
          Yield kind
        End If
      Next
    End Function

    Public Iterator Function GetBinaryOperatorKinds() As IEnumerable(Of SyntaxKind)
      Dim kinds = DirectCast([Enum].GetValues(GetType(SyntaxKind)), SyntaxKind())
      For Each kind In kinds
        If GetBinaryOperatorPrecedence(kind) > 0 Then
          Yield kind
        End If
      Next
    End Function

    Public Function GetKeywordKind(text As String) As SyntaxKind

      Select Case text.ToLower

#Region "SpecBAS"
        Case "at" : Return SyntaxKind.AtKeyword
        Case "auto" : Return SyntaxKind.AutoKeyword
        Case "border" : Return SyntaxKind.BorderKeyword
        Case "default" : Return SyntaxKind.DefaultKeyword
        Case "fill" : Return SyntaxKind.FillKeyword
        Case "full" : Return SyntaxKind.FullKeyword
        Case "go" : Return SyntaxKind.GoKeyword
        Case "grab" : Return SyntaxKind.GrabKeyword
        Case "hsv" : Return SyntaxKind.HsvKeyword
        Case "inc" : Return SyntaxKind.IncKeyword
        Case "ink" : Return SyntaxKind.InkKeyword
        Case "inverse" : Return SyntaxKind.InverseKeyword
        Case "move" : Return SyntaxKind.MoveKeyword
        Case "over" : Return SyntaxKind.OverKeyword
        Case "paper" : Return SyntaxKind.PaperKeyword
        Case "prog" : Return SyntaxKind.ProgKeyword
        Case "shl" : Return SyntaxKind.ShlKeyword
        Case "shr" : Return SyntaxKind.ShrKeyword
        Case "update" : Return SyntaxKind.UpdateKeyword
        Case "zxascii" : Return SyntaxKind.ZxAsciiKeyword
#End Region

#Region "BASIC"

        Case "absolute" : Return SyntaxKind.AbsoluteKeyword
        Case "access" : Return SyntaxKind.AccessKeyword
        Case "and" : Return SyntaxKind.AndKeyword
        'Case "andalso" : Return SyntaxKind.AndAlsoKeyword
        Case "any" : Return SyntaxKind.AnyKeyword
        Case "append" : Return SyntaxKind.AppendKeyword
        Case "as" : Return SyntaxKind.AsKeyword
        Case "base" : Return SyntaxKind.BaseKeyword
        Case "beep" : Return SyntaxKind.BeepKeyword
        Case "binary" : Return SyntaxKind.BinaryKeyword
        Case "bload" : Return SyntaxKind.BLoadKeyword
        Case "bsave" : Return SyntaxKind.BSaveKeyword
        Case "call" : Return SyntaxKind.CallKeyword
        Case "case" : Return SyntaxKind.CaseKeyword
        Case "chain" : Return SyntaxKind.ChainKeyword
        Case "chdir" : Return SyntaxKind.ChDirKeyword
        Case "circle" : Return SyntaxKind.CircleKeyword
        Case "clear" : Return SyntaxKind.ClearKeyword
        Case "close" : Return SyntaxKind.CloseKeyword
        Case "cls" : Return SyntaxKind.ClsKeyword
        Case "color" : Return SyntaxKind.ColorKeyword
        Case "com" : Return SyntaxKind.ComKeyword
        Case "common" : Return SyntaxKind.CommonKeyword
        Case "const" : Return SyntaxKind.ConstKeyword
        Case "continue" : Return SyntaxKind.ContinueKeyword
        Case "continue do" : Return SyntaxKind.ContinueDoKeyword
        Case "continue for" : Return SyntaxKind.ContinueForKeyword
        Case "continue while" : Return SyntaxKind.ContinueWhileKeyword
        Case "data" : Return SyntaxKind.DataKeyword
        Case "date$" : Return SyntaxKind.DateKeyword
        Case "declare" : Return SyntaxKind.DeclareKeyword
        Case "def" : Return SyntaxKind.DefKeyword
        Case "defdbl" : Return SyntaxKind.DefDblKeyword
        Case "defint" : Return SyntaxKind.DefIntKeyword
        Case "deflng" : Return SyntaxKind.DefLngKeyword
        Case "defsng" : Return SyntaxKind.DefSngKeyword
        Case "defstr" : Return SyntaxKind.DefStrKeyword
        Case "dim" : Return SyntaxKind.DimKeyword
        Case "do" : Return SyntaxKind.DoKeyword
        Case "double" : Return SyntaxKind.DoubleKeyword
        Case "draw" : Return SyntaxKind.DrawKeyword
        'Case "each" : Return SyntaxKind.EachKeyword
        Case "else" : Return SyntaxKind.ElseKeyword
        Case "elseif" : Return SyntaxKind.ElseIfKeyword
        Case "end" : Return SyntaxKind.EndKeyword
        'Case "end def" : Return SyntaxKind.EndDefKeyword
        'Case "end function" : Return SyntaxKind.EndFunctionKeyword
        'Case "end if" : Return SyntaxKind.EndIfKeyword
        'Case "end select" : Return SyntaxKind.EndSelectKeyword
        'Case "end sub" : Return SyntaxKind.EndSubKeyword
        'Case "end type" : Return SyntaxKind.EndTypeKeyword
        Case "environ" : Return SyntaxKind.EnvironKeyword
        Case "eqv" : Return SyntaxKind.EqvKeyword
        Case "erase" : Return SyntaxKind.EraseKeyword
        Case "error" : Return SyntaxKind.ErrorKeyword
        Case "exit" : Return SyntaxKind.ExitKeyword
        Case "exit def" : Return SyntaxKind.ExitDefKeyword
        Case "exit do" : Return SyntaxKind.ExitDoKeyword
        Case "exit for" : Return SyntaxKind.ExitForKeyword
        Case "exit function" : Return SyntaxKind.ExitFunctionKeyword
        Case "exit sub" : Return SyntaxKind.ExitSubKeyword
        Case "exit while" : Return SyntaxKind.ExitWhileKeyword
        'Case "false" : Return SyntaxKind.FalseKeyword
        Case "field" : Return SyntaxKind.FieldKeyword
        Case "files" : Return SyntaxKind.FilesKeyword
        Case "for" : Return SyntaxKind.ForKeyword
        'Case "for each" : Return SyntaxKind.ForKeyword
        Case "function" : Return SyntaxKind.FunctionKeyword
        Case "get" : Return SyntaxKind.GetKeyword
        Case "gosub" : Return SyntaxKind.GosubKeyword
        Case "goto" : Return SyntaxKind.GotoKeyword
        Case "if" : Return SyntaxKind.IfKeyword
        Case "imp" : Return SyntaxKind.ImpKeyword
        'Case "in" : Return SyntaxKind.InKeyword
        Case "input" : Return SyntaxKind.InputKeyword
        Case "integer" : Return SyntaxKind.IntegerKeyword
        Case "ioctl" : Return SyntaxKind.IoCtlKeyword
        Case "is" : Return SyntaxKind.IsKeyword
        Case "key" : Return SyntaxKind.KeyKeyword
        Case "kill" : Return SyntaxKind.KillKeyword
        Case "let" : Return SyntaxKind.LetKeyword
        Case "line" : Return SyntaxKind.LineKeyword
        Case "line input" : Return SyntaxKind.LineInputKeyword
        Case "list" : Return SyntaxKind.ListKeyword
        Case "locate" : Return SyntaxKind.LocateKeyword
        Case "lock" : Return SyntaxKind.LockKeyword
        Case "long" : Return SyntaxKind.LongKeyword
        Case "loop" : Return SyntaxKind.LoopKeyword
        Case "lprint" : Return SyntaxKind.LPrintKeyword
        Case "lset" : Return SyntaxKind.LSetKeyword
        Case "mid$" : Return SyntaxKind.MidKeyword
        Case "mkdir" : Return SyntaxKind.MkDirKeyword
        Case "mod" : Return SyntaxKind.ModKeyword
        Case "name" : Return SyntaxKind.NameKeyword
        Case "next" : Return SyntaxKind.NextKeyword
        Case "not" : Return SyntaxKind.NotKeyword
        Case "off" : Return SyntaxKind.OffKeyword
        Case "on" : Return SyntaxKind.OnKeyword
        Case "open" : Return SyntaxKind.OpenKeyword
        Case "option" : Return SyntaxKind.OptionKeyword
        Case "or" : Return SyntaxKind.OrKeyword
        'Case "orelse" : Return SyntaxKind.OrElseKeyword
        Case "out" : Return SyntaxKind.OutKeyword
        Case "output" : Return SyntaxKind.OutputKeyword
        Case "paint" : Return SyntaxKind.PaintKeyword
        Case "palette" : Return SyntaxKind.PaletteKeyword
        Case "pcopy" : Return SyntaxKind.PCopyKeyword
        'Case "peek" : Return SyntaxKind.PeekKeyword
        Case "pen" : Return SyntaxKind.PenKeyword
        Case "play" : Return SyntaxKind.PlayKeyword
        Case "poke" : Return SyntaxKind.PokeKeyword
        Case "preset" : Return SyntaxKind.PresetKeyword
        Case "preserve" : Return SyntaxKind.PreserveKeyword
        Case "print" : Return SyntaxKind.PrintKeyword
        Case "pset" : Return SyntaxKind.PsetKeyword
        Case "put" : Return SyntaxKind.PutKeyword
        Case "random" : Return SyntaxKind.RandomKeyword
        Case "randomize" : Return SyntaxKind.RandomizeKeyword
        Case "read" : Return SyntaxKind.ReadKeyword
        Case "redim" : Return SyntaxKind.RedimKeyword
        Case "rem" : Return SyntaxKind.RemKeyword
        Case "reset" : Return SyntaxKind.ResetKeyword
        Case "restore" : Return SyntaxKind.RestoreKeyword
        Case "resume" : Return SyntaxKind.ResumeKeyword
        Case "return" : Return SyntaxKind.ReturnKeyword
        Case "rmdir" : Return SyntaxKind.RmDirKeyword
        Case "rset" : Return SyntaxKind.RSetKeyword
        Case "run" : Return SyntaxKind.RunKeyword
        Case "screen" : Return SyntaxKind.ScreenKeyword
        Case "seek" : Return SyntaxKind.SeekKeyword
        Case "seg" : Return SyntaxKind.SegKeyword
        Case "select" : Return SyntaxKind.SelectKeyword
        Case "shared" : Return SyntaxKind.SharedKeyword
        Case "shell" : Return SyntaxKind.ShellKeyword
        Case "single" : Return SyntaxKind.SingleKeyword
        Case "sleep" : Return SyntaxKind.SleepKeyword
        Case "sound" : Return SyntaxKind.SoundKeyword
        Case "spc" : Return SyntaxKind.SpcKeyword
        Case "static" : Return SyntaxKind.StaticKeyword
        Case "step" : Return SyntaxKind.StepKeyword
        Case "stick" : Return SyntaxKind.StickKeyword
        Case "stop" : Return SyntaxKind.StopKeyword
        Case "strig" : Return SyntaxKind.StrigKeyword
        Case "string" : Return SyntaxKind.StringKeyword
        Case "sub" : Return SyntaxKind.SubKeyword
        Case "swap" : Return SyntaxKind.SwapKeyword
        Case "system" : Return SyntaxKind.SystemKeyword
        Case "tab" : Return SyntaxKind.TabKeyword
        Case "then" : Return SyntaxKind.ThenKeyword
        Case "time$" : Return SyntaxKind.TimeKeyword
        'Case "timer" : Return SyntaxKind.TimerKeyword
        Case "to" : Return SyntaxKind.ToKeyword
        Case "troff" : Return SyntaxKind.TroffKeyword
        Case "tron" : Return SyntaxKind.TronKeyword
        'Case "true" : Return SyntaxKind.TrueKeyword
        Case "type" : Return SyntaxKind.TypeKeyword
        Case "unlock" : Return SyntaxKind.UnlockKeyword
        Case "until" : Return SyntaxKind.UntilKeyword
        Case "using" : Return SyntaxKind.UsingKeyword
        Case "view" : Return SyntaxKind.ViewKeyword
        Case "wait" : Return SyntaxKind.WaitKeyword
        Case "wend" : Return SyntaxKind.WendKeyword
        Case "while" : Return SyntaxKind.WhileKeyword
        Case "width" : Return SyntaxKind.WidthKeyword
        Case "window" : Return SyntaxKind.WindowKeyword
        Case "write" : Return SyntaxKind.WriteKeyword
        Case "xor" : Return SyntaxKind.XorKeyword

#End Region

        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function

    Public Function GetText(kind As SyntaxKind) As String

      Select Case kind

#Region "SpecBAS"
        Case SyntaxKind.AtKeyword : Return "AT"
        Case SyntaxKind.AutoKeyword : Return "AUTO"
        Case SyntaxKind.BorderKeyword : Return "BORDER"
        Case SyntaxKind.DefaultKeyword : Return "DEFAULT"
        Case SyntaxKind.FillKeyword : Return "FILL"
        Case SyntaxKind.FullKeyword : Return "FULL"
        Case SyntaxKind.GoKeyword : Return "GO"
        Case SyntaxKind.GrabKeyword : Return "GRAB"
        Case SyntaxKind.HsvKeyword : Return "HSV"
        Case SyntaxKind.IncKeyword : Return "INC"
        Case SyntaxKind.InkKeyword : Return "INK"
        Case SyntaxKind.InverseKeyword : Return "INVERSE"
        Case SyntaxKind.MoveKeyword : Return "MOVE"
        Case SyntaxKind.OverKeyword : Return "OVER"
        Case SyntaxKind.PaperKeyword : Return "PAPER"
        Case SyntaxKind.ProgKeyword : Return "PROG"
        Case SyntaxKind.UpdateKeyword : Return "UPDATE"
        Case SyntaxKind.ZxAsciiKeyword : Return "ZXASCII"
#End Region

#Region "BASIC"
        Case SyntaxKind.AbsoluteKeyword : Return "ABSOLUTE"
        Case SyntaxKind.AccessKeyword : Return "ACCESS"
        Case SyntaxKind.AndKeyword : Return "AND"
        'Case SyntaxKind.AndAlsoKeyword : Return "AndAlso"
        Case SyntaxKind.AnyKeyword : Return "ANY"
        Case SyntaxKind.AppendKeyword : Return "APPEND"
        Case SyntaxKind.AsKeyword : Return "AS"
        Case SyntaxKind.BaseKeyword : Return "BASE"
        Case SyntaxKind.BeepKeyword : Return "BEEP"
        Case SyntaxKind.BinaryKeyword : Return "BINARY"
        Case SyntaxKind.BloadKeyword : Return "BLOAD"
        Case SyntaxKind.BsaveKeyword : Return "BSAVE"
        Case SyntaxKind.CallKeyword : Return "CALL"
        Case SyntaxKind.CaseKeyword : Return "CASE"
        Case SyntaxKind.ChainKeyword : Return "CHAIN"
        Case SyntaxKind.ChDirKeyword : Return "CHDIR"
        Case SyntaxKind.CircleKeyword : Return "CIRCLE"
        Case SyntaxKind.ClearKeyword : Return "CLEAR"
        Case SyntaxKind.CloseKeyword : Return "CLOSE"
        Case SyntaxKind.ClsKeyword : Return "CLS"
        Case SyntaxKind.ColorKeyword : Return "COLOR"
        Case SyntaxKind.ComKeyword : Return "COM"
        Case SyntaxKind.CommonKeyword : Return "COMMON"
        Case SyntaxKind.ConstKeyword : Return "CONST"
        Case SyntaxKind.ContinueKeyword : Return "CONTINUE"
        Case SyntaxKind.ContinueDoKeyword : Return "CONTINUE DO"
        Case SyntaxKind.ContinueForKeyword : Return "CONTINUE FOR"
        Case SyntaxKind.ContinueWhileKeyword : Return "CONTINUE WHILE"
        Case SyntaxKind.DataKeyword : Return "DATA"
        Case SyntaxKind.DateKeyword : Return "DATE$"
        Case SyntaxKind.DeclareKeyword : Return "DECLARE"
        Case SyntaxKind.DefKeyword : Return "DEF"
        Case SyntaxKind.DefDblKeyword : Return "DEFDBL"
        Case SyntaxKind.DefSngKeyword : Return "DEFSNG"
        Case SyntaxKind.DefLngKeyword : Return "DEFLNG"
        Case SyntaxKind.DefIntKeyword : Return "DEFINT"
        Case SyntaxKind.DefStrKeyword : Return "DEFSTR"
        Case SyntaxKind.DimKeyword : Return "DIM"
        Case SyntaxKind.DoKeyword : Return "DO"
        Case SyntaxKind.DoubleKeyword : Return "DOUBLE"
        Case SyntaxKind.DrawKeyword : Return "DRAW"
        'Case SyntaxKind.EachKeyword : Return "Each"
        Case SyntaxKind.ElseKeyword : Return "ELSE"
        Case SyntaxKind.ElseIfKeyword : Return "ELSEIF"
        Case SyntaxKind.EndKeyword : Return "END"
        'Case SyntaxKind.EndDefKeyword : Return "END DEF"
        'Case SyntaxKind.EndFunctionKeyword : Return "END FUNCTION"
        'Case SyntaxKind.EndIfKeyword : Return "END IF"
        'Case SyntaxKind.EndSelectKeyword : Return "END SELECT"
        'Case SyntaxKind.EndSubKeyword : Return "END SUB"
        'Case SyntaxKind.EndTypeKeyword : Return "END TYPE"
        Case SyntaxKind.EnvironKeyword : Return "ENVIRON"
        Case SyntaxKind.EqvKeyword : Return "EQV"
        Case SyntaxKind.EraseKeyword : Return "ERASE"
        Case SyntaxKind.ErrorKeyword : Return "ERROR"
        Case SyntaxKind.ExitKeyword : Return "EXIT"
        Case SyntaxKind.ExitDefKeyword : Return "EXIT DEF"
        Case SyntaxKind.ExitDoKeyword : Return "EXIT DO"
        Case SyntaxKind.ExitForKeyword : Return "EXIT FOR"
        Case SyntaxKind.ExitFunctionKeyword : Return "EXIT FUNCTION"
        Case SyntaxKind.ExitSubKeyword : Return "EXIT SUB"
        Case SyntaxKind.ExitWhileKeyword : Return "EXIT WHILE"
        'Case SyntaxKind.FalseKeyword : Return "FALSE"
        Case SyntaxKind.FieldKeyword : Return "FIELD"
        Case SyntaxKind.FilesKeyword : Return "FILES"
        Case SyntaxKind.ForKeyword : Return "FOR"
        Case SyntaxKind.FunctionKeyword : Return "FUNCTION"
        Case SyntaxKind.GetKeyword : Return "GET"
        Case SyntaxKind.GosubKeyword : Return "GOSUB"
        Case SyntaxKind.GotoKeyword : Return "GOTO"
        Case SyntaxKind.IfKeyword : Return "IF"
        Case SyntaxKind.ImpKeyword : Return "IMP"
        'Case SyntaxKind.InKeyword : Return "IN"
        Case SyntaxKind.InputKeyword : Return "INPUT"
        Case SyntaxKind.IntegerKeyword : Return "INTEGER"
        Case SyntaxKind.IoCtlKeyword : Return "IOCTL"
        Case SyntaxKind.IsKeyword : Return "IS"
        Case SyntaxKind.KeyKeyword : Return "KEY"
        Case SyntaxKind.KillKeyword : Return "KILL"
        Case SyntaxKind.LetKeyword : Return "LET"
        Case SyntaxKind.LineKeyword : Return "LINE"
        Case SyntaxKind.LineInputKeyword : Return "LINE INPUT"
        Case SyntaxKind.ListKeyword : Return "LIST"
        Case SyntaxKind.LocateKeyword : Return "LOCATE"
        Case SyntaxKind.LockKeyword : Return "LOCK"
        Case SyntaxKind.LongKeyword : Return "LONG"
        Case SyntaxKind.LoopKeyword : Return "LOOP"
        Case SyntaxKind.LPrintKeyword : Return "LPRINT"
        Case SyntaxKind.LSetKeyword : Return "LSET"
        Case SyntaxKind.MidKeyword : Return "MID$"
        Case SyntaxKind.MkDirKeyword : Return "MKDIR"
        Case SyntaxKind.ModKeyword : Return "MOD"
        Case SyntaxKind.NameKeyword : Return "NAME"
        Case SyntaxKind.NextKeyword : Return "NEXT"
        Case SyntaxKind.NotKeyword : Return "NOT"
        Case SyntaxKind.OffKeyword : Return "OFF"
        Case SyntaxKind.OnKeyword : Return "ON"
        Case SyntaxKind.OpenKeyword : Return "OPEN"
        Case SyntaxKind.OptionKeyword : Return "OPTION"
        Case SyntaxKind.OrKeyword : Return "OR"
        'Case SyntaxKind.OrElseKeyword : Return "OrElse"
        Case SyntaxKind.OutKeyword : Return "OUT"
        Case SyntaxKind.OutputKeyword : Return "OUTPUT"
        Case SyntaxKind.PaintKeyword : Return "PAINT"
        Case SyntaxKind.PaletteKeyword : Return "PALETTE"
        Case SyntaxKind.PCopyKeyword : Return "PCOPY"
        'Case SyntaxKind.PeekKeyword : Return "PEEK"
        Case SyntaxKind.PenKeyword : Return "PEN"
        Case SyntaxKind.PlayKeyword : Return "PLAY"
        Case SyntaxKind.PokeKeyword : Return "POKE"
        Case SyntaxKind.PResetKeyword : Return "PRESET"
        Case SyntaxKind.PrintKeyword : Return "PRINT"
        Case SyntaxKind.PSetKeyword : Return "PSET"
        Case SyntaxKind.PutKeyword : Return "PUT"
        Case SyntaxKind.RandomKeyword : Return "RANDOM"
        Case SyntaxKind.RandomizeKeyword : Return "RANDOMIZE"
        Case SyntaxKind.ReadKeyword : Return "READ"
        Case SyntaxKind.RedimKeyword : Return "REDIM"
        Case SyntaxKind.RemKeyword : Return "REM"
        Case SyntaxKind.ResetKeyword : Return "RESET"
        Case SyntaxKind.RestoreKeyword : Return "RESTORE"
        Case SyntaxKind.ResumeKeyword : Return "RESUME"
        Case SyntaxKind.ReturnKeyword : Return "RETURN"
        Case SyntaxKind.RmDirKeyword : Return "RMDIR"
        Case SyntaxKind.RSetKeyword : Return "RSET"
        Case SyntaxKind.RunKeyword : Return "RUN"
        Case SyntaxKind.ScreenKeyword : Return "SCREEN"
        Case SyntaxKind.SeekKeyword : Return "SEEK"
        Case SyntaxKind.SegKeyword : Return "SEG"
        Case SyntaxKind.SelectKeyword : Return "SELECT"
        Case SyntaxKind.SharedKeyword : Return "SHARED"
        Case SyntaxKind.ShellKeyword : Return "SHELL"
        Case SyntaxKind.SingleKeyword : Return "SINGLE"
        Case SyntaxKind.SleepKeyword : Return "SLEEP"
        Case SyntaxKind.SoundKeyword : Return "SOUND"
        Case SyntaxKind.SpcKeyword : Return "SPC"
        Case SyntaxKind.StaticKeyword : Return "STATIC"
        Case SyntaxKind.StepKeyword : Return "STEP"
        Case SyntaxKind.StickKeyword : Return "STICK"
        Case SyntaxKind.StopKeyword : Return "STOP"
        Case SyntaxKind.StrigKeyword : Return "STRIG"
        Case SyntaxKind.StringKeyword : Return "STRING"
        Case SyntaxKind.SubKeyword : Return "SUB"
        Case SyntaxKind.SwapKeyword : Return "SWAP"
        Case SyntaxKind.SystemKeyword : Return "SYSTEM"
        Case SyntaxKind.TabKeyword : Return "TAB"
        Case SyntaxKind.ThenKeyword : Return "THEN"
        Case SyntaxKind.TimeKeyword : Return "TIME$"
        'Case SyntaxKind.TimerKeyword : Return "TIMER"
        Case SyntaxKind.ToKeyword : Return "TO"
        Case SyntaxKind.TroffKeyword : Return "TROFF"
        Case SyntaxKind.TronKeyword : Return "TRON"
        'Case SyntaxKind.TrueKeyword : Return "TRUE"
        Case SyntaxKind.TypeKeyword : Return "TYPE"
        Case SyntaxKind.UnlockKeyword : Return "UNLOCK"
        Case SyntaxKind.UntilKeyword : Return "UNTIL"
        Case SyntaxKind.UsingKeyword : Return "USING"
        Case SyntaxKind.ViewKeyword : Return "VIEW"
        Case SyntaxKind.WaitKeyword : Return "WAIT"
        Case SyntaxKind.WendKeyword : Return "WEND"
        Case SyntaxKind.WhileKeyword : Return "WHILE"
        Case SyntaxKind.WidthKeyword : Return "WIDTH"
        Case SyntaxKind.WindowKeyword : Return "WINDOW"
        Case SyntaxKind.WriteKeyword : Return "WRITE"
        Case SyntaxKind.XorKeyword : Return "XOR"

#End Region

        Case SyntaxKind.PoundToken : Return "#"
        Case SyntaxKind.PlusToken : Return "+"
        Case SyntaxKind.MinusToken : Return "-"
        Case SyntaxKind.StarToken : Return "*"
        Case SyntaxKind.SlashToken : Return "/"
        Case SyntaxKind.BackslashToken : Return "\"
        Case SyntaxKind.HatToken : Return "^"
        Case SyntaxKind.OpenParenToken : Return "("
        Case SyntaxKind.CloseParenToken : Return ")"
        Case SyntaxKind.OpenBraceToken : Return "{"
        Case SyntaxKind.CloseBraceToken : Return "}"
        Case SyntaxKind.EqualToken : Return "="
        Case SyntaxKind.LessThanToken : Return "<"
        Case SyntaxKind.PeriodToken : Return "."
        Case SyntaxKind.ColonToken : Return ":"
        Case SyntaxKind.CommaToken : Return ","
        Case SyntaxKind.SemicolonToken : Return ";"
        Case SyntaxKind.QuestionToken : Return "?"
        Case SyntaxKind.GreaterThanEqualToken : Return ">="
        Case SyntaxKind.LessThanEqualToken : Return "<="
        Case SyntaxKind.LessThanGreaterThanToken : Return "<>"
        Case SyntaxKind.GreaterThanToken : Return ">"
        Case SyntaxKind.QuestionToken : Return "?"

        Case Else
          Return Nothing
      End Select

    End Function

    <Extension>
    Public Function IsComment(kind As SyntaxKind) As Boolean
      Select Case kind
        Case SyntaxKind.SingleLineCommentTrivia
          Return True
        Case Else
          Return False
      End Select
    End Function

    <Extension>
    Public Function IsTrivia(kind As SyntaxKind) As Boolean
      Select Case kind
        Case SyntaxKind.LineNumberTrivia,
             SyntaxKind.SkippedTextTrivia,
             SyntaxKind.LineBreakTrivia,
             SyntaxKind.WhiteSpaceTrivia,
             SyntaxKind.SingleLineCommentTrivia
          Return True
        Case Else
          Return False
      End Select
    End Function

    <Extension>
    Public Function Is_Keyword(kind As SyntaxKind) As Boolean
      Return kind.ToString.EndsWith("Keyword")
    End Function

    <Extension>
    Public Function IsToken(kind As SyntaxKind) As Boolean
      Return Not kind.IsTrivia AndAlso
             (kind.Is_Keyword OrElse kind.ToString.EndsWith("Token"))
    End Function

  End Module

End Namespace