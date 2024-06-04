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
        Case SyntaxKind.AndKeyword, SyntaxKind.AndAlsoKeyword : Return 5
        Case SyntaxKind.OrKeyword, SyntaxKind.OrElseKeyword : Return 4
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

        Case "absolute" : Return SyntaxKind.AbsoluteKeyword
        Case "access" : Return SyntaxKind.AccessKeyword
        Case "and" : Return SyntaxKind.AndKeyword
        Case "andalso" : Return SyntaxKind.AndAlsoKeyword
        Case "any" : Return SyntaxKind.AnyKeyword
        Case "append" : Return SyntaxKind.AppendKeyword
        Case "as" : Return SyntaxKind.AsKeyword
        Case "base" : Return SyntaxKind.BaseKeyword
        Case "beep" : Return SyntaxKind.BeepKeyword
        Case "binary" : Return SyntaxKind.BinaryKeyword
        Case "bload" : Return SyntaxKind.BloadKeyword
        Case "bsave" : Return SyntaxKind.BsaveKeyword
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
        Case "each" : Return SyntaxKind.EachKeyword
        Case "else" : Return SyntaxKind.ElseKeyword
        Case "elseif" : Return SyntaxKind.ElseIfKeyword
        Case "end" : Return SyntaxKind.EndKeyword
        Case "end def" : Return SyntaxKind.EndDefKeyword
        Case "end function" : Return SyntaxKind.EndFunctionKeyword
        Case "end if" : Return SyntaxKind.EndIfKeyword
        Case "end select" : Return SyntaxKind.EndSelectKeyword
        Case "end sub" : Return SyntaxKind.EndSubKeyword
        Case "end type" : Return SyntaxKind.EndTypeKeyword
        Case "environ" : Return SyntaxKind.EnvironKeyword
        Case "eqv" : Return SyntaxKind.EqvKeyword
        Case "erase" : Return SyntaxKind.EraseKeyword
        Case "error" : Return SyntaxKind.ErrorKeyword
        Case "exit" : Return SyntaxKind.ExitKeyword
        Case "exit def" : Return SyntaxKind.ExitDefKeyword
        Case "exit function" : Return SyntaxKind.ExitFunctionKeyword
        Case "exit sub" : Return SyntaxKind.ExitSubKeyword
        Case "exit do" : Return SyntaxKind.ExitDoKeyword
        Case "exit for" : Return SyntaxKind.ExitForKeyword
        Case "exit while" : Return SyntaxKind.ExitWhileKeyword
        Case "false" : Return SyntaxKind.FalseKeyword
        Case "field" : Return SyntaxKind.FieldKeyword
        Case "files" : Return SyntaxKind.FilesKeyword
        Case "for" : Return SyntaxKind.ForKeyword
        Case "for each" : Return SyntaxKind.ForKeyword
        Case "function" : Return SyntaxKind.FunctionKeyword
        Case "get" : Return SyntaxKind.GetKeyword
        Case "gosub" : Return SyntaxKind.GosubKeyword
        Case "goto" : Return SyntaxKind.GotoKeyword
        Case "if" : Return SyntaxKind.IfKeyword
        Case "imp" : Return SyntaxKind.ImpKeyword
        Case "in" : Return SyntaxKind.InKeyword
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
        Case "lprint" : Return SyntaxKind.LprintKeyword
        Case "lset" : Return SyntaxKind.LsetKeyword
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
        Case "orelse" : Return SyntaxKind.OrElseKeyword
        Case "out" : Return SyntaxKind.OutKeyword
        Case "output" : Return SyntaxKind.OutputKeyword
        Case "paint" : Return SyntaxKind.PaintKeyword
        Case "palette" : Return SyntaxKind.PaletteKeyword
        Case "pcopy" : Return SyntaxKind.PcopyKeyword
        Case "peek" : Return SyntaxKind.PeekKeyword
        Case "pen" : Return SyntaxKind.PenKeyword
        Case "play" : Return SyntaxKind.PlayKeyword
        Case "poke" : Return SyntaxKind.PokeKeyword
        Case "preset" : Return SyntaxKind.PresetKeyword
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
        Case "rset" : Return SyntaxKind.RsetKeyword
        Case "run" : Return SyntaxKind.RunKeyword
        Case "screen" : Return SyntaxKind.ScreenKeyword
        Case "seg" : Return SyntaxKind.SegKeyword
        Case "seek" : Return SyntaxKind.SeekKeyword
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
        Case "timer" : Return SyntaxKind.TimerKeyword
        Case "to" : Return SyntaxKind.ToKeyword
        Case "troff" : Return SyntaxKind.TroffKeyword
        Case "tron" : Return SyntaxKind.TronKeyword
        Case "true" : Return SyntaxKind.TrueKeyword
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

        Case Else
          Return SyntaxKind.IdentifierToken
      End Select

    End Function

    Public Function GetText(kind As SyntaxKind) As String

      Select Case kind

        Case SyntaxKind.AbsoluteKeyword : Return "Absolute"
        Case SyntaxKind.AccessKeyword : Return "Access"
        Case SyntaxKind.AndKeyword : Return "And"
        Case SyntaxKind.AndAlsoKeyword : Return "AndAlso"
        Case SyntaxKind.AnyKeyword : Return "Any"
        Case SyntaxKind.AsKeyword : Return "As"
        Case SyntaxKind.BaseKeyword : Return "Base"
        Case SyntaxKind.BeepKeyword : Return "Beep"
        Case SyntaxKind.BloadKeyword : Return "BLoad"
        Case SyntaxKind.BsaveKeyword : Return "BSave"
        Case SyntaxKind.CallKeyword : Return "Call"
        Case SyntaxKind.CaseKeyword : Return "Case"
        Case SyntaxKind.ChainKeyword : Return "Chain"
        Case SyntaxKind.ChDirKeyword : Return "ChDir"
        Case SyntaxKind.CircleKeyword : Return "Circle"
        Case SyntaxKind.ClearKeyword : Return "Clear"
        Case SyntaxKind.CloseKeyword : Return "Close"
        Case SyntaxKind.ClsKeyword : Return "Cls"
        Case SyntaxKind.ColorKeyword : Return "Color"
        Case SyntaxKind.ComKeyword : Return "COM"
        Case SyntaxKind.CommonKeyword : Return "Common"
        Case SyntaxKind.ConstKeyword : Return "Const"
        Case SyntaxKind.ContinueKeyword : Return "Continue"
        Case SyntaxKind.DataKeyword : Return "Data"
        Case SyntaxKind.DateKeyword : Return "Date$"
        Case SyntaxKind.DeclareKeyword : Return "Declare"
        Case SyntaxKind.DefKeyword : Return "Def"
        Case SyntaxKind.DefDblKeyword : Return "DefDbl"
        Case SyntaxKind.DefSngKeyword : Return "DefSng"
        Case SyntaxKind.DefLngKeyword : Return "DefLng"
        Case SyntaxKind.DefIntKeyword : Return "DefInt"
        Case SyntaxKind.DefStrKeyword : Return "DefStr"
        Case SyntaxKind.DimKeyword : Return "Dim"
        Case SyntaxKind.DoKeyword : Return "Do"
        Case SyntaxKind.DoubleKeyword : Return "Double"
        Case SyntaxKind.DrawKeyword : Return "Draw"
        Case SyntaxKind.EachKeyword : Return "Each"
        Case SyntaxKind.ElseKeyword : Return "Else"
        Case SyntaxKind.ElseIfKeyword : Return "ElseIf"
        Case SyntaxKind.EndKeyword : Return "End"
        Case SyntaxKind.EndDefKeyword : Return "End Def"
        Case SyntaxKind.EndFunctionKeyword : Return "End Function"
        Case SyntaxKind.EndIfKeyword : Return "End If"
        Case SyntaxKind.EndSelectKeyword : Return "End Select"
        Case SyntaxKind.EndSubKeyword : Return "End Sub"
        Case SyntaxKind.EndTypeKeyword : Return "End Type"
        Case SyntaxKind.EnvironKeyword : Return "Environ"
        Case SyntaxKind.EqvKeyword : Return "Eqv"
        Case SyntaxKind.EraseKeyword : Return "Erase"
        Case SyntaxKind.ErrorKeyword : Return "Error"
        Case SyntaxKind.ExitKeyword : Return "Exit"
        Case SyntaxKind.ExitDefKeyword : Return "Exit Def"
        Case SyntaxKind.ExitFunctionKeyword : Return "Exit Function"
        Case SyntaxKind.ExitSubKeyword : Return "Exit Sub"
        Case SyntaxKind.ExitDoKeyword : Return "Exit Do"
        Case SyntaxKind.ExitForKeyword : Return "Exit For"
        Case SyntaxKind.ExitWhileKeyword : Return "Exit While"
        Case SyntaxKind.FalseKeyword : Return "False"
        Case SyntaxKind.FieldKeyword : Return "Field"
        Case SyntaxKind.FilesKeyword : Return "Files"
        Case SyntaxKind.ForKeyword : Return "For"
        Case SyntaxKind.FunctionKeyword : Return "Function"
        Case SyntaxKind.GetKeyword : Return "Get"
        Case SyntaxKind.GotoKeyword : Return "Goto"
        Case SyntaxKind.GosubKeyword : Return "Gosub"
        Case SyntaxKind.IfKeyword : Return "If"
        Case SyntaxKind.ImpKeyword : Return "Imp"
        Case SyntaxKind.InKeyword : Return "In"
        Case SyntaxKind.InputKeyword : Return "Input"
        Case SyntaxKind.IntegerKeyword : Return "Integer"
        Case SyntaxKind.IoCtlKeyword : Return "IoCtl"
        Case SyntaxKind.KeyKeyword : Return "Key"
        Case SyntaxKind.KillKeyword : Return "Kill"
        Case SyntaxKind.LetKeyword : Return "Let"
        Case SyntaxKind.LineKeyword : Return "Line"
        Case SyntaxKind.LineInputKeyword : Return "Line Input"
        Case SyntaxKind.ListKeyword : Return "List"
        Case SyntaxKind.LocateKeyword : Return "Locate"
        Case SyntaxKind.LockKeyword : Return "Lock"
        Case SyntaxKind.LongKeyword : Return "Long"
        Case SyntaxKind.LoopKeyword : Return "Loop"
        Case SyntaxKind.LPrintKeyword : Return "LPrint"
        Case SyntaxKind.LSetKeyword : Return "LSet"
        Case SyntaxKind.MidKeyword : Return "Mid$"
        Case SyntaxKind.MkDirKeyword : Return "MkDir"
        Case SyntaxKind.ModKeyword : Return "Mod"
        Case SyntaxKind.NameKeyword : Return "Name"
        Case SyntaxKind.NextKeyword : Return "Next"
        Case SyntaxKind.NotKeyword : Return "Not"
        Case SyntaxKind.OffKeyword : Return "Off"
        Case SyntaxKind.OnKeyword : Return "On"
        Case SyntaxKind.OpenKeyword : Return "Open"
        Case SyntaxKind.OptionKeyword : Return "Option"
        Case SyntaxKind.OrKeyword : Return "Or"
        Case SyntaxKind.OrElseKeyword : Return "OrElse"
        Case SyntaxKind.OutKeyword : Return "Out"
        Case SyntaxKind.PaintKeyword : Return "Paint"
        Case SyntaxKind.PaletteKeyword : Return "Palette"
        Case SyntaxKind.PCopyKeyword : Return "PCopy"
        Case SyntaxKind.PeekKeyword : Return "Peek"
        Case SyntaxKind.PenKeyword : Return "Pen"
        Case SyntaxKind.PlayKeyword : Return "Play"
        Case SyntaxKind.PokeKeyword : Return "Poke"
        Case SyntaxKind.PresetKeyword : Return "Preset"
        Case SyntaxKind.PrintKeyword : Return "Print"
        Case SyntaxKind.PSetKeyword : Return "PSet"
        Case SyntaxKind.PutKeyword : Return "Put"
        Case SyntaxKind.RandomKeyword : Return "Random"
        Case SyntaxKind.RandomizeKeyword : Return "Randomize"
        Case SyntaxKind.ReadKeyword : Return "Read"
        Case SyntaxKind.RedimKeyword : Return "Redim"
        Case SyntaxKind.RemKeyword : Return "Rem"
        Case SyntaxKind.ResetKeyword : Return "Reset"
        Case SyntaxKind.RestoreKeyword : Return "Restore"
        Case SyntaxKind.ResumeKeyword : Return "Resume"
        Case SyntaxKind.ReturnKeyword : Return "Return"
        Case SyntaxKind.RmDirKeyword : Return "RmDir"
        Case SyntaxKind.RSetKeyword : Return "RSet"
        Case SyntaxKind.RunKeyword : Return "Run"
        Case SyntaxKind.ScreenKeyword : Return "Screen"
        Case SyntaxKind.SegKeyword : Return "Seg"
        Case SyntaxKind.SeekKeyword : Return "Seek"
        Case SyntaxKind.SelectKeyword : Return "Select"
        Case SyntaxKind.SharedKeyword : Return "Shared"
        Case SyntaxKind.ShellKeyword : Return "Shell"
        Case SyntaxKind.SleepKeyword : Return "Sleep"
        Case SyntaxKind.SingleKeyword : Return "Single"
        Case SyntaxKind.SoundKeyword : Return "Sound"
        Case SyntaxKind.SpcKeyword : Return "Spc"
        Case SyntaxKind.StaticKeyword : Return "Static"
        Case SyntaxKind.StepKeyword : Return "Step"
        Case SyntaxKind.StopKeyword : Return "Stop"
        Case SyntaxKind.StrigKeyword : Return "Strig"
        Case SyntaxKind.StringKeyword : Return "String"
        Case SyntaxKind.SubKeyword : Return "Sub"
        Case SyntaxKind.SwapKeyword : Return "Swap"
        Case SyntaxKind.SystemKeyword : Return "System"
        Case SyntaxKind.TabKeyword : Return "Tab"
        Case SyntaxKind.ThenKeyword : Return "Then"
        Case SyntaxKind.TimeKeyword : Return "Time$"
        Case SyntaxKind.TimerKeyword : Return "Timer"
        Case SyntaxKind.ToKeyword : Return "To"
        Case SyntaxKind.TroffKeyword : Return "Troff"
        Case SyntaxKind.TronKeyword : Return "Tron"
        Case SyntaxKind.TrueKeyword : Return "True"
        Case SyntaxKind.TypeKeyword : Return "Type"
        Case SyntaxKind.UnlockKeyword : Return "Unlock"
        Case SyntaxKind.UntilKeyword : Return "Until"
        Case SyntaxKind.UsingKeyword : Return "Using"
        Case SyntaxKind.ViewKeyword : Return "View"
        Case SyntaxKind.WaitKeyword : Return "Wait"
        Case SyntaxKind.WendKeyword : Return "Wend"
        Case SyntaxKind.WhileKeyword : Return "While"
        Case SyntaxKind.WidthKeyword : Return "width"
        Case SyntaxKind.WindowKeyword : Return "window"
        Case SyntaxKind.WriteKeyword : Return "write"
        Case SyntaxKind.XorKeyword : Return "Xor"

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