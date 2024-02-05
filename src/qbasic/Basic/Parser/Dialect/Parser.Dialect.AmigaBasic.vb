Namespace Global.Basic.Parser.Dialects

  'Partial Class Parser

  Friend Class AmigaBasic
    Implements IDialect

    ' Amiga Basic ' circa 1985

    Public ReadOnly Property Keywords As System.Collections.Generic.List(Of String) Implements IDialect.Keywords
      Get
        ' "DEF FN", "DEF SEG", "DEF USR" -- handled by the "DEF".
        Return New List(Of String) From {"BEEP", "CALL", "CHAIN", "CIRCLE", "CLOSE", "CLS", "COLOR", "COM", "COMMON",
                                         "DATA", "DEF", "DEFINT", "DEFSNG", "DEFDBL", "DEFSTR",
                                         "DIM", "DRAW", "END", "ENVIRON", "ERASE", "ERROR", "FIELD", "FOR", "TO", "STEP", "NEXT", "GET",
                                         "GOSUB", "GOTO", "IF", "THEN", "ELSE", "INPUT", "INPUT#", "IOCTL", "KEY", "LET", "LINE",
                                         "LINE INPUT", "LINE INPUT#", "LOCATE", "LOCK", "LPRINT", "LPRINT USING", "LSET", "RSET",
                                         "ON", "ON COM", "ON KEY", "ON PEN", "ON PLAY", "ON STRIG", "ON TIMER", "ON ERROR GOTO",
                                         "OPEN", "OPEN ""COM", "OPTION", "OUT", "PAINT", "PALETTE", "PALETTE USING",
                                         "PEN", "PLAY", "POKE", "PRESET", "PSET", "PRINT", "PRINT USING", "PRINT#", "PRINT# USING",
                                         "PUT", "RANDOMIZE", "READ", "REM", "RESTORE", "RESUME", "RETURN", "SCREEN", "SHELL",
                                         "SOUND", "STOP", "STRIG", "SWAP", "UNLOCK", "VIEW", "VIEW PRINT", "WAIT", "WHILE", "WEND",
                                         "WIDTH", "WINDOW", "WRITE"}
      End Get
    End Property

    Public ReadOnly Property Functions As System.Collections.Generic.List(Of String) Implements IDialect.Functions
      Get
        Return New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CHR$", "CINT", "COS", "CSNG", "CVI", "CVS", "CVD",
                                         "ENVIRON$", "EOF", "EXP", "EXTERR", "FIX", "FRE", "HEX$", "INP", "INPUT$", "INSTR",
                                         "INT", "IOCTL$", "LCASE$", "LEFT$", "LEN", "LOC", "LOF", "LOG", "LPOS", "MID$", "MKI$", "MKS$", "MKD$",
                                         "OCT$", "PEEK", "PEN", "PLAY", "PMAP", "POINT", "POS", "RIGHT$", "RND", "SCREEN", "SGN",
                                         "SIN", "SPACE$", "SPC", "SQR", "STICK", "STR$", "STRIG", "STRING$", "TAB", "TAN", "TIMER",
                                         "UCASE$", "VAL", "VARPTR", "VARPTR$", "LTRIM$", "RTRIM$"}
      End Get
    End Property

    Public ReadOnly Property Commands As System.Collections.Generic.List(Of String) Implements IDialect.Commands
      Get
        Return New List(Of String) From {"BLOAD", "BSAVE", "CHDIR", "CLEAR", "CONT", "DELETE", "FILES", "KILL",
                                         "LIST", "LLIST", "LOAD", "MERGE", "MKDIR", "NAME", "NEW", "PCOPY", "RESET",
                                         "RMDIR", "RUN", "SAVE", "SYSTEM", "TRON", "TROFF"}
      End Get
    End Property

    Public ReadOnly Property Variables As System.Collections.Generic.List(Of String) Implements IDialect.Variables
      Get
        Return New List(Of String) From {"CSRLIN", "DATE$", "ERDEV", "ERDEV$", "ERR", "ERL", "INKEY$", "TIME$"}
      End Get
    End Property

    Public ReadOnly Property Operators As System.Collections.Generic.List(Of String) Implements IDialect.Operators
      Get
        Return New List(Of String) From {"NOT", "AND", "OR", "XOR", "EQV", "IMP", "MOD"}
      End Get
    End Property

    Public ReadOnly Property Symbols As System.Collections.Generic.List(Of Char) Implements IDialect.Symbols
      Get
        Return New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, ","c, ";"c, ":"c, "'"c, """"c}
      End Get
    End Property

    Public ReadOnly Property GroupingOperators As System.Collections.Generic.List(Of Char) Implements IDialect.GroupingOperators
      Get
        Return New List(Of Char) From {"("c, ")"c}
      End Get
    End Property

    Public ReadOnly Property ArithmaticOperators As System.Collections.Generic.List(Of Char) Implements IDialect.ArithmaticOperators
      Get
        Return New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "\"c, "^"c}
      End Get
    End Property

    Public ReadOnly Property RelationalOperators As System.Collections.Generic.List(Of String) Implements IDialect.RelationalOperators
      Get
        Return New List(Of String) From {"=", "<>", "<", "<=", ">", ">=", "=<", "=>"}
      End Get
    End Property

    Public ReadOnly Property NumericSuffix As System.Collections.Generic.List(Of Char) Implements IDialect.NumericSuffix
      Get
        Return New List(Of Char) From {"%"c, "!"c, "#"c} ' Integer, Single, Double
      End Get
    End Property

    Public ReadOnly Property StringSuffix As System.Collections.Generic.List(Of Char) Implements IDialect.StringSuffix
      Get
        Return New List(Of Char) From {"$"c}
      End Get
    End Property

    'Public ReadOnly Property IdentifierSuffix As System.Collections.Generic.List(Of Char) Implements IDialect.IdentifierSuffix
    '  Get
    '    Dim result As New List(Of Char)
    '    For Each c In Me.NumericSuffix
    '      result.Add(c)
    '    Next
    '    For Each c In Me.StringSuffix
    '      result.Add(c)
    '    Next
    '    Return result 'New List(Of Char) From {"$"c, "%"c, "!"c, "#"c}
    '  End Get
    'End Property

    Public ReadOnly Property ReservedWords As System.Collections.Generic.List(Of String) Implements IDialect.ReservedWords
      Get

        Dim result As New List(Of String)
        For Each word In Keywords
          result.Add(word)
        Next
        For Each word In Functions
          result.Add(word)
        Next
        For Each word In Commands
          result.Add(word)
        Next
        For Each word In Variables
          result.Add(word)
        Next
        For Each word In Operators
          result.Add(word)
        Next

        Return result

      End Get
    End Property

    Public ReadOnly Property IgnoreAllWhiteSpace As Boolean Implements IDialect.IgnoreAllWhiteSpace
      Get
        Return False
      End Get
    End Property

    Public ReadOnly Property SupportsLabels As Boolean Implements IDialect.SupportsLabels
      Get
        Return True
      End Get
    End Property

  End Class

  'End Class

End Namespace