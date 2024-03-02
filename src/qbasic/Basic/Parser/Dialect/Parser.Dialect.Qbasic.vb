Namespace Global.Basic.Parser.Dialects

  ' QBasic ' circa 1991 - 2000

  Friend Class Qbasic
    Implements IDialect

    Public ReadOnly Property Keywords As List(Of String) Implements IDialect.Keywords
      Get
        ' "DEF FN", "DEF SEG", "DEF USR" -- handled by the "DEF".
        Return New List(Of String) From {"ABSOLUTE", "ACCESS", "ANY", "APPEND", "AS",
                                         "BASE", "BEEP", "BINARY", "BLOAD", "BSAVE",
                                         "CALL", "CASE", "CHAIN", "CHDIR", "CIRCLE", "CLEAR", "CLOSE", "CLS", "COLOR", "COM", "COMMON", "CONST",
                                         "DATA", "DECLARE", "DEF", "DEFDBL", "DEFINT", "DEFLNG", "DEFSNG", "DEFSTR", "DIM", "DO", "DOUBLE", "DRAW",
                                         "ELSE", "ELSEIF", "END", "END", "ENVIRON", "ERASE", "ERROR", "EXIT",
                                         "FIELD", "FOR", "FUNCTION",
                                         "GET", "GOSUB", "GOTO",
                                         "IF", "INPUT", "INPUT#", "IOCTL", "INTEGER", "IS",
                                         "KEY",
                                         "LET", "LINE", "LINE INPUT", "LINE INPUT#", "LOCATE", "LONG", "LOCK", "LOOP", "LPRINT", "LPRINT USING", "LSET",
                                         "NEXT",
                                         "ON", "ON COM", "ON KEY", "ON PEN", "ON PLAY", "ON STRIG", "ON TIMER", "ON ERROR GOTO",
                                         "OPEN", "OPEN ""COM", "OPTION", "OUT",
                                         "PAINT", "PALETTE", "PALETTE USING", "PEN", "PLAY", "POKE", "PRESET", "PSET",
                                         "PRINT", "PRINT USING", "PRINT#", "PRINT# USING", "PUT",
                                         "RSET", "RANDOMIZE", "READ", "REM", "RESTORE", "RESUME", "RETURN",
                                         "SCREEN", "SHELL", "SOUND", "STOP", "STRIG", "SWAP", "STEP", "SUB",
                                         "TO", "THEN",
                                         "UNLOCK",
                                         "VIEW", "VIEW PRINT",
                                         "WAIT", "WHILE", "WEND", "WIDTH", "WINDOW", "WRITE"}
      End Get
    End Property

    Public ReadOnly Property Functions As List(Of String) Implements IDialect.Functions
      Get
        Return New List(Of String) From {"ABS", "ASC", "ATN",
                                         "CDBL", "CHR$", "CINT", "CLNG", "COS", "CSNG", "CSRLIN", "CVD", "CVDMBF", "CVI", "CVL", "CVS", "CVSMBF",
                                         "DATE$",
                                         "ENVIRON$", "EOF", "ERDEV", "ERDEV$", "ERL", "ERR", "EXP", "EXTERR",
                                         "FILEATTR", "FIX", "FRE", "FREEFILE",
                                         "HEX$",
                                         "INKEY$", "INP", "INPUT$", "INSTR", "INT", "IOCTL$",
                                         "LCASE$", "LEFT$", "LEN", "LOC", "LOF", "LOG", "LPOS", "LTRIM$",
                                         "MID$", "MKI$", "MKS$", "MKD$",
                                         "OCT$",
                                         "PEEK", "PEN", "PLAY", "PMAP", "POINT", "POS",
                                         "RIGHT$", "RND", "RTRIM$",
                                         "SCREEN", "SGN", "SIN", "SPACE$", "SPC", "SQR", "STICK", "STR$", "STRIG", "STRING$",
                                         "TAB", "TAN", "TIMER",
                                         "UCASE$",
                                         "VAL", "VARPTR", "VARPTR$"}
      End Get
    End Property

    Public ReadOnly Property Commands As List(Of String) Implements IDialect.Commands
      Get
        Return New List(Of String) From {"FILES", "KILL",
                                         "MKDIR", "NAME", "PCOPY", "RESET",
                                         "RMDIR", "RUN", "SYSTEM", "TRON", "TROFF"}
      End Get
    End Property

    Public ReadOnly Property Variables As List(Of String) Implements IDialect.Variables
      Get
        Return New List(Of String) From {"ERDEV", "ERDEV$", "ERR", "ERL",
                                         "INKEY$",
                                         "TIME$"}
      End Get
    End Property

    Public ReadOnly Property Operators As List(Of String) Implements IDialect.Operators
      Get
        Return New List(Of String) From {"AND", "EQV", "IMP", "MOD", "NOT", "OR", "XOR"}
      End Get
    End Property

    Public ReadOnly Property Symbols As List(Of Char) Implements IDialect.Symbols
      Get
        Return New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, ","c, ";"c, ":"c, "'"c, """"c}
      End Get
    End Property

    Public ReadOnly Property GroupingOperators As List(Of Char) Implements IDialect.GroupingOperators
      Get
        Return New List(Of Char) From {"("c, ")"c}
      End Get
    End Property

    Public ReadOnly Property ArithmaticOperators As List(Of Char) Implements IDialect.ArithmaticOperators
      Get
        Return New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "\"c, "^"c}
      End Get
    End Property

    Public ReadOnly Property RelationalOperators As List(Of String) Implements IDialect.RelationalOperators
      Get
        Return New List(Of String) From {"=", "<>", "<", "<=", ">", ">=", "=<", "=>"}
      End Get
    End Property

    Public ReadOnly Property NumericSuffix As List(Of Char) Implements IDialect.NumericSuffix
      Get
        Return New List(Of Char) From {"%"c, "!"c, "#"c, "&"c} ' Integer (16bit), Single, Double, Long (32bit)
      End Get
    End Property

    Public ReadOnly Property StringSuffix As List(Of Char) Implements IDialect.StringSuffix
      Get
        Return New List(Of Char) From {"$"c}
      End Get
    End Property

    Public ReadOnly Property ReservedWords As List(Of String) Implements IDialect.ReservedWords
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

End Namespace