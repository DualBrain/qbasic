Namespace Global.Basic.Parser.Dialects

  'Partial Class Parser

  ' BASICA ' circa 1981

  ' GW-BASIC ' circa 1983 - 1986

  'Private m_gwKeywords As New List(Of String) From {"BEEP", "CALL", "CHAIN", "CIRCLE", "CLOSE", "CLS", "COLOR", "COM", "COMMON",
  '                                                  "DATA", "DEF", "DEFINT", "DEFSNG", "DEFDBL", "DEFSTR",
  '                                                  "DIM", "DRAW", "END", "ENVIRON", "ERASE", "ERROR", "FIELD", "FOR", "TO", "STEP", "NEXT", "GET",
  '                                                  "GOSUB", "GOTO", "IF", "THEN", "ELSE", "INPUT", "INPUT#", "IOCTL", "KEY", "LET", "LINE",
  '                                                  "LINE INPUT", "LINE INPUT#", "LOCATE", "LOCK", "LPRINT", "LPRINT USING", "LSET", "RSET",
  '                                                  "ON", "ON COM", "ON KEY", "ON PEN", "ON PLAY", "ON STRIG", "ON TIMER", "ON ERROR GOTO",
  '                                                  "OPEN", "OPEN ""COM", "OPTION", "OUT", "PAINT", "PALETTE", "PALETTE USING",
  '                                                  "PEN", "PLAY", "POKE", "PRESET", "PSET", "PRINT", "PRINT USING", "PRINT#", "PRINT# USING",
  '                                                  "PUT", "RANDOMIZE", "READ", "REM", "RESTORE", "RESUME", "RETURN", "SCREEN", "SHELL",
  '                                                  "SOUND", "STOP", "STRIG", "SWAP", "UNLOCK", "VIEW", "VIEW PRINT", "WAIT", "WHILE", "WEND",
  '                                                  "WIDTH", "WINDOW", "WRITE"}
  'Private m_gwFunctions As New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CHR$", "CINT", "COS", "CSNG", "CVI", "CVS", "CVD",
  '                                                   "ENVIRON$", "EOF", "EXP", "EXTERR", "FIX", "FRE", "HEX$", "INP", "INPUT$", "INSTR",
  '                                                   "INT", "IOCTL$", "LCASE$", "LEFT$", "LEN", "LOC", "LOF", "LOG", "LPOS", "MID$", "MKI$", "MKS$", "MKD$",
  '                                                   "OCT$", "PEEK", "PEN", "PLAY", "PMAP", "POINT", "POS", "RIGHT$", "RND", "SCREEN", "SGN",
  '                                                   "SIN", "SPACE$", "SPC", "SQR", "STICK", "STR$", "STRIG", "STRING$", "TAB", "TAN", "TIMER",
  '                                                   "UCASE$", "USR", "VAL", "VARPTR", "VARPTR$", "LTRIM$", "RTRIM$"}
  'Private m_gwCommands As New List(Of String) From {"AUTO", "BLOAD", "BSAVE", "CHDIR", "CLEAR", "CONT", "DELETE", "EDIT", "FILES", "KILL",
  '                                                  "LIST", "LLIST", "LOAD", "MERGE", "MKDIR", "NAME", "NEW", "PCOPY", "RENUM", "RESET",
  '                                                  "RMDIR", "RUN", "SAVE", "SYSTEM", "TRON", "TROFF", "VER", "HELP", "KEYWORDS", "OLD", "PARSER"}
  'Private m_gwVariables As New List(Of String) From {"CSRLIN", "DATE$", "ERDEV", "ERDEV$", "ERR", "ERL", "INKEY$", "TIME$"}
  'Private m_gwOperators As New List(Of String) From {"NOT", "AND", "OR", "XOR", "EQV", "IMP", "MOD"}
  ''Private m_gwSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, ":"c, "'"c, """"c}
  'Private m_gwSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, ","c, ";"c, ":"c, "'"c, """"c}
  'Private m_gwGroupingOperators As New List(Of Char) From {"("c, ")"c}
  'Private m_gwArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "\"c, "^"c}
  'Private m_gwRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">=", "=<", "=>"}
  'Private m_gwIdentifierSuffix As New List(Of Char) From {"$"c, "%"c, "!"c, "#"c}
  'Private m_gwIgnoreAllWhiteSpace As Boolean = False

  ' Single - 7 or fewer digits, exponential form using E or suffix of !.  (46.8, -1.09E-06, 3489.0, 22.5!)
  ' Double - 8 or more digits, exponential form using D or suffix of #. (345692811, -1.09432D-06, 3490.0#, 7654321.1234)

  ' Any variables beginning with the letters in the following lists are assumed to be
  ' the respective types unless the variables are defined/using a suffix identifier.
  ' The suffix always takes precedence.
  ' Usage: DEFINT A
  '        DEFSNG A-L
  '        DEFSTR A,B,C

  ' NOTE: If DEF??? is used, the suffix is assumed; meaning that if DEFSTR A, 
  ' A$ and A are the same variable.  However, if DEFINT A, A$ is different than A.

  Friend Class GwBasic
    Implements IDialect

    Private ReadOnly m_extended As Boolean = True

    Public ReadOnly Property Keywords As System.Collections.Generic.List(Of String) Implements IDialect.Keywords
      Get

        ' "DEF FN", "DEF SEG", "DEF USR" -- handled by the "DEF".
        Dim list = New List(Of String) From {"BEEP", "CALL", "CHAIN", "CIRCLE", "CLOSE", "CLS", "COLOR", "COM", "COMMON",
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

        If m_extended Then
          list.Add("SLEEP")

          'GPIO
          list.Add("DELAY")
          list.Add("PINMODE")
          list.Add("DIGITALREAD")
          list.Add("DIGITALWRITE")
        End If

        Return list

      End Get
    End Property

    Public ReadOnly Property Functions As System.Collections.Generic.List(Of String) Implements IDialect.Functions
      Get

        Dim list = New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CHR$", "CINT", "COS", "CSNG", "CVI", "CVS", "CVD",
                                             "ENVIRON$", "EOF", "EXP", "EXTERR", "FIX", "FRE", "HEX$", "INP", "INPUT$", "INSTR",
                                             "INT", "IOCTL$", "LEFT$", "LEN", "LOC", "LOF", "LOG", "LPOS", "MID$", "MKI$", "MKS$", "MKD$",
                                             "OCT$", "PEEK", "PEN", "PLAY", "PMAP", "POINT", "POS", "RIGHT$", "RND", "SCREEN", "SGN",
                                             "SIN", "SPACE$", "SPC", "SQR", "STICK", "STR$", "STRIG", "STRING$", "TAB", "TAN", "TIMER",
                                             "USR", "VAL", "VARPTR", "VARPTR$"}

        If m_extended Then
          list.Add("UCASE$")
          list.Add("LCASE$")
          list.Add("UBOUND")
          list.Add("LBOUND")
          list.Add("CLNG")
        End If

        Return list

      End Get
    End Property

    Public ReadOnly Property Commands As System.Collections.Generic.List(Of String) Implements IDialect.Commands
      Get

        Dim list = New List(Of String) From {"AUTO", "BLOAD", "BSAVE", "CHDIR", "CLEAR", "CONT", "DELETE", "EDIT", "FILES", "KILL",
                                             "LIST", "LLIST", "LOAD", "MERGE", "MKDIR", "NAME", "NEW", "PCOPY", "RENUM", "RESET",
                                             "RMDIR", "RUN", "SAVE", "SYSTEM", "TRON", "TROFF", "HELP", "FACEBOOK"}

        If m_extended Then
          ' Extensions
          'list.Add("SYSTEM.IO.FILE.CONVERTTOPACKAGE")
          'list.Add("SYSTEM.IO.FOLDER.CONVERTTOPACKAGE")
          'list.Add("SYSTEM.IO.PACKAGE.CONVERTTOFOLDER")
          'list.Add("SYSTEM.IO.PACKAGE.SETSTARTUP")
        End If

        Return list

      End Get
    End Property

    Public ReadOnly Property Variables As System.Collections.Generic.List(Of String) Implements IDialect.Variables
      Get

        Dim list = New List(Of String) From {"CSRLIN", "DATE$", "ERDEV", "ERDEV$", "ERR", "ERL", "INKEY$", "TIME$"}

        If m_extended Then

        End If

        Return list

      End Get
    End Property

    Public ReadOnly Property Operators As System.Collections.Generic.List(Of String) Implements IDialect.Operators
      Get

        Dim list = New List(Of String) From {"NOT", "AND", "OR", "XOR", "EQV", "IMP", "MOD"}

        If m_extended Then

        End If

        Return list

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
        Return False
      End Get
    End Property

  End Class

  'End Class

End Namespace