Namespace Global.Basic.Parser.Dialects

  'Partial Class Parser

  '  ' Microsoft BASIC (MBASIC) ' circa 1979

  '  Private m_mits4kKeywords As New List(Of String) From {"CLEAR", "DATA", "DIM", "END", "FOR", "GOSUB", "GOTO", "IF", "INPUT", "LET", "NEXT",
  '                                                        "PRINT", "READ", "REM", "RESTORE", "RETURN", "STEP", "STOP", "THEN", "TO"}
  '  Private m_mits4kFunctions As New List(Of String) From {"ABS", "INT", "SGN", "SIN", "SQR", "TAB", "TAN", "USR"}
  '  Private m_mits4kCommands As New List(Of String) From {"LIST", "NEW", "RUN"}
  '  Private m_mits4kVariables As New List(Of String)
  '  Private m_mits4kOperators As New List(Of String)
  '  Private m_mits4kSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, ":"c, "'"c, """"c}
  '  Private m_mits4kGroupingOperators As New List(Of Char) From {"("c, ")"c}
  '  Private m_mits4kArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c}
  '  Private m_mits4kRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">="}
  '  Private m_mits4KIdentifierSuffix As New List(Of Char)
  '  Private m_mits4kIgnoreAllWhiteSpace As Boolean = True

  '  ' Added If-Goto, 
  '  '       On-Gosub
  '  '       On-Goto
  '  Private m_mits8kKeywords As New List(Of String) From {"CLEAR", "DATA", "DEF", "DIM", "END", "FOR", "GOSUB", "GOTO", "IF", "INPUT",
  '                                                        "LET", "NEXT", "ON", "PEEK", "POKE", "PRINT", "READ", "REM", "RESTORE", "RETURN",
  '                                                        "STEP", "STOP", "THEN", "TO", "WAIT"}
  '  Private m_mits8kFunctions As New List(Of String) From {"ABS", "ASC", "ATN", "CHR$", "EQV", "EXP", "IMP", "INP", "INT", "LEFT$",
  '                                                         "LEN", "LOG", "MID$", "POS", "RIGHT$", "SGN", "SIN", "SPACES$", "SPC", "SQR",
  '                                                         "STR$", "TAB", "TAN", "USR", "VAL"}
  '  Private m_mits8kCommands As New List(Of String) From {"CLOAD", "CONT", "EDIT", "LIST", "NEW", "RUN"}
  '  Private m_mits8kVariables As New List(Of String)
  '  Private m_mits8kOperators As New List(Of String) From {"AND", "NOT", "OR", "XOR"}
  '  Private m_mits8kSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, ":"c, "'"c, """"c}
  '  Private m_mits8kGroupingOperators As New List(Of Char) From {"("c, ")"c}
  '  Private m_mits8kArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c}
  '  Private m_mits8kRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">="}
  '  Private m_mits8KIdentifierSuffix As New List(Of Char) From {"$"c}
  '  Private m_mits8kIgnoreAllWhiteSpace As Boolean = True

  '  ' Added If-Then-Else
  '  Private m_mitsExtendedKeywords As New List(Of String) From {"CLEAR", "DATA", "DEF", "DEFDBL", "DEFINT", "DEFSNG", "DEFSTR", "DEFUSR", "DIM", "END",
  '                                                              "FOR", "GOSUB", "GOTO", "IF", "INPUT", "LET", "LINE INPUT", "LPOS", "LPRINT", "LPRINT USING",
  '                                                              "NEXT", "ON ERROR GOTO", "ON", "PEEK", "POKE", "PRINT", "PRINT USING", "READ", "REM",
  '                                                              "RESTORE", "RESUME", "RESUME NEXT", "RETURN", "STEP", "STOP", "SWAP", "THEN", "TO",
  '                                                              "TROFF", "TRON", "VARPTR", "WAIT", "WIDTH"}
  '  Private m_mitsExtendedFunctions As New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CHR$", "CINT", "CSNG", "EQV", "EXP", "FIX", "FRCINT",
  '                                                               "HEX$", "IMP", "INP", "INSTR", "INT", "LEFT$", "LEN", "LOG", "MAKEINT", "MID$", "OCT$",
  '                                                               "POS", "RIGHT$", "SGN", "SIN", "SPACES$", "SPC", "SQR", "STR$", "STRING$", "TAB", "TAN",
  '                                                               "USR", "VAL"}
  '  Private m_mitsExtendedCommands As New List(Of String) From {"AUTO", "CLOAD", "CONSOLE", "CONT", "DELETE", "EDIT", "ERASE", "LIST", "LLIST", "NEW",
  '                                                              "RENUM", "RUN"}
  '  Private m_mitsExtendedVariables As New List(Of String) From {"ERL", "ERR"}
  '  Private m_mitsExtendedOperators As New List(Of String) From {"AND", "MOD", "NOT", "OR", "XOR"}
  '  Private m_mitsExtendedSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, ":"c, "'"c, """"c}
  '  Private m_mitsExtendedGroupingOperators As New List(Of Char) From {"("c, ")"c}
  '  Private m_mitsExtendedArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c}
  '  Private m_mitsExtendedRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">="}
  '  Private m_mitsExtendedIdentifierSuffix As New List(Of Char) From {"$"c, "%"c, "!"c, "#"c, "&"c}
  '  Private m_mitsExtendedIgnoreAllWhiteSpace As Boolean = True

  '  Private m_mitsDiskKeywords As New List(Of String) From {"CLEAR", "DATA", "DEF", "DEFDBL", "DEFINT", "DEFSNG", "DEFSTR", "DEFUSR", "DIM", "END",
  '                                                          "FOR", "GOSUB", "GOTO", "IF", "INPUT", "LET", "LINE INPUT", "LPOS", "LPRINT", "LPRINT USING",
  '                                                          "NEXT", "ON ERROR GOTO", "ON", "PEEK", "POKE", "PRINT", "PRINT USING", "READ", "REM",
  '                                                          "RESTORE", "RESUME", "RESUME NEXT", "RETURN", "STEP", "STOP", "SWAP", "THEN", "TO",
  '                                                          "TROFF", "TRON", "VARPTR", "WAIT", "WIDTH"}
  '  Private m_mitsDiskFunctions As New List(Of String) From {"ABS", "ASC", "ATN", "CDBL", "CHR$", "CINT", "CSNG", "EQV", "EXP", "FIX", "FRCINT",
  '                                                           "HEX$", "IMP", "INP", "INSTR", "INT", "LEFT$", "LEN", "LOG", "MAKEINT", "MID$", "OCT$",
  '                                                           "POS", "RIGHT$", "SGN", "SIN", "SPACES$", "SPC", "SQR", "STR$", "STRING$", "TAB", "TAN",
  '                                                           "USR", "VAL"}
  '  Private m_mitsDiskCommands As New List(Of String) From {"AUTO", "CLOAD", "CONSOLE", "CONT", "DELETE", "EDIT", "ERASE", "LIST", "LLIST", "NEW",
  '                                                          "RENUM", "RUN"}
  '  Private m_mitsDiskVariables As New List(Of String) From {"ERL", "ERR"}
  '  Private m_mitsDiskOperators As New List(Of String) From {"AND", "MOD", "NOT", "OR", "XOR"}
  '  Private m_mitsDiskSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, ":"c, "'"c, """"c}
  '  Private m_mitsDiskGroupingOperators As New List(Of Char) From {"("c, ")"c}
  '  Private m_mitsDiskArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c}
  '  Private m_mitsDiskRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">="}
  '  Private m_mitsDiskIdentifierSuffix As New List(Of Char) From {"$"c, "%"c, "!"c, "#"c, "&"c}
  '  Private m_mitsDiskIgnoreAllWhiteSpace As Boolean = True

  'End Class

End Namespace