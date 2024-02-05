Namespace Global.Basic.Parser.Dialects

  'Partial Class Parser

  '  ' Dartmouth ' circa 1964
  '  '
  '  '  Variable names consist of a single capital letter possibly follwed by a single digit.
  '  '  Numbers may contain up to 9 digits with our without a decimal point and possibly with a minus sign.
  '  '  Numbers may be expressed in E notation.
  '  '  Expressions are calculated a) inner parens out, raising power, multiply/divide (left to right) and plus/minus (left to right).
  '  '  LOG and SQR will make the parameter positive before calculation.
  '  '  Print zones are defined as five zones of 15 spaces.
  '  '  Print statement will do a new line unless last character is a comma.
  '  '  Each print value will consume one or more zone, next item starts at the next zone.
  '  '  Array names and variable names may be the same, A and A(1) are two different "variables".
  '  '  Single- and multi-dimensional arrays can not have the same name.
  '  '  An array defaults to 0 to 10 in size unless otherwise defined using DIM.
  '  '  Recursive/chaining GOSUBS are not supported.
  '  '  Limitations:
  '  '    Length of program can not exceed "about two feet of teletype paper filled with BASIC statements".  V4 states < 8000 for chars, variable space, etc. combined.
  '  '    Constants and printed labels must not exceed 175.  V4 states 100.
  '  '    No more than 300 DATA numbers.
  '  '    Length of printed labels can not exceed something slightly less than 600 characters.
  '  '    No more than 26 total FOR statements in a program.
  '  '    Total number of GOTO and IF/THEN statements cannot exceed 80.
  '  '    Total number of elements in all arrays can not exceed 1500.

  '  ' Versions: 
  '  '   v1 June 1964
  '  '   v2 October 1964 - Added 0 as subscript to arrays and semi-colon to the PRINT statement.
  '  '   v3 1966 - Added INPUT, MAT, RESTORE, NUM, DET
  '  '   v4 1969 - Added text manipulation and variables, also known as string variables. RND without param (0 to 1).
  '  '   v5 1970 - Added true file handling.
  '  '   v6 1971 - Added separately compilable procedures with parameters (this version represents the version most later BASIC dialects descend.
  '  '      1976 - Steve Garland added structured programming features to create Dartmouth SBASIC.
  '  '   v7 1979 - Using results of SBASIC, released version that formed the basis of ANSI BASIC.
  '  '      1979 - Kemeny and Kurtz formed True BASIC.

  '  ' V2

  '  Private m_dartmouthTwoKeywords As New List(Of String) From {"LET", "READ", "DATA", "PRINT", "GOTO", "GO", "IF", "THEN", "FOR", "TO", "STEP", "NEXT",
  '                                                              "END", "STOP", "DEF", "GOSUB", "RETURN", "DIM", "REM"}
  '  Private m_dartmouthTwoFunctions As New List(Of String) From {"ABS", "ATN", "COS", "EXP", "INT", "LOG", "RND", "SIN", "SQR", "TAN"}
  '  Private m_dartmouthTwoCommands As New List(Of String) From {"STOP", "LIST", "SAVE", "UNSAVE", "CATALOG", "NEW", "OLD", "SCRATCH", "RENAME"}
  '  Private m_dartmouthTwoVariables As New List(Of String)
  '  Private m_dartmouthTwoOperators As New List(Of String)
  '  Private m_dartmouthTwoSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, "$"c, """"c}
  '  Private m_dartmouthTwoGroupingOperators As New List(Of Char) From {"("c, ")"c}
  '  Private m_dartmouthTwoArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c}
  '  Private m_dartmouthTwoRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">="}
  '  Private m_dartmouthTwoIdentifierSuffix As New List(Of Char)
  '  Private m_dartmouthTwoIgnoreAllWhiteSpace As Boolean = False

  '  ' V4

  '  Private m_dartmouthFourKeywords As New List(Of String) From {"LET", "READ", "DATA", "PRINT", "GOTO", "GO", "IF", "THEN", "FOR", "TO", "STEP", "NEXT",
  '                                                               "END", "STOP", "DEF", "GOSUB", "RETURN", "DIM", "REM", "ON", "INPUT", "RESTORE", "MAT"}
  '  Private m_dartmouthFourFunctions As New List(Of String) From {"ABS", "ATN", "COS", "COT", "DET", "EXP", "INT", "LOG", "NUM", "RND", "SGN", "SIN", "SQR", "TAN"}
  '  Private m_dartmouthFourCommands As New List(Of String) From {"STOP", "LIST", "SAVE", "UNSAVE", "CATALOG", "NEW", "OLD", "SCRATCH", "RENAME"}
  '  Private m_dartmouthFourVariables As New List(Of String)
  '  Private m_dartmouthFourOperators As New List(Of String)
  '  Private m_dartmouthFourSymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c, "="c, "("c, ")"c, "<"c, ">"c, "."c, ","c, ";"c, """"c}
  '  Private m_dartmouthFourGroupingOperators As New List(Of Char) From {"("c, ")"c}
  '  Private m_dartmouthFourArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "^"c}
  '  Private m_dartmouthFourRelationalOperators As New List(Of String) From {"=", "<>", "<", "<=", ">", ">="}
  '  Private m_dartmouthFourIdentifierSuffix As New List(Of Char)
  '  Private m_dartmouthFourIgnoreAllWhiteSpace As Boolean = False

  '  ' CARDBASIC ' circa 1964

  'End Class

End Namespace