Namespace Global.Basic.Parser.Dialects

  'Partial Class Parser

  ' TinyBASIC ' circa 1976

  ' Notes:
  '   LET is inferred.
  '   Whitespace can be *anywhere*.
  '   Integers only?

  'Private m_tinyKeywords As New List(Of String) From {"PRINT", "PR", "IF", "THEN", "GOTO", "INPUT",
  '                                                    "LET", "GOSUB", "RETURN", "END", "REM"}
  'Private m_tinyFunctions As New List(Of String) From {"RND", "USR"}
  'Private m_tinyCommands As New List(Of String) From {"CLEAR", "LIST", "RUN"}
  'Private m_tinyVariables As New List(Of String)
  'Private m_tinyOperators As New List(Of String)
  'Private m_tinySymbols As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "="c, "("c, ")"c, "<"c, ">"c, ","c, ";"c, """"c}
  'Private m_tinyGroupingOperators As New List(Of Char) From {"("c, ")"c}
  'Private m_tinyArithmaticOperators As New List(Of Char) From {"+"c, "-"c, "*"c, "/"c}
  'Private m_tinyRelationalOperators As New List(Of String) From {"=", "<>", "><", "<", "<=", ">", ">="}
  'Private m_tinyIdentifierSuffix As New List(Of Char)
  'Private m_tinyIgnoreAllWhiteSpace As Boolean = True

  Friend Class TinyBasic
    Implements IDialect

    Public ReadOnly Property Keywords As System.Collections.Generic.List(Of String) Implements IDialect.Keywords
      Get
        Return New List(Of String) From {"PRINT", "PR", "IF", "THEN", "GOTO", "INPUT",
                                         "LET", "GOSUB", "RETURN", "END", "REM"}
      End Get
    End Property

    Public ReadOnly Property Functions As System.Collections.Generic.List(Of String) Implements IDialect.Functions
      Get
        Return New List(Of String) From {"RND", "USR"}
      End Get
    End Property

    Public ReadOnly Property Commands As System.Collections.Generic.List(Of String) Implements IDialect.Commands
      Get
        Return New List(Of String) From {"CLEAR", "LIST", "RUN"}
      End Get
    End Property

    Public ReadOnly Property Variables As System.Collections.Generic.List(Of String) Implements IDialect.Variables
      Get
        Return New List(Of String)
      End Get
    End Property

    Public ReadOnly Property Operators As System.Collections.Generic.List(Of String) Implements IDialect.Operators
      Get
        Return New List(Of String)
      End Get
    End Property

    Public ReadOnly Property Symbols As System.Collections.Generic.List(Of Char) Implements IDialect.Symbols
      Get
        Return New List(Of Char) From {"+"c, "-"c, "*"c, "/"c, "="c, "("c, ")"c, "<"c, ">"c, ","c, ";"c, """"c}
      End Get
    End Property

    Public ReadOnly Property GroupingOperators As System.Collections.Generic.List(Of Char) Implements IDialect.GroupingOperators
      Get
        Return New List(Of Char) From {"("c, ")"c}
      End Get
    End Property

    Public ReadOnly Property ArithmaticOperators As System.Collections.Generic.List(Of Char) Implements IDialect.ArithmaticOperators
      Get
        Return New List(Of Char) From {"+"c, "-"c, "*"c, "/"c}
      End Get
    End Property

    Public ReadOnly Property RelationalOperators As System.Collections.Generic.List(Of String) Implements IDialect.RelationalOperators
      Get
        Return New List(Of String) From {"=", "<>", "><", "<", "<=", ">", ">="}
      End Get
    End Property

    Public ReadOnly Property NumericSuffix As System.Collections.Generic.List(Of Char) Implements IDialect.NumericSuffix
      Get
        Return New List(Of Char)
      End Get
    End Property

    Public ReadOnly Property StringSuffix As System.Collections.Generic.List(Of Char) Implements IDialect.StringSuffix
      Get
        Return New List(Of Char)
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
    '    Return result
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
        Return True
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