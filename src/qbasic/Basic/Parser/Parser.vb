Option Explicit On
Option Strict On
Option Infer On

Namespace Global.Basic.Parser

  Public Class Parser

#Region "Keywords"

    Private m_reservedWords As New List(Of String)
    Private m_keywords As New List(Of String)
    Private m_functions As New List(Of String)
    Private m_commands As New List(Of String)
    Private m_variables As New List(Of String)
    Private m_operators As New List(Of String)
    Private m_symbols As New List(Of Char)
    Private m_groupingOperators As New List(Of Char)
    Private m_arithmaticOperators As New List(Of Char)
    Private m_relationalOperators As New List(Of String)
    Private m_numericSuffix As New List(Of Char)
    Private m_stringSuffix As New List(Of Char)
    Private m_ignoreAllWhiteSpace As Boolean
    Private m_supportsLabels As Boolean

    Private Sub PrepareDialect(dialect As Dialect)

      Dim d As IDialect '= Nothing
      Select Case dialect
        Case Dialect.Hybrid
          d = New Dialects.Hybrid
        Case Dialect.GWBasic
          d = New Dialects.GwBasic
        Case Dialect.QBasic
          d = New Dialects.Qbasic
        Case Dialect.AmigaBasic
          d = New Dialects.AmigaBasic
        Case Dialect.TinyBasic
          d = New Dialects.TinyBasic
        Case Else
          Throw New NotImplementedException(dialect.ToString)
      End Select

      If d IsNot Nothing Then
        m_keywords = d.Keywords
        m_functions = d.Functions
        m_commands = d.Commands
        m_variables = d.Variables
        m_operators = d.Operators
        m_symbols = d.Symbols
        m_groupingOperators = d.GroupingOperators
        m_arithmaticOperators = d.ArithmaticOperators
        m_relationalOperators = d.RelationalOperators
        m_numericSuffix = d.NumericSuffix
        m_stringSuffix = d.StringSuffix
        m_reservedWords = d.ReservedWords
        m_ignoreAllWhiteSpace = d.IgnoreAllWhiteSpace
        m_supportsLabels = d.SupportsLabels
      End If

    End Sub

#End Region

    ' Holds the scanned/parsed results in tokenized format.
    Private ReadOnly m_lines As New List(Of Line)

    ' Holds a reference to the passed in memory stream.
    Private ReadOnly m_memoryStream As System.IO.MemoryStream

    Public Sub New(input As System.IO.MemoryStream) ', dialect As Dialect)
      m_memoryStream = input
      PrepareDialect(Dialect.QBasic) 'dialect)
      ScanParse()
    End Sub

    ' Provides a means for the lines collection to be consumed by other classes.
    Friend ReadOnly Property Lines As List(Of Line)
      Get
        Return m_lines
      End Get
    End Property

    Private Sub ScanParse()

      ' Setting sourceLine to 1 since this will be used for error reporting purposes and displayed to the end user.
      Dim sourceLine As Integer = 1

      If Me.m_memoryStream.Peek() = -1 Then
        ' Blank line.
        Dim line As New Line("") With {.SourceLine = sourceLine}
        m_lines.Add(line)
        Return
      End If

      While m_memoryStream.Peek() <> -1

        'TODO: Need to take into account the _ line continuation character.

        Dim eol As Integer = FindEndOfLine()

        ' Initialize a new line.

        Dim source As String = m_memoryStream.PeekString(CInt(m_memoryStream.Position), eol - CInt(m_memoryStream.Position) + 1)

        Dim line As New Line(source) With {.SourceLine = sourceLine}

        ' Loop until we've reached the end of the current end of line pointer.

        'Debug.WriteLine(String.Format("bol: {0} {1} eol: {2} ({3})", m_memoryStream.Position, m_memoryStream.Peek(), eol, m_memoryStream.PeekAt(eol)))

        Do While m_memoryStream.Position <= eol

          ' If the beginning of the line (no statements), see if there is a line number.

          SkipWhiteSpace()

          If line.Statements.Count = 0 AndAlso
            m_memoryStream.PeekChar IsNot Nothing AndAlso
             IsNumeric(CStr(m_memoryStream.PeekChar()), False) Then
            '"0123456789".Contains(CChar(m_memoryStream.PeekChar())) AndAlso

            ' Next character is a numeric value, so it "should" be a line number (or numeric label).

            Dim token = PopToken(eol, False)
            Dim t = TryCast(token, NumericLiteralToken)

            If t IsNot Nothing Then

              line.LineNumber = CType(t.Value, Integer?)

              If m_memoryStream.Position <= eol Then
                SkipWhiteSpace()
              End If

            Else
              ' Blank line... skip.
            End If

          End If

          ' Find the end of the statement.

          'TODO: Need to take into account the _ line continuation character.

          If m_memoryStream.Position <= eol Then

            Dim eos As Integer = FindEndOfStatement(eol)

            'Debug.WriteLine(String.Format("bos: {0} {1} eos: {2} ({3})", m_memoryStream.Position, m_memoryStream.Peek(), eos, m_memoryStream.PeekAt(eos)))

            ' Initialize the statement "tree".

            Dim statement As New Statement

            ' Loop until we've reached the end of the statement pointer.

            Do While m_memoryStream.Position <= eos

              ' Get a token.

              If PeekToken(eos) IsNot Nothing Then
                ' Now parse the rest of the statement.
                statement = ParseStatement(statement, eos, eol)
              Else
                ' shouldn't have a NULL token, so this might be extra white space. move forward one character.
                m_memoryStream.Position += 1
              End If

            Loop

            'Debug.WriteLine(String.Format("Statement Finished Pos: {0}", m_memoryStream.Position))

            ' Check to see what the next character is.

            If m_memoryStream.Peek > -1 AndAlso m_memoryStream.PeekChar() = ":"c Then
              m_memoryStream.Seek(1, System.IO.SeekOrigin.Current)
            End If

            ' Add the statement to the line's statement list.

            line.Statements.Add(statement)

          End If

        Loop

        'Debug.WriteLine(String.Format("Line Finished Pos: {0}", m_memoryStream.Position))

        ' Add the line to the ScannerParser line list.

        m_lines.Add(line)

        ' Check to see if CR and/or LF exist. If so, swallow them.
        If m_memoryStream.Peek > -1 AndAlso (m_memoryStream.PeekChar() = CChar(vbLf) OrElse m_memoryStream.PeekChar() = CChar(vbCr)) Then
          m_memoryStream.Seek(1, System.IO.SeekOrigin.Current)
        End If
        If m_memoryStream.Peek > -1 AndAlso (m_memoryStream.PeekChar() = CChar(vbLf) OrElse m_memoryStream.PeekChar() = CChar(vbCr)) Then
          m_memoryStream.Seek(1, System.IO.SeekOrigin.Current)
        End If

        ' Increment the sourceLine counter
        sourceLine += 1 'TODO: Need to take into account the _ line continuation character.

      End While

    End Sub

    Private Function ParseStatement(ByRef statement As Statement, eos As Integer, eol As Integer) As Statement

      Dim result As Statement = statement

      ' Depending on the initial statement keyword, parse the reset of the statement 
      ' text accordingly checking for any syntax errors along the way.

      'If PeekToken(eos).IsCommentToken Then
      '  Dim token = PopToken(eos)
      '  result.Tokens.Add(token)
      '  result.Tokens(0) = ParseRem(DirectCast(result.Tokens(0), CommentToken), eol)
      'ElseIf PeekToken(eos).IsWord("REM") Then
      '  Dim token = PopToken(eos)
      '  token = ParseRem(New CommentToken, eol)
      '  result.Tokens.Add(token)

      Dim peek = PeekToken(eos)

      If peek.IsCommentToken OrElse
         peek.IsWord("REM") Then

        PopToken(eos, True)
        Dim token = ParseRem(New CommentToken, eol)
        result.Tokens.Add(token)

      ElseIf peek.IsWord("DATA") Then
        PopToken(eos, True)
        Dim token = ParseData(New DataToken, eol)
        result.Tokens.Add(token)

      ElseIf peek.IsWord("PUT#") OrElse
             peek.IsWord("GET#") OrElse
             peek.IsWord("PRINT#") OrElse
             peek.IsWord("WRITE#") OrElse
             peek.IsWord("INPUT#") Then
        Dim token = PopToken(eos, True)
        Dim keyword As String = token.ToString : keyword = keyword.Substring(0, keyword.Length - 1)
        result.Tokens.Add(New KeywordToken With {.Value = keyword})
        result.Tokens.Add(New HashToken)
        result = ParseGeneric(result, eos)
        'ElseIf PeekToken(eos).IsWord("GET#") Then
        '  Dim token = PopToken(eos)
        '  result.Tokens.Add(New KeywordToken With {.Value = "GET"})
        '  result.Tokens.Add(New HashToken)
        '  result = ParseGeneric(result, eos)
        'ElseIf PeekToken(eos).IsWord("PRINT#") Then
        '  Dim token = PopToken(eos)
        '  result.Tokens.Add(New KeywordToken With {.Value = "PRINT"})
        '  result.Tokens.Add(New HashToken)
        '  result = ParseGeneric(result, eos)
        'ElseIf PeekToken(eos).IsWord("WRITE#") Then
        '  Dim token = PopToken(eos)
        '  result.Tokens.Add(New KeywordToken With {.Value = "WRITE"})
        '  result.Tokens.Add(New HashToken)
        '  result = ParseGeneric(result, eos)
        'ElseIf PeekToken(eos).IsWord("INPUT#") Then
        '  Dim token = PopToken(eos)
        '  result.Tokens.Add(New KeywordToken With {.Value = "INPUT"})
        '  result.Tokens.Add(New HashToken)
        '  result = ParseGeneric(result, eos)

      ElseIf peek.IsIdentifierToken Then

        result.Tokens.Add(New KeywordToken With {.Value = "LET"})
        Dim token = PopToken(eos, True)
        result.Tokens.Add(token)
        result = ParseGeneric(result, eos)

      ElseIf peek.IsLabelToken Then

        Dim token = PopToken(eos, False)
        result.Tokens.Add(token)
        'result = ParseGeneric(result, eos)

      ElseIf peek.IsKeywordToken OrElse
             peek.IsCommandToken OrElse
             peek.IsFunctionToken OrElse
             peek.IsVariableToken OrElse
             peek.IsStringLiteralToken Then

        Dim token = PopToken(eos, True)
        result.Tokens.Add(token)
        result = ParseGeneric(result, eos)

        'ElseIf PeekToken(eos).IsFunctionToken Then

        '  Dim token = PopToken(eos)
        '  result.Tokens.Add(token)
        '  result = ParseGeneric(result, eos)

        'ElseIf PeekToken(eos).IsVariableToken Then

        '  Dim token = PopToken(eos)
        '  result.Tokens.Add(token)
        '  result = ParseGeneric(result, eos)

      Else

        ' Replace the token with a syntax error token.
        Dim token As New SyntaxErrorToken With {.Code = 0,
                                                .Message = String.Format("Unknown token '{0}' encountered.", peek)}
        'result.Tokens.RemoveAt(0) : result.Tokens.Insert(0, token)
        result.Tokens.Add(token)
        ' Set the stream pointer to the end of the current statement.
        m_memoryStream.Position = eos + 1

      End If

      Return result

    End Function

#Region "Parse Commands/Keywords/Etc."

    Private Function ParseGeneric(statement As Statement, eos As Integer) As Statement

      Dim result As Statement = statement

      Do While MoreTokens(eos)
        Dim token = PopToken(eos, statement IsNot Nothing AndAlso statement.Tokens.Count > 0)
        If token IsNot Nothing Then
          result.Tokens.Add(token)
        End If
      Loop

      Return result

    End Function

    Private Function ParseRem(token As CommentToken, eol As Integer) As Token

      ' Dartmouth:
      '   USAGE: REM <any string of characters whatsoever>
      '   EXAMPLE: 10 REM THIS IS THE END OF APPENDIX C

      ' Read reset of line and update the comment token's value.

      Dim length As Integer = (eol - CShort(m_memoryStream.Position)) + 1

      Dim comment As String = Nothing

      If length > 0 Then comment = m_memoryStream.ReadString(length)

      ' Set the value to hold the comment.
      token.Value = comment

      ' Return the token.
      Return token

    End Function

    Private Function ParseData(token As DataToken, eol As Integer) As Token

      ' Read rest of line and update the data token's value.

      Dim length As Integer = (eol - CShort(m_memoryStream.Position)) + 1

      Dim data As String = Nothing

      If length > 0 Then data = m_memoryStream.ReadString(length)

      ' Set the value to hold the comment.
      token.Value = data

      ' Return the token.
      Return token

    End Function

#End Region

    'Private Function ContainsSyntaxError(statement As Statement) As Boolean
    '  Return TypeOf statement.Tokens(statement.Tokens.Count - 1) Is SyntaxErrorToken
    'End Function

    'Private Function SyntaxError(statement As Statement, reason As String, eos As Integer) As Statement

    '  Dim Token = New SyntaxErrorToken With {.Code = 2,
    '                                         .Message = "Syntax Error",
    '                                         .Reason = reason}
    '  statement.Tokens.Add(Token)
    '  m_memoryStream.Position = eos + 1

    '  Return statement

    'End Function

    Private Function MoreTokens(eos As Integer) As Boolean
      Return m_memoryStream.Position <= eos
    End Function

    Private Function PeekToken(eos As Integer, Optional ByRef position As Integer = 0, Optional isStatement As Boolean = False) As Token

      Dim source As New Text.StringBuilder

      While Not m_memoryStream.EndOfStream(position) AndAlso m_memoryStream.Position + position <= eos

        ' Scan individual tokens

        Dim ch As Char? = m_memoryStream.PeekChar(position)

        If ch = CChar(vbLf) OrElse ch = CChar(vbCr) Then

          ' Found a Linefeed.

          position += 1

          ch = m_memoryStream.PeekChar(position)

          If ch = CChar(vbCr) OrElse
             ch = CChar(vbLf) Then
            position += 1
          End If

          Return Nothing

        ElseIf Char.IsWhiteSpace(CChar(ch)) Then

          ' Skip any whitespace (other than CR/LF, which is handled elsewhere).

          Do Until Not Char.IsWhiteSpace(CChar(m_memoryStream.PeekChar(position)))

            position += 1

            If m_memoryStream.EndOfStream(position) Then
              Exit Do
            End If

          Loop

        ElseIf ch = "?"c Then

          ' Print keyword.

          position += 1

          Return New KeywordToken() With {.Value = "PRINT"}

        ElseIf ch = ":"c Then

          ' Statement seperator.

          position += 1

          Return New EndOfStatementToken()

        ElseIf ch = "'"c Then

          ' Comment character,

          position += 1

          Return New CommentToken()

        ElseIf Char.IsLetter(CChar(ch)) OrElse ch = "_"c Then

          ' A letter...

          Dim name1 As String = ch.ToString.ToUpper
          Dim seeker As Integer = 1

          If Not m_ignoreAllWhiteSpace Then

            ' Honor whitespace as a word separator...

            ' Scan forward to determine word.
            Do

              If (m_memoryStream.Position + position) + seeker <= eos Then

                ch = m_memoryStream.PeekChar(position + seeker)

                If Char.IsWhiteSpace(CChar(ch)) Then
                  seeker -= 1
                  Exit Do
                ElseIf ch = CChar(vbCr) OrElse ch = CChar(vbLf) Then ' End of line.
                  seeker -= 1
                  Exit Do
                ElseIf m_symbols.Contains(CChar(ch)) OrElse
                       m_arithmaticOperators.Contains(CChar(ch)) OrElse
                       m_groupingOperators.Contains(CChar(ch)) OrElse
                       m_relationalOperators.Contains(CChar(ch)) Then
                  seeker -= 1
                  Exit Do
                Else
                  name1 &= ch.ToString.ToUpper
                End If

                seeker += 1

              Else
                seeker -= 1
                Exit Do
              End If

            Loop

            ' Now that we have a "word", determine if it is a keyword, function, command, etc.

            If name1.Length > 2 AndAlso name1.IndexOf("#").Between(1, name1.Length - 1) Then

              Do

                If name1(name1.Length - 1) = "#"c Then
                  Exit Do
                End If

                seeker -= 1
                name1 = name1.Substring(0, name1.Length - 1)

              Loop

            End If

            If m_reservedWords.Contains(name1) Then

              position += seeker + 1

              'Dim l = From p In m_allwords
              '        Where p.Length > name.Length AndAlso
              '              p.StartsWith(name)
              '        Order By p.Length Descending

              'If l.Count > 0 Then
              '  For Each keyword In l
              '    Dim peekWord As String = keyword.Substring(name.Length)
              '    If String.Compare(m_memoryStream.PeekString(position, peekWord.Length), peekWord, StringComparison.CurrentCultureIgnoreCase) = 0 Then
              '      ' found a match.
              '      name = keyword
              '      position += peekWord.Length
              '      Exit For
              '    End If
              '  Next
              'End If

              If m_keywords.Contains(name1) Then
                Return New KeywordToken() With {.Value = name1}
              ElseIf m_functions.Contains(name1) Then
                Return New FunctionToken() With {.Value = name1}
              ElseIf m_commands.Contains(name1) Then
                Return New CommandToken() With {.Value = name1}
              ElseIf m_variables.Contains(name1) Then
                Return New VariableToken() With {.Value = name1}
              ElseIf m_operators.Contains(name1) Then
                Return New ArithmaticOperatorToken() With {.Value = name1}
              End If

              Stop

            Else

              ' Set position accordingly...

              position += seeker + 1

              ch = m_memoryStream.PeekChar(position)

              If m_supportsLabels AndAlso ch = ":"c AndAlso Not isStatement Then

                position += 1
                Return New LabelToken() With {.Value = name1}

              Else

                ' If a variable, (which it should be if we got this far), check to see if there is a suffix.
                If m_memoryStream.Position + position <= eos Then
                  ch = m_memoryStream.PeekChar(position)
                  If m_numericSuffix.Contains(CChar(ch)) OrElse
                     m_stringSuffix.Contains(CChar(ch)) Then
                    name1 &= ch
                    position += 1
                  End If
                End If

                Return New IdentifierToken() With {.Value = name1}

              End If

            End If

          Else

            ' Old School...

            Do

              If (m_memoryStream.Position + position) + seeker <= eos Then

                ch = m_memoryStream.PeekChar(position + seeker)

                If Char.IsWhiteSpace(CChar(ch)) Then
                  ' Remark out the following line and white space will be completely ignored.
                  ' Otherwise, white space is honored as a "end of word" point; however, partials are still checked so 10PRINTABS(-1); will continue to be recognized.
                  If Not m_ignoreAllWhiteSpace Then
                    Exit Do
                  End If
                ElseIf ch = CChar(vbCr) OrElse ch = CChar(vbLf) Then ' End of line.
                  seeker -= 1
                  Exit Do
                ElseIf m_symbols.Contains(CChar(ch)) OrElse
                       m_arithmaticOperators.Contains(CChar(ch)) OrElse
                       m_groupingOperators.Contains(CChar(ch)) OrElse
                       m_relationalOperators.Contains(CChar(ch)) Then
                  seeker -= 1
                  Exit Do
                Else

                  name1 &= ch.ToString.ToUpper

                  If m_memoryStream.Position + position + seeker + 1 <= eos Then
                    Dim cch = m_memoryStream.PeekChar(position + seeker + 1)
                    If m_numericSuffix.Contains(CChar(cch)) OrElse
                       m_stringSuffix.Contains(CChar(cch)) Then
                      seeker += 1
                      name1 &= cch
                    End If
                  End If

                  If m_reservedWords.Contains(name1) Then

                    position += seeker + 1

                    Dim l = From p In m_reservedWords
                            Where p.Length > name1.Length AndAlso
                                  p.StartsWith(name1)
                            Order By p.Length Descending

                    If l.Any Then
                      For Each keyword In l
                        Dim peekWord As String = keyword.Substring(name1.Length)
                        If String.Compare(m_memoryStream.PeekString(position, peekWord.Length), peekWord, StringComparison.CurrentCultureIgnoreCase) = 0 Then
                          ' found a match.
                          name1 = keyword
                          position += peekWord.Length
                          Exit For
                        End If
                      Next
                    End If

                    If m_keywords.Contains(name1) Then
                      Return New KeywordToken() With {.Value = name1}
                    ElseIf m_functions.Contains(name1) Then
                      Return New FunctionToken() With {.Value = name1}
                    ElseIf m_commands.Contains(name1) Then
                      Return New CommandToken() With {.Value = name1}
                    ElseIf m_variables.Contains(name1) Then
                      Return New VariableToken() With {.Value = name1}
                    ElseIf m_operators.Contains(name1) Then
                      Return New ArithmaticOperatorToken() With {.Value = name1}
                    Else
                      Stop
                    End If

                  End If

                End If

                seeker += 1

              Else
                seeker -= 1
                Exit Do
              End If

            Loop

            position += seeker + 1

            ' If a variable, (which it should be if we got this far), check to see if there is a suffix.
            If m_memoryStream.Position + position <= eos Then
              ch = m_memoryStream.PeekChar(position)
              If m_numericSuffix.Contains(CChar(ch)) OrElse
                 m_stringSuffix.Contains(CChar(ch)) Then
                name1 &= ch
                position += 1
              End If
            End If

            Return New IdentifierToken() With {.Value = name1}

          End If

        ElseIf ch = """"c Then

          ' string literal

          Try

            Dim accum As New Text.StringBuilder()

            ' skip the '"'
            position += 1

            If m_memoryStream.EndOfStream(position) Then
              ' Nothing more available.
              'Return New SyntaxErrorToken() With {.Code = 2, .Message = "Syntax error", .Reason = "Nothing after opening character."}
              ' This is an empty string...
              Return New StringLiteralToken() With {.Value = ""}
            Else

              ' Read the next character.

              While m_memoryStream.PeekChar(position) <> """"c ' InlineAssignHelper(ch, ChrW(input.Peek())) <> Chr(34)

                accum.Append(m_memoryStream.PeekChar(position))
                position += 1

                If m_memoryStream.EndOfStream(position) Then 'm_memoryStream.Peek = -1 Then
                  Exit While
                End If

              End While

              'Dim syntaxError As Boolean

              If Not m_memoryStream.EndOfStream(position) Then ' m_memoryStream.Peek() <> -1 Then
                ' skip the terminating "
                'm_memoryStream.Seek(1, IO.SeekOrigin.Current)
                position += 1
                'Else
                '  ' Missing trailing "
                '  syntaxError = True
              End If

              'If syntaxError Then
              '  Return New SyntaxErrorToken() With {.Code = 2, .Message = "Syntax error", .Reason = "Missing closing character."}
              'Else
              Return New StringLiteralToken() With {.Value = accum.ToString()}
              'End If

            End If

          Catch ex As Exception

            Return New SyntaxErrorToken() With {.Code = 0, .Message = ex.ToString}

          End Try

        ElseIf CChar(ch) = "&" Then

          ' Possible a Hex numeric value.

          position += 1

          Dim cch As Char = CChar(m_memoryStream.PeekChar(position))

          If cch = "H"c OrElse cch = "h"c Then

            position += 1

            Dim accum As New Text.StringBuilder

            While "01234567890ABCDEFabcdef".IndexOf(CChar(m_memoryStream.PeekChar(position))) > -1

              accum.Append(m_memoryStream.PeekChar(position))
              position += 1

              If m_memoryStream.EndOfStream(position) Then
                Exit While
              End If

            End While

            Dim result As String = accum.ToString

            If result <> "" Then
              Return New NumericLiteralToken() With {.Value = CLng("&H" & result).ToString()}
            Else
              Return New SyntaxErrorToken() With {.Code = 0,
                                                  .Message = String.Format("Invalid hex value '{0}'.", ch)}
            End If

          Else

            If cch = "O"c OrElse cch = "o"c Then
              position += 1
            End If

            Dim accum As New Text.StringBuilder

            While "01234567".IndexOf(CChar(m_memoryStream.PeekChar(position))) > -1

              accum.Append(m_memoryStream.PeekChar(position))
              position += 1

              If m_memoryStream.EndOfStream(position) Then
                Exit While
              End If

            End While

            Dim result As String = accum.ToString

            If result <> "" Then

              Dim r As Long = 0
              Dim p As Long = 0

              For index As Integer = result.Length - 1 To 0 Step -1
                If index = result.Length - 1 Then
                  r = CInt(result(index).ToString)
                Else
                  r += CLng(CLng(result(index).ToString) * (8 ^ p))
                End If
                p += 1
              Next

              Return New NumericLiteralToken() With {.Value = r.ToString()}

            Else
              Return New SyntaxErrorToken() With {.Code = 0,
                                                  .Message = String.Format("Invalid hex value '{0}'.", ch)}
            End If

          End If

        ElseIf Char.IsDigit(CChar(ch)) Then

          ' Handles E notation...

          Dim accum As New Text.StringBuilder

          Dim eNotationAllowed As Boolean = True
          Dim eNotationEncountered As Boolean = False

          While Char.IsDigit(CChar(m_memoryStream.PeekChar(position))) OrElse
                m_memoryStream.PeekChar(position) = "."c

            accum.Append(m_memoryStream.PeekChar(position))
            position += 1

            If m_memoryStream.EndOfStream(position) Then
              Exit While
            End If

          End While

          If m_memoryStream.PeekChar(position) = "E"c OrElse
             m_memoryStream.PeekChar(position) = "e"c Then
            If m_memoryStream.PeekChar(position + 1) = "-"c Then

              ' Handles E notation...

              accum.Append(m_memoryStream.PeekChar(position))
              position += 1

              accum.Append(m_memoryStream.PeekChar(position))
              position += 1

              While Char.IsDigit(CChar(m_memoryStream.PeekChar(position)))

                accum.Append(m_memoryStream.PeekChar(position))
                position += 1

                If m_memoryStream.EndOfStream(position) Then
                  Exit While
                End If

              End While

            End If
          End If

          If Not m_memoryStream.EndOfStream(position) AndAlso m_numericSuffix.Contains(CChar(m_memoryStream.PeekChar(position))) Then
            accum.Append(m_memoryStream.PeekChar(position))
            position += 1
          End If

          Return New NumericLiteralToken() With {.Value = accum.ToString()}

        Else

          If ch = "#"c Then
            position += 1 : Return New HashToken()
          ElseIf ch = ","c Then
            position += 1 : Return New CommaToken()
          ElseIf ch = "."c Then
            position += 1 : Return New PeriodToken()
          ElseIf ch = ";"c Then
            position += 1 : Return New SemiColonToken()
          ElseIf ch = "("c Then
            position += 1 : Return New ParenOpenToken()
          ElseIf ch = ")"c Then
            position += 1 : Return New ParenCloseToken()
          ElseIf m_arithmaticOperators.Contains(CChar(ch)) Then
            position += 1 : Return New ArithmaticOperatorToken With {.Value = CStr(ch)}
          ElseIf m_relationalOperators.Contains(CChar(ch)) Then

            position += 1

            ' Could be a single or multiple character.

            Dim singles = From p In m_relationalOperators
                          Where p.Length = 1 AndAlso
                                (Aggregate a In m_relationalOperators
                                 Where p <> a AndAlso
                                       a.StartsWith(p)
                                 Into Count()) = 0

            If singles.Contains(CChar(ch)) Then
              Return New RelationalOperatorToken With {.Value = CStr(ch)}
            Else

              Dim seek As Integer = 0
              Dim cch As Char = ChrW(0)
              Do
                If Char.IsWhiteSpace(CChar(m_memoryStream.PeekChar(position + seek))) Then
                  seek += 1
                Else
                  cch = CChar(m_memoryStream.PeekChar(position + seek))
                  Exit Do
                End If
                If m_memoryStream.EndOfStream(position) Then
                  Exit Do
                End If
              Loop

              Dim peekValue As String = Nothing '= String.Format("{0}{1}", ch, cch) 'm_memoryStream.PeekChar(position))
              If ch = "="c AndAlso cch = ">"c Then
                peekValue = String.Format("{0}{1}", cch, ch)
              ElseIf ch = "="c AndAlso cch = "<"c Then
                peekValue = String.Format("{0}{1}", cch, ch)
              Else
                peekValue = String.Format("{0}{1}", ch, cch)
              End If

              Dim l = From p In m_relationalOperators
                      Where p.Length = 2 AndAlso
                            p = peekValue

              If l.Count = 1 Then
                position += 1 + seek : Return New RelationalOperatorToken With {.Value = peekValue}
              Else
                Return New RelationalOperatorToken With {.Value = CStr(ch)}
              End If

            End If

          Else

            position += 1
            Return New SyntaxErrorToken() With {.Code = 0,
                                                .Message = String.Format("Encountered unrecognized character '{0}'.", ch)}

          End If

        End If

      End While

      Return Nothing

    End Function

    Private Function PopToken(eos As Integer, isStatement As Boolean) As Token

      Dim position As Integer = 0
      Dim token = PeekToken(eos, position, isStatement)
      m_memoryStream.Seek(position, System.IO.SeekOrigin.Current)
      Return token

    End Function

    Private Function FindEndOfStatement(eol As Integer) As Integer

      ' Returns last character of the statement.

      'TODO: Need to take into account the _ line continuation character.

      ' Set the initial result to be the same as the end of line.
      Dim result As Integer = eol

      ' Store the current position.
      Dim pos As Short = CShort(m_memoryStream.Position)

      ' Now scan to see if there is :, ', REM or ELSE characters between current position and the end of line.

      Dim inQuot As Boolean = False

      Do

        Select Case m_memoryStream.PeekChar()
          Case ChrW(34)
            inQuot = Not inQuot
          Case ":"c
            If Not inQuot Then
              result = CShort(m_memoryStream.Position) - 1
              Exit Do
            End If

          Case "'"c
            If Not inQuot AndAlso m_memoryStream.Position > pos Then
              result = CShort(m_memoryStream.Position) - 1
              Exit Do
            End If

          Case "E"c, "e"c
            If Not inQuot AndAlso
               String.Compare(m_memoryStream.PeekString(0, 4), "ELSE", StringComparison.CurrentCultureIgnoreCase) = 0 AndAlso
               m_memoryStream.Position > pos Then
              result = CShort(m_memoryStream.Position) - 1
              Exit Do
            End If

          Case "R"c, "r"c
            If Not inQuot AndAlso
               String.Compare(m_memoryStream.PeekString(0, 3), "REM", StringComparison.CurrentCultureIgnoreCase) = 0 AndAlso
               m_memoryStream.Position > pos Then
              result = CShort(m_memoryStream.Position) - 1
              Exit Do
            End If

          Case Else

        End Select

        ' Still working through loop, so increment position.
        m_memoryStream.Seek(1, System.IO.SeekOrigin.Current)

        If Me.m_memoryStream.Peek() = -1 Then
          ' Nothing more in the stream... done.
          Exit Do
        ElseIf m_memoryStream.Position > eol Then
          Exit Do
        End If

      Loop

      ' Set stream back to original state.
      m_memoryStream.Position = pos

      Return result

    End Function

    Private Function FindEndOfLine() As Integer

      ' Returns the position of the last character of the "line" (before CR/LF).

      'TODO: Need to take into account the _ line continuation character.

      Dim result As Short '= -1 ' Should never end up as a -1; but doing this just to be sure.

      Dim pos As Short = CShort(m_memoryStream.Position)

      Do
        If m_memoryStream.Peek() = -1 Then
          ' end of stream
          result = CShort(m_memoryStream.Length - 1)
          Exit Do
        Else
          Select Case CStr(m_memoryStream.PeekChar())
            Case vbLf, vbCr
              result = CShort(m_memoryStream.Position - 1)
              Exit Do
            Case Else
              m_memoryStream.Seek(1, System.IO.SeekOrigin.Current)
          End Select
        End If
      Loop

      m_memoryStream.Position = pos

      Return result

    End Function

    Private Sub SkipWhiteSpace()

      Do

        If Me.m_memoryStream.Peek() = -1 Then
          Exit Do
        ElseIf Char.IsWhiteSpace(CChar(m_memoryStream.PeekChar())) Then
          m_memoryStream.Seek(1, System.IO.SeekOrigin.Current)
        Else
          Exit Do
        End If

      Loop

    End Sub

  End Class

End Namespace