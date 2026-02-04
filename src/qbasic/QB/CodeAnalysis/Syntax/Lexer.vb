Imports System.Collections.Immutable
Imports System.Text

Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Text

Namespace Global.QB.CodeAnalysis.Syntax

  Friend NotInheritable Class Lexer

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_syntaxTree As SyntaxTree
    Private ReadOnly m_text As SourceText
    Private m_position As Integer

    Private m_start As Integer
    Private m_kind As SyntaxKind
    Private m_value As Object
    Private ReadOnly m_triviaBuilder As ImmutableArray(Of SyntaxTrivia).Builder = ImmutableArray.CreateBuilder(Of SyntaxTrivia)

    Public Sub New(tree As SyntaxTree)
      m_syntaxTree = tree
      m_text = tree.Text
    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag
      Get
        Return m_diagnostics
      End Get
    End Property

    Private ReadOnly Property Current() As Char
      Get
        Return Peek(0)
      End Get
    End Property

    Private ReadOnly Property LookAhead() As Char
      Get
        Return Peek(1)
      End Get
    End Property

    Private ReadOnly Property Peek(offset As Integer) As Char
      Get
        Dim index = m_position + offset
        If index >= m_text.Length Then Return ChrW(0)
        Return m_text(index)
      End Get
    End Property

    Public Function Lex() As SyntaxToken

      ' If at beginning of a line, see if 
      ' we have a whole number followed by nothing but whitespace/crlf.

      'If m_position = 0 Then
      '  ' beginning of file
      '  ReadLineNumber()
      'End If

      ReadTrivia(True)

      Dim leadingTrivia = m_triviaBuilder.ToImmutable
      Dim tokenStart = m_position

      ReadToken()

      Dim tokenKind = m_kind
      Dim tokenValue = m_value
      Dim tokenLength = m_position - m_start

      ReadTrivia(False)

      Dim trailingTrivia = m_triviaBuilder.ToImmutable

      Dim tokenText = If(SyntaxFacts.GetText(tokenKind), m_text.ToString(tokenStart, tokenLength))

      Return New SyntaxToken(m_syntaxTree, tokenKind, tokenStart, tokenText, tokenValue, leadingTrivia, trailingTrivia)

    End Function

    Private Sub ReadTrivia(leading As Boolean)

      m_triviaBuilder.Clear()

      Dim done = False

      While Not done

        m_start = m_position
        m_kind = SyntaxKind.BadToken
        m_value = Nothing

        Select Case Current
          Case ChrW(0)
            done = True

          Case "'"c
            ReadSingleLineComment()
          Case "_"c
            If LookAhead = ChrW(10) OrElse LookAhead = ChrW(13) OrElse LookAhead = ChrW(0) Then
              m_kind = SyntaxKind.LineContinuationTrivia
              m_position += 1
            Else
              done = True
            End If
          Case ChrW(10), ChrW(13)
            If Not leading Then done = True
            ReadLineBreak()
          Case " "c, ChrW(9) ' Short-circuit whitespace checking (common).
            ReadWhiteSpace()
          Case Else
            If Char.IsWhiteSpace(Current) Then
              ReadWhiteSpace()
            Else
              done = True
            End If
        End Select
        Dim length = m_position - m_start
        If length > 0 Then
          Dim text = m_text.ToString(m_start, length)
          Dim trivia = New SyntaxTrivia(m_syntaxTree, m_kind, m_start, text)
          m_triviaBuilder.Add(trivia)
        End If
      End While

    End Sub

    Private Sub ReadLineBreak()

      If Current = ChrW(13) AndAlso LookAhead = ChrW(10) Then
        m_position += 2
      Else
        m_position += 1
      End If

      m_kind = SyntaxKind.LineBreakTrivia

    End Sub

    Private Sub ReadWhiteSpace()

      Dim done = False
      While Not done
        Select Case Current
          Case ChrW(0), ChrW(10), ChrW(13)
            done = True
          Case Else
            If Not Char.IsWhiteSpace(Current) Then
              done = True
            Else
              m_position += 1
            End If
        End Select
      End While

      m_kind = SyntaxKind.WhiteSpaceTrivia

    End Sub

    Private Sub ReadSingleLineComment()

      m_position += 1

      Dim done = False
      While Not done
        Select Case Current
          Case ChrW(0), ChrW(13), ChrW(10)
            done = True
          Case Else
            m_position += 1
        End Select
      End While

      m_kind = SyntaxKind.SingleLineCommentTrivia

    End Sub

    Private Sub ReadToken()

      m_start = m_position
      m_kind = SyntaxKind.BadToken
      m_value = Nothing

      Select Case Current
        Case ChrW(0) : m_kind = SyntaxKind.EndOfFileToken

        'Case "%"c : m_kind = SyntaxKind.PercentToken : m_position += 1 ' Integer
        Case "&"c
          ' Handle hexadecimal, binary and octal literals.
          '      &H, &h for hexadecimal
          '      &B, &b for binary  
          '      &O, &o for octal
          '      NOTE: Allow for underscore (_) character as a group separator.
          ReadNumberLiteral()
          'm_kind = SyntaxKind.AmpersandToken : m_position += 1 ' Long
        'Case "!"c : m_kind = SyntaxKind.ExclamationToken : m_position += 1 ' Single
        Case "#"c : m_kind = SyntaxKind.PoundToken : m_position += 1 ' Double
        'Case "$"c : m_kind = SyntaxKind.DollarToken : m_position += 1 ' String
        'Case "@"c : m_kind = SyntaxKind.AtToken : m_position += 1 ' Integer

        Case "+"c : m_kind = SyntaxKind.PlusToken : m_position += 1
        Case "-"c : m_kind = SyntaxKind.MinusToken : m_position += 1
        Case "*"c : m_kind = SyntaxKind.StarToken : m_position += 1
        Case "/"c : m_kind = SyntaxKind.SlashToken : m_position += 1
        Case "\"c : m_kind = SyntaxKind.BackslashToken : m_position += 1
        Case "^"c : m_kind = SyntaxKind.HatToken : m_position += 1
        Case "("c : m_kind = SyntaxKind.OpenParenToken : m_position += 1
        Case ")"c : m_kind = SyntaxKind.CloseParenToken : m_position += 1
        Case "{"c : m_kind = SyntaxKind.OpenBraceToken : m_position += 1
        Case "}"c : m_kind = SyntaxKind.CloseBraceToken : m_position += 1
        'Case "|"c : m_kind = SyntaxKind.PipeToken : m_position += 1
        Case "="c ' = => =<
          Select Case LookAhead
            Case ">"c : m_kind = SyntaxKind.GreaterThanEqualToken : m_position += 2
            Case "<"c : m_kind = SyntaxKind.LessThanEqualToken : m_position += 2
            Case Else : m_kind = SyntaxKind.EqualToken : m_position += 1
          End Select
        Case ">"c ' > >=
          Select Case LookAhead
            Case "="c : m_kind = SyntaxKind.GreaterThanEqualToken : m_position += 2
            Case Else : m_kind = SyntaxKind.GreaterThanToken : m_position += 1
          End Select
        Case "<"c ' < <= <>
          Select Case LookAhead
            Case "="c : m_kind = SyntaxKind.LessThanEqualToken : m_position += 2
            Case ">"c : m_kind = SyntaxKind.LessThanGreaterThanToken : m_position += 2
            Case Else : m_kind = SyntaxKind.LessThanToken : m_position += 1
          End Select
        Case ","c : m_kind = SyntaxKind.CommaToken : m_position += 1
        Case ":"c : m_kind = SyntaxKind.ColonToken : m_position += 1
        Case ";"c : m_kind = SyntaxKind.SemicolonToken : m_position += 1
        Case "?"c : m_kind = SyntaxKind.QuestionToken : m_position += 1

        Case ChrW(34)
          ReadString()
        Case "0"c To "9"c
          ReadNumberToken()
        Case "."c
          If Char.IsDigit(LookAhead) Then
            ReadNumberToken()
          Else
            m_kind = SyntaxKind.PeriodToken
            m_position += 1
          End If
        Case "_"c
          ReadIdentifierOrKeyword()
        Case Else
          If Char.IsLetter(Current) OrElse Current = "$"c Then
            ReadIdentifierOrKeyword()
            'ElseIf Char.IsWhiteSpace(Current) Then
            '  ReadWhiteSpace()
          Else
            Dim span = New TextSpan(m_position, 1)
            Dim location = New TextLocation(m_text, span)
            m_diagnostics.ReportBadCharacter(location, Current)
            m_position += 1
          End If
      End Select

      'Dim length = m_position - m_start
      'Dim text = SyntaxFacts.GetText(m_kind)
      'If text Is Nothing Then text = m_text.ToString(m_start, length)
      'Return New SyntaxToken(m_kind, m_start, text, m_value)

    End Sub

    Private Sub ReadString()

      ' "Test "" dddd"

      ' skip the current quote
      m_position += 1

      Dim sb = New StringBuilder
      Dim done = False

      While Not done
        Select Case Current
          Case ChrW(0), ChrW(13), ChrW(10)
            'TODO: Determine if we want to allow unterminated string literals???
            'Dim span = New TextSpan(m_start, 1)
            'Dim location = New TextLocation(m_text, span)
            'Diagnostics.ReportUnterminatedString(location)
            done = True
          Case """"c
            If LookAhead = """"c Then
              sb.Append(Current)
              m_position += 2
            Else
              m_position += 1
              done = True
            End If
          Case Else
            sb.Append(Current)
            m_position += 1
        End Select
      End While

      m_kind = SyntaxKind.StringToken
      m_value = sb.ToString

    End Sub

    Private Sub ReadNumberToken()

      'TODO: Forced Literal Types

      ' S - Short
      ' I - Integer
      ' L - Long
      ' D - Decimal
      ' F - Single
      ' R - Double
      ' US - UShort
      ' UI - UInteger
      ' UL - ULong
      ' C - Char

      Dim decimalCount = 0
      While Char.IsDigit(Current) OrElse
            Current = "."c
        If Current = "."c Then decimalCount += 1
        If decimalCount > 1 Then Exit While
        m_position += 1
      End While

      ' Handle exponent
      If Current = "E"c OrElse Current = "e"c Then
        m_position += 1
        If Current = "+"c OrElse Current = "-"c Then
          m_position += 1
        End If
        While Char.IsDigit(Current)
          m_position += 1
        End While
      End If

      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)
      If text.Contains("."c) OrElse
         text.Contains("E"c) OrElse text.Contains("e"c) OrElse
         Current = "#"c OrElse
         Current = "!"c Then
        Dim value As Double
        If Not Double.TryParse(text, value) Then
          Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
          m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Double)
        End If
        Dim asSingle = (Current = "#")
        If Current = "#"c OrElse Current = "!" Then
          text &= Current : m_position += 1
        End If
        If asSingle Then
          m_value = CSng(value)
        Else
          m_value = value
        End If
      Else
        Dim asLong = (Current = "&"c)
        Dim hasIntegerSuffix = (Current = "%"c)
        If hasIntegerSuffix OrElse asLong Then
          m_position += 1 ' Skip the type suffix character
        End If
        Dim value As Integer
        Dim valueLong As Long
        If asLong OrElse Not Integer.TryParse(text, value) Then
          If Not Long.TryParse(text, valueLong) Then
            Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
            m_diagnostics.ReportInvalidNumber(location, text, If(asLong, TypeSymbol.Long, TypeSymbol.Integer))
          Else
            asLong = True
            m_value = valueLong
          End If
        Else
          m_value = value
        End If
      End If

      m_kind = SyntaxKind.NumberToken

    End Sub

    Private Sub ReadNumberLiteral()

      ' Handle &H (hex), &O (octal), &B (binary) literals
      m_position += 1 ' Skip the & character
      
      ' Check what kind of literal this is
      If m_position >= m_text.Length Then
        ' Just & alone, treat as bad token
        m_kind = SyntaxKind.BadToken
        Return
      End If

      Dim literalType = Char.ToUpper(Current)
      Select Case literalType
        Case "H"c ' Hexadecimal
          m_position += 1 ' Skip the H
          ReadHexDigits()
        Case "O"c ' Octal
          m_position += 1 ' Skip the O
          ReadOctalDigits()
        Case "B"c ' Binary
          m_position += 1 ' Skip the B
          ReadBinaryDigits()
        Case Else
          ' Invalid literal type
          m_kind = SyntaxKind.BadToken
          Return
      End Select

      m_kind = SyntaxKind.NumberToken

    End Sub

    Private Sub ReadHexDigits()

      Dim hasDigits = False
      While m_position < m_text.Length AndAlso
            (Char.IsDigit(Current) OrElse
             (Current >= "A"c AndAlso Current <= "F"c) OrElse
             (Current >= "a"c AndAlso Current <= "f"c) OrElse
             Current = "_"c)
        
        If Current <> "_"c Then
          hasDigits = True
        End If
        m_position += 1
      End While

      If Not hasDigits Then
        m_kind = SyntaxKind.BadToken
        Return
      End If

      ' Parse the hex value
      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)
      Dim hexPart = text.Substring(2) ' Skip &H
      
      ' Remove underscores for parsing
      hexPart = hexPart.Replace("_", "")
      
      Dim intValue As Integer
      Dim longValue As Long
      If Integer.TryParse(hexPart, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.InvariantCulture, intValue) Then
        m_value = intValue
      ElseIf Long.TryParse(hexPart, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.InvariantCulture, longValue) Then
        m_value = longValue
      Else
        ' Too large or invalid
        Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
        m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Long)
        m_kind = SyntaxKind.BadToken
      End If

    End Sub

    Private Sub ReadOctalDigits()

      Dim hasDigits = False
      While m_position < m_text.Length AndAlso
            (Current >= "0"c AndAlso Current <= "7"c OrElse Current = "_"c)
        
        If Current <> "_"c Then
          hasDigits = True
        End If
        m_position += 1
      End While

      If Not hasDigits Then
        m_kind = SyntaxKind.BadToken
        Return
      End If

      ' Parse the octal value
      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)
      Dim octalPart = text.Substring(2) ' Skip &O
      
      ' Remove underscores for parsing
      octalPart = octalPart.Replace("_", "")
      
      Try
        Dim intValue = Convert.ToInt32(octalPart, 8)
        m_value = intValue
      Catch ex As OverflowException
        Try
          Dim longValue = Convert.ToInt64(octalPart, 8)
          m_value = longValue
        Catch ex2 As OverflowException
          Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
          m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Long)
          m_kind = SyntaxKind.BadToken
        End Try
      Catch ex As ArgumentException
        Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
        m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Integer)
        m_kind = SyntaxKind.BadToken
      End Try

    End Sub

    Private Sub ReadBinaryDigits()

      Dim hasDigits = False
      While m_position < m_text.Length AndAlso
            (Current = "0"c OrElse Current = "1"c OrElse Current = "_"c)
        
        If Current <> "_"c Then
          hasDigits = True
        End If
        m_position += 1
      End While

      If Not hasDigits Then
        m_kind = SyntaxKind.BadToken
        Return
      End If

      ' Parse the binary value
      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)
      Dim binaryPart = text.Substring(2) ' Skip &B
      
      ' Remove underscores for parsing
      binaryPart = binaryPart.Replace("_", "")
      
      Try
        Dim intValue = Convert.ToInt32(binaryPart, 2)
        m_value = intValue
      Catch ex As OverflowException
        Try
          Dim longValue = Convert.ToInt64(binaryPart, 2)
          m_value = longValue
        Catch ex2 As OverflowException
          Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
          m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Long)
          m_kind = SyntaxKind.BadToken
        End Try
      Catch ex As ArgumentException
        Dim location = New TextLocation(m_text, New TextSpan(m_start, length))
        m_diagnostics.ReportInvalidNumber(location, text, TypeSymbol.Integer)
        m_kind = SyntaxKind.BadToken
      End Try

    End Sub

    Private Sub ReadIdentifierOrKeyword()

      Const suffixChars As String = "%!#&$"

      While Char.IsLetterOrDigit(Current) OrElse
            Current = "_"c OrElse Current = "."c OrElse
            suffixChars.Contains(Current) 'Current = "$"c
        If suffixChars.Contains(Current) Then
          m_position += 1
          Exit While
        Else
          m_position += 1
        End If
      End While

      Dim length = m_position - m_start
      Dim text = m_text.ToString(m_start, length)

      If False Then

        If text.Equals("select", StringComparison.CurrentCultureIgnoreCase) AndAlso Current = " "c Then

          Dim currentPosition = m_position

          ' skip spaces...
          While Current = " "c
            m_position += 1
          End While

          Dim st = m_position

          While Char.IsLetter(Current)
            m_position += 1
          End While

          Dim l = m_position - st
          Dim possible = m_text.ToString(st, l)

          Select Case possible?.ToLower
            Case "case"
              length = m_position - m_start
              text = m_text.ToString(m_start, length)
            Case Else
              m_position = currentPosition
          End Select

        End If

        If text.Equals("case", StringComparison.CurrentCultureIgnoreCase) AndAlso Current = " "c Then

          Dim currentPosition = m_position

          ' skip spaces...
          While Current = " "c
            m_position += 1
          End While

          Dim st = m_position

          While Char.IsLetter(Current)
            m_position += 1
          End While

          Dim l = m_position - st
          Dim possible = m_text.ToString(st, l)

          Select Case possible?.ToLower
            Case "else"
              length = m_position - m_start
              text = m_text.ToString(m_start, length)
            Case Else
              m_position = currentPosition
          End Select

        End If

        If text.Equals("line", StringComparison.CurrentCultureIgnoreCase) AndAlso Current = " "c Then

          Dim currentPosition = m_position

          ' skip spaces...
          While Current = " "c
            m_position += 1
          End While

          Dim st = m_position

          While Char.IsLetter(Current)
            m_position += 1
          End While

          Dim l = m_position - st
          Dim possible = m_text.ToString(st, l)

          Select Case possible?.ToLower
            Case "input"
              length = m_position - m_start
              text = m_text.ToString(m_start, length)
            Case Else
              m_position = currentPosition
          End Select

        End If

        If text.Equals("end", StringComparison.CurrentCultureIgnoreCase) AndAlso Current = " "c Then

          Dim currentPosition = m_position

          ' skip spaces...
          While Current = " "c
            m_position += 1
          End While

          Dim st = m_position

          While Char.IsLetter(Current)
            m_position += 1
          End While

          Dim l = m_position - st
          Dim possible = m_text.ToString(st, l)

          Select Case possible?.ToLower
            Case "function", "if", "sub", "def", "type", "select"
              length = m_position - m_start
              text = m_text.ToString(m_start, length)
            Case Else
              m_position = currentPosition
          End Select

        End If

        If text.Equals("exit", StringComparison.CurrentCultureIgnoreCase) AndAlso Current = " "c Then

          Dim currentPosition = m_position

          ' skip spaces...
          While Current = " "c
            m_position += 1
          End While

          Dim st = m_position

          While Char.IsLetter(Current)
            m_position += 1
          End While

          Dim l = m_position - st
          Dim possible = m_text.ToString(st, l)

          Select Case possible?.ToLower
            Case "function", "sub", "do", "for", "while"
              length = m_position - m_start
              text = m_text.ToString(m_start, length)
            Case Else
              m_position = currentPosition
          End Select

        End If

        If text.Equals("continue", StringComparison.CurrentCultureIgnoreCase) AndAlso Current = " "c Then

          Dim currentPosition = m_position

          ' skip spaces...
          While Current = " "c
            m_position += 1
          End While

          Dim st = m_position

          While Char.IsLetter(Current)
            m_position += 1
          End While

          Dim l = m_position - st
          Dim possible = m_text.ToString(st, l)

          Select Case possible?.ToLower
            Case "do", "for", "while"
              length = m_position - m_start
              text = m_text.ToString(m_start, length)
            Case Else
              m_position = currentPosition
          End Select

        End If

      End If

      Dim bol = False
      If Current = ":"c Then
        If m_position - text.Length = 0 Then
          bol = True
        Else
          Dim ch = m_text(m_position - text.Length - 1)
          bol = (ch = vbLf)
        End If
      End If

      ' Test to see if this is a Label
      If bol AndAlso
         Current = ":"c AndAlso
         LookAhead <> " "c AndAlso
         Char.IsLetter(text(0)) AndAlso
         Not text.Contains(" "c) AndAlso
         Not text.Contains("%"c) AndAlso
         Not text.Contains("!"c) AndAlso
         Not text.Contains("&"c) AndAlso
         Not text.Contains("#"c) AndAlso
         Not text.Contains("$"c) Then

        ' start of line
        ' start with a-z character (yes)
        ' no spaces or %!&#$ (yes)
        ' no reserved words?
        ' end with a colon (:) (yes)
        m_kind = SyntaxKind.Label
        m_position += 1 ' move past the colon (:)
      Else
        m_kind = SyntaxFacts.GetKeywordKind(text)
      End If

    End Sub

  End Class

End Namespace