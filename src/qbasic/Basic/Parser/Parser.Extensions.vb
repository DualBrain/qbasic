Imports System.Runtime.CompilerServices

Namespace Global.Basic.Parser

  Friend Module ParserExtensions

    <Extension()>
    Public Function IsStringExpression(token As Token, defStrList As List(Of Char), numericSuffixList As List(Of Char)) As Boolean

      ' Symbol is not blank,
      '        has a trailing $ or
      '        has a " at the start or
      '        exists in the list of string variable names and
      '        does not have a trailing %!#

      Dim text As String = token.ToString

      Return token.IsStringLiteralToken OrElse
           (text <> "" AndAlso
            ((text(text.Length - 1) = "$"c OrElse
              text(0) = ChrW(34) OrElse
              defStrList.Contains(text(0))) AndAlso
            Not numericSuffixList.Contains(text(text.Length - 1))))

    End Function

    '<Extension()>
    'Public Function IsLineNumberOrLabel(ByVal token As Parser.Token, ByVal labels As Dictionary(Of String, Integer)) As Boolean
    '  If TypeOf token Is Parser.IdentifierToken Then
    '    Return labels.ContainsKey(token.ToString)
    '  ElseIf TypeOf token Is Parser.NumericLiteralToken Then
    '    Return True
    '  Else
    '    Return False
    '  End If
    'End Function

    <Extension()>
    Public Function IsWord(token As Token, word As String) As Boolean

      If TypeOf token Is KeywordToken OrElse
       TypeOf token Is FunctionToken OrElse
       TypeOf token Is CommandToken OrElse
       TypeOf token Is IdentifierToken OrElse
       TypeOf token Is RelationalOperatorToken OrElse
       TypeOf token Is ArithmaticOperatorToken Then

        Dim words As New List(Of String)

        If word.IndexOf("|") > -1 Then
          'Dim list = Split(word, "|")
          Dim list = word.Split("|")
          For Each entry In list
            words.Add(entry)
          Next
        Else
          words.Add(word)
        End If

        For Each entry In words
          If String.Compare(entry, token.ToString, StringComparison.CurrentCultureIgnoreCase) = 0 Then
            Return True
          End If
        Next

      End If

      Return False

    End Function

    <Extension()>
    Public Function IsHashToken(token As Token) As Boolean
      Return TypeOf token Is HashToken
    End Function

    <Extension()>
    Public Function IsCommaToken(token As Token) As Boolean
      Return TypeOf token Is CommaToken
    End Function

    <Extension()>
    Public Function IsLabelToken(token As Token) As Boolean
      Return TypeOf token Is LabelToken
    End Function

    <Extension()>
    Public Function IsIdentifierToken(token As Token) As Boolean
      Return TypeOf token Is IdentifierToken
    End Function

    <Extension()>
    Public Function IsFunctionToken(token As Token) As Boolean
      Return TypeOf token Is FunctionToken
    End Function

    <Extension()>
    Public Function IsVariableToken(token As Token) As Boolean
      Return TypeOf token Is VariableToken
    End Function

    <Extension()>
    Public Function IsParenOpenToken(token As Token) As Boolean
      Return TypeOf token Is ParenOpenToken
    End Function

    <Extension()>
    Public Function IsParenCloseToken(token As Token) As Boolean
      Return TypeOf token Is ParenCloseToken
    End Function

    <Extension()>
    Public Function IsSemiColonToken(token As Token) As Boolean
      Return TypeOf token Is SemiColonToken
    End Function

    <Extension()>
    Public Function IsDash(token As Token) As Boolean
      Return TypeOf token Is ArithmaticOperatorToken AndAlso
                  token.ToString = "-"
    End Function

    <Extension()>
    Public Function IsKeywordToken(token As Token) As Boolean
      Return TypeOf token Is KeywordToken
    End Function

    <Extension()>
    Public Function IsCommandToken(token As Token) As Boolean
      Return TypeOf token Is CommandToken
    End Function

    <Extension()>
    Public Function IsCommentToken(token As Token) As Boolean
      Return TypeOf token Is CommentToken
    End Function

    <Extension()>
    Public Function IsNumericLiteralToken(token As Token) As Boolean
      Return TypeOf token Is NumericLiteralToken
    End Function

    <Extension()>
    Public Function IsPeriodToken(token As Token) As Boolean
      Return TypeOf token Is PeriodToken
    End Function

    <Extension()>
    Public Function IsStringLiteralToken(token As Token) As Boolean
      Return TypeOf token Is StringLiteralToken
    End Function

  End Module

End Namespace