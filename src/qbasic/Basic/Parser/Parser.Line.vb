Namespace Global.Basic.Parser

  'Partial Class Parser

  Friend Class Line

    ' The physical line number of the original source document; 1 based.
    Public Property SourceLine As Integer
    ' If a line number exists, what is it?
    Public Property LineNumber As Integer?
    ' If a line label exists, what is it?
    Public Property LineLabel As String = Nothing
    ' Contains a collection of statements.  Each line should have one or more statements; however could have none.
    Public Property Statements As New List(Of Statement)

    Public Property Text As String

    Sub New(text As String)
      Me.Text = text
    End Sub

    Public Function Copy() As Line

      Dim result As New Line(Text) With {.SourceLine = SourceLine,
                                            .LineNumber = LineNumber,
                                            .LineLabel = LineLabel}

      For Each statement In Statements
        result.Statements.Add(statement.Copy)
      Next

      Return result

    End Function

    Public Overrides Function ToString() As String

      Dim result As String = ""

      If LineNumber IsNot Nothing Then
        result = LineNumber.ToString
      End If

      For index As Short = 0 To CShort(Statements.Count) - 1S

        For Each token In Statements(index).Tokens

          If token Is Nothing Then
            result &= " *NULL*"
          ElseIf TypeOf token Is StringLiteralToken Then
            result &= " """ & token.ToString & """"
          Else
            result &= " " & token.ToString
          End If

        Next

        If index < Statements.Count - 1 Then
          result &= " : "
        End If

      Next

      Return result

    End Function

  End Class

  'End Class

End Namespace