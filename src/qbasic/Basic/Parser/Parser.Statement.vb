Namespace Global.Basic.Parser

  'Partial Class Parser

  Friend Class Statement

    Public Property Tokens As New List(Of Token)

    Public Function Copy() As Statement

      Dim result As New Statement

      For Each token In Tokens
        If token IsNot Nothing Then
          result.Tokens.Add(token.Copy)
        Else
          result.Tokens.Add(Nothing)
        End If
      Next

      Return result

    End Function

  End Class

  'End Class

End Namespace