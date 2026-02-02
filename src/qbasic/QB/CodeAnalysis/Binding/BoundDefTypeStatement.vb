Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDefTypeStatement
    Inherits BoundStatement

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DefTypeStatement

    Public ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Ranges As List(Of (Char, Char))

    Public Sub New(defType As TypeSymbol, ranges As List(Of (Char, Char)))
      Me.Type = defType
      Me.Ranges = ranges
    End Sub

  End Class

End Namespace