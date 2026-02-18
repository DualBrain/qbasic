Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundDefTypeStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As DefTypeStatementSyntax

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DefTypeStatement

    Public ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Ranges As List(Of (Char, Char))

    Public Sub New(defType As TypeSymbol, ranges As List(Of (Char, Char)))
      Me.Type = defType
      Me.Ranges = ranges
    End Sub

    Public Sub New(syntax As DefTypeStatementSyntax, defType As TypeSymbol, ranges As List(Of (Char, Char)))
      m_syntax = syntax
      Me.Type = defType
      Me.Ranges = ranges
    End Sub

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
