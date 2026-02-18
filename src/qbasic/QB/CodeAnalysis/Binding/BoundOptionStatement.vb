Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundOptionStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As OptionStatementSyntax

    Public Sub New(number As Integer)
      Me.Number = number
    End Sub

    Public Sub New(syntax As OptionStatementSyntax, number As Integer)
      m_syntax = syntax
      Me.Number = number
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.OptionStatement
    Public ReadOnly Property Number As Integer

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
