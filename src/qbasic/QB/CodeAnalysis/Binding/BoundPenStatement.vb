Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundPenStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As PenStatementSyntax

    Public Sub New(verbKind As SyntaxKind)
      Me.VerbKind = verbKind
    End Sub

    Public Sub New(syntax As PenStatementSyntax, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.PenStatement
    Public ReadOnly Property VerbKind As SyntaxKind

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
