Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundTimerStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As TimerStatementSyntax

    Public Sub New(verbKind As SyntaxKind)
      Me.VerbKind = verbKind
    End Sub

    Public Sub New(syntax As TimerStatementSyntax, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.TimerStatement
    Public ReadOnly Property VerbKind As SyntaxKind

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace