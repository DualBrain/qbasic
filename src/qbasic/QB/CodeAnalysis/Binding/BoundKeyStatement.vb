Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundKeyStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As KeyStatementSyntax

    Public Sub New(keyNumber As BoundExpression, verbKind As SyntaxKind)
      Me.KeyNumber = keyNumber
      Me.VerbKind = verbKind
    End Sub

    Public Sub New(syntax As KeyStatementSyntax, keyNumber As BoundExpression, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.KeyNumber = keyNumber
      Me.VerbKind = verbKind
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.KeyStatement
    Public ReadOnly Property KeyNumber As BoundExpression
    Public ReadOnly Property VerbKind As SyntaxKind

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace