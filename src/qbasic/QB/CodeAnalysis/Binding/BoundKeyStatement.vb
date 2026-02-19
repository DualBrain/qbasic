Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundKeyStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As KeyStatementSyntax
    Private ReadOnly m_stringAssignment As BoundExpression

    Public Enum KeyStatementType
      KeyAssignment
      KeyDisplay
      KeyEvent
    End Enum

    Public Sub New(keyNumber As BoundExpression, verbKind As SyntaxKind)
      Me.KeyNumber = keyNumber
      Me.VerbKind = verbKind
      Me.StatementType = KeyStatementType.KeyEvent
    End Sub

    Public Sub New(syntax As KeyStatementSyntax, keyNumber As BoundExpression, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.KeyNumber = keyNumber
      Me.VerbKind = verbKind
      Me.StatementType = KeyStatementType.KeyEvent
    End Sub

    Public Sub New(syntax As KeyStatementSyntax, keyNumber As BoundExpression, stringAssignment As BoundExpression, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.KeyNumber = keyNumber
      m_stringAssignment = stringAssignment
      Me.VerbKind = verbKind
      Me.StatementType = KeyStatementType.KeyAssignment
    End Sub

    Public Sub New(syntax As KeyStatementSyntax, verbKind As SyntaxKind)
      m_syntax = syntax
      Me.VerbKind = verbKind
      Me.StatementType = KeyStatementType.KeyDisplay
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.KeyStatement
    Public ReadOnly Property KeyNumber As BoundExpression
    Public ReadOnly Property StringAssignment As BoundExpression
      Get
        Return m_stringAssignment
      End Get
    End Property
    Public ReadOnly Property VerbKind As SyntaxKind
    Public ReadOnly Property StatementType As KeyStatementType

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace