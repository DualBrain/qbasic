Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundWidthStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As WidthStatementSyntax

    Public Sub New(syntax As WidthStatementSyntax, columns As BoundExpression, lines As BoundExpression)
      m_syntax = syntax
      Me.Columns = columns
      Me.Lines = lines
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WidthStatement
    Public ReadOnly Property Columns As BoundExpression
    Public ReadOnly Property Lines As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

  Friend NotInheritable Class BoundWidthFileStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As WidthFileStatementSyntax

    Public Sub New(syntax As WidthFileStatementSyntax, fileNumber As BoundExpression, columns As BoundExpression)
      m_syntax = syntax
      Me.FileNumber = fileNumber
      Me.Columns = columns
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WidthFileStatement
    Public ReadOnly Property FileNumber As BoundExpression
    Public ReadOnly Property Columns As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

  Friend NotInheritable Class BoundWidthLprintStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As WidthLprintStatementSyntax

    Public Sub New(syntax As WidthLprintStatementSyntax, columns As BoundExpression)
      m_syntax = syntax
      Me.Columns = columns
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.WidthLprintStatement
    Public ReadOnly Property Columns As BoundExpression

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
