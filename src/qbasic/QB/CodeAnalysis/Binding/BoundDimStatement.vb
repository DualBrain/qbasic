Imports System.Collections.Immutable
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Binding

  Friend Class BoundDimStatement
    Inherits BoundStatement

    Private ReadOnly m_syntax As DimStatementSyntax

    Public Sub New(declarations As ImmutableArray(Of BoundVariableDeclaration), isShared As Boolean)
      Me.Declarations = declarations
      Me.IsShared = isShared
    End Sub

    Public Sub New(syntax As DimStatementSyntax, declarations As ImmutableArray(Of BoundVariableDeclaration), isShared As Boolean)
      m_syntax = syntax
      Me.Declarations = declarations
      Me.IsShared = isShared
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.DimStatement
    Public ReadOnly Property Declarations As ImmutableArray(Of BoundVariableDeclaration)
    Public ReadOnly Property IsShared As Boolean

    Public Overrides ReadOnly Property Syntax As StatementSyntax
      Get
        Return m_syntax
      End Get
    End Property

  End Class

End Namespace
