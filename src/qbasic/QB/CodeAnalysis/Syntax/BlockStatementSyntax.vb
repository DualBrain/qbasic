Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Public NotInheritable Class BlockStatementSyntax
    Inherits StatementSyntax

    Sub New(tree As SyntaxTree,
            openBraceToken As SyntaxToken,
            statements As ImmutableArray(Of StatementSyntax),
            closeBraceToken As SyntaxToken)
      MyBase.New(tree)
      Me.OpenBraceToken = openBraceToken
      Me.Statements = statements
      Me.CloseBraceToken = closeBraceToken
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind = SyntaxKind.BlockStatement
    Public ReadOnly Property OpenBraceToken As SyntaxToken
    Public ReadOnly Property Statements As ImmutableArray(Of StatementSyntax)
    Public ReadOnly Property CloseBraceToken As SyntaxToken

  End Class

  Public Class DeclarationStatementSyntax
    Inherits StatementSyntax

    Public Sub New(tree As SyntaxTree)
      MyBase.New(tree)
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind

  End Class

  Public NotInheritable Class EndBlockStatementSyntax
    Inherits DeclarationStatementSyntax

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="tree"></param>
    ''' <param name="endKeyword">The "End" keyword</param>
    ''' <param name="blockKeyword">The keyword that ends the block. Must be one of: "If", "Using", "With", "Select", "Structure", "Enum", "Interface", "Class", "Module", "Namespace", "Sub", "Function", "Get, "Set", "Property", "Operator", "Event", "AddHandler", "RemoveHandler", "RaiseEvent", "While", "Try" or "SyncLock".</param>
    Public Sub New(tree As SyntaxTree, endKeyword As SyntaxToken, blockKeyword As SyntaxToken)
      MyBase.New(tree)
      Me.EndKeyword = endKeyword
      Me.BlockKeyword = blockKeyword
      Select Case blockKeyword.Kind
        Case SyntaxKind.IfKeyword : Kind = SyntaxKind.EndIfStatement
        Case SyntaxKind.FunctionKeyword : Kind = SyntaxKind.EndFunctionStatement
        Case SyntaxKind.SelectKeyword : Kind = SyntaxKind.EndSelectStatement
        Case SyntaxKind.SubKeyword : Kind = SyntaxKind.EndSubStatement
        Case SyntaxKind.TypeKeyword : Kind = SyntaxKind.EndTypeStatement
          'Case SyntaxKind.UsingKeyword : Kind = SyntaxKind.EndUsingStatement
          'Case SyntaxKind.WithKeyword : Kind = SyntaxKind.EndWithStatement
          'Case SyntaxKind.StructureKeyword : Kind = SyntaxKind.EndStructureStatement
          'Case SyntaxKind.EnumKeyword : Kind = SyntaxKind.EndEnumStatement
          'Case SyntaxKind.InterfaceKeyword : Kind = SyntaxKind.EndInterfaceStatement
          'Case SyntaxKind.ClassKeyword : Kind = SyntaxKind.EndClassStatement
          'Case SyntaxKind.ModuleKeyword : Kind = SyntaxKind.EndModuleStatement
          'Case SyntaxKind.NamespaceKeyword : Kind = SyntaxKind.EndNamespaceStatement
          'Case SyntaxKind.GetKeyword : Kind = SyntaxKind.EndGetStatement
          'Case SyntaxKind.SetKeyword : Kind = SyntaxKind.EndSetStatement
          'Case SyntaxKind.PropertyKeyword : Kind = SyntaxKind.EndPropertyStatement
          'Case SyntaxKind.OperatorKeyword : Kind = SyntaxKind.EndOperatorStatement
          'Case SyntaxKind.EventKeyword : Kind = SyntaxKind.EndEventStatement
          'Case SyntaxKind.AddHandlerKeyword : Kind = SyntaxKind.EndAddHandlerStatement
          'Case SyntaxKind.RemoveHandlerKeyword : Kind = SyntaxKind.EndRemoveHandlerStatement
          'Case SyntaxKind.RaiseEventKeyword : Kind = SyntaxKind.EndRaiseEventStatement
          'Case SyntaxKind.WhileKeyword : Kind = SyntaxKind.EndWhileStatement
          'Case SyntaxKind.TryKeyword : Kind = SyntaxKind.EndTryStatement
          'Case SyntaxKind.SyncLockKeyword : Kind = SyntaxKind.EndSyncLockStatement
        Case Else
          Throw New InvalidOperationException
      End Select
    End Sub

    Public Overrides ReadOnly Property Kind As SyntaxKind
    ''' <summary>
    ''' The "End" keyword
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property EndKeyword As SyntaxToken
    ''' <summary>
    ''' The keyword that ends the block. Must be one of: "If", "Using", "With", "Select", "Structure", "Enum", "Interface", "Class", "Module", "Namespace", "Sub", "Function", "Get, "Set", "Property", "Operator", "Event", "AddHandler", "RemoveHandler", "RaiseEvent", "While", "Try" or "SyncLock".
    ''' </summary>
    ''' <returns></returns>
    Public ReadOnly Property BlockKeyword As SyntaxToken

  End Class

End Namespace