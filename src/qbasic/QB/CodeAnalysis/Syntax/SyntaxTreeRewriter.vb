Imports System.Collections.Immutable
Imports System.Linq

Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Base class for rewriting QBasic syntax trees.
  ''' Follows the same patterns as BoundTreeRewriter but operates on syntax nodes.
  ''' Note: Many syntax classes are Friend, so we use Object for those cases.
  ''' </summary>
  Public MustInherit Class SyntaxTreeRewriter

    ''' <summary>
    ''' Main entry point for rewriting any syntax node.
    ''' </summary>
    Public Overridable Function Rewrite(node As SyntaxNode) As SyntaxNode
      If node Is Nothing Then Return Nothing

      Select Case node.Kind
        ' Compilation unit
        Case SyntaxKind.CompilationUnit : Return RewriteCompilationUnit(DirectCast(node, CompilationUnitSyntax))

        ' Statements (only Public accessible types)
        Case SyntaxKind.PrintStatement : Return RewritePrintStatement(DirectCast(node, PrintStatementSyntax))
        Case SyntaxKind.ExpressionStatement : Return RewriteExpressionStatement(DirectCast(node, ExpressionStatementSyntax))
        Case SyntaxKind.GotoStatement : Return RewriteGotoStatement(DirectCast(node, GotoStatementSyntax))
        Case SyntaxKind.GosubStatement : Return RewriteGosubStatement(DirectCast(node, GosubStatementSyntax))

        ' Control structures
        Case SyntaxKind.IfStatement : Return RewriteIfStatement(DirectCast(node, IfStatementSyntax))
        Case SyntaxKind.ForStatement : Return RewriteForStatement(DirectCast(node, ForStatementSyntax))
        Case SyntaxKind.DoWhileStatement : Return RewriteDoWhileStatement(DirectCast(node, DoWhileStatementSyntax))
        Case SyntaxKind.DoUntilStatement : Return RewriteDoUntilStatement(DirectCast(node, DoUntilStatementSyntax))
        Case SyntaxKind.BlockStatement : Return RewriteBlockStatement(DirectCast(node, BlockStatementSyntax))

        ' Declarations
        Case SyntaxKind.DimStatement : Return RewriteDimStatement(DirectCast(node, DimStatementSyntax))
        Case SyntaxKind.FunctionDeclaration : Return RewriteFunctionDeclaration(DirectCast(node, FunctionDeclarationSyntax))
        Case SyntaxKind.VariableDeclaration : Return RewriteVariableDeclaration(DirectCast(node, VariableDeclarationSyntax))

        ' Expressions
        Case SyntaxKind.CallExpression : Return RewriteCallExpression(DirectCast(node, CallExpressionSyntax))
        Case SyntaxKind.AssignmentExpression : Return RewriteAssignmentExpression(DirectCast(node, AssignmentExpressionSyntax))
        Case SyntaxKind.BinaryExpression : Return RewriteBinaryExpression(DirectCast(node, BinaryExpressionSyntax))
        Case SyntaxKind.IdentifierSyntax : Return RewriteIdentifier(DirectCast(node, IdentifierSyntax))
        Case SyntaxKind.LiteralExpression : Return RewriteLiteralExpression(DirectCast(node, LiteralExpressionSyntax))

        ' Friend types - handle generically
        Case SyntaxKind.CallStatement,
             SyntaxKind.InputStatement,
             SyntaxKind.ExitStatement,
             SyntaxKind.ContinueStatement,
             SyntaxKind.FunctionStatement,
             SyntaxKind.BeepStatement,
             SyntaxKind.EndStatement,
             SyntaxKind.LabelStatement
          : Return RewriteFriendStatement(node)

        Case Else
          Return node
      End Select
    End Function

    ''' <summary>
    ''' Rewrites a collection of syntax nodes efficiently using the lazy builder pattern.
    ''' </summary>
    Protected Overridable Function RewriteNodes(nodes As ImmutableArray(Of SyntaxNode)) As ImmutableArray(Of SyntaxNode)
      Dim builder As ImmutableArray(Of SyntaxNode).Builder = Nothing

      For i = 0 To nodes.Length - 1
        Dim oldNode = nodes(i)
        Dim newNode = Rewrite(oldNode)

        If newNode IsNot oldNode Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of SyntaxNode)(nodes.Length)
            For j = 0 To i - 1
              builder.Add(nodes(j))
            Next
          End If
        End If

        If builder IsNot Nothing Then
          builder.Add(newNode)
        End If
      Next

      If builder Is Nothing Then
        Return nodes
      End If

      Return builder.MoveToImmutable()
    End Function

    ''' <summary>
    ''' Rewrites a collection of statements efficiently.
    ''' </summary>
    Protected Overridable Function RewriteStatements(statements As ImmutableArray(Of StatementSyntax)) As ImmutableArray(Of StatementSyntax)
      Dim builder As ImmutableArray(Of StatementSyntax).Builder = Nothing

      For i = 0 To statements.Length - 1
        Dim oldStatement = statements(i)
        Dim newStatement = DirectCast(Rewrite(oldStatement), StatementSyntax)

        If newStatement IsNot oldStatement Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of StatementSyntax)(statements.Length)
            For j = 0 To i - 1
              builder.Add(statements(j))
            Next
          End If
        End If

        If builder IsNot Nothing Then
          builder.Add(newStatement)
        End If
      Next

      If builder Is Nothing Then
        Return statements
      End If

      Return builder.MoveToImmutable()
    End Function

    ' Top-level rewrite methods
    Protected Overridable Function RewriteCompilationUnit(node As CompilationUnitSyntax) As SyntaxNode
      Dim newMembers = RewriteMembers(node.Members)
      If newMembers.Equals(node.Members) Then
        Return node
      End If

      Return New CompilationUnitSyntax(node.SyntaxTree, newMembers, node.EndOfFileToken)
    End Function

    Protected Overridable Function RewriteMembers(members As ImmutableArray(Of MemberSyntax)) As ImmutableArray(Of MemberSyntax)
      Dim builder As ImmutableArray(Of MemberSyntax).Builder = Nothing

      For i = 0 To members.Length - 1
        Dim oldMember = members(i)
        Dim newMember = DirectCast(Rewrite(oldMember), MemberSyntax)

        If newMember IsNot oldMember Then
          If builder Is Nothing Then
            builder = ImmutableArray.CreateBuilder(Of MemberSyntax)(members.Length)
            For j = 0 To i - 1
              builder.Add(members(j))
            Next
          End If
        End If

        If builder IsNot Nothing Then
          builder.Add(newMember)
        End If
      Next

      If builder Is Nothing Then
        Return members
      End If

      Return builder.MoveToImmutable()
    End Function

    ' Statement rewrite methods
    Protected Overridable Function RewritePrintStatement(node As PrintStatementSyntax) As StatementSyntax
      Dim newNodes = RewriteNodes(node.Nodes)
      If newNodes.Equals(node.Nodes) Then
        Return node
      End If

      Return New PrintStatementSyntax(node.SyntaxTree,
                                     node.PrintKeyword,
                                     node.Pound,
                                     node.FileNumber,
                                     node.Comma,
                                     node.UsingKeyword,
                                     node.Usingformat,
                                     node.UsingSemicolon,
                                     newNodes)
    End Function

    Protected Overridable Function RewriteExpressionStatement(node As ExpressionStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteGotoStatement(node As GotoStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteGosubStatement(node As GosubStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteIfStatement(node As IfStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteForStatement(node As ForStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteDoWhileStatement(node As DoWhileStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteDoUntilStatement(node As DoUntilStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteBlockStatement(node As BlockStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteDimStatement(node As DimStatementSyntax) As StatementSyntax
      Return node
    End Function

    Protected Overridable Function RewriteFunctionDeclaration(node As FunctionDeclarationSyntax) As MemberSyntax
      Return node
    End Function

    Protected Overridable Function RewriteVariableDeclaration(node As VariableDeclarationSyntax) As SyntaxNode
      Return node
    End Function

    ''' <summary>
    ''' Handles Friend statement types generically.
    ''' </summary>
    Protected Overridable Function RewriteFriendStatement(node As SyntaxNode) As SyntaxNode
      ' For Friend types, we can only rewrite child nodes, not create new instances
      Dim hadChanges = False

      ' Check all child nodes for changes
      For Each child In node.GetChildren()
        Dim newChild = Rewrite(child)
        If newChild IsNot child Then
          hadChanges = True
          Exit For
        End If
      Next

      ' For Friend types, we can't create new instances, so return original
      ' In derived classes, override specific methods to handle transformation logic
      Return node
    End Function

    ' Expression rewrite methods
    Protected Overridable Function RewriteCallExpression(node As CallExpressionSyntax) As SyntaxNode
      Return node
    End Function

    Protected Overridable Function RewriteAssignmentExpression(node As AssignmentExpressionSyntax) As SyntaxNode
      Return node
    End Function

    Protected Overridable Function RewriteBinaryExpression(node As BinaryExpressionSyntax) As SyntaxNode
      Return node
    End Function

    Protected Overridable Function RewriteIdentifier(node As IdentifierSyntax) As SyntaxNode
      Return node
    End Function

    Protected Overridable Function RewriteLiteralExpression(node As LiteralExpressionSyntax) As SyntaxNode
      Return node
    End Function

  End Class

End Namespace