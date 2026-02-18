'Imports System.Collections.Immutable
Imports System.Collections.Immutable

Imports QB.CodeAnalysis.Binding
Imports QB.CodeAnalysis.Symbols
'Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax

Namespace Global.QB.CodeAnalysis.Lowering

  ' TODO: Consider creating a BoundNodeFactory to construct nodes to make lowering easier to read.
  Friend NotInheritable Class Lowerer
    Inherits BoundTreeRewriter

    Private Const GENERATED_LABEL_PREFIX As String = "$$LABEL"

    Private m_labelCount As Integer = 0

    Private Sub New()
    End Sub

    Private Function GenerateLabel() As BoundLabel
      m_labelCount += 1
      Dim name = $"{GENERATED_LABEL_PREFIX}{m_labelCount}"
      Return New BoundLabel(name)
    End Function

    Public Shared Function Lower(func As FunctionSymbol, statement As BoundStatement) As BoundBlockStatement
      Dim lowerer = New Lowerer
      Dim result = lowerer.RewriteStatement(statement)
      Return RemoveDeadCode(Flatten(func, result))
    End Function

    Private Shared Function Flatten(func As FunctionSymbol, statement As BoundStatement) As BoundBlockStatement
      Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)
      Dim stack = New Stack(Of BoundStatement)
      stack.Push(statement)
      While stack.Count > 0
        Dim current = stack.Pop
        If TypeOf current Is BoundBlockStatement Then
          For Each s In DirectCast(current, BoundBlockStatement).Statements.Reverse()
            stack.Push(s)
          Next
        Else
          builder.Add(current)
        End If
      End While
      If func.Type Is TypeSymbol.Nothing Then
        If builder.Count = 0 OrElse CanFallThrough(builder.Last) Then
          builder.Add(New BoundReturnStatement())
        End If
      End If
      Return New BoundBlockStatement(builder.ToImmutable)
    End Function

    Private Shared Function CanFallThrough(boundStatement As BoundStatement) As Boolean
      Return boundStatement.Kind <> BoundNodeKind.ReturnStatement AndAlso
             boundStatement.Kind <> BoundNodeKind.GotoStatement
    End Function

    Private Shared Function RemoveDeadCode(node As BoundBlockStatement) As BoundBlockStatement

      Dim controlFlow = ControlFlowGraph.Create(node)
      Dim reachableStatements = New HashSet(Of BoundStatement)(controlFlow.Blocks.SelectMany(Function(b) b.Statements))

      Dim builder = node.Statements.ToBuilder
      For i = builder.Count - 1 To 0 Step -1
        If Not reachableStatements.Contains(builder(i)) Then
          builder.RemoveAt(i)
        End If
      Next

      Return New BoundBlockStatement(builder.ToImmutable)

    End Function

    Protected Overrides Function RewriteIfStatement(node As BoundIfStatement) As BoundStatement

      If node.ElseIfStatements.Length = 0 AndAlso node.ElseStatement Is Nothing Then

        ' IF *expression* THEN
        '   *statements*
        ' END IF
        '
        ' ------>
        '
        ' gotoFalse *expression* end
        '   *statements*
        ' end:

        Dim endLabel = GenerateLabel()
        Dim gotoFalse = New BoundConditionalGotoStatement(endLabel, node.Expression, False)
        Dim endLabelStatement = New BoundLabelStatement(endLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          gotoFalse,
          node.Statements,
          endLabelStatement))

        Return RewriteStatement(result)

      Else

        ' IF condition THEN
        '   statements
        ' ELSEIF condition2 THEN
        '   statements2
        ' ...
        ' ELSE
        '   statementsN
        ' END IF
        '
        ' ------>
        '
        ' gotoFalse condition elseIf1
        ' statements
        ' goto end
        ' elseIf1:
        ' gotoFalse condition2 elseIf2
        ' statements2
        ' goto end
        ' ...
        ' elseN:
        ' statementsN
        ' end:

        Dim builder = ImmutableArray.CreateBuilder(Of BoundStatement)()

        Dim endLabel = GenerateLabel()

        ' First IF condition
        Dim nextLabel = If(node.ElseIfStatements.Length > 0,
                          GenerateLabel(),
                          If(node.ElseStatement IsNot Nothing, GenerateLabel(), endLabel))
        Dim gotoFalse = New BoundConditionalGotoStatement(nextLabel, node.Expression, False)
        builder.Add(gotoFalse)
        builder.Add(node.Statements)
        builder.Add(New BoundGotoStatement(endLabel))

        ' ELSEIF clauses
        For i = 0 To node.ElseIfStatements.Length - 1
          Dim elseIfStmt = node.ElseIfStatements(i)
          Dim elseIfLabel = New BoundLabelStatement(nextLabel)
          builder.Add(elseIfLabel)

          nextLabel = If(i < node.ElseIfStatements.Length - 1,
                        GenerateLabel(),
                        If(node.ElseStatement IsNot Nothing, GenerateLabel(), endLabel))

          Dim elseIfGotoFalse = New BoundConditionalGotoStatement(nextLabel, elseIfStmt.Expression, False)
          builder.Add(elseIfGotoFalse)
          builder.Add(elseIfStmt.Statements)
          builder.Add(New BoundGotoStatement(endLabel))
        Next

        ' ELSE clause
        If node.ElseStatement IsNot Nothing Then
          Dim elseLabel = New BoundLabelStatement(nextLabel)
          builder.Add(elseLabel)
          builder.Add(node.ElseStatement)
        End If

        Dim endLabelStatement = New BoundLabelStatement(endLabel)
        builder.Add(endLabelStatement)

        Dim result = New BoundBlockStatement(builder.ToImmutable())

        Return RewriteStatement(result)

      End If

    End Function

    Protected Overrides Function RewriteWhileStatement(node As BoundWhileStatement) As BoundStatement

      ' WHILE *expression*
      '   *statements*
      ' wend
      '
      ' ------->
      '
      ' goto continue
      ' body:
      '   *statements*
      ' continue:
      '   gotoTrue *expression* body
      ' exit:

      Dim bodyLabel = GenerateLabel()

      Dim gotoContinue = New BoundGotoStatement(node.ContinueLabel)
      Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
      Dim gotoTrue = New BoundConditionalGotoStatement(bodyLabel, node.Expression)
      Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
        gotoContinue,
        bodyLabelStatement,
        node.Statements,
        continueLabelStatement,
        gotoTrue,
        exitLabelStatement))

      Return RewriteStatement(result)

    End Function

    Protected Overrides Function RewriteDoUntilStatement(node As BoundDoUntilStatement) As BoundStatement
      ' Lower DO UNTIL statements to goto/label constructs
      If node.AtBeginning Then
        ' DO UNTIL condition ... LOOP
        ' Check condition first, execute body only if condition is false

        ' do [until *expression*]
        ' ------->
        ' check:
        ' gotoTrue *expression* exit  (if condition true, exit)
        ' body:
        '   *statements*
        ' continue:
        ' loop [until *expression*]
        ' goto check

        Dim checkLabel = GenerateLabel()
        Dim bodyLabel = GenerateLabel()
        Dim checkLabelStatement = New BoundLabelStatement(checkLabel)
        Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
        Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
        ' If condition is true, goto exit (skip body)
        Dim gotoExitIfTrue = New BoundConditionalGotoStatement(node.ExitLabel, node.Expression, True)
        Dim gotoCheck = New BoundGotoStatement(checkLabel)
        Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          checkLabelStatement,
          gotoExitIfTrue,
          bodyLabelStatement,
          node.Statements,
          continueLabelStatement,
          gotoCheck,
          exitLabelStatement))

        Return RewriteStatement(result)
      Else
        ' DO ... LOOP UNTIL condition
        ' Execute body first, then check condition

        ' do [until *expression*]
        ' ------->
        ' body:
        '   *statements*
        ' loop [until *expression*]
        ' continue:
        ' gotoFalse *expression* body  (goto body if expression is false)
        ' exit:

        Dim bodyLabel = GenerateLabel()
        Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
        Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
        ' For UNTIL, goto body if condition is FALSE
        Dim gotoIfFalse = New BoundConditionalGotoStatement(bodyLabel, node.Expression, False)
        Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          bodyLabelStatement,
          node.Statements,
          continueLabelStatement,
          gotoIfFalse,
          exitLabelStatement))

        Return RewriteStatement(result)
      End If
    End Function

    Protected Overrides Function RewriteDoWhileStatement(node As BoundDoWhileStatement) As BoundStatement

      If node.AtBeginning Then
        ' DO WHILE condition ... LOOP
        ' Check condition first, execute body only if condition is true

        ' do [until *expression*]
        ' ------->
        ' check:
        ' gotoFalse *expression* exit  (if condition false, exit)
        ' body:
        '   *statements*
        ' loop [until *expression*]
        '
        ' continue:
        ' goto check

        Dim checkLabel = GenerateLabel()
        Dim bodyLabel = GenerateLabel()
        Dim checkLabelStatement = New BoundLabelStatement(checkLabel)
        Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
        Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
        ' If condition is false, goto exit (skip body)
        Dim gotoExitIfFalse = New BoundConditionalGotoStatement(node.ExitLabel, node.Expression, False)
        Dim gotoCheck = New BoundGotoStatement(checkLabel)
        Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          checkLabelStatement,
          gotoExitIfFalse,
          bodyLabelStatement,
          node.Statements,
          continueLabelStatement,
          gotoCheck,
          exitLabelStatement))

        Return RewriteStatement(result)
      Else
        ' DO ... LOOP WHILE condition
        ' Execute body first, then check condition

        ' do [until *expression*]
        ' ------->
        ' body:
        '   *statements*
        ' loop [until *expression*]
        ' continue:
        ' gotoTrue *expression* body  (goto body if expression is true)
        ' exit:

        Dim bodyLabel = GenerateLabel()
        Dim bodyLabelStatement = New BoundLabelStatement(bodyLabel)
        Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)
        ' For WHILE, goto body if condition is TRUE
        Dim gotoIfTrue = New BoundConditionalGotoStatement(bodyLabel, node.Expression, True)
        Dim exitLabelStatement = New BoundLabelStatement(node.ExitLabel)

        Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          bodyLabelStatement,
          node.Statements,
          continueLabelStatement,
          gotoIfTrue,
          exitLabelStatement))

        Return RewriteStatement(result)
      End If

    End Function

    Protected Overrides Function RewriteForStatement(node As BoundForStatement) As BoundStatement

      ' for <var> = <lower> to <upper> step <stepper>
      '   <body>
      ' next
      '
      '  ------>
      '
      ' dim <var> = <lower>
      ' while <var> <= <upper> (for positive step) or <var> >= <upper> (for negative step)
      '   <body>
      '   <var> = <var> + <stepper>
      ' wend
      '

      Dim variableDeclaration = New BoundVariableDeclaration(node.Variable, node.LowerBound)

      Dim variableExpression = New BoundVariableExpression(node.Variable)

      Dim upperBoundSymbol = New LocalVariableSymbol("upperBound", True, TypeSymbol.Integer, node.UpperBound.ConstantValue)
      Dim upperBoundDeclaration = New BoundVariableDeclaration(upperBoundSymbol, node.UpperBound)

      ' Determine if step is negative
      Dim stepValue As Integer = 1
      If node.Stepper IsNot Nothing AndAlso node.Stepper.ConstantValue IsNot Nothing Then
        stepValue = CInt(node.Stepper.ConstantValue.Value)
      End If
      Dim isNegativeStep As Boolean = stepValue < 0

      ' Use LessThanOrEqual for positive step, GreaterThanOrEqual for negative step
      Dim conditionOperator As SyntaxKind = If(isNegativeStep, SyntaxKind.GreaterThanEqualToken, SyntaxKind.LessThanEqualToken)

      Dim condition = New BoundBinaryExpression(variableExpression,
                                                BoundBinaryOperator.Bind(conditionOperator, node.Variable.Type, upperBoundSymbol.Type),
                                                New BoundVariableExpression(upperBoundSymbol))

      Dim continueLabelStatement = New BoundLabelStatement(node.ContinueLabel)

      Dim stepper = node.Stepper

      Dim increment = New BoundExpressionStatement(
              New BoundAssignmentExpression(
                New BoundVariableExpression(node.Variable),
                New BoundBinaryExpression(
                  variableExpression,
                  BoundBinaryOperator.Bind(SyntaxKind.PlusToken, TypeSymbol.Integer, TypeSymbol.Integer),
                  If(stepper, New BoundLiteralExpression(1)))))

      Dim whileBody = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
          node.Body,
          continueLabelStatement,
          increment))
      Dim whileStatement = New BoundWhileStatement(condition, whileBody, node.ExitLabel, GenerateLabel)
      Dim result = New BoundBlockStatement(ImmutableArray.Create(Of BoundStatement)(
              upperBoundDeclaration,
              variableDeclaration,
              whileStatement))

      Return RewriteStatement(result)

    End Function

    Protected Overrides Function RewriteConditionalGotoStatement(node As BoundConditionalGotoStatement) As BoundStatement

      If node.Condition.ConstantValue IsNot Nothing Then
        Dim condition = CBool(node.Condition.ConstantValue.Value)
        condition = If(node.JumpIfTrue, condition, Not condition)
        If condition Then
          Return RewriteStatement(New BoundGotoStatement(node.Label))
        Else
          Return RewriteStatement(New BoundNopStatement())
        End If
      End If

      Return MyBase.RewriteConditionalGotoStatement(node)

    End Function

  End Class

End Namespace