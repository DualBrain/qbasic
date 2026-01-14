Imports System.Collections.Immutable
Imports System.Net.NetworkInformation
Imports System.Runtime.InteropServices

Imports QB.CodeAnalysis.Lowering
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class Binder

    Private Const GOTO_LABEL_PREFIX As String = "$LABEL"

    Private m_scope As BoundScope
    Private ReadOnly m_isScript As Boolean
    Private ReadOnly m_function As FunctionSymbol
    Private ReadOnly Property Diagnostics As DiagnosticBag = New DiagnosticBag
    Private ReadOnly m_loopStack As New Stack(Of (ExitLabel As BoundLabel, ContinueLabel As BoundLabel))
    Private m_labelCounter As Integer
    Private m_optionBase As Integer = 0 ' Default to 0 as per QBasic spec
    Private m_optionBaseDeclared As Boolean = False ' Track if OPTION BASE was already declared
    Private m_arrayModeDynamic As Boolean = True ' Track current $DYNAMIC/$STATIC metacommand state (default $DYNAMIC)

    Public Sub New(isScript As Boolean, parent As BoundScope, [function] As FunctionSymbol)
      m_scope = New BoundScope(parent)
      m_isScript = isScript
      m_function = [function]
      If [function] IsNot Nothing Then
        For Each p In [function].Parameters
          m_scope.TryDeclareVariable(p)
        Next
      End If
    End Sub

    Public Shared Function BindGlobalScope(isScript As Boolean, previous As BoundGlobalScope, syntaxTrees As ImmutableArray(Of SyntaxTree)) As BoundGlobalScope

      Dim parentScope = CreateParentScope(previous)
      Dim binder = New Binder(isScript, parentScope, Nothing)

      binder.Diagnostics.AddRange(syntaxTrees.SelectMany(Function(st) st.Diagnostics))
      If binder.Diagnostics.Any Then
        Return New BoundGlobalScope(previous, binder.Diagnostics.ToImmutableArray, Nothing, Nothing, ImmutableArray(Of FunctionSymbol).Empty, ImmutableArray(Of VariableSymbol).Empty, ImmutableArray(Of BoundStatement).Empty)
      End If

      Dim functionDeclarations = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of FunctionDeclarationSyntax)

      For Each func In functionDeclarations
        binder.BindFunctionDeclaration(func)
      Next

      Dim subDeclarations = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of SubStatementSyntax)

      For Each subStmt In subDeclarations
        binder.BindSubDeclaration(subStmt)
      Next

      Dim defDeclarations = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of DefDeclarationSyntax)

      For Each func In defDeclarations
        binder.BindDefDeclaration(func)
      Next

      Dim singleLineDefDeclarations = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of SingleLineDefDeclarationSyntax)

      For Each func In singleLineDefDeclarations
        binder.BindDefDeclaration(func)
      Next

      Dim globalStatements = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of GlobalStatementSyntax)

      ' Determine if any GOTO or GOSUB statements target a numeric value (Line Number).
      Dim targetLineNumbers As New List(Of Integer)
      For Each statement In globalStatements

        Dim target As New Integer?

        If TypeOf statement.Statement Is GotoStatementSyntax Then
          Dim g = CType(statement.Statement, GotoStatementSyntax)
          If IsNumeric(g.TargetToken.Text) Then
            target = CInt(g.TargetToken.Text)
          Else
            Continue For
          End If
        ElseIf TypeOf statement.Statement Is SingleLineIfStatementSyntax Then
          ' Any GOTO statement(s)?
        Else
          Continue For
        End If

        If target IsNot Nothing Then
          Dim value = CInt(target)
          If Not targetLineNumbers.Contains(value) Then
            targetLineNumbers.Add(value)
          End If
        End If

      Next

      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      For Each globalStatement In globalStatements

        Dim lineNumbers = IterateLineNumbers(globalStatement.Statement)

        For Each token In lineNumbers
          For Each trivia In token.LeadingTrivia
            If trivia.Kind = SyntaxKind.LineNumberTrivia Then
              Dim value = CInt(trivia.Text)
              Dim label = New SyntaxToken(globalStatement.SyntaxTree, SyntaxKind.LabelStatement, token.Position, $"{GOTO_LABEL_PREFIX}{value}:", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
              Dim stmt = New LabelStatementSyntax(globalStatement.SyntaxTree, label)
              statements.Add(binder.BindGlobalStatement(stmt))
            End If
          Next
        Next

        ''TODO: Need to figure out a way to significantly improve the following code.
        'For Each child In globalStatement.Statement.GetChildren
        '  If child.Kind.Is_Keyword Then
        '    Dim token = TryCast(child, SyntaxToken)
        '    If token IsNot Nothing Then
        '      For Each trivia In token.LeadingTrivia
        '        If trivia.Kind = SyntaxKind.LineNumberTrivia Then
        '          Dim value = CInt(trivia.Text)
        '          If True OrElse targetLineNumbers.Contains(value) Then
        '            ' matching target
        '            'TODO: Need to transform the LineNumberTrivia into a numbered Label.
        '            Dim label = New SyntaxToken(globalStatement.SyntaxTree, SyntaxKind.LabelStatement, token.Position, $"{GOTO_LABEL_PREFIX}{value}:", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
        '            Dim stmt = New LabelStatementSyntax(globalStatement.SyntaxTree, label)
        '            statements.Add(binder.BindGlobalStatement(stmt))
        '          End If
        '        End If
        '      Next
        '    End If
        '  End If
        'Next

        Dim statement = binder.BindGlobalStatement(globalStatement.Statement)
        statements.Add(statement)

      Next

      ' Check global statements.

      Dim firstGlobalStatementPerSyntaxTree = syntaxTrees.Select(Function(st) st.Root.Members.OfType(Of GlobalStatementSyntax).FirstOrDefault).
                                                          Where(Function(g) g IsNot Nothing).
                                                          ToArray

      If firstGlobalStatementPerSyntaxTree.Length > 1 Then
        For Each globalStatement In firstGlobalStatementPerSyntaxTree
          binder.Diagnostics.ReportOnlyOneFileCanHaveGlobalStatements(globalStatement.Location)
        Next
      End If

      ' Check for main/script with global statements.

      Dim functions = binder.m_scope.GetDeclaredFunctions

      Dim mainFunction As FunctionSymbol
      Dim scriptFunction As FunctionSymbol

      If isScript Then

        mainFunction = Nothing

        If globalStatements.Any Then
          scriptFunction = New FunctionSymbol("$eval", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.Any)
        Else
          scriptFunction = Nothing
        End If

      Else

        mainFunction = functions.FirstOrDefault(Function(f) f.Name = "main")
        scriptFunction = Nothing

        If mainFunction IsNot Nothing Then
          If mainFunction.Type IsNot TypeSymbol.Nothing OrElse
             mainFunction.Parameters.Any Then
            binder.Diagnostics.ReportMainMustHaveCorrectSignature(mainFunction.Declaration.Identifier.Location)
          End If
        End If

        If globalStatements.Any Then
          If mainFunction IsNot Nothing Then
            binder.Diagnostics.ReportCannotMixMainAndGlobalStatements(mainFunction.Declaration.Identifier.Location)
            For Each globalStatement In firstGlobalStatementPerSyntaxTree
              binder.Diagnostics.ReportCannotMixMainAndGlobalStatements(globalStatement.Location)
            Next
          Else
            mainFunction = New FunctionSymbol("main", ImmutableArray(Of ParameterSymbol).Empty, TypeSymbol.Nothing)
          End If
        End If

      End If

      Dim diagnostics = binder.Diagnostics.ToImmutableArray

      Dim variables = binder.m_scope.GetDeclaredVariables

      If previous IsNot Nothing Then
        diagnostics = diagnostics.InsertRange(0, previous.Diagnostics)
      End If

      Return New BoundGlobalScope(previous, diagnostics, mainFunction, scriptFunction, functions, variables, statements.ToImmutable)

    End Function

    Private Shared Function IterateLineNumbers(statement As SyntaxNode) As List(Of SyntaxToken)

      Dim results As New List(Of SyntaxToken)

      For Each child In statement.GetChildren
        'If child.Kind.Is_Keyword Then
        Dim token = TryCast(child, SyntaxToken)
        If token IsNot Nothing Then
          For Each trivia In token.LeadingTrivia
            If trivia.Kind = SyntaxKind.LineNumberTrivia Then
              'Dim value = CInt(trivia.Text)
              'If True Then 'OrElse targetLineNumbers.Contains(value) Then
              'TODO: Need to transform the LineNumberTrivia into a numbered Label.
              'Dim label = New SyntaxToken(globalStatement.SyntaxTree, SyntaxKind.LabelStatement, token.Position, $"{GOTO_LABEL_PREFIX}{value}:", Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
              'Dim stmt = New LabelStatementSyntax(globalStatement.SyntaxTree, label)
              'statements.Add(Binder.BindGlobalStatement(stmt))
              results.Add(token)
              'End If
            End If
          Next
        End If
        'End If
        Dim r = IterateLineNumbers(child)
        If r.Count > 0 Then
          For Each entry In r
            results.Add(entry)
          Next
        End If
        'If TypeOf child Is StatementSyntax Then
        '  Dim r = IterateLineNumbers(CType(child, StatementSyntax))
        '  If r.Count > 0 Then
        '    For Each entry In r
        '      results.Add(entry)
        '    Next
        '  End If
        'ElseIf TypeOf child Is ExpressionSyntax Then
        '  Dim r = IterateLineNumbers(CType(child, StatementSyntax))
        '  If r.Count > 0 Then
        '    For Each entry In r
        '      results.Add(entry)
        '    Next
        '  End If
        'End If
      Next

      Return results

    End Function

    Public Shared Function BindProgram(isScript As Boolean, previous As BoundProgram, globalScope As BoundGlobalScope) As BoundProgram

      Dim parentScope = CreateParentScope(globalScope)

      If globalScope.Diagnostics.Any Then
        Return New BoundProgram(previous, globalScope.Diagnostics, Nothing, Nothing, ImmutableDictionary(Of FunctionSymbol, BoundBlockStatement).Empty)
      End If

      Dim functionBodies = ImmutableDictionary.CreateBuilder(Of FunctionSymbol, BoundBlockStatement)()
      Dim diagnostics = ImmutableArray.CreateBuilder(Of Diagnostic)()

      For Each func In globalScope.Functions
        Dim binder = New Binder(isScript, parentScope, func)
        Dim body = binder.BindStatement(func.Declaration.Statements)
        Dim loweredBody = Lowerer.Lower(func, body)
        If func.Type IsNot TypeSymbol.Nothing AndAlso Not ControlFlowGraph.AllPathsReturn(func.Name, loweredBody) Then
          binder.Diagnostics.ReportAllPathsMustReturn(func.Declaration.Identifier.Location)
        End If
        functionBodies.Add(func, loweredBody)
        diagnostics.AddRange(binder.Diagnostics)
      Next

      If globalScope.MainFunction IsNot Nothing AndAlso globalScope.Statements.Any Then
        Dim body = Lowerer.Lower(globalScope.MainFunction, New BoundBlockStatement(globalScope.Statements))
        functionBodies.Add(globalScope.MainFunction, body)
      ElseIf globalScope.ScriptFunction IsNot Nothing Then
        Dim statements = globalScope.Statements
        Dim es = TryCast(statements(0), BoundExpressionStatement)
        Dim needsReturn = statements.Length = 1 AndAlso
                          TypeOf es Is BoundExpressionStatement AndAlso
                          es.Expression.Type IsNot TypeSymbol.Nothing
        If needsReturn Then
          statements = statements.SetItem(0, New BoundReturnStatement(es.Expression))
        ElseIf statements.Any AndAlso
               statements.Last.Kind <> BoundNodeKind.ReturnStatement Then
          Dim nullValue = New BoundLiteralExpression("")
          statements = statements.Add(New BoundReturnStatement(nullValue))
        End If
        Dim body = Lowerer.Lower(globalScope.ScriptFunction, New BoundBlockStatement(statements))
        functionBodies.Add(globalScope.ScriptFunction, body)
      End If

      Return New BoundProgram(previous, diagnostics.ToImmutable, globalScope.MainFunction, globalScope.ScriptFunction, functionBodies.ToImmutable)

    End Function

    Private Sub BindFunctionDeclaration(syntax As FunctionDeclarationSyntax)

      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)()

      Dim seenParameterNames As New HashSet(Of String)

      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Identifier.Text
        Dim parameterType = BindAsClause(parameterSyntax.AsClause)
        If parameterType Is Nothing Then parameterType = TypeSymbol.Single
        If Not seenParameterNames.Add(parameterName) Then
          Diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Location, parameterName)
        Else
          Dim parameter As New ParameterSymbol(parameterName, parameterType, parameters.Count)
          parameters.Add(parameter)
        End If
      Next

      Dim type = BindAsClause(syntax.AsClause)
      If type Is Nothing Then type = TypeSymbol.Single

      Dim func As New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable(), type, syntax)
      'If func.Declaration.Identifier.Text IsNot Nothing AndAlso
      If syntax.Identifier.Text IsNot Nothing AndAlso
         Not m_scope.TryDeclareFunction(func) Then
        Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, func.Name)
      End If

    End Sub

    Private Function BindSubDeclaration(syntax As SubStatementSyntax) As FunctionSymbol

      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)()

      Dim seenParameterNames As New HashSet(Of String)

      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Identifier.Text
        Dim parameterType = BindAsClause(parameterSyntax.AsClause)
        If parameterType Is Nothing Then parameterType = TypeSymbol.Single
        If Not seenParameterNames.Add(parameterName) Then
          Diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Location, parameterName)
        Else
          Dim parameter As New ParameterSymbol(parameterName, parameterType, parameters.Count)
          parameters.Add(parameter)
        End If
      Next

      Dim type = TypeSymbol.Nothing ' SUB returns nothing

      Dim func As New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable(), type, syntax)
      If syntax.Identifier.Text IsNot Nothing AndAlso
         Not m_scope.TryDeclareFunction(func) Then
        Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, func.Name)
      End If

      Return func

    End Function

    Private Sub BindDefDeclaration(syntax As DefDeclarationSyntax)

      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)()

      Dim seenParameterNames As New HashSet(Of String)

      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Identifier.Text
        Dim parameterType = BindAsClause(parameterSyntax.AsClause)
        If parameterType Is Nothing Then parameterType = TypeSymbol.Single
        If Not seenParameterNames.Add(parameterName) Then
          Diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Location, parameterName)
        Else
          Dim parameter As New ParameterSymbol(parameterName, parameterType, parameters.Count)
          parameters.Add(parameter)
        End If
      Next

      Dim type As TypeSymbol = Nothing
      Dim identifierText = syntax.Identifier.Text
      If identifierText.Length > 0 Then
        Select Case identifierText.Last
          Case "%"c : type = TypeSymbol.Integer
          Case "&"c : type = TypeSymbol.Long
          Case "!"c : type = TypeSymbol.Single
          Case "#"c : type = TypeSymbol.Double
          Case "$"c : type = TypeSymbol.String
        End Select
      End If
      If type Is Nothing Then type = TypeSymbol.Single

      Dim func As New FunctionSymbol(syntax.Identifier.Text, parameters.ToImmutable(), type, syntax)
      'If func.Declaration.Identifier.Text IsNot Nothing AndAlso
      If syntax.Identifier.Text IsNot Nothing AndAlso
         Not m_scope.TryDeclareFunction(func) Then
        Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, func.Name)
      End If

    End Sub

    Private Sub BindDefDeclaration(syntax As SingleLineDefDeclarationSyntax)

      Dim parameters = ImmutableArray.CreateBuilder(Of ParameterSymbol)()

      Dim seenParameterNames As New HashSet(Of String)

      For Each parameterSyntax In syntax.Parameters
        Dim parameterName = parameterSyntax.Identifier.Identifier.Text
        Dim parameterType = BindAsClause(parameterSyntax.AsClause)
        If parameterType Is Nothing Then parameterType = TypeSymbol.Single
        If Not seenParameterNames.Add(parameterName) Then
          Diagnostics.ReportParameterAlreadyDeclared(parameterSyntax.Location, parameterName)
        Else
          Dim parameter As New ParameterSymbol(parameterName, parameterType, parameters.Count)
          parameters.Add(parameter)
        End If
      Next

      Dim type As TypeSymbol = Nothing
      Dim identifierText = syntax.Identifier.Identifier.Text
      If identifierText.Length > 0 Then
        Select Case identifierText.Last
          Case "%"c : type = TypeSymbol.Integer
          Case "&"c : type = TypeSymbol.Long
          Case "!"c : type = TypeSymbol.Single
          Case "#"c : type = TypeSymbol.Double
          Case "$"c : type = TypeSymbol.String
        End Select
      End If
      If type Is Nothing Then type = TypeSymbol.Single

      Dim func As New FunctionSymbol(syntax.Identifier.Identifier.Text, parameters.ToImmutable(), type, syntax)
      'If func.Declaration.Identifier.Text IsNot Nothing AndAlso
      If syntax.Identifier.Identifier.Text IsNot Nothing AndAlso
         Not m_scope.TryDeclareFunction(func) Then
        Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, func.Name)
      End If

    End Sub

    Private Shared Function CreateParentScope(previous As BoundGlobalScope) As BoundScope
      Dim stack = New Stack(Of BoundGlobalScope)
      While previous IsNot Nothing
        stack.Push(previous)
        previous = previous.Previous
      End While
      Dim parent = CreateRootScope()
      While stack.Count > 0
        previous = stack.Pop
        Dim scope = New BoundScope(parent)
        For Each f In previous.Functions
          scope.TryDeclareFunction(f)
        Next
        For Each v In previous.Variables
          scope.TryDeclareVariable(v)
        Next
        parent = scope
      End While
      Return parent
    End Function

    Private Shared Function CreateRootScope() As BoundScope
      Dim result = New BoundScope(Nothing)
      For Each f In BuiltinFunctions.GetAll
        'TODO: Handle overloading/optional parameters.
        result.TryDeclareFunction(f)
      Next
      Return result
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, targetType As TypeSymbol) As BoundExpression
      Return BindConversion(syntax, targetType)
    End Function

    Private Function BindExpression(syntax As ExpressionSyntax, Optional canBeVoid As Boolean = False) As BoundExpression
      Dim result = BindExpressionInternal(syntax)
      If Not canBeVoid AndAlso result.Type Is TypeSymbol.Nothing Then
        Diagnostics.ReportExpressionMustHaveValue(syntax.Location)
        Return New BoundErrorExpression
      End If
      Return result
    End Function

    Private Function BindExpressionInternal(syntax As ExpressionSyntax) As BoundExpression
      Select Case syntax.Kind
        Case SyntaxKind.ParenExpression : Return BindParenExpression(CType(syntax, ParenExpressionSyntax))
        Case SyntaxKind.LiteralExpression : Return BindLiteralExpression(CType(syntax, LiteralExpressionSyntax))
        Case SyntaxKind.NameExpression : Return BindNameExpression(CType(syntax, NameExpressionSyntax))
        Case SyntaxKind.IdentifierSyntax
          If TypeOf syntax Is IdentifierSyntax Then
            Return BindIdentifierExpression(DirectCast(syntax, IdentifierSyntax))
          ElseIf TypeOf syntax Is IdentifierExpressionSyntax Then
            Return BindNameExpression(DirectCast(syntax, IdentifierExpressionSyntax))
          Else
            Throw New Exception("Unexpected syntax type")
          End If
        Case SyntaxKind.AssignmentExpression : Return BindAssignmentExpression(CType(syntax, AssignmentExpressionSyntax))
        Case SyntaxKind.UnaryExpression : Return BindUnaryExpression(CType(syntax, UnaryExpressionSyntax))
        Case SyntaxKind.BinaryExpression : Return BindBinaryExpression(CType(syntax, BinaryExpressionSyntax))
        Case SyntaxKind.ArrayAccessExpression : Return BindArrayAccessExpression(CType(syntax, ArrayAccessExpressionSyntax))
        Case SyntaxKind.CallExpression : Return BindCallExpression(CType(syntax, CallExpressionSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
    End Function

    Private Shared Function BindErrorStatement() As BoundStatement
      Return New BoundExpressionStatement(New BoundErrorExpression)
    End Function

    Private Function BindGlobalStatement(syntax As StatementSyntax) As BoundStatement
      Return BindStatement(syntax, True)
    End Function

    Private Function BindStatement(syntax As StatementSyntax, Optional isGlobal As Boolean = False) As BoundStatement
      Dim result = BindStatementInternal(syntax)
      If Not m_isScript Or Not isGlobal Then
        If TypeOf result Is BoundExpressionStatement Then
          Dim es = CType(result, BoundExpressionStatement)
          Dim isAllowedExpression = es.Expression.Kind = BoundNodeKind.ErrorExpression Or
                                    es.Expression.Kind = BoundNodeKind.AssignmentExpression Or
                                    es.Expression.Kind = BoundNodeKind.CallExpression
          If Not isAllowedExpression Then
            Diagnostics.ReportInvalidExpressionStatement(syntax.Location)
          End If
        End If
      End If
      Return result
    End Function

    Private Function BindStatementInternal(syntax As StatementSyntax, Optional isGlobal As Boolean = False) As BoundStatement
      If isGlobal Then
      End If
      Select Case syntax.Kind
        Case SyntaxKind.BlockStatement : Return BindBlockStatement(CType(syntax, BlockStatementSyntax))
        Case SyntaxKind.ChDirStatement : Return BindChDirStatement(CType(syntax, ChDirStatementSyntax))
        Case SyntaxKind.CircleStatement : Return BindCircleStatement(CType(syntax, CircleStatementSyntax))
        Case SyntaxKind.ColorStatement : Return BindColorStatement(CType(syntax, ColorStatementSyntax))
        Case SyntaxKind.ClearStatement : Return BindClearStatement(CType(syntax, ClearStatementSyntax))
        Case SyntaxKind.ClsStatement : Return BindClsStatement(CType(syntax, ClsStatementSyntax))
        Case SyntaxKind.ColorStatement : Return BindColorStatement(CType(syntax, ColorStatementSyntax))
        Case SyntaxKind.ContinueStatement : Return BindContinueStatement(CType(syntax, ContinueStatementSyntax))
        Case SyntaxKind.DimStatement : Return BindDimStatement(CType(syntax, DimStatementSyntax))
        Case SyntaxKind.EraseStatement : Return BindEraseStatement(CType(syntax, EraseStatementSyntax))
        Case SyntaxKind.RedimStatement : Return BindRedimStatement(CType(syntax, RedimStatementSyntax))
        Case SyntaxKind.DoUntilStatement : Return BindDoUntilStatement(CType(syntax, DoUntilStatementSyntax))
        Case SyntaxKind.DoWhileStatement : Return BindDoWhileStatement(CType(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.EndStatement : Return BindEndStatement(CType(syntax, EndStatementSyntax))
        Case SyntaxKind.ExitStatement : Return BindExitStatement(CType(syntax, ExitStatementSyntax))
        Case SyntaxKind.ExpressionStatement : Return BindExpressionStatement(CType(syntax, ExpressionStatementSyntax))
        Case SyntaxKind.ForStatement : Return BindForStatement(CType(syntax, ForStatementSyntax))
        Case SyntaxKind.GosubStatement : Return BindGosubStatement(CType(syntax, GosubStatementSyntax))
        Case SyntaxKind.GotoStatement : Return BindGotoStatement(CType(syntax, GotoStatementSyntax))
        Case SyntaxKind.IfStatement : Return BindIfStatement(CType(syntax, IfStatementSyntax))
        Case SyntaxKind.InputStatement : Return BindInputStatement(CType(syntax, InputStatementSyntax))
        Case SyntaxKind.KillStatement : Return BindKillStatement(CType(syntax, KillStatementSyntax))
        Case SyntaxKind.LabelStatement : Return BindLabelStatement(CType(syntax, LabelStatementSyntax))
        Case SyntaxKind.LetStatement : Return BindLetStatement(CType(syntax, LetStatementSyntax))
        Case SyntaxKind.LineStatement : Return BindLineStatement(CType(syntax, LineStatementSyntax))
        Case SyntaxKind.LocateStatement : Return BindLocateStatement(CType(syntax, LocateStatementSyntax))
        Case SyntaxKind.MidStatement : Return BindMidStatement(CType(syntax, MidStatementSyntax))
        Case SyntaxKind.MkDirStatement : Return BindMkDirStatement(CType(syntax, MkDirStatementSyntax))
        Case SyntaxKind.NameStatement : Return BindNameStatement(CType(syntax, NameStatementSyntax))
        Case SyntaxKind.OptionStatement : Return BindOptionStatement(CType(syntax, OptionStatementSyntax))
        Case SyntaxKind.PrintStatement : Return BindPrintStatement(CType(syntax, PrintStatementSyntax))
        Case SyntaxKind.PsetKeyword : Return BindPsetStatement(CType(syntax, PsetStatementSyntax))
        Case SyntaxKind.PresetKeyword : Return BindPresetStatement(CType(syntax, PresetStatementSyntax))
        Case SyntaxKind.RemStatement : Return BindRemStatement(CType(syntax, RemStatementSyntax))
        Case SyntaxKind.ReturnGosubStatement : Return BindReturnGosubStatement(CType(syntax, ReturnGosubStatementSyntax))
        Case SyntaxKind.ReturnStatement : Return BindReturnStatement(CType(syntax, ReturnStatementSyntax))
        Case SyntaxKind.RmDirStatement : Return BindRmDirStatement(CType(syntax, RmDirStatementSyntax))
        Case SyntaxKind.ScreenKeyword : Return BindScreenStatement(CType(syntax, ScreenStatementSyntax))
        Case SyntaxKind.SingleLineIfStatement : Return BindSingleLineIfStatement(CType(syntax, SingleLineIfStatementSyntax))
        Case SyntaxKind.StopStatement : Return BindStopStatement(CType(syntax, StopStatementSyntax))
        Case SyntaxKind.SwapStatement : Return BindSwapStatement(CType(syntax, SwapStatementSyntax))
        Case SyntaxKind.SystemStatement : Return BindSystemStatement(CType(syntax, SystemStatementSyntax))
        Case SyntaxKind.VariableDeclarationStatement : Return BindVariableDeclaration(CType(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.WhileStatement : Return BindWhileStatement(CType(syntax, WhileStatementSyntax))
        Case SyntaxKind.DataStatement : Return BindDataStatement(CType(syntax, DataStatementSyntax))
        Case SyntaxKind.ReadStatement : Return BindReadStatement(CType(syntax, ReadStatementSyntax))
        Case SyntaxKind.CallStatement : Return BindCallStatement(CType(syntax, CallStatementSyntax))
        Case SyntaxKind.StatementSeparatorStatement : Return New BoundNopStatement()
        Case SyntaxKind.SubStatement : Throw New Exception("SUB statements should not be bound as executable statements")
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
    End Function

    Private Function BindSubscriptClause(syntax As DimensionsClauseSyntax) As (Lower As BoundExpression, Upper As BoundExpression)
      If syntax Is Nothing Then Return Nothing
      Dim lower As BoundExpression = Nothing
      Dim upper As BoundExpression = Nothing
      If syntax.Dimensions?.Count = 1 Then
        upper = BindExpression(CType(syntax.Dimensions(0), ExpressionSyntax))
      ElseIf syntax.Dimensions?.Count = 3 Then
        lower = BindExpression(CType(syntax.Dimensions(0), ExpressionSyntax))
        upper = BindExpression(CType(syntax.Dimensions(2), ExpressionSyntax))
      End If
      Return (lower, upper)
    End Function

    Private Function BindAsClause(syntax As AsClause) As TypeSymbol
      If syntax Is Nothing Then Return Nothing
      Dim type = LookupType(syntax.Identifier.Text)
      If type Is Nothing Then
        Diagnostics.ReportUndefinedType(syntax.Identifier.Location, syntax.Identifier.Text)
        Return Nothing
      End If
      Return type
    End Function

    Private Function BindAssignmentExpression(syntax As AssignmentExpressionSyntax) As BoundExpression

      Dim boundLeft = BindExpression(syntax.Variable, canBeVoid:=True)
      Dim boundRight = BindExpression(syntax.Expression)

      Return New BoundAssignmentExpression(boundLeft, boundRight)

    End Function

    Private Function BindBinaryExpression(syntax As BinaryExpressionSyntax) As BoundExpression
      Dim boundLeft = BindExpression(syntax.Left)
      Dim boundRight = BindExpression(syntax.Right)
      If boundLeft.Type Is TypeSymbol.Error OrElse boundRight.Type Is TypeSymbol.Error Then Return New BoundErrorExpression
      Dim boundOperator = BoundBinaryOperator.Bind(syntax.OperatorToken.Kind, boundLeft.Type, boundRight.Type)
      If boundOperator Is Nothing Then
        Diagnostics.ReportUndefinedBinaryOperator(syntax.OperatorToken.Location, syntax.OperatorToken.Text, boundLeft.Type, boundRight.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundBinaryExpression(boundLeft, boundOperator, boundRight, syntax)
    End Function

    Private Function BindBlockStatement(syntax As BlockStatementSyntax) As BoundStatement
      Dim statements = ImmutableArray.CreateBuilder(Of BoundStatement)
      For Each statementSyntax In syntax.Statements
        Dim statement = BindStatement(statementSyntax)
        statements.Add(statement)
      Next
      Return New BoundBlockStatement(statements.ToImmutable)
    End Function

    Private Function BindArrayAccessExpression(syntax As ArrayAccessExpressionSyntax) As BoundExpression
      ' This shouldn't be called directly since we handle array access in BindCallExpression
      Throw New Exception("ArrayAccessExpression should be handled in BindCallExpression")
    End Function

    Private Function BindCallExpression(syntax As CallExpressionSyntax) As BoundExpression

      ' First check if this is array access (identifier with parentheses)
      Dim variableSymbol = m_scope.TryLookupVariable(syntax.Identifier.Text)
      If variableSymbol IsNot Nothing AndAlso Not variableSymbol.IsArray AndAlso syntax.Arguments.Count = 1 Then
        ' Redeclare scalar as array
        Dim lowerBound = New BoundLiteralExpression(0)
        Dim upperBound = New BoundLiteralExpression(10)
        variableSymbol = BindArrayDeclaration(syntax.Identifier, TypeSymbol.Single, lowerBound, upperBound, 1)
      End If
      If variableSymbol IsNot Nothing AndAlso variableSymbol.IsArray Then
        ' This is array access
        If syntax.Arguments.Count <> 1 Then
          Diagnostics.ReportWrongArgumentCount(syntax.Identifier.Location, syntax.Identifier.Text, 1, syntax.Arguments.Count)
          Return New BoundErrorExpression
        End If
        Dim index = BindExpression(syntax.Arguments(0))
        Return New BoundArrayAccessExpression(variableSymbol, index)
      End If

      ' Check for function calls
      Dim boundArguments = ImmutableArray.CreateBuilder(Of BoundExpression)()

      Dim parameters = New List(Of TypeSymbol)
      If syntax.Arguments IsNot Nothing Then
        For Each argument In syntax.Arguments
          Dim boundArgument = BindExpression(argument)
          boundArguments.Add(boundArgument)
          parameters.Add(boundArgument.Type)
        Next
      End If

      Dim symbol = m_scope.TryLookupFunction(syntax.Identifier.Text, parameters)
      If symbol IsNot Nothing Then
        Dim func = TryCast(symbol, FunctionSymbol)
        If func Is Nothing Then
          Diagnostics.ReportNotAFunction(syntax.Identifier.Location, syntax.Identifier.Text)
          Return New BoundErrorExpression
        End If

        If If(syntax.Arguments?.Count, 0) <> func.Parameters.Length Then
          Dim span As TextSpan
          If syntax.Arguments.Count > func.Parameters.Length Then
            Dim firstExceedingNode As SyntaxNode
            If func.Parameters.Length > 0 Then
              firstExceedingNode = syntax.Arguments.GetSeparator(func.Parameters.Length - 1)
            Else
              firstExceedingNode = syntax.Arguments(0)
            End If
            Dim lastExceedingArgument = syntax.Arguments(syntax.Arguments.Count - 1)
            span = TextSpan.FromBounds(firstExceedingNode.Span.Start, lastExceedingArgument.Span.[End])
          Else
            span = If(syntax.CloseParenToken IsNot Nothing, syntax.CloseParenToken.Span, syntax.Identifier.Span)
          End If
          Dim location = New TextLocation(syntax.SyntaxTree.Text, span)
          Diagnostics.ReportWrongArgumentCount(location, func.Name, func.Parameters.Length, syntax.Arguments.Count)
          Return New BoundErrorExpression
        End If

        If syntax.Arguments IsNot Nothing Then
          For i = 0 To syntax.Arguments.Count - 1
            Dim argumentLocation = syntax.Arguments(i).Location
            Dim argument = boundArguments(i)
            Dim parameter = func.Parameters(i)
            boundArguments(i) = BindConversion(argumentLocation, argument, parameter.Type)
          Next
        End If

        Return New BoundCallExpression(func, boundArguments.ToImmutable(), syntax)
      End If

      ' Not function, check for automatic array dimensioning
      If variableSymbol Is Nothing AndAlso syntax.Arguments.Count = 1 Then
        Dim index = BindExpression(syntax.Arguments(0))
        ' Create automatic array with bounds 0-10
        Dim lowerBound = New BoundLiteralExpression(0)
        Dim upperBound = New BoundLiteralExpression(10)
        Dim autoArray = BindArrayDeclaration(syntax.Identifier, TypeSymbol.Single, lowerBound, upperBound, 1)
        Return New BoundArrayAccessExpression(autoArray, index)
      End If

      ' Error
      Diagnostics.ReportUndefinedFunction(syntax.Identifier.Location, syntax.Identifier.Text)
      Return New BoundErrorExpression

    End Function

    Private Function BindChDirStatement(syntax As ChDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundChDirStatement(path)
    End Function

    Private Function BindCircleStatement(syntax As CircleStatementSyntax) As BoundStatement
      Dim [step] = syntax.OptionalStepKeyword IsNot Nothing
      Dim x = BindExpression(syntax.X)
      Dim y = BindExpression(syntax.Y)
      Dim radius = BindExpression(syntax.Radius)
      Dim color = If(syntax.OptionalColor IsNot Nothing, BindExpression(syntax.OptionalColor), Nothing)
      Dim start = If(syntax.OptionalStart IsNot Nothing, BindExpression(syntax.OptionalStart), Nothing)
      Dim [end] = If(syntax.OptionalEnd IsNot Nothing, BindExpression(syntax.OptionalEnd), Nothing)
      Dim aspect = If(syntax.OptionalAspect IsNot Nothing, BindExpression(syntax.OptionalAspect), Nothing)
      Return New BoundCircleStatement([step], x, y, radius, color, start, [end], aspect)
    End Function

    Private Function BindClearStatement(syntax As ClearStatementSyntax) As BoundStatement
      Dim maxBytesExpression = If(syntax.MaxBytesExpression IsNot Nothing, BindExpression(syntax.MaxBytesExpression), Nothing)
      Dim stackSpaceExpression = If(syntax.StackSpaceExpression IsNot Nothing, BindExpression(syntax.StackSpaceExpression), Nothing)
      Return New BoundClearStatement(maxBytesExpression, stackSpaceExpression)
    End Function

    Private Function BindClsStatement(syntax As ClsStatementSyntax) As BoundStatement
      Dim expression = If(syntax.Expression IsNot Nothing, BindExpression(syntax.Expression), Nothing)
      Return New BoundClsStatement(expression)
    End Function

    Private Function BindColorStatement(syntax As ColorStatementSyntax) As BoundStatement
      Dim argumentExpression1 = If(syntax.ArgumentExpression1 IsNot Nothing, BindExpression(syntax.ArgumentExpression1), Nothing)
      Dim argumentExpression2 = If(syntax.ArgumentExpression2 IsNot Nothing, BindExpression(syntax.ArgumentExpression2), Nothing)
      Dim argumentExpression3 = If(syntax.ArgumentExpression3 IsNot Nothing, BindExpression(syntax.ArgumentExpression3), Nothing)
      Return New BoundColorStatement(argumentExpression1, argumentExpression2, argumentExpression3)
    End Function

    Private Function BindContinueStatement(syntax As ContinueStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ContinueKeyword.Location, syntax.ContinueKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim continueLabel = m_loopStack.Peek().ContinueLabel
      Return New BoundGotoStatement(continueLabel)

    End Function

    Private Function BindConversion(syntax As ExpressionSyntax, [type] As TypeSymbol, Optional allowExplicit As Boolean = False) As BoundExpression
      Dim expression = BindExpression(syntax)
      Return BindConversion(syntax.Location, expression, type, allowExplicit)
    End Function

    Private Function BindConversion(diagnosticLocation As TextLocation,
                                     expression As BoundExpression,
                                     [type] As TypeSymbol,
                                     Optional allowExplicit As Boolean = True) As BoundExpression
      Dim c = Conversion.Classify(expression.Type, [type])
      If Not c.Exists Then
        If expression.Type IsNot TypeSymbol.Error AndAlso [type] IsNot TypeSymbol.Error Then
          Diagnostics.ReportCannotConvert(diagnosticLocation, expression.Type, [type])
        End If
        Return New BoundErrorExpression
      End If
      If Not allowExplicit AndAlso c.IsExplicit AndAlso Not ((expression.Type Is TypeSymbol.Integer OrElse expression.Type Is TypeSymbol.Single OrElse expression.Type Is TypeSymbol.Double OrElse expression.Type Is TypeSymbol.Long) AndAlso ([type] Is TypeSymbol.Integer OrElse [type] Is TypeSymbol.Single OrElse [type] Is TypeSymbol.Double OrElse [type] Is TypeSymbol.Long)) Then
        Diagnostics.ReportCannotConvertImplicitly(diagnosticLocation, expression.Type, [type])
      End If

      If c.IsIdentity Then Return expression
      Return New BoundConversionExpression([type], expression)
    End Function

    Private Function BindDoUntilStatement(syntax As DoUntilStatementSyntax) As BoundStatement
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Dim expression = BindExpression(syntax.UntilClause.Expression, TypeSymbol.Boolean)
      Dim atBeginning = syntax.UntilClause.AtBeginning
      Return New BoundDoUntilStatement(statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Function BindDoWhileStatement(syntax As DoWhileStatementSyntax) As BoundStatement
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Dim expression As BoundExpression '= Nothing
      If syntax.WhileClause Is Nothing Then
        ' If missing, infer "True".
        Dim isTrue = True
        Dim keywordToken = New SyntaxToken(syntax.SyntaxTree, SyntaxKind.TrueKeyword, syntax.Statements.Span.Start, Nothing, Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
        Dim le = New LiteralExpressionSyntax(syntax.SyntaxTree, keywordToken, isTrue)
        expression = BindExpression(le, TypeSymbol.Boolean)
      Else
        expression = BindExpression(syntax.WhileClause.Expression, TypeSymbol.Boolean)
      End If
      Dim atBeginning = syntax.WhileClause Is Nothing OrElse syntax.WhileClause.AtBeginning
      Return New BoundDoWhileStatement(statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Shared Function BindEndStatement(syntax As EndStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundEndStatement()
    End Function

    Private Function BindExitStatement(syntax As ExitStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ExitKeyword.Location, syntax.ExitKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim exitLabel = m_loopStack.Peek().ExitLabel
      Return New BoundGotoStatement(exitLabel)

    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, canBeVoid:=True)
      Return New BoundExpressionStatement(expression)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.FromValue, TypeSymbol.Single)
      Dim upperBound = BindExpression(syntax.ToValue, TypeSymbol.Single)
      Dim stepper = If(syntax.StepClause Is Nothing, Nothing, BindExpression(syntax.StepClause.StepValue, TypeSymbol.Single))

      m_scope = New BoundScope(m_scope)

      Dim variable = BindVariableDeclaration(syntax.ControlVariable, True, TypeSymbol.Single)
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim body = BindLoopBody(syntax.Statements, exitLabel, continueLabel)

      m_scope = m_scope.Parent

      Return New BoundForStatement(variable, lowerBound, upperBound, stepper, body, exitLabel, continueLabel)

    End Function

    Private Shared Function BindGosubStatement(syntax As GosubStatementSyntax) As BoundStatement
      Dim value = syntax.IdentifierToken.Text
      If IsNumeric(value) Then
        value = $"GosubLabel{value}"
      End If
      Dim label = New BoundLabel(value)
      Return New BoundGosubStatement(label)
    End Function

    Private Shared Function BindGotoStatement(syntax As GotoStatementSyntax) As BoundStatement
      Dim value = syntax.TargetToken.Text
      If IsNumeric(value) Then
        value = $"{GOTO_LABEL_PREFIX}{value}"
      End If
      Dim label = New BoundLabel(value)
      Return New BoundGotoStatement(label)
    End Function

    Private Function BindIfStatement(syntax As IfStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Condition, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        ElseIf syntax.ElseClause IsNot Nothing Then
          Diagnostics.ReportUnreachableCode(syntax.ElseClause.Statements)
        End If
      End If

      Dim thenStatement = BindStatement(syntax.Statements)

      Dim elseIfStatementsBuilder = ImmutableArray.CreateBuilder(Of BoundElseIfStatement)
      For Each elseIfSyntax In syntax.ElseIfClauses
        Dim elseIfCondition = BindExpression(elseIfSyntax.Expression, TypeSymbol.Boolean)
        Dim elseIfStatements = BindStatement(elseIfSyntax.Statements)
        elseIfStatementsBuilder.Add(New BoundElseIfStatement(elseIfCondition, elseIfStatements))
      Next

      Dim elseClause = If(syntax.ElseClause IsNot Nothing, BindStatement(syntax.ElseClause.Statements), Nothing)
      Return New BoundIfStatement(condition, thenStatement, elseIfStatementsBuilder.ToImmutable(), elseClause)

    End Function

    Private Function BindInputStatement(syntax As InputStatementSyntax) As BoundStatement

      Dim suppressCr = syntax.OptionalSemiColonToken IsNot Nothing
      Dim suppressQuestionMark = If(syntax.SemiColonOrCommaToken?.Kind = SyntaxKind.CommaToken, False)
      Dim prompt As BoundExpression = Nothing
      If syntax.OptionalPromptExpression IsNot Nothing Then
        prompt = BindExpression(syntax.OptionalPromptExpression, TypeSymbol.String)
      End If
      Dim variables As New List(Of VariableSymbol)
      For Each token In syntax.Tokens
        If token.Kind <> SyntaxKind.CommaToken Then

          'variables.Add(DetermineVariableReference(token))

          Dim variable = DetermineVariableReference(token)
          If variable Is Nothing Then
            If OPTION_EXPLICIT Then
              ' Variable appears to not have been already declared, 
              ' run through the normal process in order to generate
              ' the appropriate error(s).
              variable = BindVariableReference(token)
            Else
              ' Variable has not been declared, let's go ahead and do so.
              'Dim type = TypeSymbol.String
              Dim type As TypeSymbol '= TypeSymbol.String
              Dim suffix = token.Text.Last
              Select Case suffix
                Case "%"c : type = TypeSymbol.Integer
                Case "&"c : type = TypeSymbol.Long
                Case "!"c : type = TypeSymbol.Single
                Case "#"c : type = TypeSymbol.Double
                Case "$"c : type = TypeSymbol.String
                Case Else
                  'TODO: This needs to be set based on current DEFINT, etc.
                  type = TypeSymbol.Single
              End Select
              'If Not token.Text.EndsWith("$") Then
              '  type = TypeSymbol.Double
              'End If
              variable = BindVariableDeclaration(token, False, type)
            End If
          End If
          If variable Is Nothing Then
            Return Nothing
          End If

          variables.Add(variable)

        End If
      Next
      Return New BoundInputStatement(suppressCr, suppressQuestionMark, prompt, variables.ToImmutableArray)
    End Function

    Private Function BindKillStatement(syntax As KillStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundKillStatement(path)
    End Function

    Private Shared Function BindLabelStatement(syntax As LabelStatementSyntax) As BoundStatement
      Dim label = syntax.Label
      Dim labelName = If(label.Kind = SyntaxKind.Label, label.Text.Substring(0, label.Text.Length - 1), label.Text)
      Dim boundLabel = New BoundLabel(labelName)
      Return New BoundLabelStatement(boundLabel)
    End Function

    Private Function BindLetStatement(syntax As LetStatementSyntax) As BoundStatement

      Dim boundExpression = BindExpression(syntax.Expression)

      For Each node In syntax.Identifiers
        If TypeOf node Is IdentifierSyntax Then
          Dim identifier = CType(node, IdentifierSyntax)
          Dim name = identifier.Identifier.Text
          Dim variable = DetermineVariableReference(identifier.Identifier)
          If variable Is Nothing Then
            If OPTION_EXPLICIT Then
              ' Variable appears to not have been already declared, 
              ' run through the normal process in order to generate
              ' the appropriate error(s).
              variable = BindVariableReference(identifier.Identifier)
            Else
              ' Variable has not been declared, let's go ahead and do so.
              Dim type As TypeSymbol '= TypeSymbol.String
              Dim suffix = identifier.Identifier.Text.Last
              Select Case suffix
                Case "%"c : type = TypeSymbol.Integer
                Case "&"c : type = TypeSymbol.Long
                Case "!"c : type = TypeSymbol.Single
                Case "#"c : type = TypeSymbol.Double
                Case "$"c : type = TypeSymbol.String
                Case Else
                  'TODO: This needs to be set based on current DEFINT, etc.
                  type = TypeSymbol.Single
                  'TODO: Infer????
                  'type = boundExpression.Type
              End Select
              variable = BindVariableDeclaration(identifier.Identifier, False, type)
            End If
          End If
          If variable.IsReadOnly Then
            Diagnostics.ReportCannotAssign(syntax.EqualToken.Location, name)
          End If

          Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type, allowExplicit:=True)

          Return New BoundLetStatement(variable, convertedExpression)

        End If
      Next

      Return Nothing

      'Dim name = CType(syntax.Identifiers(0), SyntaxToken).Text

      'Dim variable = DetermineVariableReference(CType(syntax.Identifiers(0), SyntaxToken))
      'If variable Is Nothing Then
      '  If OPTION_EXPLICIT Then
      '    ' Variable appears to not have been already declared, 
      '    ' run through the normal process in order to generate
      '    ' the appropriate error(s).
      '    variable = BindVariableReference(CType(syntax.Identifiers(0), SyntaxToken))
      '  Else
      '    ' Variable has not been declared, let's go ahead and do so.
      '    Dim type As TypeSymbol '= TypeSymbol.String
      '    Dim suffix = CType(syntax.Identifiers(0), SyntaxToken).Text.Last
      '    Select Case suffix
      '      Case "%"c : type = TypeSymbol.Integer
      '      Case "&"c : type = TypeSymbol.Long
      '      Case "!"c : type = TypeSymbol.Single
      '      Case "#"c : type = TypeSymbol.Double
      '      Case "$"c : type = TypeSymbol.String
      '      Case Else
      '        'TODO: This needs to be set based on current DEFINT, etc.
      '        type = TypeSymbol.Single
      '        'TODO: Infer????
      '        'type = boundExpression.Type
      '    End Select
      '    variable = BindVariableDeclaration(CType(syntax.Identifiers(0), SyntaxToken), False, type)
      '  End If
      'End If
      'If variable Is Nothing Then
      '  Return Nothing
      'End If

      'If variable.IsReadOnly Then
      '  Diagnostics.ReportCannotAssign(syntax.EqualToken.Location, name)
      'End If

      'Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type)

      'Return New BoundLetStatement(variable, convertedExpression)

    End Function

    Private Shared Function BindLiteralExpression(syntax As LiteralExpressionSyntax) As BoundExpression
      Dim value = If(syntax.Value, 0)
      ' Convert Int16 literals to Int32 to avoid overflow issues
      If TypeOf value Is Short Then
        value = CInt(CShort(value))
      End If
      Return New BoundLiteralExpression(value, syntax)
    End Function

    Private Function BindLineStatement(syntax As LineStatementSyntax) As BoundStatement
      Dim step1 = syntax.OptionalStep1 IsNot Nothing
      Dim x1 = If(syntax.OptionalX1 Is Nothing, Nothing, BindExpression(syntax.OptionalX1))
      Dim y1 = If(syntax.OptionalY1 Is Nothing, Nothing, BindExpression(syntax.OptionalY1))
      Dim step2 = syntax.OptionalStep2 IsNot Nothing
      Dim x2 = BindExpression(syntax.X2)
      Dim y2 = BindExpression(syntax.Y2)
      Dim attribute = If(syntax.OptionalAttribute Is Nothing, Nothing, BindExpression(syntax.OptionalAttribute))
      Dim mode As Integer = 0
      Select Case syntax.OptionalMode?.Text?.ToLower
        Case "b" : mode = 1
        Case "bf" : mode = 2
        Case Else
      End Select
      Dim style = If(syntax.OptionalStyle Is Nothing, Nothing, BindExpression(syntax.OptionalStyle))
      Return New BoundLineStatement(step1, x1, y1, step2, x2, y2, attribute, mode, style)
    End Function

    Private Function BindLocateStatement(syntax As LocateStatementSyntax) As BoundStatement
      Dim row = If(syntax.Row Is Nothing, Nothing, BindExpression(syntax.Row))
      Dim col = If(syntax.Col Is Nothing, Nothing, BindExpression(syntax.Col))
      Dim visible = If(syntax.Visible Is Nothing, Nothing, BindExpression(syntax.Visible))
      Dim scanStart = If(syntax.ScanStart Is Nothing, Nothing, BindExpression(syntax.ScanStart))
      Dim scanStop = If(syntax.ScanStop Is Nothing, Nothing, BindExpression(syntax.ScanStop))
      Return New BoundLocateStatement(row, col, visible, scanStart, scanStop)
    End Function

    Private Function BindLoopBody(statements As StatementSyntax, ByRef exitLabel As BoundLabel, ByRef continueLabel As BoundLabel) As BoundStatement

      m_labelCounter += 1
      exitLabel = New BoundLabel($"exit{m_labelCounter}")
      continueLabel = New BoundLabel($"continue{m_labelCounter}")

      m_loopStack.Push((exitLabel, continueLabel))
      Dim boundBody = BindStatement(statements)
      m_loopStack.Pop()

      Return boundBody

    End Function

    Private Function BindMidStatement(syntax As MidStatementSyntax) As BoundStatement
      Dim variable = DetermineVariableReference(syntax.IdentifierToken)
      Dim positionExpression = If(syntax.PositionExpression Is Nothing, Nothing, BindExpression(syntax.PositionExpression))
      Dim lengthExpression = If(syntax.LengthExpression Is Nothing, Nothing, BindExpression(syntax.LengthExpression))
      Dim expression = If(syntax.Expression Is Nothing, Nothing, BindExpression(syntax.Expression))
      Return New BoundMidStatement(variable, positionExpression, lengthExpression, expression)
    End Function

    Private Function BindMkDirStatement(syntax As MkDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundMkDirStatement(path)
    End Function

    Private Function BindNameStatement(syntax As NameStatementSyntax) As BoundStatement
      Dim originalPath = BindExpression(syntax.OriginalPath)
      Dim destinationPath = BindExpression(syntax.DestinationPath)
      Return New BoundNameStatement(originalPath, destinationPath)
    End Function

    'Private Function BindIdentifierExpression(syntax As IdentifierSyntax) As BoundExpression
    '  Dim name = syntax.Identifier.Text
    '  If syntax.Identifier.IsMissing Then
    '    ' This means the token was inserted by the parser. We already
    '    ' reported error so we can just return an error expression.
    '    Return New BoundErrorExpression
    '  End If
    '  Dim variable = BindVariableReference(syntax.Identifier)
    '  If variable Is Nothing Then
    '    Return New BoundErrorExpression
    '  End If
    '  Return New BoundVariableExpression(variable)
    'End Function

    Private Function BindIdentifierExpression(syntax As IdentifierSyntax) As BoundExpression
      Dim name = syntax.Identifier.Text
      If syntax.Identifier.IsMissing Then
        ' This means the token was inserted by the parser. We already
        ' reported error so we can just return an error expression.
        Return New BoundErrorExpression
      End If
      Dim symbol = m_scope.TryLookupSymbol(name)
      Dim variable As VariableSymbol = Nothing
      If TypeOf symbol Is VariableSymbol Then
        variable = CType(symbol, VariableSymbol)
      ElseIf symbol Is Nothing Then
        If syntax.OpenParen IsNot Nothing Then
          ' Implicit array - declare in current compilation's global scope
          Dim arrayType = TypeSymbol.Single
          variable = New GlobalArraySymbol(name, arrayType, New BoundLiteralExpression(0), New BoundLiteralExpression(10), True, 1)
          ' Find the current compilation's global scope to declare automatic arrays
          Dim globalScope = m_scope
          While globalScope.Parent IsNot Nothing AndAlso globalScope.Parent.Parent IsNot Nothing
            globalScope = globalScope.Parent
          End While
          globalScope.TryDeclareVariable(variable)
        Else
          ' Implicit scalar
          ' No AS clause, check variable name suffix
          Dim type = TypeSymbol.Single
          Dim suffix = name.Last
          Select Case suffix
            Case "%"c : type = TypeSymbol.Integer
            Case "&"c : type = TypeSymbol.Long
            Case "!"c : type = TypeSymbol.Single
            Case "#"c : type = TypeSymbol.Double
            Case "$"c : type = TypeSymbol.String
            Case Else
              'Type = TypeSymbol.Single ' Default for arrays without suffix
          End Select
          variable = If(m_function Is Nothing,
                        DirectCast(New GlobalVariableSymbol(name, False, type, Nothing), VariableSymbol),
                        DirectCast(New LocalVariableSymbol(name, False, type, Nothing), VariableSymbol))
          m_scope.TryDeclareVariable(variable)
        End If
      Else
        Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, name)
        Return New BoundErrorExpression
      End If
      If syntax.OpenParen IsNot Nothing Then
        If Not variable.IsArray Then
          Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, name)
          Return New BoundErrorExpression
        End If
        Dim index = BindExpression(syntax.Arguments(0))
        Return New BoundArrayAccessExpression(variable, index)
      Else
        Return New BoundVariableExpression(variable, syntax)
      End If
    End Function

    Private Function BindNameExpression(syntax As IdentifierExpressionSyntax) As BoundExpression
      Dim name = syntax.Identifier.Text
      If syntax.Identifier.IsMissing Then
        ' This means the token was inserted by the parser. We already
        ' reported error so we can just return an error expression.
        Return New BoundErrorExpression
      End If
      Dim variable = BindVariableReference(syntax.Identifier)
      If variable Is Nothing Then
        Return New BoundErrorExpression
      End If
      Return New BoundVariableExpression(variable, syntax)
    End Function

    Private Function BindNameExpression(syntax As NameExpressionSyntax) As BoundExpression
      Dim name = syntax.Identifier.Identifier.Text
      If syntax.Identifier.Identifier.IsMissing Then
        ' This means the token was inserted by the parser. We already
        ' reported error so we can just return an error expression.
        Return New BoundErrorExpression
      End If
      Dim variable = BindVariableReference(syntax.Identifier.Identifier)
      If variable Is Nothing Then
        Return New BoundErrorExpression
      End If
      Return New BoundVariableExpression(variable, syntax)
    End Function

    Private Function BindOptionStatement(syntax As OptionStatementSyntax) As BoundStatement
      ' Validate that OPTION BASE hasn't been declared already
      If m_optionBaseDeclared Then
        Diagnostics.ReportOptionBaseAlreadyDeclared(syntax.OptionKeyword.Location)
      End If

      Dim number = CInt(syntax.NumberToken.Text)
      If number <> 0 AndAlso number <> 1 Then
        Diagnostics.ReportInvalidOptionBaseValue(syntax.NumberToken.Location, number)
      End If

      m_optionBase = number
      m_optionBaseDeclared = True
      Return New BoundOptionStatement(number)
    End Function

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return New BoundParenExpression(BindExpression(syntax.Expression), syntax)
    End Function

    Private Function BindPrintStatement(syntax As PrintStatementSyntax) As BoundStatement

      Dim nodes = New List(Of BoundNode)

      For Each entry In syntax.Nodes
        If entry.Kind = SyntaxKind.SemicolonToken Then
          nodes.Add(New BoundSymbol(";"c))
        ElseIf entry.Kind = SyntaxKind.CommaToken Then
          nodes.Add(New BoundSymbol(","c))
        ElseIf entry.Kind = SyntaxKind.SpcFunction Then
          Dim spc = CType(entry, SpcFunctionSyntax)
          Dim expr = BindExpression(spc.Expression, TypeSymbol.Long)
          nodes.Add(New BoundSpcFunction(expr))
        ElseIf entry.Kind = SyntaxKind.TabFunction Then
          Dim tab = CType(entry, TabFunctionSyntax)
          Dim expr = BindExpression(tab.Expression, TypeSymbol.Long)
          nodes.Add(New BoundTabFunction(expr))
        Else
          nodes.Add(BindExpression(CType(entry, ExpressionSyntax), TypeSymbol.Any))
        End If
      Next

      Return New BoundPrintStatement(nodes.ToImmutableArray)

    End Function

    Private Function BindPsetStatement(syntax As PsetStatementSyntax) As BoundStatement
      Dim [step] = syntax.OptionalStepKeyword IsNot Nothing
      Dim x = BindExpression(syntax.XExpression, TypeSymbol.Single)
      Dim y = BindExpression(syntax.YExpression, TypeSymbol.Single)
      Dim color As BoundExpression = Nothing
      If syntax.OptionalColorExpression IsNot Nothing Then
        color = BindExpression(syntax.OptionalColorExpression, TypeSymbol.Single)
      End If
      Return New BoundPsetStatement([step], x, y, color)
    End Function

    Private Function BindPresetStatement(syntax As PresetStatementSyntax) As BoundStatement
      Dim [step] = syntax.OptionalStepKeyword IsNot Nothing
      Dim x = BindExpression(syntax.XExpression, TypeSymbol.Single)
      Dim y = BindExpression(syntax.YExpression, TypeSymbol.Single)
      Dim color As BoundExpression = Nothing
      If syntax.OptionalColorExpression IsNot Nothing Then
        color = BindExpression(syntax.OptionalColorExpression, TypeSymbol.Single)
      End If
      Return New BoundPresetStatement([step], x, y, color)
    End Function

    Private Function BindRemStatement(syntax As RemStatementSyntax) As BoundStatement
      ' Check for metacommands
      Dim comment = syntax.Comment.ToUpper().Trim()
      If comment = "$DYNAMIC" Then
        m_arrayModeDynamic = True
      ElseIf comment = "$STATIC" Then
        m_arrayModeDynamic = False
      End If

      Return New BoundRemStatement()
    End Function

    Private Shared Function BindReturnGosubStatement(syntax As ReturnGosubStatementSyntax) As BoundStatement
      Dim value = syntax.TargetToken?.Text
      If value IsNot Nothing AndAlso IsNumeric(value) Then
        value = $"{GOTO_LABEL_PREFIX}{value}"
      End If
      Dim label = If(value IsNot Nothing, New BoundLabel(value), Nothing)
      Return New BoundReturnGosubStatement(label)
    End Function

    Private Function BindReturnStatement(syntax As ReturnStatementSyntax) As BoundStatement

      Dim expression = If(syntax.Expression Is Nothing, Nothing, BindExpression(syntax.Expression))

      If m_function Is Nothing Then
        If m_isScript Then
          ' Ignore because we allow both return with and without values.
          If expression Is Nothing Then
            expression = New BoundLiteralExpression("")
          End If
        ElseIf expression IsNot Nothing Then
          ' Main does not support return values.
          Diagnostics.ReportInvalidReturnWithValueInGlobalStatements(syntax.Expression.Location)
        End If
      Else
        If m_function.Type Is TypeSymbol.Nothing Then
          If expression IsNot Nothing Then
            Diagnostics.ReportInvalidReturnExpression(syntax.Expression.Location, m_function.Name)
          End If
        Else
          If expression Is Nothing Then
            Diagnostics.ReportMissingReturnExpression(syntax.ReturnKeyword.Location, m_function.Type)
          Else
            expression = BindConversion(syntax.Expression.Location, expression, m_function.Type)
          End If
        End If
      End If

      Return New BoundReturnStatement(expression)

    End Function

    Private Function BindRmDirStatement(syntax As RmDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundRmDirStatement(path)
    End Function

    Private Function BindScreenStatement(syntax As ScreenStatementSyntax) As BoundStatement

      Dim mode As BoundExpression = Nothing
      Dim colorBurst As BoundExpression = Nothing
      Dim apage As BoundExpression = Nothing
      Dim vpage As BoundExpression = Nothing
      Dim [erase] As BoundExpression = Nothing

      If syntax.OptionalModeExpression IsNot Nothing Then
        mode = BindExpression(syntax.OptionalModeExpression, TypeSymbol.Integer)
      End If
      If syntax.OptionalColorBurstExpression IsNot Nothing Then
        colorBurst = BindExpression(syntax.OptionalColorBurstExpression, TypeSymbol.Integer)
      End If
      If syntax.OptionalApageExpression IsNot Nothing Then
        apage = BindExpression(syntax.OptionalApageExpression, TypeSymbol.Integer)
      End If
      If syntax.OptionalVpageExpression IsNot Nothing Then
        vpage = BindExpression(syntax.OptionalVpageExpression, TypeSymbol.Integer)
      End If
      If syntax.OptionalEraseExpression IsNot Nothing Then
        [erase] = BindExpression(syntax.OptionalEraseExpression, TypeSymbol.Integer)
      End If

      Return New BoundScreenStatement(mode, colorBurst, apage, vpage, [erase])

    End Function

    Private Function BindSingleLineIfStatement(syntax As SingleLineIfStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        ElseIf syntax.ElseClause IsNot Nothing Then
          Diagnostics.ReportUnreachableCode(syntax.ElseClause.Statements)
        End If
      End If

      Dim statements As BoundStatement = Nothing
      If syntax.Statements.Kind = SyntaxKind.BlockStatement Then
        Dim child = syntax.Statements.GetChildren.First
        If child.Kind = SyntaxKind.ExpressionStatement Then
          child = child.GetChildren.First
          If child.Kind = SyntaxKind.LiteralExpression Then
            child = child.GetChildren.First
            If child.Kind = SyntaxKind.NumberToken Then
              Dim value = CType(child, SyntaxToken).Text
              If IsNumeric(value) Then
                ' An inferred GOTO... old school IF statement.
                value = $"{GOTO_LABEL_PREFIX}{value}"
                Dim label = New BoundLabel(value)
                Dim statement As BoundStatement = New BoundGotoStatement(label)
                statements = New BoundBlockStatement({statement}.ToImmutableArray)
              End If
            End If
          End If
        End If
      End If
      If statements Is Nothing Then
        statements = BindStatement(syntax.Statements)
      End If
      Dim elseStatement = If(syntax.ElseClause IsNot Nothing, BindStatement(syntax.ElseClause.Statements), Nothing)
      Return New BoundIfStatement(condition, statements, ImmutableArray(Of BoundElseIfStatement).Empty, elseStatement)

    End Function

    Private Shared Function BindStopStatement(syntax As StopStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundStopStatement()
    End Function

    Private Shared Function BindSystemStatement(syntax As SystemStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundSystemStatement()
    End Function

    Private Function BindSwapStatement(syntax As SwapStatementSyntax) As BoundStatement
      Dim variable1 = BindNameExpression(CType(syntax.Variable1, NameExpressionSyntax))
      Dim variable2 = BindNameExpression(CType(syntax.Variable2, NameExpressionSyntax))
      Return New BoundSwapStatement(variable1, variable2)
    End Function

    Private Function BindUnaryExpression(syntax As UnaryExpressionSyntax) As BoundExpression
      Dim boundOperand = BindExpression(syntax.Operand)
      If boundOperand.Type Is TypeSymbol.Error Then Return New BoundErrorExpression
      Dim boundOperator = BoundUnaryOperator.Bind(syntax.OperatorToken.Kind, boundOperand.Type)
      If boundOperator Is Nothing Then
        Diagnostics.ReportUndefinedUnaryOperator(syntax.OperatorToken.Location, syntax.OperatorToken.Text, boundOperand.Type)
        Return New BoundErrorExpression
      End If
      Return New BoundUnaryExpression(boundOperator, boundOperand, syntax)
    End Function

    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax) As BoundStatement
      Dim name = syntax.Identifier.Text
      Dim bounds = syntax.Bounds
      Dim isArray = bounds IsNot Nothing
      Dim type As TypeSymbol = BindAsClause(syntax.AsClause)
      If type Is Nothing Then
        ' No AS clause, check variable name suffix
        Dim suffix = name.Last
        Select Case suffix
          Case "%"c : type = TypeSymbol.Integer
          Case "&"c : type = TypeSymbol.Long
          Case "!"c : type = TypeSymbol.Single
          Case "#"c : type = TypeSymbol.Double
          Case "$"c : type = TypeSymbol.String
          Case Else
            type = TypeSymbol.Single ' Default for arrays without suffix
        End Select
      End If
      Dim lower As BoundExpression = Nothing
      Dim upper As BoundExpression = Nothing
      Dim dimensionCount = 0
      If bounds IsNot Nothing Then
        dimensionCount = bounds.Dimensions.Where(Function(d) TypeOf d Is DimensionClauseSyntax).Count()
        BindDimensionsClause(bounds, lower, upper)
        ' Validate array bounds at compile time if they are constants
        ValidateArrayBounds(syntax.Identifier, lower, upper)
      End If
      Dim isStaticArray = Not m_arrayModeDynamic
      Dim variable = New VariableSymbol(name, isArray, type, lower, upper, isStaticArray, dimensionCount)

      Return New BoundVariableDeclaration(variable, Nothing)
    End Function

    Private Function BindArrayDeclaration(identifier As SyntaxToken, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, dimensionCount As Integer, Optional isStaticArray As Boolean = False) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(m_function Is Nothing,
                       DirectCast(New GlobalArraySymbol(name, type, lower, upper, isStaticArray, dimensionCount), VariableSymbol),
                       DirectCast(New LocalArraySymbol(name, type, lower, upper, isStaticArray, dimensionCount), VariableSymbol))
      If [declare] Then
        m_scope.TryDeclareVariable(variable)
      End If
      Return variable
    End Function

    Private Function BindVariableDeclaration(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol, Optional constant As BoundConstant = Nothing) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(m_function Is Nothing,
                        DirectCast(New GlobalVariableSymbol(name, isReadOnly, type, constant), VariableSymbol),
                        DirectCast(New LocalVariableSymbol(name, isReadOnly, type, constant), VariableSymbol))
      If [declare] AndAlso Not m_scope.TryDeclareVariable(variable) Then
        Diagnostics.ReportSymbolAlreadyDeclared(identifier.Location, name)
      End If
      Return variable
    End Function

    Private Function BindDimStatement(syntax As DimStatementSyntax) As BoundStatement

      Dim boundDeclarations = ImmutableArray.CreateBuilder(Of BoundVariableDeclaration)
      Dim isShared = syntax.OptionalSharedKeyword IsNot Nothing

      For Each variableNode In syntax.Variables
        If TypeOf variableNode Is VariableDeclarationSyntax Then
          Dim variableDecl = CType(variableNode, VariableDeclarationSyntax)
          Dim boundDecl = CType(BindVariableDeclaration(variableDecl), BoundVariableDeclaration)
          Dim variable = boundDecl.Variable
          If Not m_scope.TryDeclareVariable(variable) Then
            Diagnostics.ReportSymbolAlreadyDeclared(variableDecl.Identifier.Location, variable.Name)
          End If
          boundDeclarations.Add(boundDecl)
        End If
      Next

      Return New BoundDimStatement(boundDeclarations.ToImmutable(), isShared)
    End Function

    Private Function BindRedimStatement(syntax As RedimStatementSyntax) As BoundStatement
      Dim preserve = syntax.OptionalPreserveKeyword IsNot Nothing
      Dim isShared = syntax.OptionalSharedKeyword IsNot Nothing
      Dim boundDeclarations = ImmutableArray.CreateBuilder(Of BoundVariableDeclaration)

      For Each variableNode In syntax.Variables
        If TypeOf variableNode Is VariableDeclarationSyntax Then
          Dim variableDecl = CType(variableNode, VariableDeclarationSyntax)
          Dim variableName = variableDecl.Identifier.Text

          ' Check if variable already exists
          Dim existingSymbol = m_scope.TryLookupSymbol(variableName)

          If existingSymbol Is Nothing OrElse Not (TypeOf existingSymbol Is VariableSymbol AndAlso CType(existingSymbol, VariableSymbol).IsArray) Then
            ' Variable doesn't exist or is not an array - create new dynamic array
            ' Extract type from AS clause or default to Single
            Dim arrayType As TypeSymbol = BindAsClause(variableDecl.AsClause)
            If arrayType Is Nothing Then
              ' No AS clause, check variable name suffix
              Dim suffix = variableName.Last
              Select Case suffix
                Case "%"c : arrayType = TypeSymbol.Integer
                Case "&"c : arrayType = TypeSymbol.Long
                Case "!"c : arrayType = TypeSymbol.Single
                Case "#"c : arrayType = TypeSymbol.Double
                Case "$"c : arrayType = TypeSymbol.String
                Case Else
                  arrayType = TypeSymbol.Single ' Default for dynamic arrays
              End Select
            End If

            ' Count dimensions
            Dim dimensionCount = 0
            If variableDecl.Bounds IsNot Nothing Then
              dimensionCount = variableDecl.Bounds.Dimensions.Where(Function(d) TypeOf d Is DimensionClauseSyntax).Count()
            End If

            ' Bind bounds
            Dim lower As BoundExpression = Nothing
            Dim upper As BoundExpression = Nothing
            If variableDecl.Bounds IsNot Nothing Then
              BindDimensionsClause(variableDecl.Bounds, lower, upper)
              ' Validate array bounds at compile time if they are constants
              ValidateArrayBounds(variableDecl.Identifier, lower, upper)
            End If

            ' Create new array respecting current $STATIC/$DYNAMIC metacommand state
            Dim isStaticArray = Not m_arrayModeDynamic
            Dim newArray = BindArrayDeclaration(variableDecl.Identifier, arrayType, lower, upper, dimensionCount, isStaticArray)
            Dim boundDecl = New BoundVariableDeclaration(newArray, Nothing)
            boundDeclarations.Add(boundDecl)
          Else
            ' Variable exists and is an array - proceed with normal REDIM validation
            Dim boundDecl = CType(BindVariableDeclaration(variableDecl), BoundVariableDeclaration)
            Dim newVariable = boundDecl.Variable

            Dim existingVariable = CType(existingSymbol, VariableSymbol)
            If existingVariable.Type IsNot newVariable.Type Then
              Diagnostics.ReportRedimTypeMismatch(variableDecl.Identifier.Location, newVariable.Name, existingVariable.Type.Name, newVariable.Type.Name)
            End If
            ' Check if trying to REDIM a static array
            If existingVariable.IsStaticArray Then
              Diagnostics.ReportRedimOnStaticArray(variableDecl.Identifier.Location, newVariable.Name)
            End If
            ' Check if REDIM changes the number of dimensions
            If existingVariable.DimensionCount <> newVariable.DimensionCount Then
              Diagnostics.ReportRedimDimensionCountMismatch(variableDecl.Identifier.Location, newVariable.Name, existingVariable.DimensionCount, newVariable.DimensionCount)
            End If

            boundDeclarations.Add(boundDecl)
          End If
        End If
      Next

      Return New BoundRedimStatement(preserve, boundDeclarations.ToImmutable(), isShared)
    End Function

    Private Function BindEraseStatement(syntax As EraseStatementSyntax) As BoundStatement
      Dim boundVariables = ImmutableArray.CreateBuilder(Of BoundExpression)

      For Each variableNode In syntax.Variables
        If TypeOf variableNode Is SyntaxToken AndAlso CType(variableNode, SyntaxToken).Kind = SyntaxKind.IdentifierToken Then
          Dim token = CType(variableNode, SyntaxToken)
          Dim variableSymbol = DetermineVariableReference(token)
          If variableSymbol IsNot Nothing Then
            ' Validate that ERASE is only used on arrays
            If Not variableSymbol.IsArray Then
              Diagnostics.ReportEraseRequiresArray(token.Location, token.Text)
            Else
              Dim boundVariable = New BoundVariableExpression(variableSymbol)
              boundVariables.Add(boundVariable)
            End If
          Else
            Diagnostics.ReportUndefinedVariable(token.Location, token.Text)
          End If
        End If
      Next

      Return New BoundEraseStatement(boundVariables.ToImmutable())
    End Function

    Private Function BindCallStatement(syntax As CallStatementSyntax) As BoundStatement
      Dim callSyntax = New CallExpressionSyntax(syntax.SyntaxTree, syntax.Identifier, syntax.OpenParen, syntax.Expressions, syntax.CloseParen)
      Dim boundCall = CType(BindExpression(callSyntax, True), BoundCallExpression)
      Return New BoundCallStatement(boundCall)
    End Function

    Private Sub BindDimensionsClause(syntax As DimensionsClauseSyntax, ByRef lower As BoundExpression, ByRef upper As BoundExpression)
      Dim dimensions = syntax.Dimensions
      If dimensions.Count = 0 Then
        lower = New BoundLiteralExpression(0)
        upper = New BoundLiteralExpression(10)
        Return
      End If
      Dim dimension = CType(dimensions(0), DimensionClauseSyntax)
      Dim lowerSyntax = dimension.OptionalLower
      Dim upperSyntax = dimension.Upper
      lower = If(lowerSyntax IsNot Nothing, BindExpression(lowerSyntax), New BoundLiteralExpression(m_optionBase))
      upper = BindExpression(upperSyntax)
    End Sub

    Private Sub ValidateArrayBounds(identifier As SyntaxToken, lower As BoundExpression, upper As BoundExpression)
      ' Only validate if both bounds are compile-time constants
      If TypeOf lower Is BoundLiteralExpression AndAlso TypeOf upper Is BoundLiteralExpression Then
        Try
          Dim lowerValue = Convert.ToInt32(DirectCast(lower, BoundLiteralExpression).Value)
          Dim upperValue = Convert.ToInt32(DirectCast(upper, BoundLiteralExpression).Value)
          If lowerValue > upperValue Then
            Diagnostics.ReportInvalidArrayBounds(identifier.Location, lowerValue, upperValue)
          End If
          ' Check array size limit
          Dim arraySize = CLng(upperValue) - CLng(lowerValue) + 1
          If arraySize > 65535 Then
            Diagnostics.ReportArraySizeTooLarge(identifier.Location, arraySize)
          End If
        Catch ex As OverflowException
          Diagnostics.ReportArrayBoundsOutOfRange(identifier.Location)
        End Try
      End If
    End Sub

    Private Function BindBoundFunction(syntax As CallExpressionSyntax) As BoundExpression
      Dim functionName = syntax.Identifier.Text.ToUpper()
      Dim isLbound = (functionName = "LBOUND")
      Dim isUbound = (functionName = "UBOUND")

      ' Validate arguments: LBOUND/UBOUND(array[, dimension])
      If syntax.Arguments.Count < 1 Or syntax.Arguments.Count > 2 Then
        Diagnostics.ReportWrongArgumentCount(syntax.Identifier.Location, functionName, 1, syntax.Arguments.Count)
        Return New BoundErrorExpression
      End If

      ' First argument must be an array variable
      Dim arrayArg = BindExpression(syntax.Arguments(0))
      If Not (TypeOf arrayArg Is BoundVariableExpression AndAlso DirectCast(arrayArg, BoundVariableExpression).Variable.IsArray) Then
        Diagnostics.ReportArgumentMustBeArray(syntax.Arguments(0).Location, functionName)
        Return New BoundErrorExpression
      End If

      Dim arrayVariable = DirectCast(arrayArg, BoundVariableExpression).Variable

      ' Optional second argument is dimension (defaults to 1)
      Dim dimension As BoundExpression = Nothing
      If syntax.Arguments.Count = 2 Then
        dimension = BindExpression(syntax.Arguments(1))
      Else
        dimension = New BoundLiteralExpression(1)
      End If

      Return New BoundBoundFunctionExpression(arrayVariable, dimension, isLbound)
    End Function

    Private Function BindVariableReference(identifierToken As SyntaxToken) As VariableSymbol

      Dim name = identifierToken.Text
      Dim s = m_scope.TryLookupSymbol(name)

      If TypeOf s Is VariableSymbol Then
        Return TryCast(s, VariableSymbol)
      ElseIf s Is Nothing Then
        If Not OPTION_EXPLICIT Then
          Dim type As TypeSymbol '= TypeSymbol.String
          Dim suffix = identifierToken.Text.Last
          Select Case suffix
            Case "%"c : type = TypeSymbol.Integer
            Case "&"c : type = TypeSymbol.Long
            Case "!"c : type = TypeSymbol.Single
            Case "#"c : type = TypeSymbol.Double
            Case "$"c : type = TypeSymbol.String
            Case Else
              'TODO: This needs to be set based on current DEFINT, etc.
              type = TypeSymbol.Single
          End Select
          Dim variable = BindVariableDeclaration(identifierToken, False, type)
          If variable Is Nothing Then
            Diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
            Return Nothing
          Else
            s = m_scope.TryLookupSymbol(name)
            Return TryCast(s, VariableSymbol)
          End If
        Else
          Diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
          Return Nothing
        End If
      Else
        Diagnostics.ReportNotAVariable(identifierToken.Location, name)
        Return Nothing
      End If

    End Function

    Private Function BindWhileStatement(syntax As WhileStatementSyntax) As BoundStatement

      Dim condition = BindExpression(syntax.Expression, TypeSymbol.Boolean)

      If condition.ConstantValue IsNot Nothing Then
        If Not CBool(condition.ConstantValue.Value) Then
          Diagnostics.ReportUnreachableCode(syntax.Statements)
        End If
      End If

      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing
      Dim statements = BindLoopBody(syntax.Statements, exitLabel, continueLabel)
      Return New BoundWhileStatement(condition, statements, exitLabel, continueLabel)

    End Function

    Private Function DetermineVariableReference(identifierToken As SyntaxToken) As VariableSymbol

      Dim name = identifierToken.Text
      Dim s = m_scope.TryLookupSymbol(name)

      If TypeOf s Is VariableSymbol Then
        Return TryCast(s, VariableSymbol)
      Else
        Return Nothing
      End If

    End Function

    Private Shared Function LookupType(name As String) As TypeSymbol
      Select Case name.ToLower
        Case "any" : Return TypeSymbol.Any
        Case "object" : Return TypeSymbol.Object
        Case "udt" : Return TypeSymbol.Udt
        Case "datetime" : Return TypeSymbol.DateTime
        Case "boolean" : Return TypeSymbol.Boolean
        Case "byte" : Return TypeSymbol.Byte
        Case "SByte" : Return TypeSymbol.SByte
        Case "char" : Return TypeSymbol.Char
        Case "decimal" : Return TypeSymbol.Decimal
        Case "integer" : Return TypeSymbol.Integer
        Case "uinteger" : Return TypeSymbol.UInteger
        Case "long" : Return TypeSymbol.Long
        Case "ulong" : Return TypeSymbol.ULong
        Case "long64" : Return TypeSymbol.Long64
        Case "ulong64" : Return TypeSymbol.ULong64
        Case "single" : Return TypeSymbol.Single
        Case "double" : Return TypeSymbol.Double
        Case "string" : Return TypeSymbol.String
        Case Else
          Return Nothing
      End Select
    End Function

    Private Function BindDataStatement(syntax As DataStatementSyntax) As BoundStatement
      Dim data = ImmutableArray.CreateBuilder(Of Object)()
      Dim i = 0
      While i < syntax.Tokens.Length
        Dim token = syntax.Tokens(i)
        If token.Kind = SyntaxKind.NumberToken Then
          If i > 0 AndAlso syntax.Tokens(i - 1).Kind = SyntaxKind.MinusToken Then
            data.Add(-CDbl(token.Value))
          Else
            data.Add(token.Value)
          End If
        ElseIf token.Kind = SyntaxKind.StringToken Then
          data.Add(token.Value)
        ElseIf token.Kind = SyntaxKind.CommaToken OrElse token.Kind = SyntaxKind.MinusToken Then
          ' skip separators
        Else
          ' Diagnostics.ReportInvalidDataConstant(token.Location, token.Text)
        End If
        i += 1
      End While
      Return New BoundDataStatement(data.ToImmutable())
    End Function

    Private Function BindReadStatement(syntax As ReadStatementSyntax) As BoundStatement
      Dim variables = ImmutableArray.CreateBuilder(Of VariableSymbol)()
      Dim i = 0
      While i < syntax.Tokens.Length
        Dim token = syntax.Tokens(i)
        If token.Kind = SyntaxKind.IdentifierToken Then
          Dim variable = BindVariableReference(token)
          If variable IsNot Nothing Then
            variables.Add(variable)
          End If
        ElseIf token.Kind = SyntaxKind.CommaToken Then
          ' skip
        Else
          ' Diagnostics.ReportInvalidReadVariable(token.Location, token.Text)
        End If
        i += 1
      End While
      Return New BoundReadStatement(variables.ToImmutable())
    End Function

  End Class

End Namespace