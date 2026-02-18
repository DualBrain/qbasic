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
    Private ReadOnly m_forLoopStack As New Stack(Of SyntaxToken) ' Track FOR loop variables for NEXT validation
    Private m_labelCounter As Integer
    Private m_optionBase As Integer = 0 ' Default to 0 as per QBasic spec
    Private m_optionBaseDeclared As Boolean = False ' Track if OPTION BASE was already declared
    Private m_arrayModeDynamic As Boolean = False ' Track current $DYNAMIC/$STATIC metacommand state (default $STATIC)
    Private ReadOnly m_defTypeRanges As New Dictionary(Of String, TypeSymbol) ' Store DEF type ranges
    Private m_currentLineNumber As Integer = 0 ' Track current line number for DATA statements

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

      ' Process DECLARE statements
      ' Since DECLARE statements are optional in this implementation,
      ' we'll just collect them but defer full validation to a future enhancement
      Dim declareStatements = syntaxTrees.SelectMany(Function(st) st.Root.Members).OfType(Of GlobalStatementSyntax)().
                                            Where(Function(gs) TypeOf gs.Statement Is DeclareStatementSyntax).
                                            Select(Function(gs) CType(gs.Statement, DeclareStatementSyntax))

      ' DECLARE statements are now processed without validation since they are optional
      ' Full validation against SUB/FUNCTION definitions can be added later

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
              Dim value = $"{CInt(trivia.Text)}".ToLower
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

      ' Check for unmatched FOR loops
      While binder.m_forLoopStack.Count > 0
        Dim unmatchedFor = binder.m_forLoopStack.Pop()
        Throw New QBasicBuildException(ErrorCode.ForWithoutNext)
      End While

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
        Case SyntaxKind.MemberAccessExpression : Return BindMemberAccessExpression(CType(syntax, MemberAccessExpressionSyntax))
        Case Else
          Throw New Exception($"Unexpected syntax {syntax.Kind}")
      End Select
    End Function

    Private Shared Function BindErrorStatement() As BoundStatement
      Return New BoundErrorStatement(New BoundErrorExpression)
    End Function

    Private Function BindErrorStatement(syntax As ErrorStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression)
      Return New BoundErrorStatement(syntax, expression)
    End Function

    Private Function BindGlobalStatement(syntax As StatementSyntax) As BoundStatement
      If TypeOf syntax Is LabelStatementSyntax Then
        Dim labelSyntax = CType(syntax, LabelStatementSyntax)
        If labelSyntax.Label.Kind = SyntaxKind.NumberToken AndAlso IsNumeric(labelSyntax.Label.Text) Then
          m_currentLineNumber = CInt(labelSyntax.Label.Text)
        End If
      End If
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
        Case SyntaxKind.BeepStatement : Return BindBeepStatement(CType(syntax, BeepStatementSyntax))
        Case SyntaxKind.BlockStatement : Return BindBlockStatement(CType(syntax, BlockStatementSyntax))
        Case SyntaxKind.ChDirStatement : Return BindChDirStatement(CType(syntax, ChDirStatementSyntax))
        Case SyntaxKind.CircleStatement : Return BindCircleStatement(CType(syntax, CircleStatementSyntax))
        Case SyntaxKind.ColorStatement : Return BindColorStatement(CType(syntax, ColorStatementSyntax))
        Case SyntaxKind.ClearStatement : Return BindClearStatement(CType(syntax, ClearStatementSyntax))
        Case SyntaxKind.ClsStatement : Return BindClsStatement(CType(syntax, ClsStatementSyntax))
        Case SyntaxKind.ColorStatement : Return BindColorStatement(CType(syntax, ColorStatementSyntax))
        Case SyntaxKind.ConstStatement : Return BindConstStatement(CType(syntax, ConstStatementSyntax))
        Case SyntaxKind.ContinueStatement : Return BindContinueStatement(CType(syntax, ContinueStatementSyntax))
        Case SyntaxKind.DimStatement : Return BindDimStatement(CType(syntax, DimStatementSyntax))
        Case SyntaxKind.EraseStatement : Return BindEraseStatement(CType(syntax, EraseStatementSyntax))
        Case SyntaxKind.RedimStatement : Return BindRedimStatement(CType(syntax, RedimStatementSyntax))
        Case SyntaxKind.DoUntilStatement : Return BindDoUntilStatement(CType(syntax, DoUntilStatementSyntax))
        Case SyntaxKind.DoWhileStatement : Return BindDoWhileStatement(CType(syntax, DoWhileStatementSyntax))
        Case SyntaxKind.EndStatement : Return BindEndStatement(CType(syntax, EndStatementSyntax))
        Case SyntaxKind.ExitStatement : Return BindExitStatement(CType(syntax, ExitStatementSyntax))
        Case SyntaxKind.ExpressionStatement : Return BindExpressionStatement(CType(syntax, ExpressionStatementSyntax))
        Case SyntaxKind.FieldStatement : Return BindFieldStatement(CType(syntax, FieldStatementSyntax))
        Case SyntaxKind.ForStatement : Return BindForStatement(CType(syntax, ForStatementSyntax))
        Case SyntaxKind.GosubStatement : Return BindGosubStatement(CType(syntax, GosubStatementSyntax))
        Case SyntaxKind.GetFileStatement : Return BindGetFileStatement(CType(syntax, GetFileStatementSyntax))
        Case SyntaxKind.GotoStatement : Return BindGotoStatement(CType(syntax, GotoStatementSyntax))
        Case SyntaxKind.IfStatement : Return BindIfStatement(CType(syntax, IfStatementSyntax))
        Case SyntaxKind.InputStatement : Return BindInputStatement(CType(syntax, InputStatementSyntax))
        Case SyntaxKind.KillStatement : Return BindKillStatement(CType(syntax, KillStatementSyntax))
        Case SyntaxKind.LabelStatement : Return BindLabelStatement(CType(syntax, LabelStatementSyntax))
        Case SyntaxKind.LetStatement : Return BindLetStatement(CType(syntax, LetStatementSyntax))
        Case SyntaxKind.LsetStatement : Return BindLsetStatement(CType(syntax, LSetStatementSyntax))
        Case SyntaxKind.RsetStatement : Return BindRsetStatement(CType(syntax, RSetStatementSyntax))
        Case SyntaxKind.LineStatement : Return BindLineStatement(CType(syntax, LineStatementSyntax))
        Case SyntaxKind.LineInputFileStatement : Return BindLineInputFileStatement(CType(syntax, LineInputFileStatementSyntax))
        Case SyntaxKind.LocateStatement : Return BindLocateStatement(CType(syntax, LocateStatementSyntax))
        Case SyntaxKind.MidStatement : Return BindMidStatement(CType(syntax, MidStatementSyntax))
        Case SyntaxKind.MkDirStatement : Return BindMkDirStatement(CType(syntax, MkDirStatementSyntax))
        Case SyntaxKind.NameStatement : Return BindNameStatement(CType(syntax, NameStatementSyntax))
        Case SyntaxKind.NextStatement : Return BindNextStatement(CType(syntax, NextStatementSyntax))
        Case SyntaxKind.OnErrorGotoStatement : Return BindOnErrorGotoStatement(CType(syntax, OnErrorGotoStatementSyntax))
        Case SyntaxKind.OnTimerGosubStatement : Return BindOnTimerGosubStatement(CType(syntax, OnTimerGosubStatementSyntax))
        Case SyntaxKind.OnComGosubStatement : Return BindOnComGosubStatement(CType(syntax, OnComGosubStatementSyntax))
        Case SyntaxKind.OnKeyGosubStatement : Return BindOnKeyGosubStatement(CType(syntax, OnKeyGosubStatementSyntax))
        Case SyntaxKind.OnStrigGosubStatement : Return BindOnStrigGosubStatement(CType(syntax, OnStrigGosubStatementSyntax))
        Case SyntaxKind.OnPlayGosubStatement : Return BindOnPlayGosubStatement(CType(syntax, OnPlayGosubStatementSyntax))
        Case SyntaxKind.OnPenGosubStatement : Return BindOnPenGosubStatement(CType(syntax, OnPenGosubStatementSyntax))
        Case SyntaxKind.OnGotoStatement : Return BindOnGotoStatement(CType(syntax, OnGotoStatementSyntax))
        Case SyntaxKind.OnGosubStatement : Return BindOnGosubStatement(CType(syntax, OnGosubStatementSyntax))
        Case SyntaxKind.OpenStatement : Return BindOpenStatement(CType(syntax, OpenStatementSyntax))
        Case SyntaxKind.CloseStatement : Return BindCloseStatement(CType(syntax, CloseStatementSyntax))
        Case SyntaxKind.ResetStatement : Return BindResetStatement(CType(syntax, ResetStatementSyntax))
        Case SyntaxKind.SeekStatement : Return BindSeekStatement(CType(syntax, SeekStatementSyntax))
        Case SyntaxKind.OptionStatement : Return BindOptionStatement(CType(syntax, OptionStatementSyntax))
        Case SyntaxKind.PokeStatement : Return BindPokeStatement(CType(syntax, PokeStatementSyntax))
        Case SyntaxKind.PrintStatement : Return BindPrintStatement(CType(syntax, PrintStatementSyntax))
        Case SyntaxKind.PutFileStatement : Return BindPutFileStatement(CType(syntax, PutFileStatementSyntax))
        Case SyntaxKind.WriteStatement : Return BindWriteStatement(CType(syntax, WriteStatementSyntax))
        Case SyntaxKind.PsetKeyword : Return BindPsetStatement(CType(syntax, PsetStatementSyntax))
        Case SyntaxKind.PresetKeyword : Return BindPresetStatement(CType(syntax, PresetStatementSyntax))
        Case SyntaxKind.RemStatement : Return BindRemStatement(CType(syntax, RemStatementSyntax))
        Case SyntaxKind.ReturnGosubStatement : Return BindReturnGosubStatement(CType(syntax, ReturnGosubStatementSyntax))
        Case SyntaxKind.ReturnStatement : Return BindReturnStatement(CType(syntax, ReturnStatementSyntax))
        Case SyntaxKind.ResumeStatement : Return BindResumeStatement(CType(syntax, ResumeStatementSyntax))
        Case SyntaxKind.ResumeNextStatement : Return BindResumeNextStatement(CType(syntax, ResumeNextStatementSyntax))
        Case SyntaxKind.RmDirStatement : Return BindRmDirStatement(CType(syntax, RmDirStatementSyntax))
        Case SyntaxKind.ScreenKeyword : Return BindScreenStatement(CType(syntax, ScreenStatementSyntax))
        Case SyntaxKind.SingleLineIfStatement : Return BindSingleLineIfStatement(CType(syntax, SingleLineIfStatementSyntax))
        Case SyntaxKind.StopStatement : Return BindStopStatement(CType(syntax, StopStatementSyntax))
        Case SyntaxKind.SwapStatement : Return BindSwapStatement(CType(syntax, SwapStatementSyntax))
        Case SyntaxKind.SystemStatement : Return BindSystemStatement(CType(syntax, SystemStatementSyntax))
        Case SyntaxKind.VariableDeclarationStatement : Return BindVariableDeclaration(CType(syntax, VariableDeclarationSyntax))
        Case SyntaxKind.WhileStatement : Return BindWhileStatement(CType(syntax, WhileStatementSyntax))
        Case SyntaxKind.WendStatement : Return BindWendStatement(CType(syntax, WendStatementSyntax))
        Case SyntaxKind.DataStatement : Return BindDataStatement(CType(syntax, DataStatementSyntax))
        Case SyntaxKind.EnvironStatement : Return BindEnvironStatement(CType(syntax, EnvironStatementSyntax))
        Case SyntaxKind.ErrorStatement : Return BindErrorStatement(CType(syntax, ErrorStatementSyntax))
        Case SyntaxKind.DateStatement : Return BindDateStatement(CType(syntax, DateStatementSyntax))
        Case SyntaxKind.ReadStatement : Return BindReadStatement(CType(syntax, ReadStatementSyntax))
        Case SyntaxKind.RestoreStatement : Return BindRestoreStatement(CType(syntax, RestoreStatementSyntax))
        Case SyntaxKind.TimeStatement : Return BindTimeStatement(CType(syntax, TimeStatementSyntax))
        Case SyntaxKind.SleepStatement : Return BindSleepStatement(CType(syntax, SleepStatementSyntax))
        Case SyntaxKind.TimerStatement : Return BindTimerStatement(CType(syntax, TimerStatementSyntax))
        Case SyntaxKind.ComStatement : Return BindComStatement(CType(syntax, ComStatementSyntax))
        'Case SyntaxKind.KeyEventStatement : Return BindKeyEventStatement(CType(syntax, KeyEventStatementSyntax))
        'Case SyntaxKind.KeyOffStatement : Return BindKeyOffStatement(CType(syntax, KeyOffStatementSyntax))
        Case SyntaxKind.StrigStatement : Return BindStrigStatement(CType(syntax, StrigStatementSyntax))
        'Case SyntaxKind.PlayEventStatement : Return BindPlayEventStatement(CType(syntax, PlayEventStatementSyntax))
        Case SyntaxKind.PenStatement : Return BindPenStatement(CType(syntax, PenStatementSyntax))
        Case SyntaxKind.SelectCaseStatement : Return BindSelectCaseStatement(CType(syntax, SelectCaseStatementSyntax))
        Case SyntaxKind.CallStatement : Return BindCallStatement(CType(syntax, CallStatementSyntax))
        Case SyntaxKind.ChainStatement : Return BindChainStatement(CType(syntax, ChainStatementSyntax))
        Case SyntaxKind.OutStatement : Return BindOutStatement(CType(syntax, OutStatementSyntax))
        Case SyntaxKind.DeclareStatement : Return BindDeclareStatement(CType(syntax, DeclareStatementSyntax))
        Case SyntaxKind.DefTypeStatement : Return BindDefTypeStatement(CType(syntax, DefTypeStatementSyntax))
        Case SyntaxKind.TypeStatement : Return BindTypeStatement(CType(syntax, TypeStatementSyntax))
        Case SyntaxKind.CommonStatement : Return BindCommonStatement(CType(syntax, CommonStatementSyntax))
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

    ''' <summary>
    ''' Resolve variable type considering type characters and defaults.
    ''' DEF statements will be implemented later.
    ''' </summary>
    Private Function ResolveVariableType(variableName As String) As TypeSymbol
      If String.IsNullOrEmpty(variableName) Then Return QB.CodeAnalysis.Symbols.TypeSymbol.Single

      ' Check for type character suffix first (highest precedence)
      Dim lastChar = variableName.Last()
      Select Case lastChar
        Case "%"c : Return QB.CodeAnalysis.Symbols.TypeSymbol.Integer
        Case "&"c : Return QB.CodeAnalysis.Symbols.TypeSymbol.Long
        Case "!"c : Return QB.CodeAnalysis.Symbols.TypeSymbol.Single
        Case "#"c : Return QB.CodeAnalysis.Symbols.TypeSymbol.Double
        Case "$"c : Return QB.CodeAnalysis.Symbols.TypeSymbol.String
      End Select

      ' Check DEF statements (medium precedence)
      Dim firstChar = variableName(0).ToString().ToUpper()
      If m_defTypeRanges.ContainsKey(firstChar) Then
        Return m_defTypeRanges(firstChar)
      End If

      ' Default to SINGLE (lowest precedence)
      Return QB.CodeAnalysis.Symbols.TypeSymbol.Single
    End Function

    Private Function BindAsClause(syntax As AsClause) As QB.CodeAnalysis.Symbols.TypeSymbol
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

      'Dim left = BindExpression(syntax.Variable)
      'Dim right = BindExpression(syntax.Expression)
      'If left.Kind = BoundNodeKind.VariableExpression Then
      '  Dim var = DirectCast(left, BoundVariableExpression).Variable
      '  If right.Type IsNot var.Type Then
      '    Diagnostics.ReportCannotConvert(syntax.EqualToken.Location, right.Type, var.Type)
      '    Return New BoundErrorExpression
      '  End If
      '  Return New BoundAssignmentExpression(left, right)
      'Else
      '  Diagnostics.ReportInvalidExpressionStatement(syntax.Variable.Location)
      '  Return New BoundErrorExpression
      'End If
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

      ' First check if this is a built-in function that takes an array as first argument
      If syntax.Identifier.Text.ToUpper() = "LBOUND" Or syntax.Identifier.Text.ToUpper() = "UBOUND" Then
        Return BindBoundFunction(syntax)
      End If

      ' Then check if this is array access (identifier with parentheses)
      Dim variableSymbol = m_scope.TryLookupVariable(syntax.Identifier.Text)
      If variableSymbol IsNot Nothing AndAlso Not variableSymbol.IsArray AndAlso syntax.Arguments.Count = 1 Then
        ' Redeclare scalar as array
        Dim lowerBound = New BoundLiteralExpression(m_optionBase)
        Dim upperBound = New BoundLiteralExpression(10)
        variableSymbol = BindArrayDeclaration(syntax.Identifier, TypeSymbol.Single, lowerBound, upperBound, 1)
      End If
      If variableSymbol IsNot Nothing AndAlso variableSymbol.IsArray Then
        ' This is array access
        If syntax.Arguments.Count <> variableSymbol.DimensionCount Then
          ' Multi-dimensional arrays should have one argument per dimension
          Return New BoundErrorExpression()
        End If

        ' For multi-dimensional arrays, we need to combine the indices
        ' For now, just bind the first argument (single dimension arrays)
        ' TODO: Implement proper multi-dimensional index calculation
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
      If variableSymbol Is Nothing AndAlso syntax.Arguments IsNot Nothing AndAlso syntax.Arguments.Count = 1 Then
        Dim index = BindExpression(syntax.Arguments(0))
        ' Create automatic array with bounds [OPTION BASE]-10
        Dim lowerBound = New BoundLiteralExpression(m_optionBase)
        Dim upperBound = New BoundLiteralExpression(10)
        Dim autoArray = BindArrayDeclaration(syntax.Identifier, TypeSymbol.Single, lowerBound, upperBound, 1)
        Return New BoundArrayAccessExpression(autoArray, index)
      End If

      ' Error
      Diagnostics.ReportUndefinedFunction(syntax.Identifier.Location, syntax.Identifier.Text)
      Return New BoundErrorExpression

    End Function

    Private Function BindMemberAccessExpression(syntax As MemberAccessExpressionSyntax) As BoundExpression
      ' Bind the base expression first
      Dim baseExpr = BindExpression(syntax.Expression)

      ' Get the member name
      Dim memberName = syntax.Identifier.Text

      ' If the base is a UDT variable or array access of UDT, look up the field
      If baseExpr.Type Is TypeSymbol.Udt Then
        ' Get the variable from either BoundVariableExpression, BoundArrayAccessExpression, or BoundMemberAccessExpression
        Dim varSymbol As VariableSymbol = Nothing
        Dim nestedUdtType As UdtTypeSymbol = Nothing

        If TypeOf baseExpr Is BoundVariableExpression Then
          varSymbol = TryCast(CType(baseExpr, BoundVariableExpression).Variable, VariableSymbol)
        ElseIf TypeOf baseExpr Is BoundArrayAccessExpression Then
          varSymbol = CType(baseExpr, BoundArrayAccessExpression).Variable
        ElseIf TypeOf baseExpr Is BoundMemberAccessExpression Then
          ' Handle nested UDT member access (e.g., p.addr.street)
          ' The base member access expression has a UDT type, so we need to look up
          ' the parent UDT and then get the field from the nested UDT type
          Dim memberAccess = CType(baseExpr, BoundMemberAccessExpression)
          ' Look up the parent UDT variable
          Dim parentVarSymbol As VariableSymbol = Nothing
          If TypeOf memberAccess.Expression Is BoundVariableExpression Then
            parentVarSymbol = TryCast(CType(memberAccess.Expression, BoundVariableExpression).Variable, VariableSymbol)
          End If
          If parentVarSymbol IsNot Nothing AndAlso parentVarSymbol.UdtType IsNot Nothing Then
            Dim parentUdtType = parentVarSymbol.UdtType
            Dim parentField = parentUdtType.GetField(memberAccess.MemberName)
            If parentField IsNot Nothing Then
              ' Only try to resolve nested UDT if this is a nested UDT field
              If parentField.FieldType Is TypeSymbol.Udt AndAlso Not String.IsNullOrEmpty(parentField.UdtTypeName) Then
                ' Get the nested UDT type from the parent field's UdtTypeName
                nestedUdtType = m_scope.TryLookupType(parentField.UdtTypeName)
              End If
            End If
          End If
        End If

        If varSymbol IsNot Nothing Then
          ' Get the UDT type
          Dim udtType = varSymbol.UdtType
          If udtType IsNot Nothing Then
            ' Look up the field
            Dim field = udtType.GetField(memberName)
            If field IsNot Nothing Then
              ' Return a member access expression with the field type and fixed length
              Return New BoundMemberAccessExpression(baseExpr, memberName, field.FieldType, syntax, field.FixedLength)
            Else
              ' Field not found
              Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, memberName)
              Return New BoundErrorExpression()
            End If
          End If
        End If

        ' Handle nested UDT field access
        If nestedUdtType IsNot Nothing Then
          Dim field = nestedUdtType.GetField(memberName)
          If field IsNot Nothing Then
            Return New BoundMemberAccessExpression(baseExpr, memberName, field.FieldType, syntax, field.FixedLength)
          Else
            Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, memberName)
            Return New BoundErrorExpression()
          End If
        End If
      End If

      ' For now, return an error expression - member access needs full UDT support
      Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, memberName)
      Return New BoundErrorExpression()
    End Function

    Private Function BindPokeStatement(syntax As PokeStatementSyntax) As BoundStatement
      Dim offset = BindExpression(syntax.Offset)
      Dim value = BindExpression(syntax.Value)
      Return New BoundPokeStatement(syntax, offset, value)
    End Function

    Private Function BindOutStatement(syntax As OutStatementSyntax) As BoundStatement
      Dim port = BindExpression(syntax.Port)
      Dim data = BindExpression(syntax.Data)
      Return New BoundOutStatement(syntax, port, data)
    End Function

    Private Function BindChDirStatement(syntax As ChDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundChDirStatement(syntax, path)
    End Function

    Private Function BindChainStatement(syntax As ChainStatementSyntax) As BoundStatement
      Dim filename = BindExpression(syntax.Filename)
      Dim optionalLine = If(syntax.OptionalLine IsNot Nothing, BindExpression(syntax.OptionalLine), Nothing)
      Return New BoundChainStatement(syntax, filename, optionalLine)
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
      Return New BoundCircleStatement(syntax, [step], x, y, radius, color, start, [end], aspect)
    End Function

    Private Function BindClearStatement(syntax As ClearStatementSyntax) As BoundStatement
      Dim dummyExpression1 = If(syntax.DummyExpression1 IsNot Nothing, BindExpression(syntax.DummyExpression1), Nothing)
      Dim dummyExpression2 = If(syntax.DummyExpression2 IsNot Nothing, BindExpression(syntax.DummyExpression2), Nothing)
      Dim stackSpaceExpression = If(syntax.StackSpaceExpression IsNot Nothing, BindExpression(syntax.StackSpaceExpression), Nothing)
      Return New BoundClearStatement(syntax, dummyExpression1, dummyExpression2, stackSpaceExpression)
    End Function

    Private Function BindClsStatement(syntax As ClsStatementSyntax) As BoundStatement
      Dim expression = If(syntax.Expression IsNot Nothing, BindExpression(syntax.Expression), Nothing)
      Return New BoundClsStatement(syntax, expression)
    End Function

    Private Function BindColorStatement(syntax As ColorStatementSyntax) As BoundStatement
      Dim argumentExpression1 = If(syntax.ArgumentExpression1 IsNot Nothing, BindExpression(syntax.ArgumentExpression1), Nothing)
      Dim argumentExpression2 = If(syntax.ArgumentExpression2 IsNot Nothing, BindExpression(syntax.ArgumentExpression2), Nothing)
      Dim argumentExpression3 = If(syntax.ArgumentExpression3 IsNot Nothing, BindExpression(syntax.ArgumentExpression3), Nothing)
      Return New BoundColorStatement(syntax, argumentExpression1, argumentExpression2, argumentExpression3)
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
      Return New BoundDoUntilStatement(syntax, statements, expression, atBeginning, exitLabel, continueLabel)
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
      Return New BoundDoWhileStatement(syntax, statements, expression, atBeginning, exitLabel, continueLabel)
    End Function

    Private Shared Function BindBeepStatement(syntax As BeepStatementSyntax) As BoundStatement
      Return New BoundBeepStatement(syntax)
    End Function

    Private Shared Function BindEndStatement(syntax As EndStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundEndStatement(syntax)
    End Function

    Private Function BindExitStatement(syntax As ExitStatementSyntax) As BoundStatement

      If m_loopStack.Count = 0 Then
        Diagnostics.ReportInvalidBreakOrContinue(syntax.ExitKeyword.Location, syntax.ExitKeyword.Text)
        Return BindErrorStatement()
      End If

      Dim exitLabel = m_loopStack.Peek().ExitLabel
      Return New BoundGotoStatement(exitLabel)

    End Function

    Private Function BindEnvironStatement(syntax As EnvironStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, TypeSymbol.String)
      Return New BoundEnvironStatement(syntax, expression)
    End Function

    Private Function BindExpressionStatement(syntax As ExpressionStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, canBeVoid:=True)
      Return New BoundExpressionStatement(syntax, expression)
    End Function

    Private Function BindForStatement(syntax As ForStatementSyntax) As BoundStatement

      Dim lowerBound = BindExpression(syntax.FromValue, TypeSymbol.Single)
      Dim upperBound = BindExpression(syntax.ToValue, TypeSymbol.Single)
      Dim stepper = If(syntax.StepClause Is Nothing, Nothing, BindExpression(syntax.StepClause.StepValue, TypeSymbol.Single))

      m_scope = New BoundScope(m_scope)

      Dim variable = BindVariableDeclaration(syntax.ControlVariable, True, TypeSymbol.Single)
      Dim exitLabel As BoundLabel = Nothing
      Dim continueLabel As BoundLabel = Nothing

      ' Push FOR loop variable to stack for NEXT validation
      m_forLoopStack.Push(syntax.ControlVariable)

      Dim body = BindLoopBody(syntax.Statements, exitLabel, continueLabel)

      m_scope = m_scope.Parent

      ' For single-line FOR statements with embedded NEXT, pop the loop variable now
      ' The embedded NEXT is consumed during parsing, not binding
      If syntax.NextKeyword IsNot Nothing Then
        m_forLoopStack.Pop()
      End If

      Return New BoundForStatement(syntax, variable, lowerBound, upperBound, stepper, body, exitLabel, continueLabel)

    End Function

    Private Function BindFieldStatement(syntax As FieldStatementSyntax) As BoundStatement
      Dim fileNumber = BindExpression(syntax.Filenumber)

      Dim fieldDefinitions As New List(Of (Width As BoundExpression, VariableName As String))

      ' The identifiers list contains: width, AS, identifier, comma [, width, AS, identifier, comma ...]
      ' Pattern: Expression, AS keyword, Identifier, [Comma]
      ' Commas are not part of the field definition - they're just separators
      For i As Integer = 0 To syntax.Identifiers.Count - 1
        Dim node = syntax.Identifiers(i)

        ' Skip commas
        If TypeOf node Is SyntaxToken AndAlso DirectCast(node, SyntaxToken).Kind = SyntaxKind.CommaToken Then
          Continue For
        End If

        ' Each field definition starts with a width expression
        ' Pattern: width (Expression), AS (Token), identifier (Token)
        If TypeOf node IsNot ExpressionSyntax Then
          Continue For
        End If

        Dim widthNode = DirectCast(node, ExpressionSyntax)
        Dim asTokenIndex = i + 1
        Dim varTokenIndex = i + 2

        ' Check if we have enough tokens for a complete field definition
        If asTokenIndex >= syntax.Identifiers.Count OrElse varTokenIndex >= syntax.Identifiers.Count Then
          Exit For
        End If

        Dim asToken = syntax.Identifiers(asTokenIndex)
        Dim varToken = syntax.Identifiers(varTokenIndex)

        ' Skip commas that might appear
        If TypeOf asToken Is SyntaxToken AndAlso DirectCast(asToken, SyntaxToken).Kind = SyntaxKind.CommaToken Then
          Continue For
        End If
        If TypeOf varToken Is SyntaxToken AndAlso DirectCast(varToken, SyntaxToken).Kind = SyntaxKind.CommaToken Then
          Continue For
        End If

        If TypeOf asToken IsNot SyntaxToken Then Continue For
        If TypeOf varToken IsNot SyntaxToken Then Continue For

        Dim asKeyword = DirectCast(asToken, SyntaxToken)
        If asKeyword.Kind <> SyntaxKind.AsKeyword Then Continue For

        Dim identifier = DirectCast(varToken, SyntaxToken)
        If identifier.Kind <> SyntaxKind.IdentifierToken Then Continue For

        Dim width = BindExpression(widthNode)
        fieldDefinitions.Add((width, identifier.Text))
      Next

      Return New BoundFieldStatement(syntax, fileNumber, fieldDefinitions.ToImmutableArray())
    End Function

    Private Function BindGetFileStatement(syntax As GetFileStatementSyntax) As BoundStatement
      Dim fileNumber = BindExpression(syntax.FileNumber)
      Dim optionalRecord = If(syntax.OptionalRecord IsNot Nothing, BindExpression(syntax.OptionalRecord), Nothing)
      Dim optionalVariable = If(syntax.OptionalVariable IsNot Nothing, syntax.OptionalVariable.Text, Nothing)
      Return New BoundGetFileStatement(syntax, fileNumber, optionalRecord, optionalVariable)
    End Function

    Private Function BindPutFileStatement(syntax As PutFileStatementSyntax) As BoundStatement
      Dim fileNumber = BindExpression(syntax.FileNumber)
      Dim optionalRecord = If(syntax.OptionalRecord IsNot Nothing, BindExpression(syntax.OptionalRecord), Nothing)
      Dim optionalVariable = If(syntax.OptionalVariable IsNot Nothing, syntax.OptionalVariable.Identifier.Text, Nothing)
      Return New BoundPutFileStatement(syntax, fileNumber, optionalRecord, optionalVariable)
    End Function

    Private Shared Function BindGosubStatement(syntax As GosubStatementSyntax) As BoundStatement
      Dim value = syntax.IdentifierToken.Text.ToLower()
      If IsNumeric(value) Then
        value = $"{GOTO_LABEL_PREFIX}{value}"
      End If
      Dim label = New BoundLabel(value)
      Return New BoundGosubStatement(syntax, label)
    End Function

    Private Shared Function BindGotoStatement(syntax As GotoStatementSyntax) As BoundStatement
      Dim value = syntax.TargetToken.Text.ToLower()
      If IsNumeric(value) Then
        value = $"{GOTO_LABEL_PREFIX}{value}"
      End If
      Dim label = New BoundLabel(value)
      Return New BoundGotoStatement(syntax, label)
    End Function

    Private Function BindOnGotoStatement(syntax As OnGotoStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, TypeSymbol.Single)
      Dim targetsBuilder = ImmutableArray.CreateBuilder(Of BoundLabel)

      For i = 0 To syntax.Targets.Count - 1 Step 2
        Dim targetToken = syntax.Targets(i)
        Dim value = targetToken.Text.ToLower() ' Normalize to lowercase to match label definitions
        If IsNumeric(value) Then
          value = $"{GOTO_LABEL_PREFIX}{value}"
        End If
        Dim label = New BoundLabel(value)
        targetsBuilder.Add(label)
      Next

      Return New BoundOnGotoStatement(syntax, expression, targetsBuilder.ToImmutable())
    End Function

    Private Function BindOnGosubStatement(syntax As OnGosubStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression, TypeSymbol.Single)
      Dim targetsBuilder = ImmutableArray.CreateBuilder(Of BoundLabel)

      For i = 0 To syntax.Targets.Count - 1 Step 2
        Dim targetToken = syntax.Targets(i)
        Dim value = targetToken.Text.ToLower() ' Normalize to lowercase to match label definitions
        If IsNumeric(value) Then
          value = $"{GOTO_LABEL_PREFIX}{value}"
        End If
        Dim label = New BoundLabel(value)
        targetsBuilder.Add(label)
      Next

      Return New BoundOnGosubStatement(syntax, expression, targetsBuilder.ToImmutable())
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
      Return New BoundIfStatement(syntax, condition, thenStatement, elseIfStatementsBuilder.ToImmutable(), elseClause)

    End Function

    Private Function BindInputStatement(syntax As InputStatementSyntax) As BoundStatement

      If syntax.IsFileInput Then
        Dim fileNumber = BindExpression(syntax.FileNumber)

        Dim fileVariables As New List(Of VariableSymbol)
        For Each token In syntax.Tokens
          If token.Kind <> SyntaxKind.CommaToken Then
            Dim variable = DetermineVariableReference(token)
            If variable Is Nothing Then
              If OPTION_EXPLICIT Then
                variable = BindVariableReference(token)
              Else
                Dim type As TypeSymbol
                Dim suffix = token.Text.Last
                Select Case suffix
                  Case "%"c : type = TypeSymbol.Integer
                  Case "&"c : type = TypeSymbol.Long
                  Case "!"c : type = TypeSymbol.Single
                  Case "#"c : type = TypeSymbol.Double
                  Case "$"c : type = TypeSymbol.String
                  Case Else
                    type = TypeSymbol.Single
                End Select
                variable = BindVariableDeclaration(token, False, type)
              End If
            End If
            If variable Is Nothing Then
              Return Nothing
            End If
            fileVariables.Add(variable)
          End If
        Next
        Return New BoundInputStatement(syntax, fileNumber, fileVariables.ToImmutableArray())
      End If

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
      Return New BoundInputStatement(syntax, suppressCr, suppressQuestionMark, prompt, variables.ToImmutableArray)
    End Function

    Private Function BindKillStatement(syntax As KillStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundKillStatement(syntax, path)
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
                  'type = TypeSymbol.Single
                  'TODO: Infer????
                  type = boundExpression.Type
              End Select
              variable = BindVariableDeclaration(identifier.Identifier, False, type)
            End If
          End If
          If variable.IsReadOnly Then
            Diagnostics.ReportCannotAssign(syntax.EqualToken.Location, name)
          End If

          Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type, allowExplicit:=True)

          Return New BoundLetStatement(syntax, variable, convertedExpression)

        End If
      Next

      Return Nothing

    End Function

    Private Function BindLsetStatement(syntax As LSetStatementSyntax) As BoundStatement
      Dim boundExpression = BindExpression(syntax.Expression)

      Dim identifier = syntax.Identifier
      Dim name = identifier.Identifier.Text
      Dim variable = DetermineVariableReference(identifier.Identifier)

      If variable Is Nothing Then
        If OPTION_EXPLICIT Then
          variable = BindVariableReference(identifier.Identifier)
        Else
          ' Variable has not been declared, let's go ahead and do so.
          Dim type As TypeSymbol
          Dim suffix = identifier.Identifier.Text.Last
          Select Case suffix
            Case "%"c : type = TypeSymbol.Integer
            Case "&"c : type = TypeSymbol.Long
            Case "!"c : type = TypeSymbol.Single
            Case "#"c : type = TypeSymbol.Double
            Case "$"c : type = TypeSymbol.String
            Case Else
              type = boundExpression.Type
          End Select
          variable = BindVariableDeclaration(identifier.Identifier, False, type)
        End If
      End If

      If variable.IsReadOnly Then
        Diagnostics.ReportCannotAssign(syntax.Equal.Location, name)
      End If

      ' Bind conversion if needed
      Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type, allowExplicit:=True)

      Return New BoundLsetStatement(syntax, variable, convertedExpression)
    End Function

    Private Function BindRsetStatement(syntax As RSetStatementSyntax) As BoundStatement
      Dim boundExpression = BindExpression(syntax.Expression)

      Dim identifier = syntax.Identifier
      Dim name = identifier.Identifier.Text
      Dim variable = DetermineVariableReference(identifier.Identifier)

      If variable Is Nothing Then
        If OPTION_EXPLICIT Then
          variable = BindVariableReference(identifier.Identifier)
        Else
          ' Variable has not been declared, let's go ahead and do so.
          Dim type As TypeSymbol
          Dim suffix = identifier.Identifier.Text.Last
          Select Case suffix
            Case "%"c : type = TypeSymbol.Integer
            Case "&"c : type = TypeSymbol.Long
            Case "!"c : type = TypeSymbol.Single
            Case "#"c : type = TypeSymbol.Double
            Case "$"c : type = TypeSymbol.String
            Case Else
              type = boundExpression.Type
          End Select
          variable = BindVariableDeclaration(identifier.Identifier, False, type)
        End If
      End If

      If variable.IsReadOnly Then
        Diagnostics.ReportCannotAssign(syntax.Equal.Location, name)
      End If

      ' Bind conversion if needed
      Dim convertedExpression = BindConversion(syntax.Expression.Location, boundExpression, variable.Type, allowExplicit:=True)

      Return New BoundRsetStatement(syntax, variable, convertedExpression)
    End Function

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
      Return New BoundLineStatement(syntax, step1, x1, y1, step2, x2, y2, attribute, mode, style)
    End Function

    Private Function BindLocateStatement(syntax As LocateStatementSyntax) As BoundStatement
      Dim row = If(syntax.Row Is Nothing, Nothing, BindExpression(syntax.Row))
      Dim col = If(syntax.Col Is Nothing, Nothing, BindExpression(syntax.Col))
      Dim visible = If(syntax.Visible Is Nothing, Nothing, BindExpression(syntax.Visible))
      Dim scanStart = If(syntax.ScanStart Is Nothing, Nothing, BindExpression(syntax.ScanStart))
      Dim scanStop = If(syntax.ScanStop Is Nothing, Nothing, BindExpression(syntax.ScanStop))
      Return New BoundLocateStatement(syntax, row, col, visible, scanStart, scanStop)
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
      Dim targetExpression = BindExpression(syntax.TargetExpression)
      Dim positionExpression = If(syntax.PositionExpression Is Nothing, Nothing, BindExpression(syntax.PositionExpression))
      Dim lengthExpression = If(syntax.LengthExpression Is Nothing, Nothing, BindExpression(syntax.LengthExpression))
      Dim expression = If(syntax.Expression Is Nothing, Nothing, BindExpression(syntax.Expression))
      Return New BoundMidStatement(syntax, targetExpression, positionExpression, lengthExpression, expression)
    End Function

    Private Function BindMkDirStatement(syntax As MkDirStatementSyntax) As BoundStatement
      Dim path = BindExpression(syntax.Path)
      Return New BoundMkDirStatement(syntax, path)
    End Function

    Private Function BindNameStatement(syntax As NameStatementSyntax) As BoundStatement
      Dim originalPath = BindExpression(syntax.OriginalPath)
      Dim destinationPath = BindExpression(syntax.DestinationPath)
      Return New BoundNameStatement(syntax, originalPath, destinationPath)
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
        ' Check for known 0-parameter functions
        If name.ToLower = "csrlin" Then
          Dim func = BuiltinFunctions.CsrLin
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "date$" Then
          Dim func = BuiltinFunctions.Date
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "erdev" Then
          Dim func = BuiltinFunctions.ErDev1
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "erdev$" Then
          Dim func = BuiltinFunctions.ErDev2
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "freefile" Then
          Dim func = BuiltinFunctions.FreeFile
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "rnd" Then
          Dim func = BuiltinFunctions.Rnd1
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "time$" Then
          Dim func = BuiltinFunctions.Time
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        ElseIf name.ToLower = "timer" Then
          Dim func = BuiltinFunctions.Timer
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        Else
          ' Check for user-defined 0-parameter functions
          Dim funcSymbol = m_scope.TryLookupFunction(name, New List(Of TypeSymbol)())
          If funcSymbol IsNot Nothing AndAlso TypeOf funcSymbol Is FunctionSymbol Then
            Dim func = CType(funcSymbol, FunctionSymbol)
            If func.Parameters.Length = 0 Then
              Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
            End If
          End If
        End If
        If syntax.OpenParen IsNot Nothing Then
          ' Implicit array - declare in current compilation's global scope
          Dim arrayType = TypeSymbol.Single
          variable = New GlobalArraySymbol(name, arrayType, New BoundLiteralExpression(m_optionBase), New BoundLiteralExpression(10), True, 1)
          ' Find the current compilation's global scope to declare automatic arrays
          Dim globalScope = m_scope
          While globalScope.Parent IsNot Nothing AndAlso globalScope.Parent.Parent IsNot Nothing
            globalScope = globalScope.Parent
          End While
          globalScope.TryDeclareVariable(variable)
        Else
          ' Implicit scalar
          ' No AS clause, check variable name suffix

          Dim type As QB.CodeAnalysis.Symbols.TypeSymbol = ResolveVariableType(name)

          'Dim type = TypeSymbol.Single
          Dim suffix = name.Last
          Select Case suffix
            Case "%"c : type = QB.CodeAnalysis.Symbols.TypeSymbol.Integer
            Case "&"c : type = QB.CodeAnalysis.Symbols.TypeSymbol.Long
            Case "!"c : type = QB.CodeAnalysis.Symbols.TypeSymbol.Single
            Case "#"c : type = QB.CodeAnalysis.Symbols.TypeSymbol.Double
            Case "$"c : type = QB.CodeAnalysis.Symbols.TypeSymbol.String
            Case Else
              'Type = TypeSymbol.Single ' Default for arrays without suffix
          End Select
          variable = If(m_function Is Nothing,
                        DirectCast(New GlobalVariableSymbol(name, False, type, Nothing), VariableSymbol),
                        DirectCast(New LocalVariableSymbol(name, False, type, Nothing), VariableSymbol))
          m_scope.TryDeclareVariable(variable)
        End If
      ElseIf TypeOf symbol Is FunctionSymbol Then
        Dim func = CType(symbol, FunctionSymbol)
        If func.Parameters.Length = 0 Then
          Return New BoundCallExpression(func, ImmutableArray.Create(Of BoundExpression)(), syntax)
        Else
          Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, name)
          Return New BoundErrorExpression
        End If
      Else
        Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, name)
        Return New BoundErrorExpression
      End If
      If variable IsNot Nothing Then
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
      Else
        Diagnostics.ReportUndefinedVariable(syntax.Identifier.Location, name)
        Return New BoundErrorExpression
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
      Return New BoundOptionStatement(syntax, number)
    End Function

    Private Function BindParenExpression(syntax As ParenExpressionSyntax) As BoundExpression
      Return New BoundParenExpression(BindExpression(syntax.Expression), syntax)
    End Function

    Private Function BindPrintStatement(syntax As PrintStatementSyntax) As BoundStatement

      Dim fileNumber As BoundExpression = Nothing
      If syntax.FileNumber IsNot Nothing Then
        fileNumber = BindExpression(syntax.FileNumber)
      End If

      Dim format As BoundExpression = Nothing
      If syntax.UsingKeyword IsNot Nothing AndAlso syntax.Usingformat IsNot Nothing Then
        format = BindExpression(syntax.Usingformat, TypeSymbol.String)
      End If

      If fileNumber IsNot Nothing Then
        ' For file print, parse the nodes after the file number/using clause
        Dim nodes = New List(Of BoundNode)

        For Each entry In syntax.Nodes
          ' Process tokens - handle separators and expressions
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
          ElseIf TypeOf entry Is ExpressionSyntax Then
            nodes.Add(BindExpression(DirectCast(entry, ExpressionSyntax), TypeSymbol.Any))
          End If
        Next

        ' Check if this is a USING format statement (has format and expressions)
        ' For PRINT #filenumber USING, we need to handle format specially
        ' But for regular PRINT #filenumber, all nodes should be processed
        If syntax.UsingKeyword IsNot Nothing AndAlso syntax.Usingformat IsNot Nothing Then
          ' This is USING format - need to handle differently
          ' The format is in Usingformat, and expressions are in nodes
          ' For USING, we need to pass format and expressions separately
          ' Actually, BoundPrintFileStatement handles this via the Format property
        End If

        Return New BoundPrintFileStatement(syntax, fileNumber, format, nodes.ToImmutableArray())
      Else
        ' For screen print
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
          ElseIf TypeOf entry Is ExpressionSyntax Then
            nodes.Add(BindExpression(DirectCast(entry, ExpressionSyntax), TypeSymbol.Any))
          End If
        Next
        Dim suppressCr As Boolean = False
        If nodes.Count > 0 AndAlso nodes.Last.Kind = BoundNodeKind.Symbol Then
          suppressCr = True
        End If
        Return New BoundPrintStatement(syntax, nodes.ToImmutableArray(), format, suppressCr)
      End If

    End Function

    Private Function BindWriteStatement(syntax As WriteStatementSyntax) As BoundStatement
      Dim fileNumber As BoundExpression = Nothing
      If syntax.FileNumber IsNot Nothing Then
        fileNumber = BindExpression(syntax.FileNumber)
      End If

      Dim nodes = New List(Of BoundExpression)
      For Each entry In syntax.Expressions
        If TypeOf entry Is ExpressionSyntax Then
          nodes.Add(BindExpression(DirectCast(entry, ExpressionSyntax), TypeSymbol.Any))
        End If
      Next

      Dim suppressCr As Boolean = False
      If syntax.Expressions.Count > 0 Then
        Dim lastEntry = syntax.Expressions.Last
        If TypeOf lastEntry Is SyntaxToken Then
          Dim lastToken = CType(lastEntry, SyntaxToken)
          If lastToken.Kind = SyntaxKind.SemicolonToken Then
            suppressCr = True
          End If
        End If
      End If

      Return New BoundWriteStatement(syntax, fileNumber, nodes.ToImmutableArray(), suppressCr)
    End Function

    Private Function BindPsetStatement(syntax As PsetStatementSyntax) As BoundStatement
      Dim [step] = syntax.OptionalStepKeyword IsNot Nothing
      Dim x = BindExpression(syntax.XExpression, TypeSymbol.Single)
      Dim y = BindExpression(syntax.YExpression, TypeSymbol.Single)
      Dim color As BoundExpression = Nothing
      If syntax.OptionalColorExpression IsNot Nothing Then
        color = BindExpression(syntax.OptionalColorExpression, TypeSymbol.Single)
      End If
      Return New BoundPsetStatement(syntax, [step], x, y, color)
    End Function

    Private Function BindPresetStatement(syntax As PresetStatementSyntax) As BoundStatement
      Dim [step] = syntax.OptionalStepKeyword IsNot Nothing
      Dim x = BindExpression(syntax.XExpression, TypeSymbol.Single)
      Dim y = BindExpression(syntax.YExpression, TypeSymbol.Single)
      Dim color As BoundExpression = Nothing
      If syntax.OptionalColorExpression IsNot Nothing Then
        color = BindExpression(syntax.OptionalColorExpression, TypeSymbol.Single)
      End If
      Return New BoundPresetStatement(syntax, [step], x, y, color)
    End Function

    Private Sub ProcessMetacommandsInTrivia(triviaList As ImmutableArray(Of SyntaxTrivia))
      For Each trivia In triviaList
        If trivia.Kind = SyntaxKind.SingleLineCommentTrivia Then
          Dim comment = trivia.Text.ToUpper().Trim()
          If comment = "'$DYNAMIC" OrElse comment = "$DYNAMIC" Then
            m_arrayModeDynamic = True
          ElseIf comment = "'$STATIC" OrElse comment = "$STATIC" Then
            m_arrayModeDynamic = False
          End If
        End If
      Next
    End Sub

    Private Function BindRemStatement(syntax As RemStatementSyntax) As BoundStatement
      ' Check for metacommands
      Dim comment = syntax.Comment.ToUpper().Trim()
      If comment = "'$DYNAMIC" OrElse comment = "$DYNAMIC" Then
        m_arrayModeDynamic = True
      ElseIf comment = "'$STATIC" OrElse comment = "$STATIC" Then
        m_arrayModeDynamic = False
      End If

      Return New BoundRemStatement(syntax)
    End Function

    Private Function BindDefTypeStatement(syntax As DefTypeStatementSyntax) As BoundStatement
      Dim defType = syntax.Keyword.Text
      Dim typeSymbol As TypeSymbol

      ' Map DEF type keywords to TypeSymbol
      Select Case defType.ToUpper()
        Case "DEFINT" : typeSymbol = QB.CodeAnalysis.Symbols.TypeSymbol.Integer
        Case "DEFLNG" : typeSymbol = QB.CodeAnalysis.Symbols.TypeSymbol.Long
        Case "DEFSNG" : typeSymbol = QB.CodeAnalysis.Symbols.TypeSymbol.Single
        Case "DEFDBL" : typeSymbol = QB.CodeAnalysis.Symbols.TypeSymbol.Double
        Case "DEFSTR" : typeSymbol = QB.CodeAnalysis.Symbols.TypeSymbol.String
        Case Else
          ' This shouldn't happen if parser is working correctly
          Return New BoundRemStatement()
      End Select

      ' Process each range in the DEF statement
      For Each node In syntax.Nodes
        If TypeOf node Is DefVarRangeClause Then
          Dim range = CType(node, DefVarRangeClause)
          Dim startLetter = range.Lower.Text.ToUpper()

          If range.OptionalUpper IsNot Nothing Then
            ' This is a range like A-Z
            Dim endLetter = range.OptionalUpper.Text.ToUpper()
            If startLetter.Length = 1 AndAlso endLetter.Length = 1 Then
              For c As Integer = Microsoft.VisualBasic.Asc(startLetter) To Microsoft.VisualBasic.Asc(endLetter)
                AddDefTypeRange(Microsoft.VisualBasic.Chr(c), typeSymbol)
              Next
            End If
          Else
            ' This is a single letter like S
            If startLetter.Length = 1 Then
              AddDefTypeRange(startLetter, typeSymbol)
            End If
          End If
        End If
      Next

      ' For now, we don't need to generate any runtime code for DEF statements
      Return New BoundDefTypeStatement(syntax, typeSymbol, New List(Of (Char, Char)))
    End Function

    Private Sub AddDefTypeRange(letter As String, typeSym As TypeSymbol)
      If String.IsNullOrEmpty(letter) OrElse letter.Length <> 1 Then Return
      m_defTypeRanges(letter) = typeSym
    End Sub

    Private Function BindTypeStatement(syntax As TypeStatementSyntax) As BoundStatement
      Dim typeName = syntax.Identifier.Text
      Dim fieldList = New List(Of Binding.UdtField)

      For Each prop In syntax.Properties
        If TypeOf prop Is ParameterSyntax Then
          Dim param = CType(prop, ParameterSyntax)
          Dim fieldName = param.Identifier.Identifier.Text

          Dim fieldType As TypeSymbol = Nothing
          Dim fixedLength As Integer = 0

          If param.AsClause IsNot Nothing Then
            Dim typeNameToken = param.AsClause.Identifier
            Dim typeNameStr = typeNameToken.Text

            ' Check for fixed-length string (e.g., STRING * 20)
            If typeNameStr.ToUpper = "STRING" Then
              fieldType = TypeSymbol.String
              ' Check if this is a fixed-length string
              If param.AsClause.HasFixedLength AndAlso param.AsClause.FixedLength IsNot Nothing Then
                Dim lengthStr = param.AsClause.FixedLength.Text
                If IsNumeric(lengthStr) Then
                  fixedLength = CInt(lengthStr)
                End If
              End If
            Else
              ' Lookup the type
              fieldType = LookupType(typeNameStr)
              If fieldType Is Nothing Then
                ' Check if it's a UDT
                fieldType = LookupUserDefinedType(typeNameStr)
                If fieldType Is Nothing Then
                  Diagnostics.ReportUndefinedType(typeNameToken.Location, typeNameStr)
                  fieldType = TypeSymbol.Error
                End If
              End If
            End If

            fieldList.Add(New Binding.UdtField(fieldName, fieldType, fixedLength, If(fieldType Is TypeSymbol.Udt, typeNameStr, Nothing)))
          Else
            ' No AS clause - infer from variable name suffix
            fieldType = ResolveVariableType(fieldName)
            fieldList.Add(New Binding.UdtField(fieldName, fieldType, fixedLength))
          End If
        End If
      Next

      Dim fields = fieldList.ToImmutableArray()

      ' Resolve nested UDT types for all fields
      Dim resolvedFields = New List(Of Binding.UdtField)
      For Each field In fields
        If field.FieldType Is TypeSymbol.Udt AndAlso Not String.IsNullOrEmpty(field.UdtTypeName) Then
          ' Look up the nested UDT type
          Dim nestedUdtType = m_scope.TryLookupType(field.UdtTypeName)
          resolvedFields.Add(New Binding.UdtField(field.Name, field.FieldType, field.FixedLength, field.UdtTypeName, nestedUdtType))
        Else
          resolvedFields.Add(field)
        End If
      Next

      ' Create and register the UDT symbol
      Dim fieldSymbols = resolvedFields.Select(Function(f) New UdtFieldSymbol(f.Name, f.FieldType, f.FixedLength, f.UdtTypeName, f.NestedUdtType)).ToImmutableArray()
      Dim udtSymbol = New UdtTypeSymbol(typeName, fieldSymbols)

      If Not m_scope.TryDeclareType(udtSymbol) Then
        Diagnostics.ReportSymbolAlreadyDeclared(syntax.Identifier.Location, typeName)
      End If

      Return New BoundTypeStatement(typeName, resolvedFields.ToImmutableArray())
    End Function

    Private Function LookupUserDefinedType(typeName As String) As TypeSymbol
      Dim udt = m_scope.TryLookupType(typeName)
      If udt IsNot Nothing Then
        Return TypeSymbol.Udt
      End If
      Return Nothing
    End Function

    Private Function BindDeclareStatement(syntax As DeclareStatementSyntax) As BoundStatement
      ' DECLARE statements are declarations, not executable statements.
      ' For now, we'll just return a NOP statement since the validation
      ' will happen during a separate pass when all function/sub declarations
      ' are collected.
      Return New BoundNopStatement()
    End Function



    Private Shared Function BindReturnGosubStatement(syntax As ReturnGosubStatementSyntax) As BoundStatement
      Dim value = syntax.TargetToken?.Text
      If value IsNot Nothing AndAlso IsNumeric(value) Then
        Dim text = $"{value}".ToLower
        value = $"{GOTO_LABEL_PREFIX}{text}"
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
                Dim text = $"{value}".ToLower
                value = $"{GOTO_LABEL_PREFIX}{text}"
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
      Return New BoundStopStatement(syntax)
    End Function

    Private Shared Function BindSystemStatement(syntax As SystemStatementSyntax) As BoundStatement
      If syntax IsNot Nothing Then
      End If
      Return New BoundSystemStatement(syntax)
    End Function

    Private Function BindSwapStatement(syntax As SwapStatementSyntax) As BoundStatement
      Dim variable1 = BindNameExpression(CType(syntax.Variable1, NameExpressionSyntax))
      Dim variable2 = BindNameExpression(CType(syntax.Variable2, NameExpressionSyntax))
      Return New BoundSwapStatement(syntax, variable1, variable2)
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
      Dim type As QB.CodeAnalysis.Symbols.TypeSymbol = BindAsClause(syntax.AsClause)
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
            type = QB.CodeAnalysis.Symbols.TypeSymbol.Single ' Default for arrays without suffix
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
      Dim udtType As UdtTypeSymbol = Nothing
      If type Is TypeSymbol.Udt Then
        udtType = m_scope.TryLookupType(syntax.AsClause.Identifier.Text)
      End If
      If bounds Is Nothing Then
        Dim variable = If(m_function Is Nothing,
                       DirectCast(New GlobalVariableSymbol(name, False, type, Nothing, VariableTypeSource.DimAsClause, False, udtType), VariableSymbol),
                       DirectCast(New LocalVariableSymbol(name, False, type, Nothing, VariableTypeSource.DimAsClause, False, udtType), VariableSymbol))
        Return New BoundVariableDeclaration(variable, Nothing)
      Else
        Dim variable = New VariableSymbol(name, isArray, type, lower, upper, isStaticArray, dimensionCount, VariableTypeSource.DimAsClause, False, udtType)
        Return New BoundVariableDeclaration(variable, Nothing)
      End If
    End Function

    Private Function BindVariableDeclaration(syntax As VariableDeclarationSyntax, isCommon As Boolean) As BoundStatement
      Dim name = syntax.Identifier.Text
      Dim bounds = syntax.Bounds
      Dim isArray = bounds IsNot Nothing
      Dim type As QB.CodeAnalysis.Symbols.TypeSymbol = BindAsClause(syntax.AsClause)
      Dim udtType As UdtTypeSymbol = Nothing
      If type Is TypeSymbol.Udt Then
        udtType = m_scope.TryLookupType(syntax.AsClause.Identifier.Text)
      End If
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
            type = QB.CodeAnalysis.Symbols.TypeSymbol.Single ' Default for arrays without suffix
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
      If bounds Is Nothing Then
        Dim variable = If(m_function Is Nothing,
                       DirectCast(New GlobalVariableSymbol(name, False, type, Nothing, VariableTypeSource.DimAsClause, isCommon, udtType), VariableSymbol),
                       DirectCast(New LocalVariableSymbol(name, False, type, Nothing, VariableTypeSource.DimAsClause, isCommon, udtType), VariableSymbol))
        Return New BoundVariableDeclaration(variable, Nothing)
      Else
        Dim variable = New VariableSymbol(name, isArray, type, lower, upper, isStaticArray, dimensionCount, VariableTypeSource.DimAsClause, isCommon, udtType)
        Return New BoundVariableDeclaration(variable, Nothing)
      End If
    End Function

    Private Function BindArrayDeclaration(identifier As SyntaxToken, type As TypeSymbol, lower As BoundExpression, upper As BoundExpression, dimensionCount As Integer, Optional isStaticArray As Boolean = False, Optional isCommon As Boolean = False) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(m_function Is Nothing,
                       DirectCast(New GlobalArraySymbol(name, type, lower, upper, isStaticArray, dimensionCount, VariableTypeSource.DefaultType, isCommon), VariableSymbol),
                       DirectCast(New LocalArraySymbol(name, type, lower, upper, isStaticArray, dimensionCount, VariableTypeSource.DefaultType, isCommon), VariableSymbol))
      If [declare] Then
        m_scope.TryDeclareVariable(variable)
      End If
      Return variable
    End Function

    Private Function BindVariableDeclaration(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol, Optional constant As BoundConstant = Nothing) As VariableSymbol
      Return BindVariableDeclarationWithSource(identifier, isReadOnly, type, VariableTypeSource.DefaultType, constant)
    End Function

    Private Function BindVariableDeclarationWithSource(identifier As SyntaxToken, isReadOnly As Boolean, type As TypeSymbol, typeSource As VariableTypeSource, Optional constant As BoundConstant = Nothing, Optional isCommon As Boolean = False) As VariableSymbol
      Dim name = If(identifier.Text, "?")
      Dim [declare] = Not identifier.IsMissing
      Dim variable = If(m_function Is Nothing,
                        DirectCast(New GlobalVariableSymbol(name, isReadOnly, type, constant, typeSource, isCommon), VariableSymbol),
                        DirectCast(New LocalVariableSymbol(name, isReadOnly, type, constant, typeSource, isCommon), VariableSymbol))
      If [declare] AndAlso Not m_scope.TryDeclareVariable(variable) Then
        Diagnostics.ReportSymbolAlreadyDeclared(identifier.Location, name)
      End If
      Return variable
    End Function

    Private Function BindDimStatement(syntax As DimStatementSyntax) As BoundStatement

      ' Check for metacommands in leading trivia
      ProcessMetacommandsInTrivia(syntax.DimKeyword.LeadingTrivia)

      Dim boundDeclarations = ImmutableArray.CreateBuilder(Of BoundVariableDeclaration)
      Dim isShared = syntax.OptionalSharedKeyword IsNot Nothing

      For Each variableNode In syntax.Variables
        If TypeOf variableNode Is VariableDeclarationSyntax Then
          Dim variableDecl = CType(variableNode, VariableDeclarationSyntax)
          Dim boundDecl = CType(BindVariableDeclaration(variableDecl), BoundVariableDeclaration)
          Dim variable = boundDecl.Variable
          If Not m_scope.TryDeclareVariable(variable) Then
            ' Check if this is an array redeclaration case
            If variable.IsArray Then
              Dim existingSymbol = m_scope.TryLookupVariable(variable.Name)
              If existingSymbol IsNot Nothing AndAlso existingSymbol.IsArray Then
                ' Check if dimension count changed
                If existingSymbol.DimensionCount <> variable.DimensionCount Then
                  Diagnostics.ReportArrayDimensionCountMismatch(variableDecl.Identifier.Location, variable.Name, existingSymbol.DimensionCount, variable.DimensionCount)
                Else
                  Diagnostics.ReportArrayAlreadyDimensioned(variable.Name)
                End If
              Else
                Diagnostics.ReportSymbolAlreadyDeclared(variableDecl.Identifier.Location, variable.Name)
              End If
            Else
              Diagnostics.ReportSymbolAlreadyDeclared(variableDecl.Identifier.Location, variable.Name)
            End If
          End If
          boundDeclarations.Add(boundDecl)
        End If
      Next

      Return New BoundDimStatement(syntax, boundDeclarations.ToImmutable(), isShared)
    End Function

    Private Function BindConstStatement(syntax As ConstStatementSyntax) As BoundStatement
      Dim boundDeclarations = ImmutableArray.CreateBuilder(Of BoundVariableDeclaration)

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

      Return New BoundDimStatement(boundDeclarations.ToImmutable(), False)
    End Function

    Private Function BindCommonStatement(syntax As CommonStatementSyntax) As BoundStatement
      Dim boundDeclarations = ImmutableArray.CreateBuilder(Of BoundVariableDeclaration)
      Dim isShared = syntax.SharedKeyword IsNot Nothing

      For Each variableNode In syntax.Variables
        If TypeOf variableNode Is VariableDeclarationSyntax Then
          Dim variableDecl = CType(variableNode, VariableDeclarationSyntax)
          Dim boundDecl = CType(BindVariableDeclaration(variableDecl, True), BoundVariableDeclaration)
          Dim variable = boundDecl.Variable
          If Not m_scope.TryDeclareVariable(variable) Then
            Diagnostics.ReportSymbolAlreadyDeclared(variableDecl.Identifier.Location, variable.Name)
          End If
          boundDeclarations.Add(boundDecl)
        End If
      Next

      Return New BoundCommonStatement(syntax, boundDeclarations.ToImmutable(), isShared)
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
            ' Extract type using DEF statements, AS clause, or type character priority
            Dim arrayType As TypeSymbol = ResolveVariableType(variableDecl.Identifier.Text)
            If arrayType Is Nothing Then
              ' If no specific type found, check variable name suffix
              Dim suffix = variableDecl.Identifier.Text.Last
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

            ' DEF types are already applied in ResolveVariableType, so no need for extra logic here

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
            ' For scalar variables, create proper scalar variable with correct type source and type
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

      Return New BoundRedimStatement(syntax, preserve, boundDeclarations.ToImmutable(), isShared)
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

              ' For dynamic arrays, remove from scope to allow redeclaration
              If Not variableSymbol.IsStaticArray Then
                m_scope.TryRemoveVariable(variableSymbol.Name)
              End If
            End If
          Else
            Diagnostics.ReportUndefinedArray(token.Location, token.Text)
          End If
        End If
      Next

      Return New BoundEraseStatement(syntax, boundVariables.ToImmutable())
    End Function

    Private Function BindCallStatement(syntax As CallStatementSyntax) As BoundStatement
      Dim callSyntax = New CallExpressionSyntax(syntax.SyntaxTree, syntax.Identifier, syntax.OpenParen, syntax.Expressions, syntax.CloseParen)
      Dim boundCall = CType(BindExpression(callSyntax, True), BoundCallExpression)
      Return New BoundCallStatement(syntax, boundCall)
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
          If arraySize > 16383 Then
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

    Private Function BindOnErrorGotoStatement(syntax As OnErrorGotoStatementSyntax) As BoundStatement
      ' Check if target is the literal 0 (disable error handling)
      If TypeOf syntax.Target Is LiteralExpressionSyntax AndAlso
          CInt(DirectCast(syntax.Target, LiteralExpressionSyntax).Value) = 0 Then
        Return New BoundOnErrorGotoZeroStatement()
      Else
        ' ON ERROR GOTO target (label or line number)
        Dim targetExpr = BindExpression(syntax.Target)
        Return New BoundOnErrorGotoStatement(syntax, targetExpr)
      End If
    End Function

    Private Function BindOnTimerGosubStatement(syntax As OnTimerGosubStatementSyntax) As BoundStatement
      Dim interval = BindExpression(syntax.Interval)
      Dim target = BindExpression(syntax.Target)
      Return New BoundOnTimerGosubStatement(syntax, interval, target)
    End Function

    Private Function BindOnComGosubStatement(syntax As OnComGosubStatementSyntax) As BoundStatement
      Dim channel = BindExpression(syntax.Channel)
      Dim target = BindExpression(syntax.Target)
      Return New BoundOnComGosubStatement(syntax, channel, target)
    End Function

    Private Function BindOnKeyGosubStatement(syntax As OnKeyGosubStatementSyntax) As BoundStatement
      Dim keyNumber = BindExpression(syntax.KeyNumber)
      Dim target = BindExpression(syntax.Target)
      Return New BoundOnKeyGosubStatement(syntax, keyNumber, target)
    End Function

    Private Function BindOnStrigGosubStatement(syntax As OnStrigGosubStatementSyntax) As BoundStatement
      Dim triggerNumber = BindExpression(syntax.TriggerNumber)
      Dim target = BindExpression(syntax.Target)
      Return New BoundOnStrigGosubStatement(syntax, triggerNumber, target)
    End Function

    Private Function BindOnPlayGosubStatement(syntax As OnPlayGosubStatementSyntax) As BoundStatement
      Dim queueSize = BindExpression(syntax.QueueSize)
      Dim target = BindExpression(syntax.Target)
      Return New BoundOnPlayGosubStatement(syntax, queueSize, target)
    End Function

    Private Function BindOnPenGosubStatement(syntax As OnPenGosubStatementSyntax) As BoundStatement
      Dim target = BindExpression(syntax.Target)
      Return New BoundOnPenGosubStatement(syntax, target)
    End Function

    Private Function BindOpenStatement(syntax As OpenStatementSyntax) As BoundStatement
      If syntax.IsShorthandForm Then
        ' For shorthand OPEN "mode",#filenum,"filename",len:
        ' - syntax.Mode = first expression = mode string (like "r")
        ' - syntax.Filename = filename expression (we store mode there for backward compat)
        ' - syntax.File = filename (we swapped in constructor)
        Dim shorthandMode = BindExpression(syntax.Mode)
        Dim shorthandFileNumber = BindExpression(syntax.FileNumber1)
        Dim shorthandFile = BindExpression(syntax.File)
        Dim shorthandRecLen = If(syntax.RecLen IsNot Nothing, BindExpression(syntax.RecLen), Nothing)
        Return New BoundOpenStatement(shorthandFile, shorthandMode, ImmutableArray(Of BoundExpression).Empty, ImmutableArray(Of BoundExpression).Empty, shorthandFileNumber, shorthandRecLen)
      End If

      Dim file = BindExpression(syntax.File)
      Dim mode = If(syntax.ModeKeyword IsNot Nothing, New BoundLiteralExpression(syntax.ModeKeyword.Text), Nothing)
      Dim access As New List(Of BoundExpression)
      For Each a In syntax.Access
        access.Add(New BoundLiteralExpression(DirectCast(a, SyntaxToken).Text))
      Next
      Dim lock As New List(Of BoundExpression)
      For Each l In syntax.Lock
        lock.Add(New BoundLiteralExpression(DirectCast(l, SyntaxToken).Text))
      Next
      Dim fileNumber = BindExpression(syntax.FileNumber)
      Dim recLen = If(syntax.RecLen IsNot Nothing, BindExpression(syntax.RecLen), Nothing)
      Return New BoundOpenStatement(syntax, file, mode, access.ToImmutableArray(), lock.ToImmutableArray(), fileNumber, recLen)
    End Function

    Private Function BindCloseStatement(syntax As CloseStatementSyntax) As BoundStatement
      Dim fileNumbers As ImmutableArray(Of BoundExpression)
      If syntax.Expressions IsNot Nothing Then
        fileNumbers = syntax.Expressions.Select(Function(fn) BindExpression(fn)).ToImmutableArray()
      Else
        fileNumbers = ImmutableArray(Of BoundExpression).Empty
      End If
      Return New BoundCloseStatement(syntax, fileNumbers)
    End Function

    Private Function BindResetStatement(syntax As ResetStatementSyntax) As BoundStatement
      Return New BoundResetStatement(syntax)
    End Function

    Private Function BindSeekStatement(syntax As SeekStatementSyntax) As BoundStatement
      Dim fileNumber = BindExpression(syntax.FileNumber)
      Dim position = BindExpression(syntax.Position)
      Return New BoundSeekStatement(syntax, fileNumber, position)
    End Function

    Private Function BindLineInputFileStatement(syntax As LineInputFileStatementSyntax) As BoundStatement
      Dim fileNumber = BindExpression(syntax.FileNumber)
      Dim variable = BindExpression(syntax.Identifier)
      Return New BoundLineInputFileStatement(syntax, fileNumber, variable)
    End Function

    Private Function BindResumeStatement(syntax As ResumeStatementSyntax) As BoundStatement
      If syntax.OptionalLine IsNot Nothing Then
        ' RESUME label/line
        Dim lineExpr = BindExpression(syntax.OptionalLine)
        Return New BoundResumeStatement(syntax, lineExpr)
      Else
        ' RESUME (resume at error line)
        Return New BoundResumeStatement(syntax, Nothing)
      End If
    End Function

    Private Function BindResumeNextStatement(syntax As ResumeNextStatementSyntax) As BoundStatement
      Return New BoundResumeNextStatement(syntax)
    End Function

    Private Function BindNextStatement(syntax As NextStatementSyntax) As BoundStatement
      ' Handle NEXT statement with or without identifiers
      If syntax.Identifiers.Count > 0 Then
        ' NEXT with identifiers - validate each corresponds to a FOR loop
        For Each identifier In syntax.Identifiers
          If m_forLoopStack.Count = 0 Then
            ' NEXT without corresponding FOR
            Throw New QBasicBuildException(ErrorCode.NextWithoutFor)
          End If

          Dim expectedForVariable = m_forLoopStack.Peek()
          If StringComparer.OrdinalIgnoreCase.Compare(expectedForVariable.Text, identifier.Text) <> 0 Then
            ' NEXT variable doesn't match FOR variable
            Throw New QBasicBuildException(ErrorCode.NextWithoutFor)
          End If

          ' Pop the matching FOR loop
          m_forLoopStack.Pop()
        Next
      Else
        ' NEXT without identifiers - pop the most recent FOR loop
        If m_forLoopStack.Count = 0 Then
          Throw New QBasicBuildException(ErrorCode.NextWithoutFor)
        End If

        m_forLoopStack.Pop()
      End If

      ' For now, return nop statement - the actual loop handling will be in lowering
      Return New BoundNopStatement()
    End Function

    Private Function BindVariableReference(identifierToken As SyntaxToken) As VariableSymbol

      Dim name = identifierToken.Text
      Dim s = m_scope.TryLookupSymbol(name)

      If TypeOf s Is VariableSymbol Then
        Return TryCast(s, VariableSymbol)
      ElseIf s Is Nothing Then
        ' Before creating a new variable, check if this is a function name
        ' Don't create local variables for function names (e.g., FNfactorial in DEF FNfactorial(n) = ...)
        ' Try looking for functions with different parameter counts
        Dim foundFunction = False
        For paramCount As Integer = 0 To 20
          Dim funcKey = $"{name.ToLower()}[{paramCount}]"
          Dim funcSymbol = m_scope.TryLookupSymbol(funcKey)
          If funcSymbol IsNot Nothing AndAlso TypeOf funcSymbol Is FunctionSymbol Then
            foundFunction = True
            Exit For
          End If
          ' Also check parent scopes
          Dim parentScope As BoundScope = m_scope.Parent
          While parentScope IsNot Nothing
            Dim parentFuncResult = parentScope.TryLookupSymbol(funcKey)
            If parentFuncResult IsNot Nothing AndAlso TypeOf parentFuncResult Is FunctionSymbol Then
              foundFunction = True
              Exit While
            End If
            parentScope = parentScope.Parent
          End While
          If foundFunction Then Exit For
        Next

        If foundFunction Then
          ' There's a function with this name - don't create a local variable
          Diagnostics.ReportNotAVariable(identifierToken.Location, name)
          Return Nothing
        End If

        If Not OPTION_EXPLICIT Then
          If String.IsNullOrEmpty(identifierToken.Text) Then
            Diagnostics.ReportUndefinedVariable(identifierToken.Location, name)
            Return Nothing
          End If
          Dim type As QB.CodeAnalysis.Symbols.TypeSymbol = ResolveVariableType(identifierToken.Text)
          Dim typeSource As VariableTypeSource

          ' Determine type source
          Dim suffix = identifierToken.Text.Last
          Select Case suffix
            Case "%"c, "&"c, "!"c, "#"c, "$"c
              typeSource = VariableTypeSource.TypeCharacter
            Case Else
              ' Check if this is from a DEF statement
              If m_defTypeRanges.ContainsKey(identifierToken.Text(0).ToString().ToUpper()) Then
                typeSource = VariableTypeSource.DefStatement
              Else
                typeSource = VariableTypeSource.DefaultType
              End If
          End Select

          Dim variable = BindVariableDeclarationWithSource(identifierToken, False, type, typeSource)
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

    Private Function BindWendStatement(syntax As WendStatementSyntax) As BoundStatement
      Throw New QBasicBuildException(ErrorCode.WendWithoutWHILE)
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

    Private Function LookupType(name As String) As TypeSymbol
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
          ' Check if it's a user-defined type
          Dim udt = m_scope.TryLookupType(name)
          If udt IsNot Nothing Then
            Return TypeSymbol.Udt
          End If
          Return Nothing
      End Select
    End Function

    Private Function BindDataStatement(syntax As DataStatementSyntax) As BoundStatement
      Dim data = ImmutableArray.CreateBuilder(Of Object)()

      For Each value As String In syntax.Data
        If String.IsNullOrWhiteSpace(value) Then
          data.Add(value)
        Else
          ' Try to parse as a number first
          Dim trimmedValue = value.Trim()
          Dim doubleValue As Double
          If Double.TryParse(trimmedValue, doubleValue) Then
            data.Add(doubleValue)
          Else
            data.Add(trimmedValue)
          End If
        End If
      Next

      Return New BoundDataStatement(data.ToImmutable(), m_currentLineNumber)
    End Function

    Private Function BindReadStatement(syntax As ReadStatementSyntax) As BoundStatement
      Dim expressions = ImmutableArray.CreateBuilder(Of BoundExpression)()

      For Each variableSyntax In syntax.Variables
        Dim boundExpression = BindExpression(variableSyntax)
        expressions.Add(boundExpression)
      Next

      Return New BoundReadStatement(expressions.ToImmutable())
    End Function

    Private Function BindRestoreStatement(syntax As RestoreStatementSyntax) As BoundStatement
      If syntax.NumberToken IsNot Nothing Then
        Dim value = syntax.NumberToken.Text.ToLower()
        Dim label = New BoundLabel(value)
        Return New BoundRestoreStatement(label)
      End If
      Return New BoundRestoreStatement(Nothing)
    End Function

    Private Function BindDateStatement(syntax As DateStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression)
      Return New BoundDateStatement(expression)
    End Function

    Private Function BindTimeStatement(syntax As TimeStatementSyntax) As BoundStatement
      Dim expression = BindExpression(syntax.Expression)
      Return New BoundTimeStatement(expression)
    End Function

    Private Function BindSleepStatement(syntax As SleepStatementSyntax) As BoundStatement
      Dim expression As BoundExpression = Nothing
      If syntax.Seconds IsNot Nothing Then
        expression = BindExpression(syntax.Seconds)
      End If
      Return New BoundSleepStatement(expression)
    End Function

    Private Function BindTimerStatement(syntax As TimerStatementSyntax) As BoundStatement
      Return New BoundTimerStatement(syntax.Verb.Kind)
    End Function

    Private Function BindComStatement(syntax As ComStatementSyntax) As BoundStatement
      Dim channel = BindExpression(syntax.Channel)
      Return New BoundComStatement(channel, syntax.Verb.Kind)
    End Function

    'Private Function BindKeyEventStatement(syntax As KeyEventStatementSyntax) As BoundStatement
    '  Dim keyNumber = BindExpression(syntax.KeyNumber)
    '  Return New BoundKeyStatement(keyNumber, syntax.Verb.Kind)
    'End Function

    'Private Function BindKeyOffStatement(syntax As KeyOffStatementSyntax) As BoundStatement
    '  Return New BoundKeyOffStatement()
    'End Function

    Private Function BindStrigStatement(syntax As StrigStatementSyntax) As BoundStatement
      Dim triggerNumber = BindExpression(syntax.TriggerNumber)
      Return New BoundStrigStatement(triggerNumber, syntax.Verb.Kind)
    End Function

    'Private Function BindPlayEventStatement(syntax As PlayEventStatementSyntax) As BoundStatement
    '  Dim queueSize = BindExpression(syntax.QueueSize)
    '  Return New BoundPlayEventStatement(queueSize, syntax.Verb.Kind)
    'End Function

    Private Function BindPenStatement(syntax As PenStatementSyntax) As BoundStatement
      Return New BoundPenStatement(syntax, syntax.VerbKeyword.Kind)
    End Function

    Private Function BindSelectCaseStatement(syntax As SelectCaseStatementSyntax) As BoundStatement
      Dim test = BindExpression(syntax.Test)

      Dim cases = ImmutableArray.CreateBuilder(Of BoundCaseStatement)()
      For Each caseClause In syntax.Cases
        Dim matches = ImmutableArray.CreateBuilder(Of BoundCaseMatchStatement)()
        For Each matchExpr In caseClause.Matches
          If TypeOf matchExpr Is CaseMatchExpressionSyntax Then
            Dim caseMatch = CType(matchExpr, CaseMatchExpressionSyntax)
            matches.Add(New BoundCaseMatchStatement(CaseMatchType.Value, CType(0, SyntaxKind), BindExpression(caseMatch.Expression), Nothing))
          ElseIf TypeOf matchExpr Is CaseMatchRangeExpressionSyntax Then
            Dim caseRange = CType(matchExpr, CaseMatchRangeExpressionSyntax)
            matches.Add(New BoundCaseMatchStatement(CaseMatchType.Range, CType(0, SyntaxKind), BindExpression(caseRange.Start), BindExpression(caseRange.End)))
          ElseIf TypeOf matchExpr Is CaseIsMatchExpressionSyntax Then
            Dim caseIs = CType(matchExpr, CaseIsMatchExpressionSyntax)
            matches.Add(New BoundCaseMatchStatement(CaseMatchType.IsComparison, caseIs.Comparison.Kind, BindExpression(caseIs.Expression), Nothing))
          End If
        Next
        Dim statement = BindStatement(caseClause.Statement)
        cases.Add(New BoundCaseStatement(matches.ToImmutable(), statement))
      Next

      Dim elseStatement As BoundStatement = Nothing
      If syntax.CaseElseClause IsNot Nothing Then
        elseStatement = BindStatement(syntax.CaseElseClause.Statement)
      End If

      Return New BoundSelectCaseStatement(syntax, test, cases.ToImmutable(), elseStatement)
    End Function

  End Class

End Namespace