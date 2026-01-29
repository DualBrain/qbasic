Imports System.Collections.Immutable
Imports System.IO

Namespace Global.QB.CodeAnalysis.Syntax

  ''' <summary>
  ''' Transforms QBasic code to VB.NET by adding required boilerplate and modernizing constructs.
  ''' Adds Module/End Module and SUB MAIN/END SUB structure where needed.
  ''' </summary>
  Public Class QBasicToVbNetRewriter
    Inherits SyntaxTreeRewriter

    ''' <summary>
    ''' Transformation options for QBasic to VB.NET conversion.
    ''' </summary>
    Public Class ConversionOptions
      Public Property AddModuleBoilerplate As Boolean = True
      Public Property AddSubMain As Boolean = True
      Public Property ModernizePrint As Boolean = True
      Public Property AddImports As Boolean = True
      Public Property PreserveOriginalComments As Boolean = True
      Public Property GenerateWarnings As Boolean = True
    End Class

    Public ReadOnly Property Options As New ConversionOptions()
    Public ReadOnly Property Analysis As New ConversionAnalysis()

    ''' <summary>
    ''' Analysis results for VB.NET conversion.
    ''' </summary>
    Public Class ConversionAnalysis
      Public ReadOnly Property Warnings As New List(Of String)()
      Public ReadOnly Property ChangesMade As New List(Of String)()
      Public ReadOnly Property RequiredImports As New HashSet(Of String)()
      Public Property AddedModule As Boolean = False
      Public Property AddedSubMain As Boolean = False
      Public Property ModernizedPrintStatements As Integer
      Public Property VariablesRequiringTypes As Integer
      Public Property GotoStatementsFound As Integer
      Public ReadOnly Property GosubStatementTargets As New HashSet(Of String)()
    End Class

    Public Sub New(options As ConversionOptions)
      Me.Options = options
    End Sub

    Public Sub New()
      Me.Options = New ConversionOptions()
    End Sub

    Public Overrides Function Rewrite(node As SyntaxNode) As SyntaxNode
      ' Analyze the code structure first
      AnalyzeCodeStructure(node)
      ' Perform the transformation
      Return MyBase.Rewrite(node)
    End Function

    ''' <summary>
    ''' Analyzes code to determine what transformations are needed.
    ''' </summary>
    Private Sub AnalyzeCodeStructure(node As SyntaxNode)
      If node.Kind = SyntaxKind.CompilationUnit Then
        Dim compilationUnit = DirectCast(node, CompilationUnitSyntax)
        AnalyzeCompilationUnit(compilationUnit)
      End If
      For Each child In node.GetChildren()
        AnalyzeStatementForConversion(child)
        AnalyzeCodeStructure(child)
      Next
    End Sub

    ''' <summary>
    ''' Analyzes compilation unit structure.
    ''' </summary>
    Private Sub AnalyzeCompilationUnit(compilationUnit As CompilationUnitSyntax)

      Dim hasModule = False
      Dim hasProcedures = False
      Dim hasGosubStatements = False

      ' Check for existing Module, Function, or Sub declarations
      For Each member In compilationUnit.Members
        Select Case member.Kind
          Case SyntaxKind.FunctionDeclaration, SyntaxKind.SubStatement
            hasModule = True
            hasProcedures = True
            Exit For
        End Select
      Next

      ' Check if there are only executable statements
      Dim hasOnlyStatements = True
      For Each member In compilationUnit.Members
        ' Check if this is NOT a statement type
        If Not IsStatementKind(member.Kind) Then
          hasOnlyStatements = False
          Exit For
        End If
      Next

      ' Check for GOSUB statements (not supported in VB.NET)
      For Each member In compilationUnit.Members
        If member.Kind = SyntaxKind.GosubStatement Then
          hasGosubStatements = True
          Exit For
        End If
      Next

      ' Module wrapper is always required in VB.NET
      Analysis.AddedModule = True
      Analysis.ChangesMade.Add("Added Module/End Module wrapper")

      ' Determine if we need to add Sub Main() for non-procedure code
      If Options.AddSubMain AndAlso hasOnlyStatements Then
        Analysis.AddedSubMain = True
        Analysis.ChangesMade.Add("Added Sub Main() for executable code")
      End If

      ' Check for PRINT statements that might need modernization
      If Options.ModernizePrint Then
        Dim printCount = CountPrintStatements(compilationUnit)
        Analysis.ModernizedPrintStatements = printCount
        If printCount > 0 Then
          Analysis.ChangesMade.Add($"Modernized {printCount} PRINT statements")
          Analysis.RequiredImports.Add("System.Console")
        End If
      End If

      ' Generate warnings for unsupported constructs
      If Options.GenerateWarnings Then
        ' GOSUB is not supported in VB.NET - generate error
        If hasGosubStatements Then
          Analysis.Warnings.Add("GOSUB statements are not supported in VB.NET and will cause compile errors")
        End If
      End If

    End Sub

    ''' <summary>
    ''' Analyzes individual statements for conversion needs.
    ''' </summary>
    Private Sub AnalyzeStatementForConversion(node As SyntaxNode)

      ' Check for PRINT statements
      If node.Kind = SyntaxKind.PrintStatement AndAlso Options.ModernizePrint Then Analysis.RequiredImports.Add("System.Console")

      ' Check for variable usage that might need explicit types
      If node.Kind = SyntaxKind.VariableDeclaration Then
        Analysis.VariablesRequiringTypes += 1
        If Options.GenerateWarnings Then Analysis.Warnings.Add("Variable declarations may need explicit types for VB.NET")
      End If

      ' Check for GOTO statements
      If node.Kind = SyntaxKind.GotoStatement Then Analysis.GotoStatementsFound += 1

      ' Check for GOSUB statements
      If node.Kind = SyntaxKind.GosubStatement Then

        If Options.GenerateWarnings Then Analysis.Warnings.Add("GOSUB statements should be converted to SUB procedures")

        ' Extract GOSUB target label using structured syntax properties
        Dim gosubStatement = DirectCast(node, GosubStatementSyntax)
        If gosubStatement.IdentifierToken IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(gosubStatement.IdentifierToken.Text) Then
          Dim targetLabel = gosubStatement.IdentifierToken.Text
          Analysis.GosubStatementTargets.Add(targetLabel)
        End If

      End If

    End Sub

    ''' <summary>
    ''' Counts PRINT statements in a compilation unit.
    ''' </summary>
    Private Shared Function CountPrintStatements(compilationUnit As CompilationUnitSyntax) As Integer
      Dim count = 0
      For Each member In compilationUnit.Members
        count += CountPrintStatementsRecursive(member)
      Next
      Return count
    End Function

    ''' <summary>
    ''' Recursively counts PRINT statements.
    ''' </summary>
    Private Shared Function CountPrintStatementsRecursive(node As SyntaxNode) As Integer
      Dim count = 0
      If node.Kind = SyntaxKind.PrintStatement Then count += 1
      For Each child In node.GetChildren()
        count += CountPrintStatementsRecursive(child)
      Next
      Return count
    End Function

    ''' <summary>
    ''' Rewrites compilation unit to add VB.NET boilerplate.
    ''' </summary>
    Protected Overrides Function RewriteCompilationUnit(node As CompilationUnitSyntax) As SyntaxNode
      Dim members = RewriteMembers(node.Members)
      ' If we need to add Module structure, wrap existing members
      If Options.AddModuleBoilerplate AndAlso ShouldWrapInModule(members) Then Return WrapInModule(node.SyntaxTree, members, node.EndOfFileToken)
      If Not members.Equals(node.Members) Then Return New CompilationUnitSyntax(node.SyntaxTree, members, node.EndOfFileToken)
      Return node
    End Function

    ''' <summary>
    ''' Determines if code should be wrapped in a Module.
    ''' </summary>
    Private Shared Function ShouldWrapInModule(members As ImmutableArray(Of MemberSyntax)) As Boolean
      ' If there are no existing Module, Sub, or Function declarations
      For Each member In members
        Select Case member.Kind
          Case SyntaxKind.FunctionDeclaration, SyntaxKind.SubStatement : Return False
        End Select
      Next
      Return True
    End Function

    ''' <summary>
    ''' Wraps members in a Module structure.
    ''' </summary>
    Private Shared Function WrapInModule(tree As SyntaxTree, members As ImmutableArray(Of MemberSyntax), endOfFileToken As SyntaxToken) As CompilationUnitSyntax
      ' This would create Module declaration syntax
      ' Due to Friend accessibility issues, we'll return the original for now
      ' In a full implementation, you'd need to create actual syntax nodes
      ' For now, we'll create a simple version by just returning the original
      ' The actual Module wrapper would be added during code generation
      Return New CompilationUnitSyntax(tree, members, endOfFileToken)
    End Function

    ''' <summary>
    ''' Rewrites PRINT statements to use WriteLine/Write with proper syntax.
    ''' </summary>
    Protected Overrides Function RewritePrintStatement(node As PrintStatementSyntax) As StatementSyntax
      If Not Options.ModernizePrint Then Return MyBase.RewritePrintStatement(node)
      ' Process the PRINT statement nodes and convert to proper VB.NET Console calls
      Dim convertedPrintCalls = ConvertPrintNodesToConsoleCalls(node.Nodes)
      Analysis.ChangesMade.Add("PRINT statement converted to WriteLine/Write calls")
      ' Return the original node - the actual transformation happens in text generation
      Return node
    End Function

    ''' <summary>
    ''' Converts PRINT statement nodes to VB.NET Console calls.
    ''' </summary>
    Private Shared Function ConvertPrintNodesToConsoleCalls(nodes As ImmutableArray(Of SyntaxNode)) As List(Of String)

      Dim result = New List(Of String)()
      Dim currentLine = New List(Of String)()

      For i = 0 To nodes.Length - 1

        Dim currentNode = nodes(i)

        Select Case currentNode.Kind

          Case SyntaxKind.SemicolonToken
            ' Semicolon means continue on same line - accumulate for current line
            Continue For

          Case SyntaxKind.CommaToken
            ' Comma means tab to next zone - use Write with spaces or separate calls
            Continue For

          Case SyntaxKind.SpcFunction
            Dim spcFunc = DirectCast(currentNode, SpcFunctionSyntax)
            currentLine.Add($"Space({WriteExpression(spcFunc.Expression)})")

          Case SyntaxKind.TabFunction
            Dim tabFunc = DirectCast(currentNode, TabFunctionSyntax)
            ' TAB(n) is more complex in .NET - we'll approximate
            currentLine.Add($"vbTab & New String("" "", {WriteExpression(tabFunc.Expression)})")

          Case Else
            ' Regular expression
            If TypeOf currentNode Is ExpressionSyntax Then
              Dim expr = DirectCast(currentNode, ExpressionSyntax)
              currentLine.Add(WriteExpression(expr))
            End If

        End Select

        ' Check if this is the end of the line (next token is not semicolon)
        Dim isEndOfLine = (i = nodes.Length - 1) OrElse
                           (i + 1 < nodes.Length AndAlso
                            nodes(i + 1).Kind <> SyntaxKind.SemicolonToken)

        If isEndOfLine AndAlso currentLine.Count > 0 Then
          If result.Count > 0 Then
            ' Previous lines exist, this should be a new line
            result.Add($"WriteLine({String.Join("" & "", currentLine)})")
          Else
            ' First line of the PRINT statement
            result.Add($"WriteLine({String.Join("" & "", currentLine)})")
          End If
          currentLine.Clear()
        ElseIf isEndOfLine = False AndAlso currentLine.Count > 0 Then
          ' Not end of line, use Write
          result.Add($"Write({String.Join("" & "", currentLine)})")
          currentLine.Clear()
        End If

      Next

      Return result

    End Function

    ''' <summary>
    ''' Writes an expression to string.
    ''' </summary>
    Private Shared Function WriteExpression(expr As ExpressionSyntax) As String
      ' For now, just return a placeholder - in a full implementation,
      ' you would recursively process the expression
      Return "EXPRESSION"
    End Function

    ''' <summary>
    ''' Rewrites Friend statement types (CLS, INPUT, etc.) for VB.NET compatibility.
    ''' </summary>
    Protected Overrides Function RewriteFriendStatement(node As SyntaxNode) As SyntaxNode
      ' Check if this is a command that needs parentheses for VB.NET
      Select Case node.Kind
        Case SyntaxKind.ClsStatement
          Analysis.ChangesMade.Add("CLS statement converted to use parentheses")
        Case SyntaxKind.InputStatement
          Analysis.ChangesMade.Add("INPUT statement converted to use parentheses")
        Case SyntaxKind.LocateStatement
          Analysis.ChangesMade.Add("LOCATE statement converted to use parentheses")
        Case SyntaxKind.BeepStatement
          Analysis.ChangesMade.Add("BEEP statement converted to use parentheses")
        Case SyntaxKind.SoundStatement
          Analysis.ChangesMade.Add("SOUND statement converted to use parentheses")
        Case SyntaxKind.CircleStatement
          Analysis.ChangesMade.Add("CIRCLE statement converted to use parentheses")
        Case SyntaxKind.ColorStatement
          Analysis.ChangesMade.Add("COLOR statement converted to use parentheses")
        Case SyntaxKind.RandomizeStatement
          Analysis.ChangesMade.Add("RANDOMIZE statement converted to use parentheses")
      End Select

      Return MyBase.RewriteFriendStatement(node)
    End Function

    ''' <summary>
    ''' Generates VB.NET code with appropriate imports and structure from a syntax tree.
    ''' </summary>
    Public Function GenerateVbNetCode(syntaxTree As SyntaxTree) As String
      Dim compilationUnit = DirectCast(syntaxTree.Root, CompilationUnitSyntax)
      Return GenerateVbNetCode(compilationUnit)
    End Function

    ''' <summary>
    ''' Generates VB.NET code with appropriate imports and structure from a compilation unit.
    ''' </summary>
    Public Function GenerateVbNetCode(compilationUnit As CompilationUnitSyntax) As String

      ' Generate the base content
      Dim baseCode As String
      Using writer = New StringWriter()
        WriteSyntaxNodeToText(writer, compilationUnit)
        baseCode = writer.ToString()
      End Using

      ' Add Module wrapper if needed
      If Analysis.AddedModule Then
        If Analysis.AddedSubMain Then
          ' Extract all executable statements using syntax tree traversal
          Dim executableCode = ExtractExecutableStatementsFromSyntax(compilationUnit)
          Return GetCodeWithImports($"Module Program{vbCrLf}{executableCode}{vbCrLf}End Module")
        Else
          ' Just wrap in Module without Sub Main
          Return GetCodeWithImports($"Module Program{vbCrLf}{baseCode}{vbCrLf}End Module")
        End If
      Else
        ' Generate content from syntax tree without wrapper
        Return GetCodeWithImports(baseCode)
      End If

    End Function

    ''' <summary>
    ''' Adds required imports to the code if needed.
    ''' </summary>
    Private Function GetCodeWithImports(code As String) As String
      ' Add required imports at the beginning if needed
      If Options.AddImports AndAlso Analysis.RequiredImports.Count > 0 Then
        Dim importsText = String.Join(vbCrLf, Analysis.RequiredImports.Select(Function(imp) $"Imports {imp}"))
        Return importsText + vbCrLf + vbCrLf + code
      Else
        Return code
      End If
    End Function

    ''' <summary>
    ''' Writes a syntax node to text with trivia (preserving formatting and whitespace).
    ''' </summary>
    Private Shared Sub WriteSyntaxNodeToText(writer As TextWriter, node As SyntaxNode)
      WriteNodeWithTrivia(writer, node)
    End Sub

    ''' <summary>
    ''' Writes a syntax node to text with trivia preservation.
    ''' </summary>
    Private Shared Sub WriteNodeWithTrivia(writer As TextWriter, node As SyntaxNode)

      If TypeOf node Is SyntaxToken Then

        Dim token = CType(node, SyntaxToken)

        ' Write leading trivia
        For Each trivia In token.LeadingTrivia
          writer.Write(trivia.Text)
        Next

        ' Write token text
        writer.Write(token.Text)

        ' Write trailing trivia
        For Each trivia In token.TrailingTrivia
          writer.Write(trivia.Text)
        Next

      Else

        ' For non-token nodes, recursively process children
        For Each child In node.GetChildren()
          WriteNodeWithTrivia(writer, child)
        Next

      End If

    End Sub

    ''' <summary>
    ''' Gets the text representation of a syntax node with trivia.
    ''' </summary>
    Private Shared Function GetSyntaxNodeText(node As SyntaxNode) As String
      Using writer = New StringWriter()
        WriteNodeWithTrivia(writer, node)
        Return writer.ToString()
      End Using
    End Function

    ''' <summary>
    ''' Extracts executable statements for Sub Main() using syntax tree traversal.
    ''' </summary>
    Private Function ExtractExecutableStatementsFromSyntax(compilationUnit As CompilationUnitSyntax) As String

      Dim result = New List(Of String)()

      ' Add Sub Main declaration
      result.Add($"{vbCrLf}  Sub Main()")

      ' Process each member in the compilation unit
      For Each member In compilationUnit.Members
        ProcessMemberForSubMain(member, result)
      Next

      ' Add End Sub and close with proper spacing
      result.Add($"  End Sub{vbCrLf}")

      Return String.Join(vbCrLf, result)

    End Function

    ''' <summary>
    ''' Processes a member for inclusion in Sub Main() using syntax tree analysis.
    ''' </summary>
    Private Sub ProcessMemberForSubMain(member As MemberSyntax, result As List(Of String))

      Select Case member.Kind

        Case SyntaxKind.GosubStatement
          ' Handle GOSUB statements using syntax properties
          result.Add("      ' TODO: GOSUB statements should be converted to SUB procedures")
          result.Add($"      {member}")

        Case SyntaxKind.LabelStatement
          ' Handle label statements using syntax properties
          result.Add($"{member}")
          ' Check if this label is a GOSUB target and add TODO comment
          Dim labelText = member.ToString().Trim(":"c)
          Dim isGosubTarget = Analysis.GosubStatementTargets.Any(Function(target) String.Equals(target, labelText, StringComparison.OrdinalIgnoreCase))
          If isGosubTarget Then result.Add("      ' TODO: Need to refactor as a SUB/END SUB")

        Case SyntaxKind.StopStatement
          ' Skip END statements (will be added after Sub Main)

        Case SyntaxKind.GlobalStatement
          ' Handle wrapped statements using syntax tree visitor pattern
          Dim globalStmt = DirectCast(member, GlobalStatementSyntax)
          If globalStmt.Statement IsNot Nothing Then
            Dim convertedLine = ProcessStatementForVbNet(globalStmt.Statement)
            ' Handle special case for RETURN GOSUB
            If globalStmt.Statement.Kind = SyntaxKind.ReturnGosubStatement Then
              result.Add($"      ' RETURN from GOSUB - needs SUB conversion")
            Else
              result.Add($"      {convertedLine}")
            End If
          End If

        Case Else
          ' Handle other members (function declarations, sub statements, etc.)
          result.Add($"      {member}")

      End Select

    End Sub

    ''' <summary>
    ''' Determines if a SyntaxKind represents a statement type.
    ''' </summary>
    Private Shared Function IsStatementKind(kind As SyntaxKind) As Boolean
      ' Check if kind name ends with "Statement"
      Dim kindName = kind.ToString()
      Return kindName.EndsWith("Statement")
    End Function

    ''' <summary>
    ''' Gets required imports for the conversion.
    ''' </summary>
    Public Function GetRequiredImports() As IEnumerable(Of String)
      Return Analysis.RequiredImports
    End Function

    ''' <summary>
    ''' Adds parentheses to QBasic commands for VB.NET compatibility using syntax tree analysis.
    ''' This implements a visitor pattern for statement transformation.
    ''' </summary>
    Private Function ProcessStatementForVbNet(statement As StatementSyntax) As String
      Select Case statement.Kind
        Case SyntaxKind.PrintStatement : Return ConvertPrintStatementUsingSyntax(DirectCast(statement, PrintStatementSyntax))
        Case SyntaxKind.ClsStatement : Return ConvertClsStatementUsingSyntax(DirectCast(statement, ClsStatementSyntax))
        Case SyntaxKind.InputStatement : Return ConvertInputStatementUsingSyntax(DirectCast(statement, InputStatementSyntax))
        Case SyntaxKind.LocateStatement : Return ConvertLocateStatementUsingSyntax(DirectCast(statement, LocateStatementSyntax))
        Case SyntaxKind.BeepStatement : Return ConvertBeepStatementUsingSyntax(DirectCast(statement, BeepStatementSyntax))
        Case SyntaxKind.ColorStatement : Return ConvertColorStatementUsingSyntax(DirectCast(statement, ColorStatementSyntax))
        Case SyntaxKind.RandomizeStatement : Return ConvertRandomizeStatementUsingSyntax(DirectCast(statement, RandomizeStatementSyntax))
        Case SyntaxKind.GosubStatement : Return ConvertGosubStatementUsingSyntax(DirectCast(statement, GosubStatementSyntax))
        Case SyntaxKind.GotoStatement : Return ConvertGotoStatementUsingSyntax(DirectCast(statement, GotoStatementSyntax))
        Case SyntaxKind.LabelStatement : Return ConvertLabelStatementUsingSyntax(DirectCast(statement, LabelStatementSyntax))
        Case SyntaxKind.StopStatement, SyntaxKind.EndStatement
          Return "' Statement removed for VB.NET compatibility"
        Case Else
          Return statement.ToString()
      End Select
    End Function

    ''' <summary>
    ''' Converts BEEP statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertBeepStatementUsingSyntax(beepStmt As BeepStatementSyntax) As String
      ' BEEP in QBasic becomes Beep() or MessageBox.Show in VB.NET
      Return "Beep()"
    End Function

    ''' <summary>
    ''' Converts GOSUB statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertGosubStatementUsingSyntax(gosubStmt As GosubStatementSyntax) As String
      If gosubStmt.IdentifierToken IsNot Nothing Then
        Return $"GOSUB {gosubStmt.IdentifierToken.Text}() ' TODO: Convert GOSUB to SUB call"
      End If
      Return "GOSUB ' TODO: Convert GOSUB statement to SUB call"
    End Function

    ''' <summary>
    ''' Converts GOTO statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertGotoStatementUsingSyntax(gotoStmt As GotoStatementSyntax) As String
      Return GetSyntaxNodeText(gotoStmt)
    End Function

    ''' <summary>
    ''' Converts Label statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertLabelStatementUsingSyntax(labelStmt As LabelStatementSyntax) As String
      Return GetSyntaxNodeText(labelStmt)
    End Function

    ''' <summary>
    ''' Converts PRINT statement using syntax tree properties instead of text parsing.
    ''' Handles QBasic semicolon (;) and comma (,) separators properly.
    ''' </summary>
    Private Shared Function ConvertPrintStatementUsingSyntax(printStmt As PrintStatementSyntax) As String

      If printStmt.Nodes.Length = 0 Then Return "WriteLine()"

      Dim result = New List(Of String)()
      Dim currentSegment = New List(Of String)()

      For i = 0 To printStmt.Nodes.Length - 1
        Dim currentNode = printStmt.Nodes(i)

        Select Case currentNode.Kind

          Case SyntaxKind.SemicolonToken
            ' Semicolon means continue on same line (no newline)
            ' Just continue to next item without ending the line

          Case SyntaxKind.CommaToken
            ' Comma means tab to next zone (14 character columns)
            ' Add current segment and then add a tab
            If currentSegment.Count > 0 Then
              result.Add($"Write({String.Join("" & "", currentSegment)})")
              result.Add("Write(vbTab)")
              currentSegment.Clear()
            Else
              result.Add("Write(vbTab)")
            End If

          Case SyntaxKind.SpcFunction
            Dim spcFunc = DirectCast(currentNode, SpcFunctionSyntax)
            currentSegment.Add($"Space({GetExpressionText(spcFunc.Expression)})")

          Case SyntaxKind.TabFunction
            Dim tabFunc = DirectCast(currentNode, TabFunctionSyntax)
            ' TAB(n) - position to column n
            currentSegment.Add($"New String("" ""c, {GetExpressionText(tabFunc.Expression)})")

          Case Else
            ' Handle expressions and literals
            If TypeOf currentNode Is ExpressionSyntax Then
              Dim expr = DirectCast(currentNode, ExpressionSyntax)
              currentSegment.Add(GetExpressionText(expr))
            ElseIf TypeOf currentNode Is SyntaxToken AndAlso
                   DirectCast(currentNode, SyntaxToken).Kind <> SyntaxKind.SemicolonToken AndAlso
                   DirectCast(currentNode, SyntaxToken).Kind <> SyntaxKind.CommaToken Then
              Dim token = DirectCast(currentNode, SyntaxToken)
              If Not String.IsNullOrWhiteSpace(token.Text) Then
                currentSegment.Add($"""{token.Text}""")
              End If
            End If

        End Select

        ' Check if this is the end of the line (next token is not semicolon)
        Dim isEndOfLine = (i = printStmt.Nodes.Length - 1) OrElse
                           (i + 1 < printStmt.Nodes.Length AndAlso
                            printStmt.Nodes(i + 1).Kind <> SyntaxKind.SemicolonToken)

        If isEndOfLine Then
          ' End of line - use WriteLine
          If currentSegment.Count > 0 Then
            result.Add($"WriteLine({String.Join("" & "", currentSegment)})")
            currentSegment.Clear()
          End If
        ElseIf currentSegment.Count > 0 Then
          ' Not End of line (semicolon) - use Write
          result.Add($"Write({String.Join("" & "", currentSegment)})")
          currentSegment.Clear()
        End If
      Next

      Return String.Join(" : ", result)

    End Function

    ''' <summary>
    ''' Gets text representation of an expression for Console output.
    ''' </summary>
    Private Shared Function GetExpressionText(expr As ExpressionSyntax) As String
      Using writer = New StringWriter()
        WriteNodeWithTrivia(writer, expr)
        Dim exprText = writer.ToString().Trim()
        ' Check if it's a string literal
        'If exprText.StartsWith(""""c) AndAlso exprText.EndsWith(""""c) Then
        Return exprText ' Keep quotes for strings
        'Else
        'Return exprText ' Return as-is for numbers, variables, etc.
        'End If
      End Using
    End Function

    ''' <summary>
    ''' Converts CLS statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertClsStatementUsingSyntax(clsStmt As ClsStatementSyntax) As String
      Dim clsText = GetSyntaxNodeText(clsStmt).Trim()
      ' Add parentheses for VB.NET compatibility
      Return $"{clsText}()"
    End Function

    ''' <summary>
    ''' Converts INPUT statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertInputStatementUsingSyntax(inputStmt As InputStatementSyntax) As String
      Dim inputText = GetSyntaxNodeText(inputStmt).Trim()
      ' Basic conversion for now
      Return $"{inputText}()" ' Add parentheses for VB.NET compatibility
    End Function

    ''' <summary>
    ''' Converts LOCATE statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertLocateStatementUsingSyntax(locateStmt As LocateStatementSyntax) As String
      Dim locateText = GetSyntaxNodeText(locateStmt).Trim()
      ' Add parentheses for VB.NET compatibility
      Return $"{locateText}()"
    End Function

    ''' <summary>
    ''' Converts COLOR statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertColorStatementUsingSyntax(colorStmt As ColorStatementSyntax) As String
      Dim colorText = GetSyntaxNodeText(colorStmt).Trim()
      ' Add parentheses for VB.NET compatibility
      Return $"{colorText}()"
    End Function

    ''' <summary>
    ''' Converts RANDOMIZE statement using syntax tree properties.
    ''' </summary>
    Private Shared Function ConvertRandomizeStatementUsingSyntax(randomizeStmt As RandomizeStatementSyntax) As String
      Dim randomizeText = GetSyntaxNodeText(randomizeStmt).Trim()
      ' Add parentheses for VB.NET compatibility
      Return $"{randomizeText}()"
    End Function

  End Class

End Namespace