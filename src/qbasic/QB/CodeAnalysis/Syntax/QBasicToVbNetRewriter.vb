Imports System.Collections.Immutable

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
        Dim kindName = member.Kind.ToString()
        If kindName.Contains("Module") OrElse kindName.Contains("Function") OrElse kindName.Contains("Sub") Then
          hasModule = True
          hasProcedures = True
          Exit For
        End If
      Next
      
      ' Check if there are only executable statements
      Dim hasOnlyStatements = True
      For Each member In compilationUnit.Members
        Dim kindName = member.Kind.ToString()
        If Not kindName.Contains("Statement") Then
          hasOnlyStatements = False
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

        ' Extract GOSUB target label for TODO comments
        Try
          Dim labelProp = node.GetType().GetProperty("IdentifierToken")
          If labelProp IsNot Nothing Then
            Dim identifierToken = labelProp.GetValue(node)
            If identifierToken IsNot Nothing Then
              Dim textProp = identifierToken.GetType().GetProperty("Text")
              If textProp IsNot Nothing Then
                Dim targetText = DirectCast(textProp.GetValue(identifierToken), String)
                If Not String.IsNullOrWhiteSpace(targetText) Then
                  Dim targetLabel = targetText  ' Use the full identifier text (e.g., "Label300")
                  Analysis.GosubStatementTargets.Add(targetLabel)
                End If
              End If
            End If
          End If
        Catch ex As Exception
          ' Ignore reflection errors
        End Try

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
        Dim kindName = member.Kind.ToString()
        If kindName.Contains("Module") OrElse kindName.Contains("Function") OrElse kindName.Contains("Sub") Then
          Return False
        End If
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
    ''' Rewrites PRINT statements to use Console.WriteLine/Write with proper syntax.
    ''' </summary>
    Protected Overrides Function RewritePrintStatement(node As PrintStatementSyntax) As StatementSyntax
      If Not Options.ModernizePrint Then Return MyBase.RewritePrintStatement(node)
      ' Process the PRINT statement nodes and convert to proper VB.NET Console calls
      Dim convertedPrintCalls = ConvertPrintNodesToConsoleCalls(node.Nodes)
      Analysis.ChangesMade.Add("PRINT statement converted to Console.WriteLine/Write calls")
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
            ' Comma means tab to next zone - use Console.Write with spaces or separate calls
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
            result.Add($"Console.WriteLine({String.Join("" & "", currentLine)})")
          Else
            ' First line of the PRINT statement
            result.Add($"Console.WriteLine({String.Join("" & "", currentLine)})")
          End If
          currentLine.Clear()
        ElseIf isEndOfLine = False AndAlso currentLine.Count > 0 Then
          ' Not end of line, use Console.Write
          result.Add($"Console.Write({String.Join("" & "", currentLine)})")
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
    ''' Generates VB.NET code with appropriate imports and structure.
    ''' </summary>
    Public Function GenerateVbNetCode(originalCode As String) As String

      Dim result = originalCode

      ' Add Module wrapper if needed
      If Analysis.AddedModule Then
        If Analysis.AddedSubMain Then
          ' Extract all executable statements and move them into Sub Main()
          Dim executableCode = ExtractExecutableStatements(result)
          result = $"Module Program{vbCrLf}{executableCode}{vbCrLf}End Module"
          Analysis.ChangesMade.Add("Added Module/End Module wrapper with Sub Main()")
        Else
          ' Just wrap in Module without Sub Main
          result = $"Module Program{vbCrLf}{result}{vbCrLf}End Module"
          Analysis.ChangesMade.Add("Added Module/End Module wrapper")
        End If
      End If

      ' Add required imports at the beginning (before Module)
      If Options.AddImports AndAlso Analysis.RequiredImports.Count > 0 Then
        Dim importsText = String.Join(vbCrLf, Analysis.RequiredImports.Select(Function(imp) $"Imports {imp}"))
        result = importsText + vbCrLf + vbCrLf + result
        Analysis.ChangesMade.Add($"Added {Analysis.RequiredImports.Count} IMPORT statements")
      End If

      Return result

    End Function

''' <summary>
    ''' Extracts executable statements for Sub Main() - handles GOTO targets properly.
    ''' </summary>
    Private Function ExtractExecutableStatements(code As String) As String

      Dim lines = code.Split({vbCrLf, vbLf}, StringSplitOptions.None)
      Dim result = New List(Of String)()
      
      ' Add Sub Main declaration
      result.Add("  Sub Main()")
      
      For Each line In lines

        Dim trimmedLine = line.Trim()

        ' Skip empty lines and transformation headers
        If String.IsNullOrWhiteSpace(trimmedLine) OrElse trimmedLine.StartsWith("'"c) OrElse
           trimmedLine.StartsWith("======") Then
          Continue For
        End If

        ' Skip END statements (will be added after Sub Main)
        If trimmedLine.ToUpper().StartsWith("END") Then
          Continue For
        End If

        ' Handle GOSUB statements - keep code but add TODO comments before and add parentheses
        If trimmedLine.ToUpper().StartsWith("GOSUB") Then

          ' Add TODO comment before GOSUB (maintain functionality)
          result.Add($"      ' TODO: GOSUB statements should be converted to SUB procedures")
          
          ' Extract GOSUB target and add parentheses for VB.NET compatibility
          Dim gosubParts = trimmedLine.Split({" "c}, StringSplitOptions.RemoveEmptyEntries)
          If gosubParts.Length >= 2 Then
            Dim targetLabel = String.Join(" ", gosubParts.Skip(1)).Trim()
            result.Add($"{New String(" "c, 6)}GOSUB {targetLabel}()")
          Else
            ' Fallback if parsing fails
            result.Add($"{New String(" "c, 6)}{trimmedLine}()")
          End If

          Continue For

        End If

        ' Handle label lines (including LabelXXX: targets for GOTO)
        If trimmedLine.EndsWith(":"c) Then

          ' Extract label name - everything before the colon
          Dim colonIndex = trimmedLine.IndexOf(":"c)
          Dim labelName = trimmedLine.Substring(0, colonIndex).Trim()

          ' Check if this label is a GOSUB target and add TODO comment (case-insensitive)
          Dim isGosubTarget = Analysis.GosubStatementTargets.Any(Function(target) String.Equals(target, labelName, StringComparison.OrdinalIgnoreCase))

          ' Keep label with colon for GOTO compatibility, positioned to far left
          result.Add(trimmedLine)

          If isGosubTarget Then result.Add($"      ' TODO: Need to refactor as a SUB/END SUB")

        Else
          ' Check if this is a QBasic command that needs parentheses and add them
          Dim processedLine = AddParenthesesToQBasicCommands(trimmedLine)
          ' Add executable statement with proper 2-space indentation
          result.Add($"      {processedLine}")
        End If

      Next

      ' Add End Sub and close with proper spacing
      result.Add("  End Sub")
      
      Return String.Join(vbCrLf, result)

    End Function

    ''' <summary>
    ''' Gets required imports for the conversion.
    ''' </summary>
    Public Function GetRequiredImports() As IEnumerable(Of String)
      Return Analysis.RequiredImports
    End Function

    ''' <summary>
    ''' Adds parentheses to QBasic commands for VB.NET compatibility.
    ''' </summary>
    Private Shared Function AddParenthesesToQBasicCommands(line As String) As String

      If String.IsNullOrWhiteSpace(line) Then Return line

      ' Check for specific commands
      If line.StartsWith("PRINT ", StringComparison.OrdinalIgnoreCase) Then
        Return ConvertPrintStatement(line.Substring(6).Trim)
      ElseIf line.StartsWith("CLS", StringComparison.OrdinalIgnoreCase) Then
        Return "CLS()"
      ElseIf line.StartsWith("INPUT ", StringComparison.OrdinalIgnoreCase) Then
        Return $"INPUT({line.Substring(6).Trim})"
      ElseIf line.StartsWith("LOCATE ", StringComparison.OrdinalIgnoreCase) Then
        Return $"LOCATE({line.Substring(7).Trim})"
      ElseIf line.StartsWith("COLOR ", StringComparison.OrdinalIgnoreCase) Then
        Return $"COLOR({line.Substring(6).Trim})"
      ElseIf line.StartsWith("BEEP", StringComparison.OrdinalIgnoreCase) Then
        Return "BEEP()"
      ElseIf line.StartsWith("RANDOMIZE", StringComparison.OrdinalIgnoreCase) Then
        If line.Length > 10 Then
          Return $"RANDOMIZE({line.Substring(10).Trim})"
        Else
          Return "RANDOMIZE()"
        End If
      End If

      Return line

    End Function

    ''' <summary>
    ''' Converts a PRINT statement to VB.NET Console calls.
    ''' </summary>
    Private Shared Function ConvertPrintStatement(printArgs As String) As String

      If String.IsNullOrWhiteSpace(printArgs) Then Return "Console.WriteLine()"

      ' Parse the PRINT arguments - this is a simplified parser
      Dim result = New List(Of String)()
      Dim currentSegment = ""
      Dim i = 0
      Dim inString = False

      While i < printArgs.Length

        Dim ch = printArgs(i)

        Select Case ch

          Case """"c
            ' Quote character - toggle string mode
            inString = Not inString
            currentSegment += ch
            i += 1

          Case ";"c
            If Not inString Then
              ' Semicolon - continue on same line
              If Not String.IsNullOrWhiteSpace(currentSegment) Then
                result.Add($"Write({currentSegment.Trim()})")
                currentSegment = ""
              End If
              i += 1
            Else
              ' Semicolon inside string - keep it
              currentSegment += ch
              i += 1
            End If

          Case ","c
            If Not inString Then
              ' Comma - tab to next zone (approximate with spaces)
              If Not String.IsNullOrWhiteSpace(currentSegment) Then
                result.Add($"Write({currentSegment.Trim()})")
                currentSegment = ""
              End If
              result.Add("Write(vbTab)")
              i += 1
            Else
              ' Comma inside string - keep it
              currentSegment += ch
              i += 1
            End If

          Case "'"c
            If Not inString Then
              ' Start of comment - stop processing
              If Not String.IsNullOrWhiteSpace(currentSegment) Then
                result.Add($"WriteLine({currentSegment.Trim()})")
              End If
              ' Clear current segment since we already processed it
              currentSegment = ""
              ' Add only the comment part (from current position)
              result.Add(printArgs.Substring(i))
              Exit While
            Else
              ' Apostrophe inside string - keep it
              currentSegment += ch
              i += 1
            End If

          Case Else
            currentSegment += ch
            i += 1

        End Select

      End While

      ' Handle the last segment
      If Not String.IsNullOrWhiteSpace(currentSegment) Then
        ' Check if it ends with semicolon (suppress newline)
        If currentSegment.EndsWith(";"c) Then
          currentSegment = currentSegment.Substring(0, currentSegment.Length - 1)
          result.Add($"Write({currentSegment.Trim()})")
        Else
          result.Add($"WriteLine({currentSegment.Trim()})")
        End If
      End If

      ' Join all the Console calls
      Return String.Join(" : ", result)

    End Function

  End Class

End Namespace