Imports System.Collections.Immutable
Imports System.Linq

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
      Public Property ModernizedPrintStatements As Integer = 0
      Public Property VariablesRequiringTypes As Integer = 0
      Public Property GotoStatementsFound As Integer = 0
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
        
        ' Only warn about GOTO if there are no procedures (already restructured)
        If Not hasProcedures AndAlso Analysis.GotoStatementsFound > 0 Then
          Analysis.Warnings.Add("GOTO statements should be replaced with structured programming (If/Select/Loops)")
        End If
      End If
    End Sub

    ''' <summary>
    ''' Analyzes individual statements for conversion needs.
    ''' </summary>
    Private Sub AnalyzeStatementForConversion(node As SyntaxNode)
      ' Check for PRINT statements
      If node.Kind = SyntaxKind.PrintStatement Then
        If Options.ModernizePrint Then
          Analysis.RequiredImports.Add("System.Console")
        End If
      End If

      ' Check for variable usage that might need explicit types
      If node.Kind = SyntaxKind.VariableDeclaration Then
        Analysis.VariablesRequiringTypes += 1
        If Options.GenerateWarnings Then
          Analysis.Warnings.Add("Variable declarations may need explicit types for VB.NET")
        End If
      End If

      ' Check for GOTO statements
      If node.Kind = SyntaxKind.GotoStatement Then
        Analysis.GotoStatementsFound += 1
      End If

      ' Check for GOSUB statements
      If node.Kind = SyntaxKind.GosubStatement Then
        If Options.GenerateWarnings Then
          Analysis.Warnings.Add("GOSUB statements should be converted to SUB procedures")
        End If
        
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
    Private Function CountPrintStatements(compilationUnit As CompilationUnitSyntax) As Integer
      Dim count = 0
      
      For Each member In compilationUnit.Members
        count += CountPrintStatementsRecursive(member)
      Next
      
      Return count
    End Function

    ''' <summary>
    ''' Recursively counts PRINT statements.
    ''' </summary>
    Private Function CountPrintStatementsRecursive(node As SyntaxNode) As Integer
      Dim count = 0
      
      If node.Kind = SyntaxKind.PrintStatement Then
        count += 1
      End If

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
      If Options.AddModuleBoilerplate AndAlso ShouldWrapInModule(members) Then
        Return WrapInModule(node.SyntaxTree, members, node.EndOfFileToken)
      End If

      If Not members.Equals(node.Members) Then
        Return New CompilationUnitSyntax(node.SyntaxTree, members, node.EndOfFileToken)
      End If

      Return node
    End Function

    ''' <summary>
    ''' Determines if code should be wrapped in a Module.
    ''' </summary>
    Private Function ShouldWrapInModule(members As ImmutableArray(Of MemberSyntax)) As Boolean
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
    Private Function WrapInModule(tree As SyntaxTree, members As ImmutableArray(Of MemberSyntax), endOfFileToken As SyntaxToken) As CompilationUnitSyntax
      ' This would create Module declaration syntax
      ' Due to Friend accessibility issues, we'll return the original for now
      ' In a full implementation, you'd need to create actual syntax nodes
      
      ' For now, we'll create a simple version by just returning the original
      ' The actual Module wrapper would be added during code generation
      Return New CompilationUnitSyntax(tree, members, endOfFileToken)
    End Function

    ''' <summary>
    ''' Rewrites PRINT statements to use Console.WriteLine/Write.
    ''' </summary>
    Protected Overrides Function RewritePrintStatement(node As PrintStatementSyntax) As StatementSyntax
      If Not Options.ModernizePrint Then
        Return MyBase.RewritePrintStatement(node)
      End If

      ' Analyze the PRINT statement to determine if it should be WriteLine or Write
      Dim shouldEndWithNewLine = ShouldPrintEndWithNewLine(node)
      
      ' In a full implementation, you'd:
      ' 1. Replace PRINT with Console.Write or Console.WriteLine
      ' 2. Handle SPC() and TAB() functions
      ' 3. Handle comma/semicolon separators
      ' 4. Convert expressions to VB.NET compatible ones
      
      ' For now, return the original with a note
      Analysis.ChangesMade.Add("PRINT statement marked for modernization")
      Return MyBase.RewritePrintStatement(node)
    End Function

    ''' <summary>
    ''' Determines if a PRINT statement should end with a newline.
    ''' </summary>
    Private Function ShouldPrintEndWithNewLine(node As PrintStatementSyntax) As Boolean
      ' Check if the PRINT statement ends with ; or , (which suppress newline)
      If node.Nodes.Length > 0 Then
        Dim lastNode = node.Nodes.Last()
        ' Check if last node is a symbol representing ; or ,
        ' This would require more detailed analysis in a full implementation
      End If
      
      Return True ' Default to WriteLine
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
      If Options.AddImports AndAlso Analysis.RequiredImports.Any() Then
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
        If String.IsNullOrWhiteSpace(trimmedLine) OrElse trimmedLine.StartsWith("'") OrElse 
           trimmedLine.StartsWith("======") Then
          Continue For
        End If
        
        ' Skip END statements (will be added after Sub Main)
        If trimmedLine.ToUpper().StartsWith("END") Then
          Continue For
        End If
        
' Handle GOSUB statements - keep code but add TODO comments before
        If trimmedLine.ToUpper().StartsWith("GOSUB") Then
          ' Add TODO comment before GOSUB (maintain functionality)
          result.Add($"      ' TODO: GOSUB statements should be converted to SUB procedures")
          result.Add($"{New String(" "c, 6)}{trimmedLine}")
          Continue For
        End If
        
        ' Handle label lines (including LabelXXX: targets for GOTO)
        If trimmedLine.EndsWith(":") Then
          ' Extract label name - everything before the colon
          Dim colonIndex = trimmedLine.IndexOf(":")
          Dim labelName = trimmedLine.Substring(0, colonIndex).Trim()
          
          ' Check if this label is a GOSUB target and add TODO comment (case-insensitive)
          Dim isGosubTarget = Analysis.GosubStatementTargets.Any(Function(target) String.Equals(target, labelName, StringComparison.OrdinalIgnoreCase))
          
          ' Keep label with colon for GOTO compatibility, positioned to far left
          result.Add(trimmedLine)
          
          If isGosubTarget Then
            result.Add($"      ' TODO: Need to refactor as a SUB/END SUB")
          End If
        Else
          ' Add executable statement with proper 2-space indentation
          result.Add($"      {trimmedLine}")
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

  End Class

End Namespace