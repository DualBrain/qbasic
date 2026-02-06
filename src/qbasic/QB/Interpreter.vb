Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax
Imports QB.IO
Imports QB.CodeAnalysis.Binding

Namespace Global.QB

  Public Class Interpreter

    Private m_previous As Compilation = Nothing
    Private ReadOnly m_variables As New Dictionary(Of String, Object)

    Public ReadOnly Property Variables As Dictionary(Of String, Object)
      Get
        Return m_variables
      End Get
    End Property

    Sub New()

    End Sub

    Shared Function DebugTree(text As String) As String
      Dim tree = SyntaxTree.Parse(text)
      Return tree.Root.ToString
      'tree.Root.WriteTo(Console.Out)
    End Function

    Sub DebugProgram(text As String)
      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = QB.CodeAnalysis.Compilation.CreateScript(m_previous, tree)
      compilation.EmitTree(Console.Out)
    End Sub

    Sub Run(text As String, Optional dumpGlobals As Boolean = False, Optional commandLineArgs As String() = Nothing)

      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = QB.CodeAnalysis.Compilation.CreateScript(m_previous, tree)

      'If m_showTree Then tree.Root.WriteTo(Console.Out)
      'If m_showProgram Then compilation.EmitTree(Console.Out)

      Dim result = compilation.Evaluate(m_variables, commandLineArgs)
      Console.Out.WriteDiagnostics(result.Diagnostics)

      If Not result.Diagnostics.HasErrors Then

        ' Check for chain request
        If result.ChainRequest IsNot Nothing Then
          Console.WriteLine($"CHAIN requested: {result.ChainRequest.Filename}")
          If result.ChainRequest.LineNumber.HasValue Then
            Console.WriteLine($"Chain to line: {result.ChainRequest.LineNumber.Value}")
          End If
          
          ' Basic CHAIN handling for now - just indicate success
          ' Full implementation would require:
          ' 1. Load and parse target file
          ' 2. Reset all state except COMMON variables
          ' 3. Continue execution at target file (optionally at specific line)
          
          Environment.Exit(0)
        End If

        If TypeOf result.Value Is UInt64 Then
          Environment.Exit(0)
        End If

        ' The expression evaluator will return a 
        ' result of some sort, the below code will
        ' output this final "top level" result.
        ' Now that we have `print`, shouldn't need.
        'If result.Value IsNot Nothing Then
        '  Console.ForegroundColor = ConsoleColor.White
        '  Console.WriteLine(result.Value)
        '  Console.ResetColor()
        'End If

        m_previous = compilation

      End If

      If dumpGlobals Then
        Console.WriteLine("Global variables:")
        For Each kv In m_variables
          Console.WriteLine($"{kv.Key} = {kv.Value}")
        Next
      End If
    End Sub

  End Class

End Namespace