Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Symbols
Imports QB.CodeAnalysis.Syntax
Imports QB.IO

Public Class Interpreter

  Private m_previous As Compilation = Nothing
  Private ReadOnly m_variables As New Dictionary(Of VariableSymbol, Object)

  Sub New()

  End Sub

  Sub Run(text As String)

    Dim tree = SyntaxTree.Parse(text)
    Dim compilation = QB.CodeAnalysis.Compilation.CreateScript(m_previous, tree)

    'If m_showTree Then tree.Root.WriteTo(Console.Out)
    'If m_showProgram Then compilation.EmitTree(Console.Out)

    Dim result = compilation.Evaluate(m_variables)
    Console.Out.WriteDiagnostics(result.Diagnostics)

    If Not result.Diagnostics.HasErrors Then

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
  End Sub

End Class
