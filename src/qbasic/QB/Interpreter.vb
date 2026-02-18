Imports System.IO
Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax
Imports QB.IO

Namespace Global.QB

  Public Class Interpreter

    Private m_previous As Compilation = Nothing
    Private ReadOnly m_variables As New Dictionary(Of String, Object)
    Private m_logWriter As StreamWriter = Nothing
    Private m_logToConsole As Boolean = False

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

    Private Sub SetupLogging(logFilePath As String, logToConsole As Boolean)
      If logToConsole Then
        m_logToConsole = True
        m_logWriter = Nothing
        Console.WriteLine()
        Console.WriteLine("# QBasic Execution Log")
        Console.WriteLine($"# Generated: {DateTime.Now:yyyy-MM-dd HH:mm:ss}")
        Console.WriteLine()
        Return
      End If

      If String.IsNullOrEmpty(logFilePath) Then Return
      Try
        m_logWriter = New StreamWriter(logFilePath, False)
        m_logWriter.WriteLine("# QBasic Execution Log")
        m_logWriter.WriteLine($"# Generated: {DateTime.Now:yyyy-MM-dd HH:mm:ss}")
        m_logWriter.WriteLine()
      Catch ex As Exception
        Console.WriteLine($"Warning: Could not open log file '{logFilePath}': {ex.Message}")
        m_logWriter = Nothing
      End Try
    End Sub

    Private Sub CloseLogging()
      If m_logWriter IsNot Nothing Then
        m_logWriter.Flush()
        m_logWriter.Close()
        m_logWriter.Dispose()
        m_logWriter = Nothing
      End If
      m_logToConsole = False
    End Sub

    Private Sub WriteLogLine(line As String)
      If m_logWriter IsNot Nothing Then
        m_logWriter.WriteLine(line)
      ElseIf m_logToConsole Then
        Console.WriteLine(line)
      End If
    End Sub

    Private Sub OnStatementExecuting(sender As Object, e As StatementExecutingEventArgs)
      If m_logWriter Is Nothing AndAlso Not m_logToConsole Then Return
      Dim qbasicLine = If(e.QBasicLineNumber > 0, $" (QBasic line {e.QBasicLineNumber})", "")
      WriteLogLine($"[{e.PhysicalLineNumber + 1}]{qbasicLine} {e.ContainerName}: {e.StatementKind} - {e.StatementText}")
    End Sub

    Private Sub OnVariableChanged(sender As Object, e As VariableChangedEventArgs)
      If m_logWriter Is Nothing AndAlso Not m_logToConsole Then Return
      Dim indices = If(e.ArrayIndices IsNot Nothing, $"({String.Join(",", e.ArrayIndices)})", "")
      Dim oldVal = If(e.OldValue Is Nothing, "<unset>", e.OldValue?.ToString())
      Dim newVal = e.NewValue?.ToString()
      WriteLogLine($"  VAR: {e.VariableName}{indices} = {oldVal} -> {newVal}")
    End Sub

    Private Sub OnErrorOccurred(sender As Object, e As ErrorOccurredEventArgs)
      If m_logWriter Is Nothing AndAlso Not m_logToConsole Then Return
      Dim qbasicLine = If(e.QBasicLineNumber > 0, $" (QBasic line {e.QBasicLineNumber})", "")
      WriteLogLine($"  ERROR at line {e.PhysicalLineNumber + 1}{qbasicLine}: [{e.ErrorCode}] {e.ErrorMessage}")
      If e.StatementText.Length > 0 Then
        WriteLogLine($"    Statement: {e.StatementText}")
      End If
    End Sub

    Sub Run(text As String, Optional dumpGlobals As Boolean = False, Optional commandLineArgs As String() = Nothing, Optional logFilePath As String = Nothing, Optional logToConsole As Boolean = False)

      SetupLogging(logFilePath, logToConsole)

      Dim tree = SyntaxTree.Parse(text)
      Dim compilation = QB.CodeAnalysis.Compilation.CreateScript(m_previous, tree)

      ' Subscribe to evaluation events if logging is enabled
      If m_logWriter IsNot Nothing OrElse m_logToConsole Then
        AddHandler compilation.StatementExecuting, AddressOf OnStatementExecuting
        AddHandler compilation.VariableChanged, AddressOf OnVariableChanged
        AddHandler compilation.ErrorOccurred, AddressOf OnErrorOccurred
      End If

      Try
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

          m_previous = compilation

        End If

      Finally
        ' Unsubscribe from events
        If m_logWriter IsNot Nothing OrElse m_logToConsole Then
          RemoveHandler compilation.StatementExecuting, AddressOf OnStatementExecuting
          RemoveHandler compilation.VariableChanged, AddressOf OnVariableChanged
          RemoveHandler compilation.ErrorOccurred, AddressOf OnErrorOccurred
        End If

        CloseLogging()
      End Try

      If dumpGlobals Then
        Console.WriteLine("Global variables:")
        For Each kv In m_variables
          Console.WriteLine($"{kv.Key} = {kv.Value}")
        Next
      End If
    End Sub

  End Class

End Namespace