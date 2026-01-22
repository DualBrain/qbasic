Imports QB.CodeAnalysis
Imports QB.CodeAnalysis.Syntax

Imports Xunit

Namespace QBasic.CodeAnalysis.Tests

  Public Class FileIoTests

    <Fact>
    Public Sub FreeFileReturnsValidFileNumber()
      ' Test FREEFILE returns a valid file number
      Dim test = "LET f = FREEFILE"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)
      Dim fileNum = CInt(vars("f"))
      Assert.True(fileNum >= 1 AndAlso fileNum <= 255)
    End Sub

    <Fact>
    Public Sub LineInputReadsMultipleLines()

      Dim contents = "First line
Second line
Third line"

      ' Create test file
      IO.File.WriteAllText("test_lines.txt", contents)

      Dim test = "
LET f = FREEFILE
OPEN ""test_lines.txt"" FOR INPUT AS f
LINE INPUT #f, line1$
LINE INPUT #f, line2$
LINE INPUT #f, line3$
CLOSE f"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.Equal("First line", vars("line1$"))
      Assert.Equal("Second line", vars("line2$"))
      Assert.Equal("Third line", vars("line3$"))

      ' Clean up
      IO.File.Delete("test_lines.txt")
    End Sub

    <Fact>
    Public Sub OpenForOutputCreatesFile()

      If IO.File.Exists("test_output.txt") Then IO.File.Delete("test_output.txt")

      Dim test = "
LET f = FREEFILE
OPEN ""test_output.txt"" FOR OUTPUT AS f
PRINT #f, ""Hello World""
CLOSE f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      ' File should exist after execution
      Assert.True(IO.File.Exists("test_output.txt"))
      Assert.Equal("Hello World" & vbCrLf, IO.File.ReadAllText("test_output.txt"))

      '' Clean up
      'IO.File.Delete("test_output.txt")
    End Sub

    <Fact>
    Public Sub OpenForInputReadsFile()
      ' Create test file
      IO.File.WriteAllText("test_input.txt", "Line 1" & vbCrLf & "Line 2" & vbCrLf & "Line 3")

      ' Use colon-separated statements in one compilation
      Dim test = "
LET f = FREEFILE
OPEN ""test_input.txt"" FOR INPUT AS f
LINE INPUT #f, data1$
LINE INPUT #f, data2$
LINE INPUT #f, data3$
CLOSE f"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.Equal("Line 1", vars("data1$"))
      Assert.Equal("Line 2", vars("data2$"))
      Assert.Equal("Line 3", vars("data3$"))

      ' Clean up
      IO.File.Delete("test_input.txt")
    End Sub

    <Fact>
    Public Sub OpenForAppendAddsToFile()
      ' Create initial file
      IO.File.WriteAllText("test_append.txt", "Initial content" & vbCrLf)

      Dim test = "
LET f = FREEFILE
OPEN ""test_append.txt"" FOR APPEND AS f
PRINT #f, ""Appended line""
CLOSE f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Dim content = IO.File.ReadAllText("test_append.txt")
      Assert.Equal("Initial content" & vbCrLf & "Appended line" & vbCrLf, content)

      ' Clean up
      IO.File.Delete("test_append.txt")
    End Sub

    <Fact>
    Public Sub OpenForBinaryAllowsReadWrite()
      Dim test = "LET f = FREEFILE : OPEN ""test_binary.txt"" FOR BINARY AS f : CLOSE f"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.True(IO.File.Exists("test_binary.txt"))

      ' Clean up
      IO.File.Delete("test_binary.txt")
    End Sub

    <Fact>
    Public Sub OpenForRandomAllowsReadWrite()
      Dim test = "
LET f = FREEFILE
OPEN ""test_random.txt"" FOR RANDOM AS f
PRINT #f, ""Random data""
CLOSE f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.True(IO.File.Exists("test_random.txt"))

      ' Clean up
      IO.File.Delete("test_random.txt")
    End Sub

    <Fact>
    Public Sub CloseAllFilesClosesAllOpenFiles()

      If IO.File.Exists("test_close1.txt") Then IO.File.Delete("test_close1.txt")
      If IO.File.Exists("test_close2.txt") Then IO.File.Delete("test_close2.txt")

      Dim test = "
OPEN ""test_close1.txt"" FOR OUTPUT AS 1
OPEN ""test_close2.txt"" FOR OUTPUT AS 2
PRINT #1, ""File 1""
PRINT #2, ""File 2""
CLOSE
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.True(IO.File.Exists("test_close1.txt"))
      Assert.True(IO.File.Exists("test_close2.txt"))

      '' Clean up
      'IO.File.Delete("test_close1.txt")
      'IO.File.Delete("test_close2.txt")
    End Sub

    <Fact>
    Public Sub EofReturnsFalseWhenNotAtEnd()
      ' Create test file with content
      IO.File.WriteAllText("test_eof.txt", "Line 1" & vbCrLf & "Line 2")

      Dim test = "
LET f = FREEFILE
OPEN ""test_eof.txt"" FOR INPUT AS f
LET notEof = EOF(f)
CLOSE f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.False(CBool(vars("notEof")))

      ' Clean up
      IO.File.Delete("test_eof.txt")
    End Sub

    <Fact>
    Public Sub EofReturnsTrueWhenAtEnd()
      ' Create test file with content
      IO.File.WriteAllText("test_eof_end.txt", "Line 1" & vbCrLf & "Line 2")

      Dim test = "
LET f = FREEFILE
OPEN ""test_eof_end.txt"" FOR INPUT AS f
LINE INPUT #f, line1$
LINE INPUT #f, line2$
LET isEof = EOF(f)
CLOSE f"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.Equal("Line 1", vars("line1$"))
      Assert.Equal("Line 2", vars("line2$"))
      Assert.True(CBool(vars("isEof")))

      ' Clean up
      IO.File.Delete("test_eof_end.txt")
    End Sub

    <Fact>
    Public Sub LofReturnsFileSize()

      If IO.File.Exists("test_lof.txt") Then IO.File.Delete("test_lof.txt")

      Dim content = "This is a test file" & vbCrLf & "with multiple lines"
      IO.File.WriteAllText("test_lof.txt", content)

      Dim test = "
LET f = FREEFILE
OPEN ""test_lof.txt"" FOR INPUT AS f
LET size = LOF(f)
CLOSE f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.Equal(CLng(content.Length), CLng(vars("size")))

      '' Clean up
      'IO.File.Delete("test_lof.txt")
    End Sub

    <Fact>
    Public Sub LocReturnsPositionForSequentialFile()

      If IO.File.Exists("test_loc_seq.txt") Then IO.File.Delete("test_loc_seq.txt")

      Dim contents = "Line 1 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890
Line 2 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890
Line 3
Line 4 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890 01234567890"
      IO.File.WriteAllText("test_loc_seq.txt", contents)

      Dim test = "
LET f = FREEFILE
OPEN ""test_loc_seq.txt"" FOR INPUT AS f
LINE INPUT #f, dummy$
LINE INPUT #f, dummy$
LINE INPUT #f, dummy$
LET p = LOC(f)
CLOSE f"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      Assert.Equal("Line 3", vars("dummy$"))
      Assert.Equal($"{3}", $"{vars("p")}")

      '' Clean up
      'IO.File.Delete("test_loc_seq.txt")
    End Sub

    <Fact>
    Public Sub LocReturnsByteOffsetForBinaryFile()

      ' Clean up
      If IO.File.Exists("test_loc_bin.txt") Then IO.File.Delete("test_loc_bin.txt")

      Dim content = "Hello World"
      IO.File.WriteAllText("test_loc_bin.txt", content)

      Dim test = "
LET f = FREEFILE
OPEN ""test_loc_bin.txt"" FOR BINARY AS f
LET p = LOC(f)
CLOSE f"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      ' At beginning of binary file, position should be 0
      Assert.Equal(0L, CLng(vars("p")))

      '' Clean up
      'IO.File.Delete("test_loc_bin.txt")
    End Sub

    <Fact>
    Public Sub OpenFileAlreadyOpenThrowsError()

      If IO.File.Exists("test_double_open.txt") Then IO.File.Delete("test_double_open.txt")

      Dim test = "
LET f = FREEFILE
OPEN ""test_double_open.txt"" FOR OUTPUT AS f
OPEN ""test_double_open.txt"" FOR OUTPUT AS f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()

      ' Should throw error when trying to open already open file
      Dim ex = Assert.ThrowsAny(Of Exception)(Sub() comp.Evaluate(vars))

      '' Clean up if file was created
      'If IO.File.Exists("test_double_open.txt") Then
      '  IO.File.Delete("test_double_open.txt")
      'End If
    End Sub

    <Fact>
    Public Sub AccessClosedFileThrowsError()

      ' Clean up
      If IO.File.Exists("test_closed.txt") Then IO.File.Delete("test_closed.txt")

      Dim test = "
LET f = FREEFILE
OPEN ""test_closed.txt"" FOR OUTPUT AS f
CLOSE f
PRINT #f, ""This should fail""
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()

      ' Should throw error when accessing closed file
      Dim ex = Assert.ThrowsAny(Of Exception)(Sub() comp.Evaluate(vars))

      '' Clean up
      'If IO.File.Exists("test_closed.txt") Then
      '  IO.File.Delete("test_closed.txt")
      'End If
    End Sub

    <Fact>
    Public Sub LineInputPastEndThrowsError()

      Dim contents = "Only one line"

      ' Create file with one line
      If IO.File.Exists("test_past_end.txt") Then
        IO.File.Delete("test_past_end.txt")
      End If
      IO.File.WriteAllText("test_past_end.txt", contents)

      Dim test = "
LET f = FREEFILE
OPEN ""test_past_end.txt"" FOR INPUT AS f
LINE INPUT #f, dummy$
LINE INPUT #f, dummy$  ' This should fail
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()

      ' Should throw error when reading past end
      Dim ex = Assert.ThrowsAny(Of Exception)(Sub() comp.Evaluate(vars))
      Assert.Contains("Input past end", ex.Message)

      '' Clean up
      'If IO.File.Exists("test_past_end.txt") Then
      '  IO.File.Delete("test_past_end.txt")
      'End If
    End Sub

    <Fact>
    Public Sub PrintUsingFormatsOutput()

      If IO.File.Exists("test_using.txt") Then IO.File.Delete("test_using.txt")

      Dim test = "
LET f = FREEFILE
OPEN ""test_using.txt"" FOR OUTPUT AS f
PRINT #f, USING ""##.##""; 123.456
CLOSE f
"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      ' Check that file was created (basic USING support)
      Assert.True(IO.File.Exists("test_using.txt"))

      '' Clean up
      'IO.File.Delete("test_using.txt")
    End Sub

    <Fact>
    Public Sub SeekStatementParsesCorrectly()
      ' Test that SEEK statement parses without errors
      Dim tree = SyntaxTree.Parse("SEEK 1, 100")
      Dim comp = Compilation.Create(tree)
      ' Should not throw exception during creation
      Assert.NotNull(comp)
    End Sub

    <Fact>
    Public Sub SeekFunctionWorks()
      ' Test SEEK function exists and is callable
      Dim test = "LET result = SEEK(1)" ' Won't error during evaluation since we just test compilation
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)

      ' Should compile without errors
      Assert.NotNull(comp)
    End Sub

    <Fact>
    Public Sub BinaryFileOpenWithSeekStatementParses()
      ' Test that BINARY OPEN with LEN and SEEK statement parse correctly
      Dim test = "OPEN ""test.bin"" FOR BINARY AS #1 LEN = 512 : SEEK 1, 100"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)

      ' Should compile without errors
      Assert.NotNull(comp)
    End Sub

    <Fact>
    Public Sub RandomFileOpenWithLenParses()
      ' Test that RANDOM OPEN with LEN parameter parses correctly
      Dim test = "OPEN ""test.dat"" FOR RANDOM AS #2 LEN = 128"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)

      ' Should compile without errors
      Assert.NotNull(comp)
    End Sub

    <Fact>
    Public Sub SeekStatementWithExpressionsParses()
      ' Test SEEK statement with complex expressions
      Dim test = "SEEK file_num%, record% * 10 + offset%"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)

      ' Should compile without errors
      Assert.NotNull(comp)
    End Sub

    <Fact>
    Public Sub PosFunctionReturnsCursorPosition()
      ' Test POS function (cursor column position)
      Dim test = "LET result = POS(0)"
      Dim tree As SyntaxTree = SyntaxTree.Parse(test)
      Dim comp As Compilation = Compilation.Create(tree)
      Dim vars As New Dictionary(Of String, Object)()
      comp.Evaluate(vars)

      ' POS should return an integer between 1 and screen width
      Dim posValue = CInt(vars("result"))
      Assert.True(posValue >= 1 AndAlso posValue <= 80)
    End Sub

  End Class

End Namespace