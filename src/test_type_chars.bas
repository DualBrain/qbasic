Imports QB.CodeAnalysis.Syntax
Imports QB.CodeAnalysis.Text

Module Module1
    Sub Main()
        ' Test basic type character parsing
        TestLex("x%")
        TestLex("x!")
        TestLex("x#")
        TestLex("x$")
        TestLex("x&")
        
        ' Test that they work in expressions
        TestLex("LET x% = 123")
        TestLex("LET x! = 123.45")
        TestLex("LET x# = 123.45")
        TestLex("LET x$ = ""hello""")
        TestLex("LET x& = 123456")
    End Sub
    
    Sub TestLex(text As String)
        Console.WriteLine($"Testing: {text}")
        Dim syntaxTree = SyntaxTree.Parse(text)
        Console.WriteLine($"  Diagnostics: {syntaxTree.Diagnostics.Count}")
        If syntaxTree.Diagnostics.Count > 0 Then
            For Each diag In syntaxTree.Diagnostics
                Console.WriteLine($"    {diag}")
            Next
        End If
        Console.WriteLine()
    End Sub
End Module