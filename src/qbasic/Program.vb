Imports System.Drawing
Imports System.IO
Imports System.IO.Compression
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Runtime.InteropServices.RuntimeInformation
Imports System.Text

Imports Basic

Imports QB.CodeAnalysis.Syntax

Imports QBLib.Video

Imports VbPixelGameEngine

Friend Module Program

  Sub Main(args As String())
    Dim commandLineArgs As String() = Nothing
    If args.Length > 0 Then
      If Not HandleCommandLineArguments(args, commandLineArgs) Then
        ' Command line processing failed or showed help/error, exit
        Return
      End If
    Else
      ' No arguments, start GUI normally
      Dim demo As New QBasic
      If demo.Construct(640, 400, 1, 1) Then ', False, True) Then
        demo.ShowEngineName = False : demo.ShowIPS = False
        demo.Start()
      End If
    End If
  End Sub

  Private Function HandleCommandLineArguments(args As String(), ByRef commandLineArgs As String()) As Boolean
    Dim filename As String = Nothing
    Dim showSyntaxTree As Boolean = False
    Dim showMethods As Boolean = False
    Dim stdoutMode As Boolean = False
    Dim dumpGlobals As Boolean = False
    Dim showHelp As Boolean = False
    Dim roundtripMode As Boolean = False
    Dim upgradeGwBasicMode As Boolean = False
    Dim convertToVbNetMode As Boolean = False
    Dim programArgs As New List(Of String)()

    Dim fileArgIndex = -1

    ' First pass: find the .bas file and validate options
    For i = 0 To args.Length - 1
      Dim arg = args(i)
      If arg.StartsWith("--") OrElse arg.StartsWith("-") Then
        Select Case arg.ToLower()
          Case "--syntax-tree", "-t"
            showSyntaxTree = True
          Case "--methods", "-m"
            showMethods = True
          Case "--stdout", "-s"
            stdoutMode = True
          Case "--dump-globals", "-d"
            dumpGlobals = True
          Case "--help", "-h"
            showHelp = True
          Case "--roundtrip", "-r"
            roundtripMode = True
          Case "--upgrade-gwbasic", "-g"
            upgradeGwBasicMode = True
          Case "--convert-vbnet", "-v"
            convertToVbNetMode = True
          Case Else
            Console.WriteLine($"Unknown option: {arg}")
            ShowUsage()
            Return False
        End Select
      ElseIf arg.EndsWith(".bas", StringComparison.OrdinalIgnoreCase) Then
        If filename IsNot Nothing Then
          Console.WriteLine("Error: Multiple .bas files specified")
          ShowUsage()
          Return False
        End If
        filename = arg
        fileArgIndex = i
      Else
        ' Check if this could be a program argument (only allowed after .bas file)
        If filename Is Nothing Then
          Console.WriteLine($"Unknown argument: {arg}")
          ShowUsage()
          Return False
        End If
        ' This is a program argument that comes after the file
      End If
    Next

    ' Second pass: collect program arguments (everything after the .bas file)
    If fileArgIndex >= 0 Then
      For j = fileArgIndex + 1 To args.Length - 1
        ' Skip options that might come after the file
        Dim arg = args(j)
        If Not ((arg.StartsWith("--") OrElse arg.StartsWith("-")) AndAlso
                 (arg.ToLower() = "--syntax-tree" OrElse arg.ToLower() = "-t" OrElse
                  arg.ToLower() = "--methods" OrElse arg.ToLower() = "-m" OrElse
                  arg.ToLower() = "--stdout" OrElse arg.ToLower() = "-s" OrElse
                  arg.ToLower() = "--dump-globals" OrElse arg.ToLower() = "-d" OrElse
                  arg.ToLower() = "--help" OrElse arg.ToLower() = "-h" OrElse
                  arg.ToLower() = "--roundtrip" OrElse arg.ToLower() = "-r" OrElse
                  arg.ToLower() = "--upgrade-gwbasic" OrElse arg.ToLower() = "-g" OrElse
                  arg.ToLower() = "--convert-vbnet" OrElse arg.ToLower() = "-v")) Then
          programArgs.Add(arg)
        End If
      Next
    End If

    commandLineArgs = programArgs.ToArray()

    If showHelp Then
      ShowUsage()
      Return False
    End If

    If showSyntaxTree Or showMethods Then
      If filename Is Nothing Then
        Console.WriteLine("Error: --syntax-tree or --methods requires a .bas file")
        ShowUsage()
        Return False
      End If
      HandleAnalysisMode(filename, showSyntaxTree, showMethods)
      Return False ' Exit after showing analysis
    ElseIf filename IsNot Nothing AndAlso roundtripMode Then
      HandleRoundtripMode(filename)
      Return False ' Exit after roundtrip
    ElseIf filename IsNot Nothing AndAlso stdoutMode Then
      HandleRunMode(filename, stdoutMode, dumpGlobals, commandLineArgs)
      Return False ' Exit after running program
    ElseIf filename IsNot Nothing AndAlso upgradeGwBasicMode Then
      HandleUpgradeGwBasicMode(filename)
      Return False ' Exit after upgrading GW-BASIC
    ElseIf filename IsNot Nothing AndAlso convertToVbNetMode Then
      HandleConvertToVbNetMode(filename)
      Return False ' Exit after converting to VB.NET
    ElseIf filename IsNot Nothing Then
      ' Load file into IDE
      Dim demo As New QBasic(filename)
      If demo.Construct(640, 400, 1, 1) Then ', False, True) Then
        demo.ShowEngineName = False : demo.ShowIPS = False
        demo.Start()
      End If
      Return False
    Else
      Console.WriteLine("Error: No .bas file specified")
      ShowUsage()
      Return False
    End If
  End Function

  Private Sub ShowUsage()
    Console.WriteLine("QBasic Interpreter")
    Console.WriteLine()
    Console.WriteLine("Usage:")
    Console.WriteLine("  qbasic                         Start GUI IDE")
    Console.WriteLine("  qbasic <filename.bas>          Load file into GUI IDE")
    Console.WriteLine("  qbasic <filename.bas> --stdout Run program in console")
    Console.WriteLine("  qbasic <filename.bas> --roundtrip Test syntax tree roundtrip")
    Console.WriteLine("  qbasic <filename.bas> --upgrade-gwbasic Upgrade GW-BASIC to QBasic")
    Console.WriteLine("  qbasic <filename.bas> --convert-vbnet Convert QBasic to VB.NET")
    Console.WriteLine("  qbasic <filename.bas> [options] Analyze QBasic program")
    Console.WriteLine()
    Console.WriteLine("Options:")
    Console.WriteLine("  --syntax-tree, -t    Display syntax tree")
    Console.WriteLine("  --methods, -m        Display method definitions (SUB, FUNCTION, DEF FN)")
    Console.WriteLine("  --stdout, -s         Run program and output to console instead of GUI")
    Console.WriteLine("  --roundtrip, -r       Test syntax tree roundtrip fidelity")
    Console.WriteLine("  --upgrade-gwbasic, -g  Upgrade GW-BASIC line numbers to QBasic")
    Console.WriteLine("  --convert-vbnet, -v   Convert QBasic to VB.NET code")
    Console.WriteLine("  --help, -h           Show this help message")
    Console.WriteLine()
    Console.WriteLine("Examples:")
    Console.WriteLine("  qbasic program.bas")
    Console.WriteLine("  qbasic program.bas --syntax-tree")
    Console.WriteLine("  qbasic program.bas --methods")
    Console.WriteLine("  qbasic program.bas --roundtrip")
    Console.WriteLine("  qbasic --help")
  End Sub

  Private Sub HandleAnalysisMode(filename As String, showSyntaxTree As Boolean, showMethods As Boolean)
    Try
      Dim sourceText = File.ReadAllText(filename)
      Dim syntaxTree As QB.CodeAnalysis.Syntax.SyntaxTree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(sourceText)

      If showSyntaxTree Then
        Console.WriteLine("Syntax Tree:")
        Console.WriteLine(syntaxTree.Root.ToString())
      End If

      If showMethods Then
        Console.WriteLine("Methods (SUB, FUNCTION, DEF FN):")
        FindAndDisplayMethods(syntaxTree.Root)
      End If

    Catch ex As Exception
      Console.WriteLine($"Error: {ex.Message}")
    End Try
  End Sub

  Private Sub FindAndDisplayMethods(node As SyntaxNode)
    ' Check if this node itself is a method definition
    If TypeOf node Is FunctionDeclarationSyntax Then
      Dim func = CType(node, FunctionDeclarationSyntax)
      Console.WriteLine($"FUNCTION {func.Identifier.Text}")
    ElseIf TypeOf node Is FunctionStatementSyntax Then
      Dim funcStmt = CType(node, FunctionStatementSyntax)
      Console.WriteLine($"FUNCTION {funcStmt.Identifier.Text}")
    ElseIf TypeOf node Is DefDeclarationSyntax Then
      Console.WriteLine("DEF FN")
    ElseIf TypeOf node Is SingleLineDefDeclarationSyntax Then
      Console.WriteLine("DEF FN")
    ElseIf TypeOf node Is SubStatementSyntax Then
      Dim subStmt = CType(node, SubStatementSyntax)
      Console.WriteLine($"SUB {subStmt.Identifier.Text}")
    End If

    ' Recursively search child nodes
    For Each child In node.GetChildren()
      FindAndDisplayMethods(child)
    Next
  End Sub

  Private Sub HandleRoundtripMode(filename As String)
    Try
      Dim originalText = File.ReadAllText(filename)
      Dim syntaxTree As QB.CodeAnalysis.Syntax.SyntaxTree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(originalText)

      Console.WriteLine("Original source:")
      Console.WriteLine(originalText)
      Console.WriteLine()

      Console.WriteLine("Roundtripped source:")
      Dim roundtrippedText = WriteSyntaxTreeToText(syntaxTree.Root)
      Console.WriteLine(roundtrippedText)
      Console.WriteLine()

      ' Compare original and roundtripped text
      If originalText = roundtrippedText Then
        Console.WriteLine("✓ Roundtrip successful: No differences detected")
      Else
        Console.WriteLine("✗ Roundtrip failed: Differences detected")
        Console.WriteLine()
        Console.WriteLine("Differences (first 500 chars):")
        Dim maxLength = Math.Min(500, originalText.Length)
        For i = 0 To maxLength - 1
          If i < roundtrippedText.Length AndAlso originalText(i) <> roundtrippedText(i) Then
            Console.WriteLine($"Position {i}: Original '{originalText(i)}' ≠ Roundtripped '{If(i < roundtrippedText.Length, roundtrippedText(i).ToString(), "MISSING")}'")
            Exit For
          ElseIf i >= roundtrippedText.Length Then
            Console.WriteLine($"Position {i}: Original '{originalText(i)}' ≠ Roundtripped MISSING")
            Exit For
          End If
        Next

        ' Show length differences
        If originalText.Length <> roundtrippedText.Length Then
          Console.WriteLine()
          Console.WriteLine($"Length difference: Original {originalText.Length} chars, Roundtripped {roundtrippedText.Length} chars")
        End If

        Console.WriteLine()
        Console.WriteLine("Note: QBasic keywords are case-insensitive and may be normalized to uppercase during parsing.")
        Console.WriteLine("This is expected behavior and doesn't affect program functionality.")
      End If

    Catch ex As Exception
      Console.WriteLine($"Error during roundtrip: {ex.Message}")
    End Try
  End Sub

  Private Function WriteSyntaxTreeToText(node As SyntaxNode) As String
    Using writer = New System.IO.StringWriter()
      WriteNodeWithTrivia(writer, node)
      Return writer.ToString()
    End Using
  End Function

  Private Sub WriteNodeWithTrivia(writer As System.IO.TextWriter, node As SyntaxNode)
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

  Private Sub HandleRunMode(filename As String, stdoutMode As Boolean, dumpGlobals As Boolean, commandLineArgs As String())
    Dim interpreter As QB.Interpreter = Nothing
    Try
      Dim sourceText = File.ReadAllText(filename)
      If Not stdoutMode Then
        Console.WriteLine($"Running program: {filename}")
        ' Initialize graphics for GUI mode
        QBLib.Video.ScreenInit()
      End If

      ' Set stdout mode
      QBLib.Video.StdoutMode = stdoutMode

      ' Create interpreter and run the program
      interpreter = New QB.Interpreter()
      interpreter.Run(sourceText, dumpGlobals, commandLineArgs)

    Catch ex As Exception
      Console.WriteLine($"Error: {ex.Message}")
      If dumpGlobals AndAlso interpreter IsNot Nothing Then
        Console.WriteLine("Global variables:")
        For Each kv In interpreter.Variables
          Console.WriteLine($"{kv.Key} = {kv.Value}")
        Next
      End If
    End Try
  End Sub

  Private Sub HandleUpgradeGwBasicMode(filename As String)
    Try
      Console.WriteLine($"Upgrading GW-BASIC file: {filename}")
      
      Dim sourceText = File.ReadAllText(filename)
      Dim syntaxTree As QB.CodeAnalysis.Syntax.SyntaxTree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(sourceText)
      
      Dim rewriter = New GwBasicToQBasicRewriter()
      Dim rewrittenTree = rewriter.Rewrite(syntaxTree.Root)
      
      ' Generate transformed code using text transformation (this updates the analysis)
      Dim originalCode = File.ReadAllText(filename)
      Dim transformedCode = rewriter.GenerateUpgradedCode(originalCode)
      
      ' Generate suggestions based on analysis
      rewriter.GenerateSuggestions()
      
      Console.WriteLine($"GW-BASIC to QBasic Analysis:")
      Console.WriteLine($"  Line numbers found: {rewriter.Analysis.LineNumbersRemoved}")
      Console.WriteLine($"  GOTO statements found: {rewriter.Analysis.GotoStatementsFound}")
      Console.WriteLine($"  GOSUB statements found: {rewriter.Analysis.GosubStatementsFound}")
      Console.WriteLine($"  Labels created: {rewriter.Analysis.LabelsCreated}")
      
      If rewriter.Analysis.Suggestions.Any() Then
        Console.WriteLine()
        Console.WriteLine("Suggestions:")
        For Each suggestion In rewriter.Analysis.Suggestions
          Console.WriteLine($"  - {suggestion}")
        Next
      End If
      
      If rewriter.Analysis.Warnings.Any() Then
        Console.WriteLine()
        Console.WriteLine("Warnings:")
        For Each warning In rewriter.Analysis.Warnings
          Console.WriteLine($"  - {warning}")
        Next
      End If
      
      ' Create output filename
      Dim outputFilename = Path.ChangeExtension(filename, ".upgraded.bas")
      File.WriteAllText(outputFilename, transformedCode)

      Console.WriteLine()
      Console.WriteLine($"Upgraded file saved as: {outputFilename}")
      
    Catch ex As Exception
      Console.WriteLine($"Error upgrading GW-BASIC file: {ex.Message}")
    End Try
  End Sub

  Private Sub HandleConvertToVbNetMode(filename As String)
    Try
      Console.WriteLine($"Converting QBasic to VB.NET: {filename}")
      
      Dim sourceText = File.ReadAllText(filename)
      Dim syntaxTree As QB.CodeAnalysis.Syntax.SyntaxTree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(sourceText)
      
      Dim options = New QBasicToVbNetRewriter.ConversionOptions()
      options.AddModuleBoilerplate = True
      options.AddSubMain = True
      options.ModernizePrint = True
      options.AddImports = True
      options.GenerateWarnings = True
      
      Dim rewriter = New QBasicToVbNetRewriter(options)
      Dim rewrittenTree = rewriter.Rewrite(syntaxTree.Root)
      
      ' Generate VB.NET code
      Using writer = New StringWriter()
        WriteNodeWithTrivia(writer, rewrittenTree)
        Dim qbasicCode = writer.ToString()
        
        ' Apply VB.NET transformations
        Dim vbNetCode = rewriter.GenerateVbNetCode(qbasicCode)
        
        ' Create output filename
        Dim outputFilename = Path.ChangeExtension(filename, ".vb")
        File.WriteAllText(outputFilename, vbNetCode)
        
        Console.WriteLine($"VB.NET Conversion Analysis:")
        Console.WriteLine($"  Added Module wrapper: {If(rewriter.Analysis.AddedModule, "Yes", "No")}")
        Console.WriteLine($"  Added SUB MAIN: {If(rewriter.Analysis.AddedSubMain, "Yes", "No")}")
        Console.WriteLine($"  PRINT statements modernized: {rewriter.Analysis.ModernizedPrintStatements}")
        Console.WriteLine($"  Variables needing types: {rewriter.Analysis.VariablesRequiringTypes}")
        
        If rewriter.GetRequiredImports().Any() Then
          Console.WriteLine()
          Console.WriteLine("Required Imports:")
          For Each import In rewriter.GetRequiredImports()
            Console.WriteLine($"  - {import}")
          Next
        End If
        
        If rewriter.Analysis.Warnings.Any() Then
          Console.WriteLine()
          Console.WriteLine("Warnings:")
          For Each warning In rewriter.Analysis.Warnings
            Console.WriteLine($"  - {warning}")
          Next
        End If
        
        If rewriter.Analysis.ChangesMade.Any() Then
          Console.WriteLine()
          Console.WriteLine("Changes Made:")
          For Each change In rewriter.Analysis.ChangesMade
            Console.WriteLine($"  - {change}")
          Next
        End If
        
        Console.WriteLine()
        Console.WriteLine($"VB.NET file saved as: {outputFilename}")
      End Using
      
    Catch ex As Exception
      Console.WriteLine($"Error converting to VB.NET: {ex.Message}")
    End Try
  End Sub

End Module

Friend Class QBasic
  Inherits PixelGameEngine

  Private ReadOnly m_initialFile As String

  'Private m_selected As Integer
  Private m_result As DialogResult = DialogResult.None
  Private m_context As IContext

  Private WithEvents Menu As MainMenu
  Private ReadOnly m_help As New HelpPanel
  Private WithEvents Document1 As New DocumentPanel
  Private WithEvents Document2 As New DocumentPanel
  Private ReadOnly m_immediate As New ImmediatePanel
  Private ReadOnly m_statusPanel As New StatusPanel

  Private m_t As Single

  'Private WithEvents InterpreterTimer As New Timer(1)
  'Private WithEvents DisplayTimer As New Timer(1)

  'Private WithEvents Interpreter As Interactive
  'Private ReadOnly Display As New Display(DisplayTimer)

  Private ReadOnly m_pathspec As String
  Private m_path As String

  Private m_runner As Threading.Thread

  Private m_screenMode As Integer
  Private m_scrn() As Integer
  Private m_scrn0() As UShort
  Private m_buffer() As Pixel
  Private m_fg As Integer
  Private m_bg As Integer
  Private m_cr As Integer
  Private m_cc As Integer

  Friend Sub New(Optional initialFile As String = Nothing)
    AppName = "Community QBasic"
    m_initialFile = initialFile
    m_pathspec = System.IO.Path.Combine(System.Environment.CurrentDirectory(), If(IsOSPlatform(OSPlatform.Windows), "*.BAS", "*.*"))
  End Sub

  Private Sub LoadFile(path As String)
    m_path = path
    If System.IO.File.Exists(m_path) Then

      Document1.Clear()
      Document2.Clear()

      Dim code = System.IO.File.ReadAllText(m_path)
      Document1.Text = code
      Document1.Title = System.IO.Path.GetFileName(m_path)

      ' B#
      Dim tree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(code)
      Document2.Text = tree.Root.ToString
      Document2.Title = "Tree"

      ' GW - commented out as p is not initialized
      ' Dim p As Basic.Parser.Parser
      ' Using s As New MemoryStream()
      '   Dim buffer() = code.ToByteArray
      '   s.Write(buffer, 0, buffer.Length)
      '   s.Seek(0, SeekOrigin.Begin)
      ' End Using

      ' 'm_subs.Clear()
      ' For Each line1 In p.Lines
      '   For Each statement In line1.Statements
      '     Dim token = If(statement?.Tokens?.Count > 0, statement.Tokens(0), Nothing)
      '     If token IsNot Nothing Then
      '       Select Case token.Keyword
      '         Case "END"
      '           Dim nextToken = If(statement?.Tokens?.Count > 1, statement.Tokens(1), Nothing)
      '           If nextToken Is Nothing Then
      '           ElseIf nextToken?.Keyword = "FUNCTION" Then
      '           ElseIf nextToken?.Keyword = "IF" Then
      '           ElseIf nextToken?.Keyword = "SELECT" Then
      '           ElseIf nextToken?.Keyword = "SUB" Then
      '           ElseIf nextToken?.Keyword = "TYPE" Then
      '           Else
      '             Stop
      '           End If
      '         Case "BEEP"
      '         Case "CALL", "CIRCLE", "CLS", "COLOR"
      '         Case "DEFINT", "DEF", "DIM"
      '         Case "ELSE", "END"
      '         Case "FOR"
      '         Case "GET", "GOSUB"
      '         Case "IF", "INPUT"
      '         Case "LET", "LINE", "LOCATE"
      '         Case "NEXT"
      '         Case "ON"
      '         Case "PAINT", "PALETTE", "PLAY", "POKE", "PRINT", "PSET", "PUT"
      '         Case "RANDOMIZE", "READ", "REM", "RESTORE", "RESUME", "RETURN"
      '         Case "SCREEN"
      '         Case "VIEW"
      '         Case "WEND", "WHILE", "WIDTH"
      '         Case "SUB", "FUNCTION"
      '           token = If(statement?.Tokens?.Count > 1, statement.Tokens(1), Nothing)

      '         Case Else
      '           Debug.WriteLine(token.Keyword)
      '       End Select
      '     End If
      '     Exit For
      '   Next
      ' Next

    End If
  End Sub

  <DllImport("user32.dll", SetLastError:=True, CharSet:=CharSet.Auto)>
  Private Shared Function SendMessage(hWnd As IntPtr, Msg As UInteger, wParam As UInteger, lParam As IntPtr) As IntPtr
  End Function

  <DllImport("user32.dll")>
  Private Shared Function SetWindowLongPtr(hWnd As IntPtr, nIndex As Integer, dwNewLong As IntPtr) As IntPtr
  End Function

  <DllImport("user32.dll")>
  Private Shared Function GetWindowLongPtr(hWnd As IntPtr, nIndex As Integer) As IntPtr
  End Function

  <DllImport("user32.dll")>
  Private Shared Function CallWindowProc(lpPrevWndFunc As IntPtr, hWnd As IntPtr, msg As UInteger, wParam As IntPtr, lParam As IntPtr) As IntPtr
  End Function

  <DllImport("user32.dll")>
  Private Shared Function SetCursor(hCursor As IntPtr) As IntPtr
  End Function

  <DllImport("user32.dll")>
  Private Shared Function GetCursorPos(ByRef lpPoint As POINT) As Boolean
  End Function

  <DllImport("user32.dll")>
  Private Shared Function ScreenToClient(hWnd As IntPtr, ByRef lpPoint As POINT) As Boolean
  End Function

  <StructLayout(LayoutKind.Sequential)>
  Private Structure POINT
    Public x As Integer
    Public y As Integer
  End Structure

  <DllImport("libX11.so")>
  Private Shared Function XOpenDisplay(display_name As String) As IntPtr
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XDefineCursor(display As IntPtr, w As IntPtr, cursor As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XRootWindow(display As IntPtr, screen_number As Integer) As IntPtr
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XCreatePixmap(display As IntPtr, d As IntPtr, width As UInteger, height As UInteger, depth As UInteger) As IntPtr
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XCreatePixmapCursor(display As IntPtr, source As IntPtr, mask As IntPtr, ByRef foreground_color As XColor, ByRef background_color As XColor, x As UInteger, y As UInteger) As IntPtr
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XFreePixmap(display As IntPtr, pixmap As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XFreeCursor(display As IntPtr, cursor As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XDefaultScreen(display As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XFlush(display As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XDefaultRootWindow(display As IntPtr) As IntPtr
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XQueryTree(display As IntPtr, w As IntPtr, ByRef root_return As IntPtr, ByRef parent_return As IntPtr, ByRef children_return As IntPtr, ByRef nchildren_return As UInteger) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XFetchName(display As IntPtr, w As IntPtr, ByRef window_name_return As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XFree(data As IntPtr) As Integer
  End Function

  <DllImport("libX11.so")>
  Private Shared Function XGetWMName(display As IntPtr, w As IntPtr, ByRef text_prop_return As XTextProperty) As Integer
  End Function

  <StructLayout(LayoutKind.Sequential)>
  Private Structure XTextProperty
    Public value As IntPtr
    Public encoding As IntPtr
    Public format As Integer
    Public nitems As ULong
  End Structure

  <StructLayout(LayoutKind.Sequential)>
  Private Structure XColor
    Public pixel As ULong
    Public red As UShort
    Public green As UShort
    Public blue As UShort
    Public flags As SByte
    Public pad As SByte
  End Structure

  Private Function FindWindowByName(display As IntPtr, window As IntPtr, name As String) As IntPtr
    Dim textProp As XTextProperty
    If XGetWMName(display, window, textProp) <> 0 AndAlso textProp.value <> IntPtr.Zero Then
      Dim windowName = Marshal.PtrToStringAnsi(textProp.value)
      XFree(textProp.value)
      If windowName IsNot Nothing AndAlso windowName.Contains("QBasic") Then
        Return window
      End If
    End If
    ' Recurse on children
    Dim dummy As IntPtr
    Dim children As IntPtr
    Dim nchildren As UInteger
    XQueryTree(display, window, dummy, dummy, children, nchildren)
    If children <> IntPtr.Zero Then
      For i = 0 To nchildren - 1
        Dim child = Marshal.ReadIntPtr(New IntPtr(children.ToInt64() + CLng(i) * Marshal.SizeOf(GetType(IntPtr))))
        Dim found = FindWindowByName(display, child, name)
        If found <> IntPtr.Zero Then
          XFree(children)
          Return found
        End If
      Next
      XFree(children)
    End If
    Return IntPtr.Zero
  End Function

  Private Delegate Function WndProcDelegate(hWnd As IntPtr, msg As UInteger, wParam As IntPtr, lParam As IntPtr) As IntPtr

  Private m_originalWndProc As IntPtr
  Private m_newWndProcDelegate As WndProcDelegate
  Private m_invisibleCursor As IntPtr
  Private m_display As IntPtr
  Private m_pixmap As IntPtr
  Private m_x11Window As IntPtr
  Private m_frameCount As Integer

  Protected Overrides Function OnUserCreate() As Boolean

    Dim w As Integer, h As Integer
    Dim result = GetScreenSize()
    w = result.w : h = result.h

    Select Case h
      Case <= 1080
      Case <= 1440
        IncreasePixelSize()
      Case Else
        IncreasePixelSize()
        IncreasePixelSize()
    End Select

    If OperatingSystem.IsWindowsVersionAtLeast(7) Then
      Dim assem = Assembly.GetExecutingAssembly
      Dim ico = Icon.ExtractAssociatedIcon(assem.Location)
      Const WM_SETICON As UInteger = &H80
      Const ICON_SMALL As UInteger = 0
      Const ICON_BIG As UInteger = 1
      Dim unused = SendMessage(m_hWnd, WM_SETICON, ICON_SMALL, ico.Handle)
      unused = SendMessage(m_hWnd, WM_SETICON, ICON_BIG, ico.Handle)
    End If

    ScreenInit()

    Menu = New MainMenu With {
      .Items = {New MenuItem("&File") With {
        .Items = {New MenuItem("&New", "Removes currently loaded file from memory"),
                  New MenuItem("&Open...", "Loads new file into memory"),
                  New MenuItem("&Save", "Saves current file"),
                  New MenuItem("Save &As...", "Saves current file with specified name"),
                  New MenuItem("-"),
                  New MenuItem("&Print...", "Prints specified text"),
                  New MenuItem("-"),
                  New MenuItem("E&xit", "Exits editor and returns to DOS")}.ToList},
        New MenuItem("&Edit") With {
        .Items = {New MenuItem("Cu&t", "Deletes selected text and copies it to buffer") With {.Hotkey = "Shift+Del", .Enabled = False},
                  New MenuItem("&Copy", "Copies selected text to buffer") With {.Hotkey = "Ctrl+Ins", .Enabled = False},
                  New MenuItem("&Paste", "Inserts buffer contents at current location") With {.Hotkey = "Shift+Ins", .Enabled = False},
                  New MenuItem("Cl&ear", "Deletes selected text without copying it to buffer") With {.Hotkey = "Del", .Enabled = False},
                  New MenuItem("-"),
                  New MenuItem("New &SUB...", "Opens a window for a new subprogram"),
                  New MenuItem("New &FUNCTION...", "Opens a window for a new FUNCTION procedure")}.ToList},
        New MenuItem("&View") With {
        .Items = {New MenuItem("&SUBs...", "Displays a loaded SUB or FUNCTION") With {.Hotkey = "F2"},
                  New MenuItem("S&plit", "Divides screen into two View Windows"),
                  New MenuItem("&Output Screen", "Displays output screen") With {.Hotkey = "F4"}}.ToList},
        New MenuItem("&Search") With {
        .Items = {New MenuItem("&Find...", "Finds specified text"),
                  New MenuItem("&Repeat Last Find", "Finds next occurrence of text specified in previous search") With {.Hotkey = "F3"},
                  New MenuItem("&Change...", "Finds and changes specified text")}.ToList},
        New MenuItem("&Run") With {
        .Items = {New MenuItem("&Start", "Runs current program") With {.Hotkey = "Shift+F5"},
                  New MenuItem("&Restart", "Clears variables in preparation for restarting single stepping"),
                  New MenuItem("&Continue", "Continues execution after a break") With {.Hotkey = "F5"}}.ToList},
        New MenuItem("&Debug") With {
        .Items = {New MenuItem("&Step", "Executes next program") With {.Hotkey = "F8"},
                  New MenuItem("&Procedure Step", "Executes next program statement, tracing over procedure calls") With {.Hotkey = "F10"},
                  New MenuItem("-"),
                  New MenuItem("&Trace On", "Highlights statement currently executing"),
                  New MenuItem("-"),
                  New MenuItem("Toggle &Breakpoint", "Sets/clears breakpoint at cursor location") With {.Hotkey = "F9"},
                  New MenuItem("&Clear All Breakpoints", "Removes all breakoints"),
                  New MenuItem("Set &Next Statement", "Makes the statement at the cursor the next statement to execute") With {.Enabled = False}}.ToList},
        New MenuItem("&Options") With {
        .Items = {New MenuItem("&Display...", "Changes display attributes"),
                  New MenuItem("Help &Path...", "Sets search path for Help files"),
                  New MenuItem("&Syntax Checking", "Turns editor's syntax checking on or off.") With {.Checked = True}}.ToList},
        New MenuItem("&Help") With {
        .Items = {New MenuItem("&Index", "Displays help index"),
                  New MenuItem("&Contents", "Displays help table of contents"),
                  New MenuItem("Topic:", "Displays information about the Basic keyword the cursor is on") With {.Enabled = False, .Hotkey = "F1"},
                  New MenuItem("Using &Help", "Displays information about how to use online help") With {.Hotkey = "Shift+F1"},
                  New MenuItem("-"),
                  New MenuItem("&About...", "Displays product version and copyright information")}.ToList}}.ToList}

    Dim basePath = Path.GetDirectoryName(Assembly.GetEntryAssembly.Location)

    Dim includeTrs80gp = File.Exists(System.IO.Path.Combine(basePath, "extras\trs80gp.exe"))
    Dim includeQbjs = True

    Dim fbcPath As String = Nothing

    If File.Exists(System.IO.Path.Combine(basePath, "extras\fbc64.exe")) Then
      fbcPath = basePath
    End If
    If fbcPath Is Nothing Then
      Dim paths = System.Environment.GetEnvironmentVariable("PATH")
      Dim pathList = paths.Split(";"c)
      For Each path In pathList
        If fbcPath Is Nothing Then
          Dim testPath = System.IO.Path.Combine(path, "fbc64.exe")
          If System.IO.File.Exists(testPath) Then
            fbcPath = testPath
          End If
        End If
      Next
    End If

    Dim includeFbc = fbcPath IsNot Nothing

    If includeQbjs OrElse includeFbc OrElse includeTrs80gp Then
      Menu.Items(4).Items.Add(New MenuItem("-"))
    End If
    If includeTrs80gp Then
      Menu.Items(4).Items.Add(New MenuItem("Launch w/ trs80gp (&Model IV)...", "Launches in trs80gp in M4 mode."))
      Menu.Items(4).Items.Add(New MenuItem("Launch w/ trs80gp (&Coco)...", "Launches in trs80gp in Coco mode."))
    End If
    If includeFbc Then
      Menu.Items(4).Items.Add(New MenuItem("Make E&XE File (FBC)...", "Creates executable file on disk"))
    End If
    If includeQbjs Then
      Menu.Items(4).Items.Add(New MenuItem("In &Web browser (QBJS)...", "Launch using QBJS in default web browser"))
    End If

    Menu.CalculateOffsets()

    Document1.Focused = True
    Document2.Visible = False

    m_context = New WelcomeDialog()

    DrawScreen()

    If m_initialFile IsNot Nothing Then
      LoadFile(m_initialFile)
      DrawScreen()
    End If

    'InterpreterTimer.Enabled = True

    'Dim env As IEnvironment = Nothing
    'Dim fs As IVirtualFileSystem = New WindowsFileSystem
    'Dim snd As ISound = Nothing
    'Dim kbd As IKeyboard = Nothing
    'Dim gpio As IGpio = Nothing

    'Interpreter = New Interactive(Dialect.QBasic,
    '                              env,
    '                              Display,
    '                              InterpreterTimer,
    '                              fs,
    '                              snd,
    '                              kbd,
    '                              gpio,
    '                              "QBasic",
    '                              1000,
    '                              "en-US")

    If OperatingSystem.IsWindows() Then
      Const GWLP_WNDPROC As Integer = -4
      m_newWndProcDelegate = AddressOf NewWndProc
      m_originalWndProc = GetWindowLongPtr(m_hWnd, GWLP_WNDPROC)
      SetWindowLongPtr(m_hWnd, GWLP_WNDPROC, Marshal.GetFunctionPointerForDelegate(m_newWndProcDelegate))
    End If

    Return True

  End Function

  Private Function NewWndProc(hWnd As IntPtr, msg As UInteger, wParam As IntPtr, lParam As IntPtr) As IntPtr
    Const WM_SETCURSOR As UInteger = &H20
    Const HTCLIENT As Integer = 1
    If msg = WM_SETCURSOR Then
      If (lParam.ToInt32() And &HFFFF) = HTCLIENT Then
        Dim pt As POINT
        GetCursorPos(pt)
        ScreenToClient(hWnd, pt)
        Dim mr = (pt.y \ m_textH) + 1
        Dim overContent = False
        If Document1.Visible AndAlso mr >= Document1.EditorTop AndAlso mr <= Document1.EditorTop + Document1.EditorHeight - 1 Then overContent = True
        If Document2.Visible AndAlso mr >= Document2.EditorTop AndAlso mr <= Document2.EditorTop + Document2.EditorHeight - 1 Then overContent = True
        If m_immediate.Visible AndAlso mr >= m_immediate.EditorTop AndAlso mr <= m_immediate.EditorTop + m_immediate.EditorHeight - 1 Then overContent = True
        If m_help.Visible AndAlso mr >= m_help.EditorTop AndAlso mr <= m_help.EditorTop + m_help.EditorHeight - 1 Then overContent = True
        If overContent Then
          SetCursor(IntPtr.Zero)
          Return New IntPtr(1)
        End If
      End If
    End If
    Return CallWindowProc(m_originalWndProc, hWnd, msg, wParam, lParam)
  End Function

  Private Sub DrawScreen()

    m_help.Render()
    Document1.Render()
    Document2.Render()
    m_immediate.Render()

    If Menu.Focused AndAlso Menu.Expanded Then
      m_statusPanel.Dialog = False
      m_statusPanel.Text = $"F1=Help {ChrW(179)} {If(Menu.Items(Menu.Selected).Items(Menu.SubSelected).Help, "Help not defined")}"
    ElseIf m_context IsNot Nothing Then
      m_statusPanel.Dialog = True
      m_statusPanel.Text = "F1=Help   Enter=Execute   Esc=Cancel   Tab=Next Field   Arrow=Next Item"
    Else
      m_statusPanel.Dialog = False
      m_statusPanel.Text = "<Shift+F1=Help> <F6=Window> <F2=Subs> <F5=Run> <F8=Step>"
    End If

    If m_help.Focused Then
      m_statusPanel.DocumentRow = m_help.DocumentRow
      m_statusPanel.DocumentCol = m_help.DocumentCol
    ElseIf Document1.Focused Then
      m_statusPanel.DocumentRow = Document1.DocumentRow
      m_statusPanel.DocumentCol = Document1.DocumentCol
    ElseIf Document2.Focused Then
      m_statusPanel.DocumentRow = Document2.DocumentRow
      m_statusPanel.DocumentCol = Document2.DocumentCol
    ElseIf m_immediate.Focused Then
      m_statusPanel.DocumentRow = m_immediate.DocumentRow
      m_statusPanel.DocumentCol = m_immediate.DocumentCol
    End If
    m_statusPanel.Render()

    Menu.Render()

    m_context?.Render()

  End Sub

  Private m_exit As Boolean
  Private m_isAlt As Boolean
  Private m_isK As Boolean
  Private m_isQ As Boolean
  Private m_isCtrlP As Boolean

  Private ReadOnly m_subs As New List(Of String)

  'Private ReadOnly m_keyDecay As New Dictionary(Of ConsoleKey, Date)

  Protected Overrides Function OnUserUpdate(elapsedTime As Single) As Boolean

    m_frameCount += 1
    If OperatingSystem.IsLinux() AndAlso m_frameCount = 10 AndAlso m_x11Window = IntPtr.Zero Then
      m_display = XOpenDisplay(Nothing)
      If m_display <> IntPtr.Zero Then
        Dim root As IntPtr = XDefaultRootWindow(m_display)
        m_x11Window = FindWindowByName(m_display, root, "QBasic")
        Console.WriteLine($"X11 window found: {m_x11Window <> IntPtr.Zero}")
        If m_x11Window <> IntPtr.Zero Then
          Dim screen = XDefaultScreen(m_display)
          Dim root2 = XRootWindow(m_display, screen)
          m_pixmap = XCreatePixmap(m_display, root2, 1, 1, 1)
          Dim color As XColor
          m_invisibleCursor = XCreatePixmapCursor(m_display, m_pixmap, m_pixmap, color, color, 0, 0)
          XDefineCursor(m_display, m_x11Window, m_invisibleCursor)
          XFlush(m_display)
        End If
      End If
    End If

    m_t += elapsedTime

    Dim cursorVisible = True

    Dim keys = GetPressed()
    'For Each key In m_keyDecay.Keys
    '  If m_keyDecay(key) < Now Then
    '    m_keyDecay.Remove(key)
    '  End If
    'Next
    'If keys IsNot Nothing Then
    '  Dim filtered = New List(Of ConsoleKey)
    '  For index = keys.Count - 1 To 0 Step -1
    '    If filtered.Contains(keys(index)) Then
    '      keys.RemoveAt(index)
    '    Else
    '      If m_keyDecay.ContainsKey(keys(index)) Then
    '        keys.RemoveAt(index)
    '      Else
    '        m_keyDecay.Add(keys(index), Now.AddMilliseconds(100))
    '        filtered.Add(keys(index))
    '      End If
    '    End If
    '  Next
    'End If
    Dim mButton1 = GetMouse(0)
    Dim mButton2 = GetMouse(1)
    Dim mButton3 = GetMouse(2)
    Dim mButton4 = GetMouse(3)
    Dim mMouseX = GetMouseX()
    Dim mMouseY = GetMouseY()
    Dim mButton = mButton1.Pressed
    Dim mr = (mMouseY \ m_textH) + 1
    Dim mc = (mMouseX \ m_textW) + 1

    Dim isAlt = GetKey(Key.ALT).Held OrElse GetKey(Key.ALT).Pressed
    Dim isShift = GetKey(Key.SHIFT).Held OrElse GetKey(Key.SHIFT).Pressed
    Dim isControl = GetKey(Key.CTRL).Held OrElse GetKey(Key.CTRL).Pressed

    ' These keys are "always active".
    If keys IsNot Nothing Then
      For index = keys.Count - 1 To 0 Step -1
        If Not isControl AndAlso Not isAlt AndAlso Not isShift Then
          Select Case keys(index)
            Case ConsoleKey.Insert
              ToggleInsertMode()
              keys.RemoveAt(index)
            Case ConsoleKey.F11
              ToggleFullScreen()
              If Not IsFullScreen Then
                ' Whenever we restore the window, will need to "reattach" the icon in the title bar.
                If OperatingSystem.IsWindowsVersionAtLeast(7) Then
                  Dim assem = Assembly.GetExecutingAssembly
                  Dim ico = Icon.ExtractAssociatedIcon(assem.Location)
                  Const WM_SETICON As UInteger = &H80
                  Const ICON_SMALL As UInteger = 0
                  Const ICON_BIG As UInteger = 1
                  Dim unused = SendMessage(m_hWnd, WM_SETICON, ICON_SMALL, ico.Handle)
                  unused = SendMessage(m_hWnd, WM_SETICON, ICON_BIG, ico.Handle)
                Else
                  'TODO: How to set the icon on Linux?
                End If
              End If
              keys.RemoveAt(index)
            Case Else
          End Select
        ElseIf isControl AndAlso Not isAlt AndAlso Not isShift Then
          Select Case keys(index)
            Case ConsoleKey.V ' Insert and Overstrike
              ToggleInsertMode()
              keys.RemoveAt(index)
            Case ConsoleKey.C
              If m_running Then
                s_cancelTokenSource.Cancel()
                m_running = False
              End If
            Case ConsoleKey.Attention
              If m_running Then
                s_cancelTokenSource.Cancel()
                m_running = False
              End If
            Case Else
              Debug.WriteLine($"Unhandled key: {keys(index)}")
          End Select
        ElseIf isControl AndAlso isAlt AndAlso isShift Then
          Select Case keys(index)
            Case ConsoleKey.Add, ConsoleKey.OemPlus
              IncreasePixelSize()
              keys.RemoveAt(index)
            Case ConsoleKey.Subtract, ConsoleKey.OemMinus
              DecreasePixelSize()
              keys.RemoveAt(index)
            Case Else
          End Select
        End If
      Next
    End If

    If m_context IsNot Nothing Then
      m_context.Render()
      If Not m_context.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift, mButton, mr, mc) Then
        If TypeOf m_context Is SavePrompt Then
          'm_selected = CType(m_context, SavePrompt).Selected
          m_result = CType(m_context, Form).DialogResult
        ElseIf TypeOf m_context Is WelcomeDialog Then
          m_result = CType(m_context, Form).DialogResult
          'm_selected = CType(m_context, WelcomeDialog).Selected
          If m_result = DialogResult.Ok Then 'm_selected = 0 Then
            m_help.Title = "Survival Guide"
            m_help.Content = <content><![CDATA[
Using QBasic:

  * To activate the QBasic menu bar, press Alt.
  * To activate menus and commands, press the highlighted letter.
  * To move between menus and command, use the direction keys.
  * To get help on a selected keyword, command, or dialog box, press F1.
  * To exit Help, press Esc.

Browsing the QBasic Help system:

  * To select one of the following topics, press the Tab key or the first
    letter of the topic. Then press the Enter key to see:

    <Index>         The Index for QBasic Help
    <Contents>      The Table of Contents for QBasic Help topics
    <Using Help>    Information on using QBasic Help

Tip: These topics are also available from the Help menu.
          ]]></content>.Value
            ShowHelp()
          End If
        ElseIf TypeOf m_context Is OpenDialog Then
          Dim o = CType(m_context, OpenDialog)
          Select Case o.DialogResult
            Case DialogResult.Ok
              m_path = o.Path
              If System.IO.File.Exists(m_path) Then

                Document1.Clear()
                Document2.Clear()

                Dim code = System.IO.File.ReadAllText(m_path)
                Document1.Text = code
                Document1.Title = System.IO.Path.GetFileName(m_path)

                ' B#
                Dim tree = QB.CodeAnalysis.Syntax.SyntaxTree.Parse(code)
                Document2.Text = tree.Root.ToString
                Document2.Title = "Tree"

                ' GW
                Dim p As Basic.Parser.Parser = Nothing
                Using s As New MemoryStream()
                  Dim buffer() = code.ToByteArray
                  s.Write(buffer, 0, buffer.Length)
                  s.Seek(0, SeekOrigin.Begin)

                End Using

                'm_subs.Clear()
                If p IsNot Nothing Then
                  For Each line1 In p.Lines
                    For Each statement In line1.Statements
                      Dim token = If(statement?.Tokens?.Count > 0, statement.Tokens(0), Nothing)
                      If token IsNot Nothing Then
                        Select Case token.Keyword
                          Case "END"
                            Dim nextToken = If(statement?.Tokens?.Count > 1, statement.Tokens(1), Nothing)
                            If nextToken Is Nothing Then
                            ElseIf nextToken?.Keyword = "FUNCTION" Then
                            ElseIf nextToken?.Keyword = "IF" Then
                            ElseIf nextToken?.Keyword = "SELECT" Then
                            ElseIf nextToken?.Keyword = "SUB" Then
                            ElseIf nextToken?.Keyword = "TYPE" Then
                            Else
                              Stop
                            End If
                          Case "BEEP"
                          Case "CALL", "CIRCLE", "CLS", "COLOR"
                          Case "DEFINT", "DEF", "DIM"
                          Case "ELSE", "END"
                          Case "FOR"
                          Case "GET", "GOSUB"
                          Case "IF", "INPUT"
                          Case "LET", "LINE", "LOCATE"
                          Case "NEXT"
                          Case "ON"
                          Case "PAINT", "PALETTE", "PLAY", "POKE", "PRINT", "PSET", "PUT"
                          Case "RANDOMIZE", "READ", "REM", "RESTORE", "RESUME", "RETURN"
                          Case "SCREEN"
                          Case "VIEW"
                          Case "WEND", "WHILE", "WIDTH"
                          Case "SUB", "FUNCTION"
                            token = If(statement?.Tokens?.Count > 1, statement.Tokens(1), Nothing)

                          Case Else
                            Debug.WriteLine(token.Keyword)
                        End Select
                      End If
                      Exit For
                    Next
                  Next
                End If

              End If
            Case DialogResult.Cancel
            Case DialogResult.Help
            Case Else
          End Select
        ElseIf TypeOf m_context Is SaveAsDialog Then
          Dim o = CType(m_context, SaveAsDialog)
          Select Case o.DialogResult
            Case DialogResult.Ok
              m_path = o.Path
              System.IO.File.WriteAllText(m_path, Document1.Text)
              Document1.Title = System.IO.Path.GetFileName(m_path)
              Document1.Changed = False
            Case DialogResult.Cancel
            Case DialogResult.Help
            Case Else
          End Select
        End If
        m_context = Nothing : DrawScreen()
      End If

    Else

      If m_running Then 'Interpreter.IsRunning Then

        ' need to feed the keys into the runner
        If keys IsNot Nothing Then
          For index = keys.Count - 1 To 0 Step -1
            If Not isControl AndAlso Not isAlt Then
              Dim k = GetChar(keys(index), CapsLock, isShift)
              QBLib.Video.KeyPush(k)
            End If
            keys.RemoveAt(index)
          Next
        End If

        If m_runner.IsAlive Then
          m_scrn0 = Nothing
          m_scrn = Nothing
          m_buffer = Nothing
        Else
          If m_scrn0 Is Nothing Then
            'TODO: Take a "snapshot" of the screen
            '      Capture the current cursor location
            '      Capture the current foreground and background colors
            '      Capture the current screen mode? Width?
            ReDim m_scrn0(Screen0.Length - 1)
            Array.Copy(Screen0, 0, m_scrn0, 0, Screen0.Length)
            ReDim m_buffer(Buffer.Length - 1)
            Array.Copy(Buffer, 0, m_buffer, 0, Buffer.Length)
            m_fg = QBLib.Video.m_fgColor
            m_bg = QBLib.Video.m_bgColor
            m_cr = QBLib.Video.CursorRow
            m_cc = QBLib.Video.CursorCol
            LOCATE(25, 1) : PRINT("Press any key to continue", True)
          End If
          If keys IsNot Nothing Then
            s_cancelTokenSource.Cancel()
            m_running = False
            'For Each key In keys
            '  Select Case key
            '    Case ConsoleKey.Escape
            '      s_cancelTokenSource.Cancel()
            '      m_running = False
            '    Case Else
            '  End Select
            'Next
          End If
        End If

      Else

        Dim processed = Menu.ProcessMouse(mButton, mr, mc)
        If Not processed AndAlso mButton Then
          If Not processed AndAlso m_immediate.Visible Then
            If mr >= m_immediate.EditorTop AndAlso mr <= m_immediate.EditorTop + m_immediate.EditorHeight - 1 Then
              If Not m_immediate.Focused Then FocusImmediate()
              'processed = m_immediate.ProcessMouse(buttonPressed, mr, mc)
              processed = True
            End If
          End If
          If Not processed AndAlso Document2.Visible Then
            If mr >= Document2.EditorTop AndAlso mr <= Document2.EditorTop + Document2.EditorHeight - 1 Then
              If Not Document2.Focused Then FocusDocument2()
              'processed = Document2.ProcessMouse(buttonPressed, mr, mc)
              processed = True
            End If
          End If
          If Not processed AndAlso Document1.Visible Then
            If mr >= Document1.EditorTop AndAlso mr <= Document1.EditorTop + Document1.EditorHeight - 1 Then
              If Not Document1.Focused Then FocusDocument1()
              'processed = Document1.ProcessMouse(buttonPressed, mr, mc)
              processed = True
            End If
          End If
          If Not processed AndAlso m_help.Visible Then
            If mr >= m_help.EditorTop AndAlso mr <= m_help.EditorTop + m_help.EditorHeight - 1 Then
              If Not m_help.Focused Then FocusHelp()
              'processed = m_help.ProcessMouse(buttonPressed, mr, mc)
              processed = True
            End If
          End If
        End If

        If Not m_isAlt Then
          m_isAlt = GetKey(Key.ALT).Pressed
          Menu.AltPressed = True
        Else
          m_isAlt = GetKey(Key.ALT).Held
          If Not m_isAlt AndAlso Menu.AltPressed Then Menu.ShowAccelerators = True
        End If

        If Not Menu.ShowAccelerators AndAlso
           Not Menu.Focused Then

          ' End+Enter - Insert Line below???
          ' End,Enter - Insert a blank line below the cursor position
          ' Home,Ctrl+N - Insert a blank line above the cursor position (WordStar)

          Dim handled = False

          If keys IsNot Nothing Then

            For Each key In keys

              If m_isCtrlP Then
                If isControl Then
                  Select Case key
                    Case Else
                      m_isCtrlP = False
                  End Select
                Else
                  m_isCtrlP = False
                End If
                handled = True
              ElseIf m_isK Then
                Select Case key
                  Case ConsoleKey.D0, ConsoleKey.D1, ConsoleKey.D2, ConsoleKey.D3 ' Set bookmarks (maximum of 4)
                  Case Else
                    m_isK = False
                End Select
                handled = True
              ElseIf m_isQ Then
                Select Case key
                  Case ConsoleKey.D0, ConsoleKey.D1, ConsoleKey.D2, ConsoleKey.D3 ' Go to set bookmarks
                  Case ConsoleKey.A ' Search for and replace text
                  Case ConsoleKey.D ' Move cursor to end of line (WordStar)
                  Case ConsoleKey.E ' Move cursor to top of window (WordStar)
                  Case ConsoleKey.F ' Search for text (WordStar)
                  Case ConsoleKey.S ' Cursor to beginning of current line (WordStar)
                  Case ConsoleKey.X ' Move cursor to bottom of window (WordStar)
                  Case ConsoleKey.Y ' Cut to end of line (WordStar)
                  Case Else
                    m_isQ = False
                End Select
                handled = True
              ElseIf isShift Then
                Select Case key
                  Case ConsoleKey.F1 ' Help on QBasic Help
                    UsingHelpAction()
                    handled = True
                  Case ConsoleKey.F2 ' Display the next procedure
                    handled = True
                  Case ConsoleKey.F5 ' Start program execution from beginning
                    StartAction()
                    handled = True
                  Case ConsoleKey.F6 ' Make the previous window the active window
                    If Document1.Focused Then
                      If m_help.Visible Then
                        FocusHelp()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      Else
                        FocusDocument1()
                      End If
                    ElseIf Document2.Visible AndAlso Document2.Focused Then
                      If Document1.Visible Then
                        FocusDocument1()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      Else
                        FocusDocument2()
                      End If
                    ElseIf m_immediate.Visible AndAlso m_immediate.Focused Then
                      If Document2.Visible Then
                        FocusDocument2()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      Else
                        FocusImmediate()
                      End If
                    ElseIf m_help.Visible AndAlso m_help.Focused Then
                      If m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      Else
                        FocusHelp()
                      End If
                    End If
                    handled = True
                  Case Else
                End Select
              ElseIf isShift AndAlso isControl Then
                Select Case key
                  Case ConsoleKey.UpArrow ' Select Words
                    handled = True
                  Case ConsoleKey.DownArrow ' Select Words
                    handled = True
                  Case ConsoleKey.LeftArrow ' Select Words
                    handled = True
                  Case ConsoleKey.RightArrow ' Select Words
                  Case ConsoleKey.Home ' Select to beginning of file
                    handled = True
                  Case ConsoleKey.End ' Select to end of file
                    handled = True
                  Case Else
                End Select
              ElseIf isControl Then
                Select Case key
                  Case ConsoleKey.F2 ' Display the previous procedure
                    handled = True
                  Case ConsoleKey.F10 ' Switch between multiple windows and full-screen active window
                    ZoomEditorAction()
                    handled = True
                  Case Else
                End Select
              ElseIf m_isAlt Then
                Select Case key
                  Case ConsoleKey.Add ' Increase size of active window
                    'NOTE: The OemPlus doesn't seem to work for this purpose in the original; just beeps.
                    Menu.AltPressed = False
                    Dim max = 24
                    Dim visibleCount = 3
                    If Not m_help.Visible Then visibleCount -= 1
                    If Not Document2.Visible Then visibleCount -= 1
                    If Not m_immediate.Visible Then visibleCount -= 1
                    max -= visibleCount
                    If m_help.Visible AndAlso m_help.Focused AndAlso m_help.EditorHeight < max Then
                      If Document1.Visible AndAlso Document1.EditorHeight > 2 Then ' Shrink Document1?
                        Document1.EditorHeight -= 1 : Document1.EditorTop += 1
                        m_help.EditorHeight += 1
                      ElseIf Document2.Visible AndAlso Document2.EditorHeight > 2 Then ' Shrink Document2?
                        Document2.EditorHeight -= 1 : Document2.EditorTop += 1
                        If Document1.Visible Then Document1.EditorTop += 1 ' If shrink, move Document1 down.
                        m_help.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight > 2 Then ' Shrink Immediate?
                        m_immediate.EditorTop += 1
                        m_immediate.EditorHeight -= 1
                        If Document1.Visible Then Document1.EditorTop += 1 ' If shrink, move Document1 down.
                        If Document2.Visible Then Document2.EditorTop += 1 ' If shrink, move Document2 down.
                        m_help.EditorHeight += 1
                      End If
                    ElseIf Document1.Visible AndAlso Document1.Focused AndAlso Document1.EditorHeight < max Then
                      If Document2.Visible AndAlso Document2.EditorHeight > 2 Then
                        Document2.EditorTop += 1 : Document2.EditorHeight -= 1
                        Document1.EditorHeight += 1
                      ElseIf m_help.Visible AndAlso m_help.EditorHeight > 2 Then
                        m_help.EditorHeight -= 1
                        Document1.EditorTop -= 1 : Document1.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight > 2 Then
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                        Document1.EditorHeight += 1
                      End If
                    ElseIf Document2.Visible AndAlso Document2.Focused AndAlso Document2.EditorHeight < max Then
                      If Document1.Visible AndAlso Document1.EditorHeight > 2 Then
                        Document1.EditorHeight -= 1
                        Document2.EditorTop -= 1 : Document2.EditorHeight += 1
                      ElseIf m_help.Visible AndAlso m_help.EditorHeight > 2 Then
                        m_help.EditorHeight -= 1
                        If Document1.Visible Then Document1.EditorTop -= 1
                        Document2.EditorTop -= 1 : Document2.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight > 2 Then
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                        Document2.EditorHeight += 1
                      End If
                    ElseIf m_immediate.Visible AndAlso m_immediate.Focused AndAlso m_immediate.EditorHeight < 12 Then
                      If Document2.Visible AndAlso Document2.EditorHeight > 2 Then
                        Document2.EditorHeight -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      ElseIf Document1.Visible AndAlso Document1.EditorHeight > 2 Then
                        Document1.EditorHeight -= 1
                        If Document2.Visible Then Document2.EditorTop -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      ElseIf m_help.Visible AndAlso m_help.EditorHeight > 2 Then
                        m_help.EditorHeight -= 1
                        If Document1.Visible Then Document1.EditorTop -= 1
                        If Document2.Visible Then Document2.EditorTop -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      End If
                    End If
                    handled = True
                  Case ConsoleKey.Subtract, ConsoleKey.OemMinus ' Decrease size of active window
                    Menu.AltPressed = False
                    If m_help.Focused AndAlso m_help.EditorHeight > 2 Then
                      m_help.EditorHeight -= 1
                      Document1.EditorTop -= 1 : Document1.EditorHeight += 1
                    ElseIf Document1.Focused AndAlso Document1.EditorHeight > 2 Then
                      If m_help.Visible Then
                        m_help.EditorHeight += 1
                        Document1.EditorTop += 1 : Document1.EditorHeight -= 1
                      ElseIf Document2.Visible Then
                        Document1.EditorHeight -= 1
                        Document2.EditorTop -= 1 : Document2.EditorHeight += 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight < 12 Then
                        Document1.EditorHeight -= 1
                        m_immediate.EditorTop -= 1 : m_immediate.EditorHeight += 1
                      End If
                    ElseIf Document2.Focused AndAlso Document2.EditorHeight > 2 Then
                      If Document1.Visible Then
                        Document1.EditorHeight += 1
                        Document2.EditorTop += 1 : Document2.EditorHeight -= 1
                      ElseIf m_immediate.Visible AndAlso m_immediate.EditorHeight < 12 Then
                        Document2.EditorHeight -= 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight += 1
                      End If
                    ElseIf m_immediate.Focused AndAlso m_immediate.EditorHeight > 2 Then
                      If Document2.Visible Then
                        Document2.EditorHeight += 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                      ElseIf Document1.Visible Then
                        Document1.EditorHeight += 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                      ElseIf m_help.Visible Then
                        m_help.EditorHeight += 1
                        m_immediate.EditorTop += 1 : m_immediate.EditorHeight -= 1
                      End If
                    End If
                    handled = True
                  Case ConsoleKey.F, ConsoleKey.E, ConsoleKey.V, ConsoleKey.S, ConsoleKey.R, ConsoleKey.D, ConsoleKey.O, ConsoleKey.H
                    Menu.ShowAccelerators = True
                    Menu.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift, mButton, mr, mc)
                    handled = True
                  Case Else
                End Select
              ElseIf Not isControl AndAlso Not isAlt AndAlso Not isShift Then
                Select Case key

                  Case ConsoleKey.Escape
                    If Menu.AltPressed AndAlso Menu.Focused Then
                      Menu.Reset()
                      handled = True
                    End If

                  Case ConsoleKey.F1 ' Help on keywords or topics (or right mouse click on word)
                    'TODO: if in a document and on a keyword, open help.
                    TopicAction()
                    handled = True
                  Case ConsoleKey.F2 ' Display a list of loaded SUB procedures
                    SubsAction()
                    handled = True
                  Case ConsoleKey.F3 ' Repeat find for same text
                    RepeatLastFindAction()
                    handled = True
                  Case ConsoleKey.F4 ' Switch between the output screen and the View window
                    OutputScreenAction()
                    handled = True
                  Case ConsoleKey.F5 ' Continue running
                    RestartAction()
                    handled = True
                  Case ConsoleKey.F6 ' Make the next window the active window
                    If Document1.Focused Then
                      If Document2.Visible Then
                        FocusDocument2()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      Else
                        FocusDocument1()
                      End If
                    ElseIf Document2.Visible AndAlso Document2.Focused Then
                      If m_immediate.Visible Then
                        FocusImmediate()
                      ElseIf m_help.Visible Then
                        FocusHelp()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      Else
                        FocusDocument2()
                      End If
                    ElseIf m_immediate.Visible AndAlso m_immediate.Focused Then
                      If m_help.Visible Then
                        FocusHelp()
                      ElseIf Document1.Visible Then
                        FocusDocument1()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      Else
                        FocusImmediate()
                      End If
                    ElseIf m_help.Visible AndAlso m_help.Focused Then
                      If Document1.Visible Then
                        FocusDocument1()
                      ElseIf Document2.Visible Then
                        FocusDocument2()
                      ElseIf m_immediate.Visible Then
                        FocusImmediate()
                      Else
                        FocusHelp()
                      End If
                    End If
                    handled = True
                  Case ConsoleKey.F7 ' Execute to cursor
                    handled = True
                  Case ConsoleKey.F8 ' Single step
                    StepAction()
                    handled = True
                  Case ConsoleKey.F9 ' Toggle breakpoint
                    ToggleBreakpointAction()
                    handled = True
                  Case ConsoleKey.F10 ' Procedure step
                    ProcedureStepAction()
                    handled = True

                  Case Else
                End Select
              End If

            Next

          End If

          If cursorVisible Then
            If m_help.Focused Then
              cursorVisible = m_help.EditorHeight > 2
            ElseIf Document1.Focused Then
              cursorVisible = Document1.EditorHeight > 2
            ElseIf Document2.Focused Then
              cursorVisible = Document2.EditorHeight > 2
            ElseIf m_immediate.Focused Then
              cursorVisible = m_immediate.EditorHeight > 2
            Else
            End If
          End If

          If cursorVisible AndAlso Not handled Then
            If m_help.Focused Then
              If Not m_help.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift, mButton, mr, mc) Then
                HideHelp()
              End If
            ElseIf Document1.Focused Then
              Document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift, mButton, mr, mc)
            ElseIf Document2.Focused Then
              Document2.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift, mButton, mr, mc)
            ElseIf m_immediate.Focused Then
              Document1.ProcessKeys(keys, CapsLock, isControl, isAlt, isShift, mButton, mr, mc)
            Else
            End If
          End If

        Else
          Menu.ProcessKeys(keys, Me.CapsLock, isControl, isAlt, isShift, mButton, mr, mc)
        End If

      End If

    End If

    If m_running Then 'Interpreter.IsRunning Then

      'TODO: Need to determine cursor visibility based
      '      on INPUT being active certainly; but also
      '      based on the current state of the most
      '      recent LOCATE statement which provides for
      '      mechanism to not only show/hide the text
      '      cursor, but also the size of the cursor.
      '      Additionally, if memory servse, there is
      '      no visible cursor in graphics modes except
      '      for during the INPUT statement; and even
      '      then, it's not animated.
      cursorVisible = QBLib.Video.IsInputActive

      If g_display IsNot Nothing Then

        Dim p = g_display.Pixels(g_display.VisualPage)
        Dim w = g_display.ScreenWidth
        Dim h = g_display.ScreenHeight
        Dim pxl As Pixel
        For y = 0 To h - 1
          For x = 0 To w - 1
            Dim c = p(y * w + x)
            pxl.I = c
            If pxl.R <> 0 OrElse pxl.B <> 0 Then ' swap the red and blue
              Dim b = pxl.R : pxl.R = pxl.B : pxl.B = b
            End If
            Draw(x, y, pxl)
          Next
        Next

      Else

        'SyncLock Me
        Dim w = m_screenPixelWidth
        Dim h = m_screenPixelHeight
        For y = 0 To h - 1
          For x = 0 To w - 1
            If h > m_screenPixelHeight Then GoTo Abort
            If w > m_screenPixelWidth Then GoTo Abort
            Dim c = Buffer(y * w + x)
            Draw(x, y, c)
          Next
        Next
        'End SyncLock
Abort:

        'For r = 0 To 24
        '  For c = 0 To 79

        '    Dim index = (r * 80) + c
        '    Dim ch = CByte(Screen0(index) And &HFF)
        '    Dim clr = ((Screen0(index) And &HFF00) \ 256) And &HFF
        '    Dim map = CharMap(m_textH, ch)

        '    Dim x = c * m_textW
        '    Dim y = r * m_textH

        '    Dim fg, bg As Integer
        '    SplitColor(clr, fg, bg)

        '    Dim fgc = m_palette(fg)
        '    Dim bgc = m_palette(bg)

        '    For dy = 0 To m_textH - 1
        '      Draw(x + 0, dy + y, If((map(dy) And 1) > 0, fgc, bgc))
        '      Draw(x + 1, dy + y, If((map(dy) And 2) > 0, fgc, bgc))
        '      Draw(x + 2, dy + y, If((map(dy) And 4) > 0, fgc, bgc))
        '      Draw(x + 3, dy + y, If((map(dy) And 8) > 0, fgc, bgc))
        '      Draw(x + 4, dy + y, If((map(dy) And 16) > 0, fgc, bgc))
        '      Draw(x + 5, dy + y, If((map(dy) And 32) > 0, fgc, bgc))
        '      Draw(x + 6, dy + y, If((map(dy) And 64) > 0, fgc, bgc))
        '      Draw(x + 7, dy + y, If((map(dy) And 128) > 0, fgc, bgc))
        '    Next

        '  Next
        'Next

      End If

    Else

      If Not ScreenMode = 0 Then
        m_screenMode = ScreenMode
        SCREEN(0)
      End If

      DrawScreen()

      Dim charIndex = LBound(Screen0) + (((mr - 1) * m_textColumns) + (mc - 1))
      Dim charValue = Screen0(charIndex) And &HFF ' split color
      Dim charColor = ((Screen0(charIndex) And &HFF00) \ 256) And &HFF ' split character

      Dim fg, bg As Integer
      SplitColor(charColor, fg, bg)

      'DrawRect(mc * m_textW, mr * m_textH, m_textW, m_textH, Presets.Black)
      QPrintRC(ChrW(charValue), mr, mc, OneColor(bg, 7 - If(bg < 8, bg, 7)))

      ' Draws pixels to screen...
      For r = 0 To m_textRows - 1
        For c = 0 To m_textColumns - 1

          If r > m_textRows - 1 OrElse c > m_textColumns - 1 Then Exit For
          Dim index = (r * m_textColumns) + c
          If index > Screen0.Length - 1 Then Exit For
          Dim ch = CByte(Screen0(index) And &HFF)
          Dim clr = ((Screen0(index) And &HFF00) \ 256) And &HFF
          Dim map = CharMap(m_textH, ch)

          Dim x = c * m_textW
          Dim y = r * m_textH

          SplitColor(clr, fg, bg)

          Dim fgc = m_palette(fg)
          Dim bgc = m_palette(bg)

          For dy = 0 To m_textH - 1
            Draw(x + 0, dy + y, If((map(dy) And 1) > 0, fgc, bgc))
            Draw(x + 1, dy + y, If((map(dy) And 2) > 0, fgc, bgc))
            Draw(x + 2, dy + y, If((map(dy) And 4) > 0, fgc, bgc))
            Draw(x + 3, dy + y, If((map(dy) And 8) > 0, fgc, bgc))
            Draw(x + 4, dy + y, If((map(dy) And 16) > 0, fgc, bgc))
            Draw(x + 5, dy + y, If((map(dy) And 32) > 0, fgc, bgc))
            Draw(x + 6, dy + y, If((map(dy) And 64) > 0, fgc, bgc))
            Draw(x + 7, dy + y, If((map(dy) And 128) > 0, fgc, bgc))
          Next

        Next
      Next

    End If

    ' Blinking cursor...
    If cursorVisible AndAlso m_cursorVisible Then
      If CInt(Fix(m_t * 8)) Mod 2 = 0 Then

        Dim cc = QBLib.Video.CursorCol
        Dim cr = QBLib.Video.CursorRow
        'TODO: Need to determine how/where this value is overflowing...
        If cc > m_textColumns Then cc = m_textColumns
        If m_running Then
          ' uses CursorCol/CursorRow
        ElseIf m_context IsNot Nothing Then
          cc = m_context.CursorCol
          cr = m_context.CursorRow
        ElseIf m_help.Focused Then
          cc = m_help.CursorCol
          cr = m_help.CursorRow
        ElseIf Document1.Focused Then
          cc = Document1.CursorCol
          cr = Document1.CursorRow
        ElseIf Document2.Focused Then
          cc = Document2.CursorCol
          cr = Document2.CursorRow
        ElseIf m_immediate.Focused Then
          cc = m_immediate.CursorCol
          cr = m_immediate.CursorRow
        Else
        End If

        If cr < 1 OrElse cr > m_textRows Then cr = 1

        Dim strt = CursorStart
        Dim stp = CursorStop

        If stp = 0 Then ' RULE: If stop=0, nothing...
          If strt = 0 Then ' ...unless start=0
            ' draw a single line at top of block
          Else
            strt = -1
          End If
        ElseIf stp < strt Then ' RULE: If stop<start, from stop to bottom
          strt = stp : stp = 14
        ElseIf strt = stp Then ' RULE: If start=stop, single line at start...
          If strt <= 3 Then
          Else '...(or bottom if start=>4)
            strt = 14 : stp = 14
          End If
        ElseIf strt >= 3 Then ' RULE: Calcuate difference between start and stop
          If stp - strt = 1 Then ' If difference is 1, draw two lines at bottom
            strt = 14 : stp = 14
          ElseIf stp - strt = 2 Then ' If difference is 2, draw two lines at bottom
            strt = 13 : stp = 14
          Else ' Draw "half" cursor...
            strt = 7 : stp = 14
          End If
        ElseIf strt <= 1 Then ' RULE: If start<=1 then...
          If stp <= 3 Then ' ... start to difference + 1
            stp = stp - strt + 1
          Else ' ...start to bottom...
            stp = 14
          End If
        ElseIf stp = 2 Then ' RULE: If start=2...
          If stp = 3 Then ' If stop=3, then 2 to 3
          ElseIf stp = 4 Then ' If stop=4, then 3 lines at bottom
            strt = 12
            stp = 14
          Else ' Otherwise 2 to bottom
            strt = 2
            stp = 14
          End If
        End If

        If strt > -1 Then
          Dim x = (cc - 1) * m_textW
          Dim y = (cr - 1) * m_textH
          Dim c = SCREEN(cr, cc, 1)
          Dim fg, bg As Integer
          SplitColor(c, fg, bg)
          Dim cclr = Presets.Gray
          If bg = 8 Then cclr = Presets.Black
          For i = strt To stp
            DrawLine(x, y + i, x + 7, y + i, cclr)
          Next
        End If

      End If
    End If

    Return Not m_exit

  End Function

  Private Sub ShowHelp()
    m_help.EditorTop = 2 : m_help.EditorHeight = 21
    Dim row = 22
    If Document1.Visible Then
      Document1.EditorTop = row
      If Document2.Visible Then
        If m_immediate.Visible Then
          Document2.EditorTop = 23
          m_immediate.EditorTop = 24
          Document2.EditorHeight = 2
          Document1.EditorHeight = 2
          m_immediate.EditorHeight = 2
        Else
          Document2.EditorTop = 24
          Document2.EditorHeight = 2
          Document1.EditorHeight = 3
        End If
      Else
        If m_immediate.Visible Then
          m_immediate.EditorTop = 24
          Document1.EditorHeight = 3
          m_immediate.EditorHeight = 2
        Else
          Document1.EditorHeight = 4
        End If
      End If
    End If
    m_help.Visible = True : FocusHelp()
  End Sub

  Private Sub HideHelp()
    m_help.Visible = False
    Dim h = (Document1.EditorTop - 1) + Document1.EditorHeight
    Document1.EditorTop = 2
    Document1.EditorHeight = h
    FocusDocument1()
  End Sub

  Private Sub FocusDocument1()
    m_help.Focused = False
    m_help.ScrollBars = False
    m_immediate.Focused = False
    Document2.Focused = False
    Document2.ScrollBars = False
    Document1.Focused = True
    Document1.ScrollBars = True
    QBLib.Video.LOCATE(Document1.EditorTop + 1, Document1.EditorLeft + 1)
  End Sub

  Private Sub FocusDocument2()
    m_help.Focused = False
    m_help.ScrollBars = False
    m_immediate.Focused = False
    Document1.Focused = False
    Document1.ScrollBars = False
    Document2.Focused = True
    Document2.ScrollBars = True
    QBLib.Video.LOCATE(Document2.EditorTop + 1, Document2.EditorLeft + 1)
  End Sub

  Private Sub FocusHelp()
    Document1.Focused = False
    Document1.ScrollBars = False
    Document2.Focused = False
    Document2.ScrollBars = False
    m_immediate.Focused = False
    m_help.Focused = True
    m_help.ScrollBars = True
    QBLib.Video.LOCATE(m_help.EditorTop + 1, m_help.EditorLeft + 1)
  End Sub

  Private Sub FocusImmediate()
    m_help.Focused = False
    m_help.ScrollBars = False
    Document1.Focused = False
    Document1.ScrollBars = False
    Document2.Focused = False
    Document2.ScrollBars = False
    m_immediate.Focused = True
    QBLib.Video.LOCATE(m_immediate.EditorTop + 1, m_immediate.EditorLeft + 1)
  End Sub

  Private Function GetPressed() As List(Of ConsoleKey)

    Dim result As New List(Of ConsoleKey)

    If GetKey(Key.LEFT).Pressed Then result.Add(ConsoleKey.LeftArrow)
    If GetKey(Key.RIGHT).Pressed Then result.Add(ConsoleKey.RightArrow)
    If GetKey(Key.UP).Pressed Then result.Add(ConsoleKey.UpArrow)
    If GetKey(Key.DOWN).Pressed Then result.Add(ConsoleKey.DownArrow)
    If GetKey(Key.HOME).Pressed Then result.Add(ConsoleKey.Home)
    If GetKey(Key.END).Pressed Then result.Add(ConsoleKey.End)
    If GetKey(Key.PGUP).Pressed Then result.Add(ConsoleKey.PageUp)
    If GetKey(Key.PGDN).Pressed Then result.Add(ConsoleKey.PageDown)
    If GetKey(Key.INS).Pressed Then result.Add(ConsoleKey.Insert)
    If GetKey(Key.DEL).Pressed Then result.Add(ConsoleKey.Delete)
    If GetKey(Key.BACK).Pressed Then result.Add(ConsoleKey.Backspace)
    If GetKey(Key.TAB).Pressed Then result.Add(ConsoleKey.Tab)
    If GetKey(Key.ENTER).Pressed Then result.Add(ConsoleKey.Enter)

    If GetKey(Key.NP_ADD).Pressed Then result.Add(ConsoleKey.Add)
    If GetKey(Key.NP_SUB).Pressed Then result.Add(ConsoleKey.Subtract)
    If GetKey(Key.NP_MUL).Pressed Then result.Add(ConsoleKey.Multiply)
    If GetKey(Key.NP_DIV).Pressed Then result.Add(ConsoleKey.Divide)

    If GetKey(Key.OEM_1).Pressed Then result.Add(ConsoleKey.Oem1) ' ;: (186)
    If GetKey(Key.EQUALS).Pressed Then result.Add(ConsoleKey.OemPlus) ' =+ (187)
    If GetKey(Key.COMMA).Pressed Then result.Add(ConsoleKey.OemComma) ' ,< (188)
    If GetKey(Key.MINUS).Pressed Then result.Add(ConsoleKey.OemMinus) ' -_ (189)
    If GetKey(Key.PERIOD).Pressed Then result.Add(ConsoleKey.OemPeriod) ' .> (190)
    If GetKey(Key.OEM_2).Pressed Then result.Add(ConsoleKey.Oem2) ' /?
    If GetKey(Key.OEM_3).Pressed Then result.Add(ConsoleKey.Oem3) ' `~
    If GetKey(Key.OEM_4).Pressed Then result.Add(ConsoleKey.Oem4) ' [{
    If GetKey(Key.OEM_5).Pressed Then result.Add(ConsoleKey.Oem5) ' \|
    If GetKey(Key.OEM_6).Pressed Then result.Add(ConsoleKey.Oem6) ' ]}
    If GetKey(Key.OEM_7).Pressed Then result.Add(ConsoleKey.Oem7) ' '"

    If GetKey(Key.ESCAPE).Pressed Then result.Add(ConsoleKey.Escape)

    If GetKey(Key.SCROLL).Pressed Then result.Add(ConsoleKey.Attention)
    If GetKey(Key.PAUSE).Pressed Then result.Add(ConsoleKey.Attention)

    If GetKey(Key.F1).Pressed Then result.Add(ConsoleKey.F1)
    If GetKey(Key.F2).Pressed Then result.Add(ConsoleKey.F2)
    If GetKey(Key.F3).Pressed Then result.Add(ConsoleKey.F3)
    If GetKey(Key.F4).Pressed Then result.Add(ConsoleKey.F4)
    If GetKey(Key.F5).Pressed Then result.Add(ConsoleKey.F5)
    If GetKey(Key.F6).Pressed Then result.Add(ConsoleKey.F6)
    If GetKey(Key.F7).Pressed Then result.Add(ConsoleKey.F7)
    If GetKey(Key.F8).Pressed Then result.Add(ConsoleKey.F8)
    If GetKey(Key.F9).Pressed Then result.Add(ConsoleKey.F9)
    If GetKey(Key.F10).Pressed Then result.Add(ConsoleKey.F10)
    If GetKey(Key.F11).Pressed Then result.Add(ConsoleKey.F11)
    If GetKey(Key.F12).Pressed Then result.Add(ConsoleKey.F12)

    If GetKey(Key.SPACE).Pressed Then result.Add(ConsoleKey.Spacebar)
    If GetKey(Key.A).Pressed Then result.Add(ConsoleKey.A)
    If GetKey(Key.B).Pressed Then result.Add(ConsoleKey.B)
    If GetKey(Key.C).Pressed Then result.Add(ConsoleKey.C)
    If GetKey(Key.D).Pressed Then result.Add(ConsoleKey.D)
    If GetKey(Key.E).Pressed Then result.Add(ConsoleKey.E)
    If GetKey(Key.F).Pressed Then result.Add(ConsoleKey.F)
    If GetKey(Key.G).Pressed Then result.Add(ConsoleKey.G)
    If GetKey(Key.H).Pressed Then result.Add(ConsoleKey.H)
    If GetKey(Key.I).Pressed Then result.Add(ConsoleKey.I)
    If GetKey(Key.J).Pressed Then result.Add(ConsoleKey.J)
    If GetKey(Key.K).Pressed Then result.Add(ConsoleKey.K)
    If GetKey(Key.L).Pressed Then result.Add(ConsoleKey.L)
    If GetKey(Key.M).Pressed Then result.Add(ConsoleKey.M)
    If GetKey(Key.N).Pressed Then result.Add(ConsoleKey.N)
    If GetKey(Key.O).Pressed Then result.Add(ConsoleKey.O)
    If GetKey(Key.P).Pressed Then result.Add(ConsoleKey.P)
    If GetKey(Key.Q).Pressed Then result.Add(ConsoleKey.Q)
    If GetKey(Key.R).Pressed Then result.Add(ConsoleKey.R)
    If GetKey(Key.S).Pressed Then result.Add(ConsoleKey.S)
    If GetKey(Key.T).Pressed Then result.Add(ConsoleKey.T)
    If GetKey(Key.U).Pressed Then result.Add(ConsoleKey.U)
    If GetKey(Key.V).Pressed Then result.Add(ConsoleKey.V)
    If GetKey(Key.W).Pressed Then result.Add(ConsoleKey.W)
    If GetKey(Key.X).Pressed Then result.Add(ConsoleKey.X)
    If GetKey(Key.Y).Pressed Then result.Add(ConsoleKey.Y)
    If GetKey(Key.Z).Pressed Then result.Add(ConsoleKey.Z)

    If GetKey(Key.K0).Pressed Then result.Add(ConsoleKey.D0)
    If GetKey(Key.K1).Pressed Then result.Add(ConsoleKey.D1)
    If GetKey(Key.K2).Pressed Then result.Add(ConsoleKey.D2)
    If GetKey(Key.K3).Pressed Then result.Add(ConsoleKey.D3)
    If GetKey(Key.K4).Pressed Then result.Add(ConsoleKey.D4)
    If GetKey(Key.K5).Pressed Then result.Add(ConsoleKey.D5)
    If GetKey(Key.K6).Pressed Then result.Add(ConsoleKey.D6)
    If GetKey(Key.K7).Pressed Then result.Add(ConsoleKey.D7)
    If GetKey(Key.K8).Pressed Then result.Add(ConsoleKey.D8)
    If GetKey(Key.K9).Pressed Then result.Add(ConsoleKey.D9)

    If result.Count = 0 Then Return Nothing Else Return result

  End Function

  Private Async Sub Menu_OnClick(sender As Object, e As MenuClickEventArgs) Handles Menu.OnClick

    Select Case e.Item.Text
      Case "New" : Await NewActionAsync()
      Case "Open..." : OpenAction()
      Case "Save" : SaveAction()
      Case "Save As..." : SaveAsAction()
      Case "Print..." : PrintAction()
      Case "Exit" : Await ExitActionAsync()
      Case "Cut" : CutAction()
      Case "Copy" : CopyAction()
      Case "Paste" : PasteAction()
      Case "Clear" : ClearAction()
      Case "New SUB..." : NewSubAction()
      Case "New FUNCTION..." : NewFunctionAction()
      Case "SUBs..." : SubsAction()
      Case "Split" : SplitAction()
      Case "Output Screen" : OutputScreenAction()
      Case "Find..." : FindAction()
      Case "Repeat Last Find" : RepeatLastFindAction()
      Case "Change..." : ChangeAction()
      Case "Start" : StartAction()
      Case "Restart" : RestartAction()
      Case "Continue" : ContinueAction()
      Case "Launch w/ trs80gp (Model IV)..." : LaunchInTrs80gpM4Action()
      Case "Launch w/ trs80gp (Coco)..." : LaunchInTrs80gpCocoAction()
      Case "Make EXE File (FBC)..." : MakeExeFileFbcAction()
      Case "In Web browser (QBJS)..." : LaunchInWebBrowserAction()
      Case "Step" : StepAction()
      Case "Procedure Step" : ProcedureStepAction()
      Case "Trace On" : TraceOnAction()
      Case "Toggle Breakpoint" : ToggleBreakpointAction()
      Case "Clear All Breakpoints" : ClearAllBreakpointsAction()
      Case "Set Next Statement" : SetNextStatementAction()
      Case "Display..." : DisplayAction()
      Case "Help Path..." : HelpPathAction()
      Case "Syntax Checking" : SyntaxCheckingAction()
      Case "Index" : IndexAction()
      Case "Contents" : ContentsAction()
      Case "Using Help" : UsingHelpAction()
      Case "About..."
        AboutAction()
      Case Else
        If e.Item.Text.StartsWith("Topic:") Then
          TopicAction()
        Else
          m_context = New MessageDialog($"Unhandled menu '{e.Item.Text}'")
        End If
    End Select
  End Sub

#Region "Actions"

  Private Async Function NewActionAsync() As Task
    If Document1.Changed Then
      'm_selected = 0
      m_result = DialogResult.None
      Do
        m_context = New SavePrompt() 'm_selected)
        While m_result = DialogResult.None
          Await Task.Delay(1)
        End While
        Select Case m_result
          Case DialogResult.Yes ' Yes
            ' Save file...
            Exit Do
          Case DialogResult.No ' No
            Exit Do
          Case DialogResult.Cancel ' Cancel
            Return
          Case DialogResult.Help ' Help
            m_context = Nothing
            Dim body = "Your file has either never been saved or has not been saved
since it last changed. Either:
  - Choose <Yes> to save the file, preserving your
    changes.
  - Choose <No> to discard your changes.
  - Choose <Cancel> to cancel the command, leaving
    everything as it was before you chose this command.".Replace("-"c, ChrW(254))
            m_context = New HelpDialog("File not saved", body)
            While m_context IsNot Nothing
              Await Task.Delay(1)
            End While
            'If m_selected < 0 Then m_selected = 0
            m_result = DialogResult.None
          Case Else
        End Select
      Loop
    End If
    Document1.Clear()
    Document2.Clear()
    DrawScreen()
  End Function

  Private Sub OpenAction()
    m_context = New OpenDialog(m_pathspec)
  End Sub

  Private Sub SaveAction()
    If Document1.Title = "Untitled" Then
      SaveAsAction()
    Else
      System.IO.File.WriteAllText(m_path, Document1.Text)
      Document1.Changed = False
    End If
  End Sub

  Private Sub SaveAsAction()
    m_context = New SaveAsDialog(m_pathspec, If(Document1.Title <> "Untitled", Document1.Title, "*.BAS"))
  End Sub

  Private Sub PrintAction()
    m_context = New PrintDialog()
  End Sub

  Private Async Function ExitActionAsync() As Task
    If Document1.Changed Then
      'm_selected = 0
      m_result = DialogResult.None
      Do
        m_context = New SavePrompt() 'm_selected)
        While m_result = DialogResult.None 'm_context IsNot Nothing
          'm_result = CType(m_context, Form).DialogResult
          Await Task.Delay(1)
        End While
        Select Case m_result 'Math.Abs(m_selected)
          Case DialogResult.Yes ' Yes
            ' Save file...
            Exit Do
          Case DialogResult.No ' No
            Exit Do
          Case DialogResult.Cancel ' Cancel
            Return
          Case DialogResult.Help ' Help
            m_context = Nothing
            Dim body = "Your file has either never been saved or has not been saved
since it last changed. Either:
  - Choose <Yes> to save the file, preserving your
    changes.
  - Choose <No> to discard your changes.
  - Choose <Cancel> to cancel the command, leaving
    everything as it was before you chose this command.".Replace("-"c, ChrW(254))
            m_context = New HelpDialog("File not saved", body)
            While m_context IsNot Nothing
              Await Task.Delay(1)
            End While
            'If m_selected < 0 Then m_selected = 0
            m_result = DialogResult.None
          Case Else
        End Select
      Loop
    End If
    m_exit = True
  End Function

  Private Sub CutAction()
    If Document1.CanCutSelection Then
      Document1.CutSelection()
    End If
  End Sub

  Private Sub CopyAction()
    If Document1.CanCopySelection Then
      Document1.CopySelection()
    End If
  End Sub

  Private Sub PasteAction()
    If Document1.CanPaste Then
      Document1.Paste()
    End If
  End Sub

  Private Sub ClearAction()
    If Document1.CanDeleteSelection Then
      Document1.DeleteSelection()
    End If
  End Sub

  Private Sub NewSubAction()
    m_context = New NewSubDialog()
  End Sub

  Private Sub NewFunctionAction()
    m_context = New NewFunctionDialog
  End Sub

  Private Sub TopicAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub UsingHelpAction()
    m_help.Title = "Using Help"
    m_help.Content = <content><![CDATA[  <Contents>  <Index>  <Back>
---
Using QBasic Help:

  * To get help on a Basic keyword, place the cursor on it and press F1
    or click the right mouse button.

  * To get help on a QBasic menu, command, or dialog box, place the cursor
    on the menu item or <Help> button and press F1.

  * To view QBasic Help topics, press Alt+H, then press the highlighted
    letter to choose a command.

  * To move the cursor into the Help window, press Shift+F6.

  * To scroll the help information, press PgDn or PgUp.

  * To copy help information (such as a programming example) to the
    View window, use the commands on the QBasic Edit menu.

  * To close the Help window, press Esc.

In the Help window, you can display information on:

  * Help topics (identified by highlighted arrowheads < >)

  * Basic keywords

To move the cursor to a Help topic, press Tab or the first letter of the
topic. To display the topic or keyword information, place the cursor
anywhere on the topic or keyword and press F1 or click the right mouse
button.

QBasic saves the last 20 Help topics you viewed. To review them press
Alt+F1 or click the <Back> button repeatedly.
]]></content>.Value
    ShowHelp()
  End Sub

  Private Sub ContentsAction()
    m_help.Title = "Table of Contents"
    m_help.Content = <content><![CDATA[  <Contents>  <Index>  <Back>
---

  +Orientation-------------------------+ +Keys--------------------------+
  | <Using Help>                       | | <Shortcut Keys Summary>      |
  | <Using Menus and Commands>         | | <Editing Keys>               |
  | <Using a Dialog Box>               | | <View and Search Keys>       |
  | <Syntax Conventions>               | | <Run and Debug Keys>         |
  +------------------------------------+ | <Help Keys>                  |
  +Using QBasic------------------------+ +------------------------------+ 
  | <QBasic Command Line>              | +Quick Reference---------------+
  | <Basic Character Set>              | | <ASCII Character Codes>      |
  | <Keywords by Programming Task>     | | <Keyboard Scan Codes>        |
  | <QBasic Environment Limits>        | | <Run-Time Error Codes>       |
  | <Version Differences>              | | <Copyright and Trademarks>   |
  | <Converting BASICA Programs>       | +------------------------------+ 
  | <Beyond QBasic>                    | 
  +------------------------------------+

]]></content>.Value
    ShowHelp()
  End Sub

  Private Sub IndexAction()
    m_help.Title = "QBasic Online Help Index"
    m_help.Content = <content><![CDATA[  <Contents>  <Index>  <Back>
---
To get help on a QBasic keyword in the list below:
    1. Press the key of the first letter of the keyword.
    2. Use the direction keys to move the cursor to the keyword.
    3. Press F1 to display the help text in the Help window.
                                                                  +===+
==================================================================| A |
                                                                  +===+
  ABS Function                       APPEND Keyword
  ABSOLUTE Keyword                   AS Keyword
  ACCESS Keyword                     ASC Function
  AND Operator                       ATN Function
  ANY Keyword
                                                                  +===+
==================================================================| B |
                                                                  +===+
  BASE Keyword                       BLOAD Statement
  Basic Character Set                Boolean Operators
  BEEP Statement                     BSAVE Statement
  BINARY Keyword
                                                                  +===+
==================================================================| C |
                                                                  +===+
  CALL Statement                     COLOR Statement
  CALL ABSOLUTE Statement            COM Statement
  CASE Keyword                       COMMON Statement
  ...

]]></content>.Value
    ShowHelp()
  End Sub

  Private Sub SyntaxCheckingAction()
    ' Toggle value
    Menu.Items(6).Items(2).Checked = Not Menu.Items(6).Items(2).Checked
    DrawScreen()
  End Sub

  Private Sub HelpPathAction()
    m_context = New HelpPathDialog()
  End Sub

  Private Sub DisplayAction()
    m_context = New DisplayDialog()
  End Sub

  Private Sub SetNextStatementAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ClearAllBreakpointsAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ToggleBreakpointAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub TraceOnAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ProcedureStepAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub StepAction()
    Dim tree = QB.Interpreter.DebugTree(Document1.Text)
    Document1.Text = tree
    'm_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub ContinueAction()
    m_runner = New Threading.Thread(AddressOf RunnerBs)
    m_runner.Start()
  End Sub

  Private Sub RestartAction()
    m_runner = New Threading.Thread(AddressOf RunnerBs)
    m_runner.Start()
  End Sub

  Private Sub StartAction()
    m_runner = New Threading.Thread(AddressOf RunnerBs)
    m_runner.Start()
  End Sub

  Private Sub RunnerGw()

    s_cancelTokenSource = New System.Threading.CancellationTokenSource()
    s_cancelToken = s_cancelTokenSource.Token
    m_running = True

    'InterpreterTimer.Enabled = True

    Dim env As Basic.Environment.IEnvironment = Nothing
    Dim fs As Basic.IO.IVirtualFileSystem = New WindowsFileSystem
    Dim snd As Basic.Audio.ISound = Nothing
    Dim kbd As Basic.Input.IKeyboard = Nothing
    'Dim gpio As Basic.IO.IGpio '= Nothing
    'm_display = New Display()

    If m_scrn0 IsNot Nothing Then
      Array.Copy(m_scrn0, 0, Screen0, 0, Screen0.Length)
      Array.Copy(m_buffer, 0, Buffer, 0, Buffer.Length)
      'QBLib.Video.m_fgColor = m_fg
      'QBLib.Video.m_bgColor = m_bg
      'QBLib.Video.m_cursorRow = m_cr
      'QBLib.Video.m_cursorCol = m_cc
      QBLib.Video.COLOR(m_fg, m_bg)
      QBLib.Video.LOCATE(m_cr, m_cc)
      m_scrn0 = Nothing
      m_buffer = Nothing
    Else
      QBLib.Video.CLS()
      QBLib.Video.COLOR(8, 0)
    End If

    g_display = New Display
    Dim interpreter = New Basic.Interactive(env,
                                            g_display,
                                            Nothing,
                                            snd,
                                            kbd,
                                            "en-US") With {.Source = Document1.Text}
    interpreter.Run()

    'm_display = Nothing

  End Sub

  Private Sub RunnerBs()
    g_display = Nothing : m_running = True
    s_cancelTokenSource = New System.Threading.CancellationTokenSource()
    s_cancelToken = s_cancelTokenSource.Token
    If m_scrn0 IsNot Nothing Then
      If m_screenMode > 0 Then SCREEN(m_screenMode)
      Array.Copy(m_scrn0, 0, Screen0, 0, Screen0.Length)
      Array.Copy(m_buffer, 0, Buffer, 0, Buffer.Length)
      'QBLib.Video.m_fgColor = m_fg
      'QBLib.Video.m_bgColor = m_bg
      'QBLib.Video.m_cursorRow = m_cr
      'QBLib.Video.m_cursorCol = m_cc
      QBLib.Video.COLOR(m_fg, m_bg)
      QBLib.Video.LOCATE(m_cr, m_cc)
      m_scrn0 = Nothing
      m_buffer = Nothing
    Else
      QBLib.Video.COLOR(8, 0)
      QBLib.Video.CLS()
    End If
    Dim i = New QB.Interpreter()
    i.Run(Document1.Text)
  End Sub

  Private Sub MakeExeFileFbcAction()

    Dim compiler = "c:\fb\fbc64.exe"

    If File.Exists(compiler) Then

      If File.Exists(m_path) Then

        Dim target = Path.ChangeExtension(m_path, ".exe")

        If File.Exists(target) Then
          File.Delete(target)
        End If

        Dim compile = New ProcessStartInfo() With {.FileName = compiler,
                                                   .Arguments = $"""{m_path}"" -lang qb -x ""{target}""",
                                                   .UseShellExecute = False,
                                                   .CreateNoWindow = True,
                                                   .RedirectStandardOutput = True}
        Using p = Process.Start(compile)
          If Not p.HasExited Then
            p.WaitForExit()
          Else
            m_context = New MessageDialog("Process start failed (1).") : Return
          End If
        End Using

        If File.Exists(target) Then

          Dim execute = New ProcessStartInfo() With {.FileName = target,
                                                     .UseShellExecute = True,
                                                     .CreateNoWindow = False}
          Using p = Process.Start(execute)
            If Not p.HasExited Then
              p.WaitForExit()
            Else
              m_context = New MessageDialog("Process start failed (2).") : Return
            End If
          End Using

        Else
          m_context = New MessageDialog("Failed.")
        End If

      Else
        m_context = New MessageDialog("File not found.")
      End If

    Else
      m_context = New MessageDialog("Missing FBC64.EXE.")
    End If

  End Sub

  Private Sub LaunchInTrs80gpM4Action()

    Dim basePath = System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly.Location)
    Dim tool = System.IO.Path.Combine(basePath, "extras\trs80gp.exe")

    If File.Exists(tool) Then

      If File.Exists(m_path) Then

        Dim execute = New ProcessStartInfo() With {.FileName = tool,
                                                   .Arguments = $"-m4 ""{m_path}""",
                                                   .UseShellExecute = False,
                                                   .CreateNoWindow = False,
                                                   .RedirectStandardOutput = False}
        Using p = Process.Start(execute)
          If Not p.HasExited Then
            p.WaitForExit()
          Else
            m_context = New MessageDialog("Process start failed.") : Return
          End If
        End Using

      End If

    End If

  End Sub

  Private Sub LaunchInTrs80gpCocoAction()

    Dim basePath = System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly.Location)
    Dim tool = System.IO.Path.Combine(basePath, "extras\trs80gp.exe")

    If File.Exists(tool) Then

      If File.Exists(m_path) Then

        Dim execute = New ProcessStartInfo() With {.FileName = tool,
                                                   .Arguments = $"-mc ""{m_path}""",
                                                   .UseShellExecute = False,
                                                   .CreateNoWindow = False,
                                                   .RedirectStandardOutput = False}
        Using p = Process.Start(execute)
          If Not p.HasExited Then
            p.WaitForExit()
          Else
            m_context = New MessageDialog("Process start failed.") : Return
          End If
        End Using

      End If

    End If

  End Sub

  Private Sub LaunchInWebBrowserAction()

    Dim compress = True

    If Document1.Text <> "" Then 'Title <> "Untitled" Then

      Dim decoded = Document1.Text
      decoded = decoded.Replace(vbCrLf, vbLf)
      Dim bytes = Encoding.UTF8.GetBytes(decoded)

      Dim buffer() As Byte
      Using ms = New MemoryStream()
        Using gs = New GZipStream(ms, CompressionMode.Compress, True)
          gs.Write(bytes, 0, bytes.Length)
        End Using
        ms.Position = 0
        ReDim buffer(CInt(ms.Length - 1))
        ms.Read(buffer, 0, buffer.Length)
      End Using

      Dim encoded = Convert.ToBase64String(If(compress, buffer, bytes))
      'Dim url = $"https://qbjs.org/#code={encoded}"
      Dim url = $"https://boxgaming.github.io/qbjs/#gzcode={encoded}"

      If False Then ' For testing...

        Dim dBuffer = Convert.FromBase64String(encoded)
        Using ms = New MemoryStream(dBuffer)
          Using gs = New GZipStream(ms, CompressionMode.Decompress)
            Using ds = New MemoryStream()
              gs.CopyTo(ds)
              bytes = ds.ToArray
            End Using
          End Using
        End Using

        decoded = Encoding.UTF8.GetString(bytes)

        Document1.Text = $"'decoded
{decoded}"

      End If

      Dim p As New Process
      p.StartInfo.UseShellExecute = True
      p.StartInfo.FileName = url
      p.Start()
      'Process.Start("cmd", "/C start" + " " + url)
    End If

  End Sub

  Private Sub ChangeAction()
    m_context = New ChangeDialog()
  End Sub

  Private Sub RepeatLastFindAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub FindAction()
    m_context = New FindDialog()
  End Sub

  Private Sub OutputScreenAction()
    m_context = New MessageDialog("Not implemented.")
  End Sub

  Private Sub SplitAction()
    Dim visible = Document2.Visible
    If visible Then
      Document2.Visible = False
      Document1.EditorHeight += Document2.EditorHeight - 1 '21
      If Not Document1.Focused AndAlso Not m_immediate.Focused Then
        FocusDocument1()
      End If
    Else
      Dim h = Document1.EditorHeight - 1
      Dim h2 = h \ 2
      Dim h1 = h - h2
      Document1.EditorHeight = h1 + 1
      Document2.EditorTop = 2 + h1
      Document2.EditorHeight = h2 + 1
      Document2.ScrollBars = False
      Document2.Visible = True
    End If
  End Sub

  Private Sub SubsAction()
    Dim dlg = New SubsDialog(Document1.Title, m_subs)
    m_context = dlg
  End Sub

  Private Sub AboutAction()
    m_context = New AboutDialog()
  End Sub

  Private m_windowFocused As Boolean

  Private m_help_visible As Boolean
  Private m_help_top As Integer
  Private m_help_height As Integer
  Private m_document1_visible As Boolean
  Private m_document1_top As Integer
  Private m_document1_height As Integer
  Private m_document2_visible As Boolean
  Private m_document2_top As Integer
  Private m_document2_height As Integer
  Private m_immediate_visible As Boolean
  Private m_immediate_top As Integer
  Private m_immediate_height As Integer

  Private Sub ZoomEditorAction()
    If m_windowFocused Then
      m_help.Visible = m_help_visible
      m_help.EditorTop = m_help_top
      m_help.EditorHeight = m_help_height
      Document1.Visible = m_document1_visible
      Document1.EditorTop = m_document1_top
      Document1.EditorHeight = m_document1_height
      Document2.Visible = m_document2_visible
      Document2.EditorTop = m_document2_top
      Document2.EditorHeight = m_document2_height
      m_immediate.Visible = m_immediate_visible
      m_immediate.EditorTop = m_immediate_top
      m_immediate.EditorHeight = m_immediate_height
      m_windowFocused = False
    Else
      m_help_visible = m_help.Visible
      m_help_top = m_help.EditorTop
      m_help_height = m_help.EditorHeight
      m_document1_visible = Document1.Visible
      m_document1_top = Document1.EditorTop
      m_document1_height = Document1.EditorHeight
      m_document2_visible = Document2.Visible
      m_document2_top = Document2.EditorTop
      m_document2_height = Document2.EditorHeight
      m_immediate_visible = m_immediate.Visible
      m_immediate_top = m_immediate.EditorTop
      m_immediate_height = m_immediate.EditorHeight
      If m_help.Focused Then
        m_help.EditorTop = 2
        m_help.EditorHeight = 24
        Document1.Visible = False
        Document2.Visible = False
        m_immediate.Visible = False
        m_windowFocused = True
      ElseIf Document1.Focused Then
        m_help.Visible = False
        Document1.EditorTop = 2
        Document1.EditorHeight = 24
        Document2.Visible = False
        m_immediate.Visible = False
        m_windowFocused = True
      ElseIf Document2.Focused Then
        m_help.Visible = False
        Document1.Visible = False
        Document2.EditorTop = 2
        Document2.EditorHeight = 24
        m_immediate.Visible = False
        m_windowFocused = True
      Else
        'NOTE: Not valid if currently focused on the Immediate window.
        'm_help.Visible = False
        'm_document1.Visible = False
        'm_document2.Visible = False
        'm_immediate.EditorTop = 2
        'm_immediate.EditorHeight = 24
      End If
    End If
  End Sub

  Private Sub Document1_SelectionChanged() Handles Document1.SelectionChanged
    Menu.Items(1).Items(0).Enabled = Document1.CanCutSelection ' Cut
    Menu.Items(1).Items(1).Enabled = Document1.CanCopySelection ' Copy
    Menu.Items(1).Items(2).Enabled = Document1.CanPaste ' Paste
    Menu.Items(1).Items(3).Enabled = Document1.CanDeleteSelection ' Clear
  End Sub

#End Region

End Class