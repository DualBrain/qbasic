'Imports Microsoft.CodeAnalysis.VisualBasic
Imports QB.CodeAnalysis.Syntax

Friend Class SyntaxTreeForm

  Private m_code As String
  Private ReadOnly m_vb As Boolean

  Friend Sub New(code As String, Optional vb As Boolean = False)

    ' This call is required by the designer.
    InitializeComponent()

    ' Add any initialization after the InitializeComponent() call.

    m_vb = vb

    If vb Then
      Me.Text &= " (VB)"
      ReloadVb(code)
    Else
      Me.Text &= " (QB)"
      Reload(code)
    End If

  End Sub

  Friend Sub ReloadVb(code As String)

    Dim syntax = Microsoft.CodeAnalysis.VisualBasic.VisualBasicSyntaxTree.ParseText(code)
    Dim root = syntax.GetRoot()

    TreeView1.Nodes.Clear()
    m_code = code
    PopulateTreeviewVb(root, Nothing)
    TreeView1.Nodes.Item(0).Expand()

  End Sub

  Private Sub PopulateTreeviewVb(node As Microsoft.CodeAnalysis.VisualBasic.VisualBasicSyntaxNode, view As TreeNode)

    Dim current As TreeNode

    If view Is Nothing Then
      'Dim txt = m_code.Substring(node.FullSpan.Start, node.FullSpan.Length)
      current = TreeView1.Nodes.Add($"{node.Kind} [{node.Span.Start}..{node.Span.Start + node.Span.Length - 1}]")
    Else
      'Dim txt = m_code.Substring(node.FullSpan.Start, node.FullSpan.Length)
      current = view.Nodes.Add($"{node.Kind} [{node.Span.Start}..{node.Span.Start + node.Span.Length - 1}]")
    End If

    For Each token In node.ChildTokens
      'Dim txt = m_code.Substring(token.FullSpan.Start, token.FullSpan.Length)
      current.Nodes.Add($"tkn: {CType(token.RawKind, Microsoft.CodeAnalysis.VisualBasic.SyntaxKind)} [{token.Span.Start}..{token.Span.Length}]")
    Next

    If node.ContainsDiagnostics Then
      For Each diag In node.GetDiagnostics
        current.Nodes.Add($"{diag}")
      Next
    End If

    If node.HasLeadingTrivia Then
      For Each trivia In node.GetLeadingTrivia
        'Dim txt = m_code.Substring(trivia.FullSpan.Start, node.FullSpan.Length)
        Dim kind = CType(trivia.RawKind, Microsoft.CodeAnalysis.VisualBasic.SyntaxKind)
        Dim nn = current.Nodes.Add($"L: {kind} [{trivia.Span.Start}..{trivia.Span.Start + trivia.Span.Length - 1}]")
        nn.ForeColor = Color.DarkGray
      Next
    End If

    If node.HasTrailingTrivia Then
      For Each trivia In node.GetTrailingTrivia
        Dim kind = CType(trivia.RawKind, Microsoft.CodeAnalysis.VisualBasic.SyntaxKind)
        'Dim txt As String
        'Try
        '  txt = m_code.Substring(trivia.FullSpan.Start, node.FullSpan.Length)
        'Catch ex As Exception
        '  txt = ex.Message
        'End Try
        Dim nn = current.Nodes.Add($"T: {kind} [{trivia.Span.Start}..{trivia.Span.Start + trivia.Span.Length - 1}]")
        nn.ForeColor = Color.DarkGray
      Next
    End If

    For Each child In node.ChildNodes
      PopulateTreeviewVb(child, current)
    Next

  End Sub

  Friend Sub Reload(code As String)
    If m_vb Then ReloadVb(code) : Return
    TreeView1.Nodes.Clear()
    m_code = code
    Dim syntax = SyntaxTree.Parse(code)
    PopulateTreeview(syntax.Root, Nothing)
    TreeView1.Nodes.Item(0).Expand()
  End Sub

  Private Sub PopulateTreeview(syntax As SyntaxNode, view As TreeNode)

    Dim current As TreeNode = view

    Dim token = TryCast(syntax, SyntaxToken)

    If TypeOf syntax Is SyntaxToken Then Return

    If token IsNot Nothing Then
      For Each trivia In token.LeadingTrivia
        Dim nn = view.Nodes.Add($"L: {trivia.Kind} [{trivia.Span.Start}..{trivia.Span.Length}]")
        nn.ForeColor = Color.DarkGray
      Next
    End If

    Dim hasTrailingTrivia = token IsNot Nothing AndAlso token.TrailingTrivia.Any

    If syntax.Kind <> SyntaxKind.GlobalStatement Then

      'Dim txt = m_code.Substring(syntax.Span.Start, syntax.Span.Length)
      If view Is Nothing Then
        current = TreeView1.Nodes.Add($"{syntax.Kind} [{syntax.Span.Start}..{syntax.Span.Length}]")
      Else
        current = view.Nodes.Add($"{syntax.Kind} [{syntax.Span.Start}..{syntax.Span.Length}]")
      End If

      If token IsNot Nothing Then
        If syntax.Kind = SyntaxKind.IdentifierToken Then
          If token.Text Is Nothing Then
            Dim nn = view.Nodes.Add(" !!SN!!")
            nn.ForeColor = Color.Red
          Else
            view.Nodes.Add($" '{token.Text}':{token.Span.Length}")
          End If
        ElseIf token.Value IsNot Nothing Then
          view.Nodes.Add($" '{token.Value}' [{token.Span.Start}..{token.Span.Length}]")
        End If
      End If

      If token IsNot Nothing Then
        For Each trivia In token.TrailingTrivia

          If trivia.Kind = SyntaxKind.LineBreakTrivia Then
            Dim nn = view.Nodes.Add($"T: {trivia.Kind}")
            nn.ForeColor = Color.DarkGray
          Else
            Dim nn = view.Nodes.Add($"T: {trivia.Kind}  [{trivia.Span.Start}..{trivia.Span.Length}]")
            nn.ForeColor = Color.DarkGray
          End If

        Next
      End If

    End If

    For Each child In syntax.GetChildren
      PopulateTreeview(child, current)
    Next

  End Sub

End Class