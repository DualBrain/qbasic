Imports QB.CodeAnalysis.Syntax

Friend Class SyntaxTreeForm

  Private m_code As String
  Private m_vb As Boolean

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

    If node IsNot Nothing Then
      For Each trivia In node.GetLeadingTrivia
        Select Case trivia.RawKind
          Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOfLineTrivia
            Dim nn = view.Nodes.Add("L: EndOfLineTrivia")
            nn.ForeColor = Color.DarkGray
          Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ColonTrivia
            Dim nn = view.Nodes.Add("L: ColonTrivia")
            nn.ForeColor = Color.DarkGray
          Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhitespaceTrivia
            Dim nn = view.Nodes.Add("L: WhitespaceTrivia")
            nn.ForeColor = Color.DarkGray
          Case Else
            Dim nn = view.Nodes.Add($"L: {trivia.RawKind} '{trivia.ToFullString}'")
            nn.ForeColor = Color.DarkGray
        End Select
      Next
    End If

    Dim hasTrailingTrivia = node IsNot Nothing AndAlso node.GetTrailingTrivia.Any

    Dim current As TreeNode

    Dim txt = m_code.Substring(node.FullSpan.Start, node.FullSpan.Length)

    If view Is Nothing Then
      current = TreeView1.Nodes.Add($"{node.Kind} '{txt}'")
    Else
      current = view.Nodes.Add($"{node.Kind} '{txt}'")
    End If

    If node IsNot Nothing Then
      'Select Case node.Kind
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.None = 0
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.[List] = 1 '= GreenNode.ListKind
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EmptyStatement '= 2                       ' EmptyStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndIfStatement '= 5                       ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndUsingStatement '= 6                    ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndWithStatement '= 7                     ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndSelectStatement '= 8                   ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndStructureStatement '= 9                ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndEnumStatement '= 10                     ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndInterfaceStatement '= 11                ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndClassStatement '= 12                   ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndModuleStatement '= 13                  ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndNamespaceStatement '= 14               ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndSubStatement '= 15                     ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndFunctionStatement '= 16                ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndGetStatement '= 17                     ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndSetStatement '= 18                     ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndPropertyStatement '= 19                ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOperatorStatement '= 20                ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndEventStatement '= 21                   ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndAddHandlerStatement '= 22              ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndRemoveHandlerStatement '= 23           ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndRaiseEventStatement '= 24              ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndWhileStatement '= 25                   ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndTryStatement '= 26                     ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndSyncLockStatement '= 27                ' EndBlockStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CompilationUnit '= 38                     ' CompilationUnitSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OptionStatement '= 41                     ' OptionStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ImportsStatement '= 42                    ' ImportsStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AliasImportsClause '= 43                ' Removed.
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleImportsClause '= 44                ' SimpleImportsClauseSyntax : ImportsClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlNamespaceImportsClause '= 45           ' XmlNamespaceImportsClauseSyntax : ImportsClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NamespaceBlock '= 48                      ' NamespaceBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NamespaceStatement '= 49                  ' NamespaceStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ModuleBlock '= 50                         ' ModuleBlockSyntax : TypeBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StructureBlock '= 51                      ' StructureBlockSyntax : TypeBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterfaceBlock '= 52                      ' InterfaceBlockSyntax : TypeBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ClassBlock '= 53                          ' ClassBlockSyntax : TypeBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EnumBlock '= 54                           ' EnumBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InheritsStatement '= 57                   ' InheritsStatementSyntax : InheritsOrImplementsStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ImplementsStatement '= 58                 ' ImplementsStatementSyntax : InheritsOrImplementsStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ModuleStatement '= 59                     ' ModuleStatementSyntax : TypeStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StructureStatement '= 60                  ' StructureStatementSyntax : TypeStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterfaceStatement '= 61                  ' InterfaceStatementSyntax : TypeStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ClassStatement '= 62                      ' ClassStatementSyntax : TypeStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EnumStatement '= 63                       ' EnumStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeParameterList '= 66                   ' TypeParameterListSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeParameter '= 67                       ' TypeParameterSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeParameterSingleConstraintClause '= 70  ' TypeParameterSingleConstraintClauseSyntax : TypeParameterConstraintClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeParameterMultipleConstraintClause '= 71  ' TypeParameterMultipleConstraintClauseSyntax : TypeParameterConstraintClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NewConstraint '= 72                       ' SpecialConstraintSyntax : ConstraintSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ClassConstraint '= 73                     ' SpecialConstraintSyntax : ConstraintSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StructureConstraint '= 74                 ' SpecialConstraintSyntax : ConstraintSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeConstraint '= 75                      ' TypeConstraintSyntax : ConstraintSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EnumMemberDeclaration '= 78               ' EnumMemberDeclarationSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubBlock '= 79                            ' MethodBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FunctionBlock '= 80                       ' MethodBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConstructorBlock '= 81                    ' ConstructorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OperatorBlock '= 82                       ' OperatorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetAccessorBlock '= 83                    ' AccessorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SetAccessorBlock '= 84                    ' AccessorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddHandlerAccessorBlock '= 85                     ' AccessorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RemoveHandlerAccessorBlock '= 86                  ' AccessorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RaiseEventAccessorBlock '= 87                     ' AccessorBlockSyntax : MethodBlockBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PropertyBlock '= 88                       ' PropertyBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EventBlock '= 89                          ' EventBlockSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ParameterList '= 92                       ' ParameterListSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubStatement '= 93                        ' MethodStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FunctionStatement '= 94                   ' MethodStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubNewStatement '= 95                     ' SubNewStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DeclareSubStatement '= 96                 ' DeclareStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DeclareFunctionStatement '= 97            ' DeclareStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DelegateSubStatement '= 98                ' DelegateStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DelegateFunctionStatement '= 99           ' DelegateStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EventStatement '= 102                      ' EventStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OperatorStatement '= 103                   ' OperatorStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PropertyStatement '= 104                   ' PropertyStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetAccessorStatement '= 105                ' AccessorStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SetAccessorStatement '= 106                ' AccessorStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddHandlerAccessorStatement '= 107         ' AccessorStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RemoveHandlerAccessorStatement '= 108      ' AccessorStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RaiseEventAccessorStatement '= 111         ' AccessorStatementSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ImplementsClause '= 112                    ' ImplementsClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.HandlesClause '= 113                       ' HandlesClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.KeywordEventContainer '= 114               ' KeywordEventContainerSyntax : EventContainerSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WithEventsEventContainer '= 115            ' WithEventsEventContainerSyntax : EventContainerSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WithEventsPropertyEventContainer '= 116    ' WithEventsPropertyEventContainerSyntax : EventContainerSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.HandlesClauseItem '= 117                   ' HandlesClauseItemSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IncompleteMember '= 118                    ' IncompleteMemberSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FieldDeclaration '= 119                    ' FieldDeclarationSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.VariableDeclarator '= 122                  ' VariableDeclaratorSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleAsClause '= 123                      ' SimpleAsClauseSyntax : AsClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AsNewClause '= 124                         ' AsNewClauseSyntax : AsClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ObjectMemberInitializer '= 125             ' ObjectMemberInitializerSyntax : ObjectCreationInitializerSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ObjectCollectionInitializer '= 126         ' ObjectCollectionInitializerSyntax : ObjectCreationInitializerSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InferredFieldInitializer '= 127            ' InferredFieldInitializerSyntax : FieldInitializerSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NamedFieldInitializer '= 128               ' NamedFieldInitializerSyntax : FieldInitializerSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EqualsValue '= 129                         ' EqualsValueSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.Parameter '= 132                           ' ParameterSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ModifiedIdentifier '= 133                  ' ModifiedIdentifierSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ArrayRankSpecifier '= 134                 ' ArrayRankSpecifierSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AttributeList '= 135                      ' AttributeListSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.Attribute '= 136                          ' AttributeSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AttributeTarget '= 137                    ' AttributeTargetSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AttributesStatement '= 138                ' AttributesStatementSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExpressionStatement '= 139                ' ExpressionStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PrintStatement '= 140                     ' PrintStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhileBlock '= 141                         ' WhileBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UsingBlock '= 144                         ' UsingBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SyncLockBlock '= 145                      ' SyncLockBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WithBlock '= 146                          ' WithBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LocalDeclarationStatement '= 147          ' LocalDeclarationStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LabelStatement '= 148                     ' LabelStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GoToStatement '= 149                      ' GoToStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IdentifierLabel '= 150                    ' LabelSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NumericLabel '= 151                       ' LabelSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NextLabel '= 152                          ' LabelSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StopStatement '= 153                      ' StopOrEndStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndStatement '= 156                       ' StopOrEndStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitDoStatement '= 157                    ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitForStatement '= 158                   ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitSubStatement '= 159                   ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitFunctionStatement '= 160              ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitOperatorStatement '= 161              ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitPropertyStatement '= 162              ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitTryStatement '= 163                   ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitSelectStatement '= 164                ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitWhileStatement '= 165                 ' ExitStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ContinueWhileStatement '= 166             ' ContinueStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ContinueDoStatement '= 167                ' ContinueStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ContinueForStatement '= 168               ' ContinueStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReturnStatement '= 169                    ' ReturnStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleLineIfStatement '= 170              ' SingleLineIfStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleLineIfPart '= 171                   ' SingleLineIfPartSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleLineElseClause '= 172                 ' SingleLineElseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MultiLineIfBlock '= 173                   ' MultiLineIfBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IfPart '= 179                           ' This node was removed.
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseIfBlock '= 180                         ' ElseIfBlockSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseBlock '= 181                           ' ElseBlockSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IfStatement '= 182                        ' IfStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseIfStatement '= 183                    ' IfStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseStatement '= 184                      ' ElseStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TryBlock '= 185                           ' TryBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TryPart '= 186                            ' This node was removed.
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CatchBlock '= 187                          ' CatchBlockSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FinallyBlock '= 188                        ' FinallyBlockSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TryStatement '= 189                       ' TryStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CatchStatement '= 190                     ' CatchStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CatchFilterClause '= 191                  ' CatchFilterClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FinallyStatement '= 194                   ' FinallyStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ErrorStatement '= 195                     ' ErrorStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OnErrorGoToZeroStatement '= 196           ' OnErrorGoToStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OnErrorGoToMinusOneStatement '= 197       ' OnErrorGoToStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OnErrorGoToLabelStatement '= 198          ' OnErrorGoToStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OnErrorResumeNextStatement '= 199         ' OnErrorResumeNextStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ResumeStatement '= 200                    ' ResumeStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ResumeLabelStatement '= 201               ' ResumeStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ResumeNextStatement '= 202                ' ResumeStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SelectBlock '= 203                        ' SelectBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SelectStatement '= 204                    ' SelectStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseBlock '= 207                          ' CaseBlockSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseElseBlock '= 210                      ' CaseBlockSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseStatement '= 211                      ' CaseStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseElseStatement '= 212                  ' CaseStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseCaseClause '= 213                     ' ElseCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleCaseClause '= 214                   ' SimpleCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RangeCaseClause '= 215                    ' RangeCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseEqualsClause '= 216                   ' RelationalCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseNotEqualsClause '= 217                ' RelationalCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseLessThanClause '= 218                 ' RelationalCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseLessThanOrEqualClause '= 219          ' RelationalCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseGreaterThanOrEqualClause '= 222       ' RelationalCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseGreaterThanClause '= 223              ' RelationalCaseClauseSyntax : CaseClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SyncLockStatement '= 226                  ' SyncLockStatementSyntax : StatementSyntax
      '  'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoLoopTopTestBlock '= 227                'Removed
      '  'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoLoopBottomTestBlock '= 228             'Removed
      '    'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoLoopForeverBlock '= 229                'Removed
      '    'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoStatement '= 230                       'Removed
      '    'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LoopStatement '= 231                     'Removed
      '    'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhileClause '= 232                       'Removed
      '    'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UntilClause '= 233                       'Removed
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhileStatement '= 234                     ' WhileStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ForBlock '= 237                           ' ForBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ForEachBlock '= 238                       ' ForBlockSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ForStatement '= 239                       ' ForStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ForStepClause '= 240                      ' ForStepClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ForEachStatement '= 241                   ' ForEachStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NextStatement '= 242                      ' NextStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UsingStatement '= 243                     ' UsingStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ThrowStatement '= 246                     ' ThrowStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleAssignmentStatement '= 247          ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MidAssignmentStatement '= 248             ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddAssignmentStatement '= 249             ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubtractAssignmentStatement '= 250        ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MultiplyAssignmentStatement '= 251        ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DivideAssignmentStatement '= 252          ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IntegerDivideAssignmentStatement '= 253   ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExponentiateAssignmentStatement '= 254    ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LeftShiftAssignmentStatement '= 255       ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RightShiftAssignmentStatement '= 258      ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConcatenateAssignmentStatement '= 259     ' AssignmentStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MidExpression '= 260                      ' MidExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CallStatement '= 261                      ' CallStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddHandlerStatement '= 262                ' AddRemoveHandlerStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RemoveHandlerStatement '= 263             ' AddRemoveHandlerStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RaiseEventStatement '= 264                ' RaiseEventStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WithStatement '= 265                      ' WithStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReDimStatement '= 266                     ' ReDimStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReDimPreserveStatement '= 267             ' ReDimStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RedimClause '= 270                        ' RedimClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EraseStatement '= 271                     ' EraseStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CharacterLiteralExpression '= 272         ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TrueLiteralExpression '= 273              ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FalseLiteralExpression '= 274             ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NumericLiteralExpression '= 275           ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DateLiteralExpression '= 276              ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StringLiteralExpression '= 279            ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NothingLiteralExpression '= 280           ' LiteralExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ParenthesizedExpression '= 281            ' ParenthesizedExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MeExpression '= 282                       ' MeExpressionSyntax : InstanceExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MyBaseExpression '= 283                   ' MyBaseExpressionSyntax : InstanceExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MyClassExpression '= 284                  ' MyClassExpressionSyntax : InstanceExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetTypeExpression '= 285                  ' GetTypeExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeOfIsExpression '= 286                 ' TypeOfExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeOfIsNotExpression '= 287              ' TypeOfExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetXmlNamespaceExpression '= 290          ' GetXmlNamespaceExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleMemberAccessExpression '= 291       ' MemberAccessExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DictionaryAccessExpression '= 292         ' MemberAccessExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlElementAccessExpression '= 293         ' XmlMemberAccessExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlDescendantAccessExpression '= 294      ' XmlMemberAccessExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlAttributeAccessExpression '= 295       ' XmlMemberAccessExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InvocationExpression '= 296               ' InvocationExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ObjectCreationExpression '= 297           ' ObjectCreationExpressionSyntax : NewExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AnonymousObjectCreationExpression '= 298  ' AnonymousObjectCreationExpressionSyntax : NewExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ArrayCreationExpression '= 301            ' ArrayCreationExpressionSyntax : NewExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CollectionInitializer '= 302              ' CollectionInitializerSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CTypeExpression '= 303                    ' CTypeExpressionSyntax : CastExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DirectCastExpression '= 304               ' DirectCastExpressionSyntax : CastExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TryCastExpression '= 305                  ' TryCastExpressionSyntax : CastExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PredefinedCastExpression '= 306           ' PredefinedCastExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddExpression '= 307                      ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubtractExpression '= 308                 ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MultiplyExpression '= 309                 ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DivideExpression '= 310                   ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IntegerDivideExpression '= 311            ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExponentiateExpression '= 314             ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LeftShiftExpression '= 315                ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RightShiftExpression '= 316               ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConcatenateExpression '= 317              ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ModuloExpression '= 318                   ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EqualsExpression '= 319                   ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NotEqualsExpression '= 320                ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanExpression '= 321                 ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanOrEqualExpression '= 322          ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GreaterThanOrEqualExpression '= 323       ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GreaterThanExpression '= 324              ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IsExpression '= 325                       ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IsNotExpression '= 326                    ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LikeExpression '= 327                     ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OrExpression '= 328                       ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExclusiveOrExpression '= 329              ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AndExpression '= 330                      ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OrElseExpression '= 331                   ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AndAlsoExpression '= 332                  ' BinaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UnaryPlusExpression '= 333                ' UnaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UnaryMinusExpression '= 334               ' UnaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NotExpression '= 335                      ' UnaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddressOfExpression '= 336                ' UnaryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BinaryConditionalExpression '= 337        ' BinaryConditionalExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TernaryConditionalExpression '= 338       ' TernaryConditionalExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleLineFunctionLambdaExpression '= 339 ' SingleLineLambdaExpressionSyntax : LambdaExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleLineSubLambdaExpression '= 342      ' SingleLineLambdaExpressionSyntax : LambdaExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MultiLineFunctionLambdaExpression '= 343  ' MultiLineLambdaExpressionSyntax : LambdaExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MultiLineSubLambdaExpression '= 344       ' MultiLineLambdaExpressionSyntax : LambdaExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubLambdaHeader '= 345                    ' LambdaHeaderSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FunctionLambdaHeader '= 346               ' LambdaHeaderSyntax : MethodBaseSyntax : DeclarationStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ArgumentList '= 347                       ' ArgumentListSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OmittedArgument '= 348                    ' OmittedArgumentSyntax : ArgumentSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleArgument '= 349                     ' SimpleArgumentSyntax : ArgumentSyntax
      '  'Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NamedArgument = 350                    ' Removed
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RangeArgument '= 351                      ' RangeArgumentSyntax : ArgumentSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.QueryExpression '= 352                    ' QueryExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CollectionRangeVariable '= 353            ' CollectionRangeVariableSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExpressionRangeVariable '= 354            ' ExpressionRangeVariableSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AggregationRangeVariable '= 355           ' AggregationRangeVariableSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.VariableNameEquals '= 356                 ' VariableNameEqualsSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FunctionAggregation '= 357                ' FunctionAggregationSyntax : AggregationSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GroupAggregation '= 358                   ' GroupAggregationSyntax : AggregationSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FromClause '= 359                         ' FromClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LetClause '= 360                          ' LetClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AggregateClause '= 361                    ' AggregateClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DistinctClause '= 362                     ' DistinctClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhereClause '= 363                        ' WhereClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SkipWhileClause '= 364                    ' PartitionWhileClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TakeWhileClause '= 365                    ' PartitionWhileClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SkipClause '= 366                         ' PartitionClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TakeClause '= 367                         ' PartitionClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GroupByClause '= 368                      ' GroupByClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.JoinCondition '= 369                      ' JoinConditionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleJoinClause '= 370                   ' SimpleJoinClauseSyntax : JoinClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GroupJoinClause '= 371                    ' GroupJoinClauseSyntax : JoinClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OrderByClause '= 372                      ' OrderByClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AscendingOrdering '= 375                  ' OrderingSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DescendingOrdering '= 376                 ' OrderingSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SelectClause '= 377                       ' SelectClauseSyntax : QueryClauseSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlDocument '= 378                        ' XmlDocumentSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlDeclaration '= 379                     ' XmlDeclarationSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlDeclarationOption '= 380               ' XmlDeclarationOptionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlElement '= 381                         ' XmlElementSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlText '= 382                            ' XmlTextSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlElementStartTag '= 383                 ' XmlElementStartTagSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlElementEndTag '= 384                   ' XmlElementEndTagSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlEmptyElement '= 385                    ' XmlEmptyElementSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlAttribute '= 386                       ' XmlAttributeSyntax : BaseXmlAttributeSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlString '= 387                          ' XmlStringSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlPrefixName '= 388                      ' XmlPrefixNameSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlName '= 389                            ' XmlNameSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlBracketedName '= 390                   ' XmlBracketedNameSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlPrefix '= 391                          ' XmlPrefixSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlComment '= 392                         ' XmlCommentSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlProcessingInstruction '= 393           ' XmlProcessingInstructionSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlCDataSection '= 394                    ' XmlCDataSectionSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlEmbeddedExpression '= 395              ' XmlEmbeddedExpressionSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ArrayType '= 396                          ' ArrayTypeSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NullableType '= 397                       ' NullableTypeSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PredefinedType '= 398                     ' PredefinedTypeSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IdentifierName '= 399                     ' IdentifierNameSyntax : SimpleNameSyntax : NameSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GenericName '= 400                        ' GenericNameSyntax : SimpleNameSyntax : NameSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.QualifiedName '= 401                      ' QualifiedNameSyntax : NameSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GlobalName '= 402                         ' GlobalNameSyntax : NameSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeArgumentList '= 403                   ' TypeArgumentListSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CrefReference '= 404                      ' CrefReferenceSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CrefSignature '= 407                      ' CrefSignatureSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CrefSignaturePart '= 408                  ' CrefSignaturePartSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CrefOperatorReference '= 409              ' CrefOperatorReferenceSyntax : NameSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.QualifiedCrefOperatorReference '= 410     ' QualifiedCrefOperatorReferenceSyntax : NameSyntax : TypeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.YieldStatement '= 411                     ' YieldStatementSyntax : ExecutableStatementSyntax : StatementSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AwaitExpression '= 412                    ' AwaitExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddHandlerKeyword '= 413                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AddressOfKeyword '= 414                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AliasKeyword '= 415                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AndKeyword '= 416                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AndAlsoKeyword '= 417                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AsKeyword '= 418                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BooleanKeyword '= 421                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ByRefKeyword '= 422                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ByteKeyword '= 423                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ByValKeyword '= 424                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CallKeyword '= 425                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaseKeyword '= 426                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CatchKeyword '= 427                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CBoolKeyword '= 428                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CByteKeyword '= 429                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CCharKeyword '= 432                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CDateKeyword '= 433                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CDecKeyword '= 434                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CDblKeyword '= 435                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CharKeyword '= 436                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CIntKeyword '= 437                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ClassKeyword '= 438                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CLngKeyword '= 439                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CObjKeyword '= 440                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConstKeyword '= 441                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReferenceKeyword '= 442                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ContinueKeyword '= 443                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CSByteKeyword '= 444                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CShortKeyword '= 445                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CSngKeyword '= 446                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CStrKeyword '= 447                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CTypeKeyword '= 448                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CUIntKeyword '= 449                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CULngKeyword '= 450                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CUShortKeyword '= 453                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DateKeyword '= 454                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DecimalKeyword '= 455                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DeclareKeyword '= 456                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DefaultKeyword '= 457                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DelegateKeyword '= 458                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DimKeyword '= 459                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DirectCastKeyword '= 460                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoKeyword '= 461                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoubleKeyword '= 462                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EachKeyword '= 463                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseKeyword '= 464                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseIfKeyword '= 465                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndKeyword '= 466                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EnumKeyword '= 467                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EraseKeyword '= 468                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ErrorKeyword '= 469                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EventKeyword '= 470                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExitKeyword '= 471                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FalseKeyword '= 474                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FinallyKeyword '= 475                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ForKeyword '= 476                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FriendKeyword '= 477                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FunctionKeyword '= 478                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetKeyword '= 479                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetTypeKeyword '= 480                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GetXmlNamespaceKeyword '= 481             ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GlobalKeyword '= 482                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GoToKeyword '= 483                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.HandlesKeyword '= 484                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IfKeyword '= 485                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ImplementsKeyword '= 486                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ImportsKeyword '= 487                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InKeyword '= 488                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InheritsKeyword '= 489                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IntegerKeyword '= 490                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterfaceKeyword '= 491                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IsKeyword '= 492                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IsNotKeyword '= 495                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LetKeyword '= 496                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LibKeyword '= 497                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LikeKeyword '= 498                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LongKeyword '= 499                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LoopKeyword '= 500                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MeKeyword '= 501                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ModKeyword '= 502                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ModuleKeyword '= 503                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MustInheritKeyword '= 504                 ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MustOverrideKeyword '= 505                ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MyBaseKeyword '= 506                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MyClassKeyword '= 507                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NamespaceKeyword '= 508                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NarrowingKeyword '= 509                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NextKeyword '= 510                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NewKeyword '= 511                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NotKeyword '= 512                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NothingKeyword '= 513                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NotInheritableKeyword '= 516              ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NotOverridableKeyword '= 517              ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ObjectKeyword '= 518                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OfKeyword '= 519                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OnKeyword '= 520                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OperatorKeyword '= 521                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OptionKeyword '= 522                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OptionalKeyword '= 523                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OrKeyword '= 524                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OrElseKeyword '= 525                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OverloadsKeyword '= 526                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OverridableKeyword '= 527                 ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OverridesKeyword '= 528                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ParamArrayKeyword '= 529                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PartialKeyword '= 530                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PrivateKeyword '= 531                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PropertyKeyword '= 532                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ProtectedKeyword '= 533                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PublicKeyword '= 534                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RaiseEventKeyword '= 537                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReadOnlyKeyword '= 538                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReDimKeyword '= 539                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.REMKeyword '= 540                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RemoveHandlerKeyword '= 541               ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ResumeKeyword '= 542                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReturnKeyword '= 543                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SByteKeyword '= 544                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SelectKeyword '= 545                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SetKeyword '= 546                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ShadowsKeyword '= 547                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SharedKeyword '= 548                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ShortKeyword '= 549                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleKeyword '= 550                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StaticKeyword '= 551                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StepKeyword '= 552                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StopKeyword '= 553                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StringKeyword '= 554                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StructureKeyword '= 555                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SubKeyword '= 558                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SyncLockKeyword '= 559                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ThenKeyword '= 560                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ThrowKeyword '= 561                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ToKeyword '= 562                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TrueKeyword '= 563                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TryKeyword '= 564                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TryCastKeyword '= 565                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeOfKeyword '= 566                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UIntegerKeyword '= 567                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ULongKeyword '= 568                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UShortKeyword '= 569                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UsingKeyword '= 570                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhenKeyword '= 571                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhileKeyword '= 572                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WideningKeyword '= 573                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WithKeyword '= 574                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WithEventsKeyword '= 575                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WriteOnlyKeyword '= 578                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XorKeyword '= 579                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndIfKeyword '= 580                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GosubKeyword '= 581                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.VariantKeyword '= 582                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WendKeyword '= 583                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AggregateKeyword '= 584                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AllKeyword '= 585                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AnsiKeyword '= 586                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AscendingKeyword '= 587                   ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AssemblyKeyword '= 588                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AutoKeyword '= 589                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BinaryKeyword '= 590                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ByKeyword '= 591                          ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CompareKeyword '= 592                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CustomKeyword '= 593                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DescendingKeyword '= 594                  ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DisableKeyword '= 595                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DistinctKeyword '= 596                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EnableKeyword '= 599                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EqualsKeyword '= 600                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExplicitKeyword '= 601                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExternalSourceKeyword '= 602              ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExternalChecksumKeyword '= 603            ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FromKeyword '= 604                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GroupKeyword '= 605                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InferKeyword '= 606                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IntoKeyword '= 607                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IsFalseKeyword '= 608                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IsTrueKeyword '= 609                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.JoinKeyword '= 610                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.KeyKeyword '= 611                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MidKeyword '= 612                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OffKeyword '= 613                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OrderKeyword '= 614                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OutKeyword '= 615                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PreserveKeyword '= 616                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RegionKeyword '= 617                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SkipKeyword '= 620                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StrictKeyword '= 621                      ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TakeKeyword '= 622                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TextKeyword '= 623                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UnicodeKeyword '= 624                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UntilKeyword '= 625                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WarningKeyword '= 626                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhereKeyword '= 627                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypeKeyword '= 628                        ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlKeyword '= 629                         ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AsyncKeyword '= 630                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AwaitKeyword '= 631                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IteratorKeyword '= 632                    ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.YieldKeyword '= 633                       ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExclamationToken '= 634                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AtToken '= 635                            ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CommaToken '= 636                         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.HashToken '= 637                          ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AmpersandToken '= 638                     ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SingleQuoteToken '= 641                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OpenParenToken '= 642                     ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CloseParenToken '= 643                    ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.OpenBraceToken '= 644                     ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CloseBraceToken '= 645                    ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SemicolonToken '= 646                     ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AsteriskToken '= 647                      ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PlusToken '= 648                          ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MinusToken '= 649                         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DotToken '= 650                           ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SlashToken '= 651                         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ColonToken '= 652                         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanToken '= 653                      ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanEqualsToken '= 654                ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanGreaterThanToken '= 655           ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EqualsToken '= 656                        ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GreaterThanToken '= 657                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GreaterThanEqualsToken '= 658             ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BackslashToken '= 659                     ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaretToken '= 662                         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ColonEqualsToken '= 663                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AmpersandEqualsToken '= 664               ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.AsteriskEqualsToken '= 665                ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PlusEqualsToken '= 666                    ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MinusEqualsToken '= 667                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SlashEqualsToken '= 668                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BackslashEqualsToken '= 669               ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CaretEqualsToken '= 670                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanLessThanToken '= 671              ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GreaterThanGreaterThanToken '= 672        ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanLessThanEqualsToken '= 673        ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.GreaterThanGreaterThanEqualsToken '= 674  ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.QuestionToken '= 675                      ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoubleQuoteToken '= 676                   ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StatementTerminatorToken '= 677           ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOfFileToken '= 678                     ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EmptyToken '= 679                         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SlashGreaterThanToken '= 680              ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanSlashToken '= 683                 ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanExclamationMinusMinusToken '= 684 ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.MinusMinusGreaterThanToken '= 685         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanQuestionToken '= 686              ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.QuestionGreaterThanToken '= 687           ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LessThanPercentEqualsToken '= 688         ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.PercentGreaterThanToken '= 689            ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BeginCDataToken '= 690                    ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndCDataToken '= 691                      ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOfXmlToken '= 692                      ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BadToken '= 693                           ' BadTokenSyntax : PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlNameToken '= 694                       ' XmlNameTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlTextLiteralToken '= 695                ' XmlTextTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlEntityLiteralToken '= 696              ' XmlTextTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DocumentationCommentLineBreakToken '= 697 ' XmlTextTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IdentifierToken '= 700                    ' IdentifierTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IntegerLiteralToken '= 701                ' IntegerLiteralTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.FloatingLiteralToken '= 702               ' FloatingLiteralTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DecimalLiteralToken '= 703                ' DecimalLiteralTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DateLiteralToken '= 704                   ' DateLiteralTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.StringLiteralToken '= 705                 ' StringLiteralTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CharacterLiteralToken '= 706              ' CharacterLiteralTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SkippedTokensTrivia '= 709                ' SkippedTokensTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DocumentationCommentTrivia '= 710         ' DocumentationCommentTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlCrefAttribute '= 711                   ' XmlCrefAttributeSyntax : BaseXmlAttributeSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.XmlNameAttribute '= 712                   ' XmlNameAttributeSyntax : BaseXmlAttributeSyntax : XmlNodeSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConditionalAccessExpression '= 713        ' ConditionalAccessExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhitespaceTrivia '= 729                   ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOfLineTrivia '= 730                    ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ColonTrivia '= 731                        ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.CommentTrivia '= 732                      ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LineContinuationTrivia '= 733             ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DocumentationCommentExteriorTrivia '= 734 ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DisabledTextTrivia '= 735                 ' SyntaxTrivia
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConstDirectiveTrivia '= 736               ' ConstDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IfDirectiveTrivia '= 737                  ' IfDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseIfDirectiveTrivia '= 738              ' IfDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ElseDirectiveTrivia '= 739                ' ElseDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndIfDirectiveTrivia '= 740               ' EndIfDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.RegionDirectiveTrivia '= 741              ' RegionDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndRegionDirectiveTrivia '= 744           ' EndRegionDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExternalSourceDirectiveTrivia '= 745      ' ExternalSourceDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndExternalSourceDirectiveTrivia '= 746   ' EndExternalSourceDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ExternalChecksumDirectiveTrivia '= 747    ' ExternalChecksumDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EnableWarningDirectiveTrivia '= 748       ' EnableWarningDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DisableWarningDirectiveTrivia '= 749      ' DisableWarningDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ReferenceDirectiveTrivia '= 750           ' ReferenceDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.BadDirectiveTrivia '= 753                 ' BadDirectiveTriviaSyntax : DirectiveTriviaSyntax : StructuredTriviaSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ImportAliasClause '= 754                   ' ImportAliasClauseSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NameColonEquals '= 755
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleDoLoopBlock '= 756                 ' DoLoopBlockSyntax : ExecutableStatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoWhileLoopBlock '= 757                 ' DoLoopBlockSyntax : ExecutableStatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoUntilLoopBlock '= 758                 ' DoLoopBlockSyntax : ExecutableStatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoLoopWhileBlock '= 759                 ' DoLoopBlockSyntax : ExecutableStatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoLoopUntilBlock '= 760                 ' DoLoopBlockSyntax : ExecutableStatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleDoStatement '= 770                 ' DoStatement : StatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoWhileStatement '= 771                 ' DoStatement : StatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DoUntilStatement '= 772                 ' DoStatement : StatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.SimpleLoopStatement '= 773               ' LoopStatement : StatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LoopWhileStatement '= 774               ' LoopStatement : StatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.LoopUntilStatement '= 775               ' LoopStatement : StatementSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhileClause '= 776                       ' WhileOrUntilClause : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.UntilClause '= 777                       ' WhileOrUntilClause : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NameOfKeyword '= 778                     ' KeywordSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NameOfExpression '= 779                  ' NameOfExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterpolatedStringExpression '= 780                              ' InterpolatedStringExpressionSyntax : ExpressionSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterpolatedStringText '= 781                                    ' InterpolatedStringTextSyntax : InterpolatedStringContentSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.Interpolation '= 782                                             ' InterpolationSyntax : InterpolatedStringContentSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterpolationAlignmentClause '= 783                              ' InterpolationAlignmentClauseSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterpolationFormatClause '= 784                                 ' InterpolationFormatClauseSyntax : VisualBasicSyntaxNode
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.DollarSignDoubleQuoteToken '= 785                                ' DollarSignDoubleQuoteTokenSyntax : PunctuationSyntax
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.InterpolatedStringTextToken '= 786                               ' InterpolatedStringTextTokenSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOfInterpolatedStringToken '= 787                              ' PunctuationSyntax : SyntaxToken
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TupleExpression '= 788
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TupleType '= 789
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.TypedTupleElement '= 790
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.NamedTupleElement '= 791
      '  Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ConflictMarkerTrivia '= 792
      'End Select

      If node.Kind = Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.IdentifierToken Then
        If node.ToString Is Nothing Then
          Dim nn = view.Nodes.Add(" !!SN!!")
          nn.ForeColor = Color.Red
        Else
          view.Nodes.Add($" '{node.ToString}':{node.Span.Length}")
        End If
        'ElseIf token.Value IsNot Nothing Then
        '  view.Nodes.Add($" '{token.Value}':{token.Span.Length}")
      End If
    End If

    If node IsNot Nothing Then
      For Each trivia In node.GetTrailingTrivia

        Select Case trivia.RawKind
          Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.EndOfLineTrivia
            Dim nn = view.Nodes.Add("T: EndOfLineTrivia")
            nn.ForeColor = Color.DarkGray
          Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.ColonTrivia
            Dim nn = view.Nodes.Add("T: ColonTrivia")
            nn.ForeColor = Color.DarkGray
          Case Microsoft.CodeAnalysis.VisualBasic.SyntaxKind.WhitespaceTrivia
            Dim nn = view.Nodes.Add("T: WhitespaceTrivia")
            nn.ForeColor = Color.DarkGray
          Case Else
            Dim nn = view.Nodes.Add($"T: {trivia.RawKind} '{trivia.ToString}'")
            nn.ForeColor = Color.DarkGray
        End Select

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

    Dim token = TryCast(syntax, SyntaxToken)

    If TypeOf syntax Is SyntaxToken Then Return

    If token IsNot Nothing Then
      For Each trivia In token.LeadingTrivia
        Dim nn = view.Nodes.Add($"L: {trivia.Kind} '{trivia.Text}'")
        nn.ForeColor = Color.DarkGray
      Next
    End If

    Dim hasTrailingTrivia = token IsNot Nothing AndAlso token.TrailingTrivia.Any

    Dim current As TreeNode

    Dim txt = m_code.Substring(syntax.Span.Start, syntax.Span.Length)
    If view Is Nothing Then
      current = TreeView1.Nodes.Add($"{syntax.Kind} '{txt}'")
    Else
      current = view.Nodes.Add($"{syntax.Kind} '{txt}'")
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
        view.Nodes.Add($" '{token.Value}':{token.Span.Length}")
      End If
    End If

    If token IsNot Nothing Then
      For Each trivia In token.TrailingTrivia

        If trivia.Kind = SyntaxKind.LineBreakTrivia Then
          Dim nn = view.Nodes.Add($"T: {trivia.Kind}")
          nn.ForeColor = Color.DarkGray
        Else
          Dim nn = view.Nodes.Add($"T: {trivia.Kind} '{trivia.Text}'")
          nn.ForeColor = Color.DarkGray
        End If

      Next
    End If

    For Each child In syntax.GetChildren
      PopulateTreeview(child, current)
    Next

  End Sub

End Class