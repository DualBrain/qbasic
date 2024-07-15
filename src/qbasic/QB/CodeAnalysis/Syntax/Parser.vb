Imports System.Collections.Immutable

Namespace Global.QB.CodeAnalysis.Syntax

  Friend NotInheritable Class Parser

#Region "Overall Scope/Structure"

    Private ReadOnly m_diagnostics As New DiagnosticBag
    Private ReadOnly m_syntaxTree As SyntaxTree
    Private ReadOnly m_text As Text.SourceText
    Private ReadOnly m_tokens As ImmutableArray(Of SyntaxToken)

    Private m_position As Integer

    Public Sub New(tree As SyntaxTree) 'text As SourceText)

      Dim tokens = New List(Of SyntaxToken)
      Dim badTokens = New List(Of SyntaxToken)
      Dim lexer = New Lexer(tree)
      Dim token As SyntaxToken
      'Do

      '  token = lexer.Lex

      '  If token.Kind <> SyntaxKind.WhitespaceToken AndAlso token.Kind <> SyntaxKind.BadToken Then
      '    tokens.Add(token)
      '  End If

      'Loop While token.Kind <> SyntaxKind.EndOfFileToken

      'm_text = text
      'm_tokens = tokens.ToImmutableArray
      'm_diagnostics.AddRange(lexer.Diagnostics)

      Do

        token = lexer.Lex

        If token.Kind = SyntaxKind.BadToken Then
          badTokens.Add(token)
        Else
          If badTokens.Count > 0 Then
            Dim leadingTrivia = token.LeadingTrivia.ToBuilder
            Dim index = 0
            For Each badToken In badTokens
              For Each lt In badToken.LeadingTrivia
                leadingTrivia.Insert(index, lt) : index += 1
              Next
              Dim trivia = New SyntaxTrivia(tree, SyntaxKind.SkippedTextTrivia, badToken.Position, badToken.Text)
              leadingTrivia.Insert(index, trivia) : index += 1
              For Each tt In badToken.TrailingTrivia
                leadingTrivia.Insert(index, tt) : index += 1
              Next
            Next
            badTokens.Clear()
            token = New SyntaxToken(token.SyntaxTree, token.Kind, token.Position, token.Text, token.Value, leadingTrivia.ToImmutable, token.TrailingTrivia)
          End If
          tokens.Add(token)
        End If

      Loop While token.Kind <> SyntaxKind.EndOfFileToken
      m_syntaxTree = tree
      m_text = tree.Text
      m_diagnostics.AddRange(lexer.Diagnostics)
      m_tokens = tokens.ToImmutableArray

    End Sub

    Public ReadOnly Property Diagnostics As DiagnosticBag
      Get
        Return m_diagnostics
      End Get
    End Property

    Public Function ParseCompilationUnit() As CompilationUnitSyntax
      Dim members = ParseMembers()
      Dim endOfFileToken = MatchToken(SyntaxKind.EndOfFileToken)
      Return New CompilationUnitSyntax(m_syntaxTree, members, endOfFileToken)
    End Function

    Private Function ParseMembers() As ImmutableArray(Of MemberSyntax)

      Dim members = ImmutableArray.CreateBuilder(Of MemberSyntax)()

      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim startToken = Current()

        Dim member = ParseMember()
        members.Add(member)

        ' If ParseMember() did not consume any tokens,
        ' we need to skip the current token and continue
        ' in order to avoid an infinite loop.
        '
        ' We don't need to report an error, because we'll
        ' already tried to parse an expression statement
        ' and reported one.
        If Current() Is startToken Then
          NextToken()
        End If

      End While

      Return members.ToImmutable()

    End Function

    Private Function ParseMember() As MemberSyntax
      If Current.Kind = SyntaxKind.FunctionKeyword Then Return ParseFunctionDeclaration()
      If Current.Kind = SyntaxKind.DefKeyword AndAlso Peek(1).Text?.ToLower?.StartsWith("fn") Then Return ParseDefFnDeclaration()
      Return ParseGlobalStatement()
    End Function

    Private Function ParseGlobalStatement() As MemberSyntax
      Dim statement = ParseStatement(True)
      Return New GlobalStatementSyntax(m_syntaxTree, statement)
    End Function

#End Region

    Private Function ParseStatement(isTopLevel As Boolean) As StatementSyntax

      Select Case Current.Kind

#Region "SpecBAS"
        Case SyntaxKind.AutoKeyword : Return ParseSbAutoStatement()
        Case SyntaxKind.BorderKeyword : Return ParseSbBorderStatement()
        Case SyntaxKind.GoKeyword : Return ParseSbGoStatement()
        Case SyntaxKind.IncKeyword : Return ParseIncSbStatement()
        Case SyntaxKind.InkKeyword : Return ParseSbInkStatement()
        Case SyntaxKind.OverKeyword : Return ParseSbOverStatement()
        Case SyntaxKind.PaperKeyword : Return ParseSbPaperStatement()
        Case SyntaxKind.ProgKeyword : Return ParseSbProgStatement()
        Case SyntaxKind.ZxAsciiKeyword : Return ParseSbZxAsciiStatement()
#End Region

#Region "QBasic"

        Case SyntaxKind.OpenBraceToken : Return ParseBlockStatement(isTopLevel)

        Case SyntaxKind.BeepKeyword : Return ParseBeepStatement()
        Case SyntaxKind.BLoadKeyword : Return ParseBloadStatement()
        Case SyntaxKind.BSaveKeyword : Return ParseBsaveStatement()
        Case SyntaxKind.CallKeyword : Return ParseCallStatement()
        Case SyntaxKind.ChainKeyword : Return ParseChainStatement()
        Case SyntaxKind.ChDirKeyword : Return ParseChDirStatement()
        Case SyntaxKind.CircleKeyword : Return ParseCircleStatement()
        Case SyntaxKind.ClearKeyword : Return ParseClearStatement()
        Case SyntaxKind.CloseKeyword : Return ParseCloseStatement()
        Case SyntaxKind.ClsKeyword : Return ParseClsStatement()
        Case SyntaxKind.ColonToken : Return ParseStatementSeparatorSyntax()
        Case SyntaxKind.ColorKeyword : Return ParseColorStatement()
        Case SyntaxKind.ComKeyword : Return ParseComStatement()
        Case SyntaxKind.CommonKeyword : Return ParseCommonStatement()
        Case SyntaxKind.ConstKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.ContinueKeyword : Return ParseContinueStatement()
        Case SyntaxKind.DataKeyword : Return ParseDataStatement()
        Case SyntaxKind.DateKeyword : Return ParseDateStatement()
        Case SyntaxKind.DeclareKeyword : Return ParseDeclareStatement()
        Case SyntaxKind.DefKeyword : Return ParseDefStatement()
        Case SyntaxKind.DefDblKeyword : Return ParseDefTypeStatement()
        Case SyntaxKind.DefIntKeyword : Return ParseDefTypeStatement()
        Case SyntaxKind.DefLngKeyword : Return ParseDefTypeStatement()
        Case SyntaxKind.DefSngKeyword : Return ParseDefTypeStatement()
        Case SyntaxKind.DefStrKeyword : Return ParseDefTypeStatement()
        Case SyntaxKind.DimKeyword : Return ParseVariableDeclaration()
        Case SyntaxKind.DoKeyword : Return ParseDoStatement(isTopLevel)
        Case SyntaxKind.DrawKeyword : Return ParseDrawStatement()
        Case SyntaxKind.EndKeyword : Return ParseEndStatement()
        Case SyntaxKind.EnvironKeyword : Return ParseEnvironStatement()
        Case SyntaxKind.EraseKeyword : Return ParseEraseStatement()
        Case SyntaxKind.ErrorKeyword : Return ParseErrorStatement()
        Case SyntaxKind.ExitKeyword : Return ParseExitStatement()
        Case SyntaxKind.FieldKeyword : Return ParseFieldStatement()
        Case SyntaxKind.FilesKeyword : Return ParseFilesStatement()
        Case SyntaxKind.ForKeyword : Return ParseForStatement(isTopLevel)
        Case SyntaxKind.FunctionKeyword : Return ParseFunctionStatement()
        Case SyntaxKind.GetKeyword : Return ParseGetStatement()
        Case SyntaxKind.GosubKeyword : Return ParseGosubStatement()
        Case SyntaxKind.GotoKeyword : Return ParseGotoStatement()
        Case SyntaxKind.IfKeyword : Return ParseIfStatement(isTopLevel)
        Case SyntaxKind.InputKeyword : Return ParseInputStatement()
        Case SyntaxKind.IoCtlKeyword : Return ParseIoCtlStatement()
        Case SyntaxKind.KeyKeyword : Return ParseKeyStatement()
        Case SyntaxKind.KillKeyword : Return ParseKillStatement()
        Case SyntaxKind.Label : Return ParseLabelStatement()
        Case SyntaxKind.LetKeyword : Return ParseLetStatement()
        Case SyntaxKind.LineKeyword : Return ParseLineStatement()
        Case SyntaxKind.LocateKeyword : Return ParseLocateStatement()
        Case SyntaxKind.LockKeyword : Return ParseLockStatement()
        Case SyntaxKind.LPrintKeyword : Return ParseLprintStatement()
        Case SyntaxKind.LSetKeyword : Return ParseLSetStatement()
        Case SyntaxKind.MkDirKeyword : Return ParseMkDirStatement()
        Case SyntaxKind.MidKeyword : Return ParseMidStatement()
        Case SyntaxKind.NameKeyword : Return ParseNameStatement()
        Case SyntaxKind.OnKeyword : Return ParseOnStatement()
        Case SyntaxKind.OpenKeyword : Return ParseOpenStatement()
        Case SyntaxKind.OptionKeyword : Return ParseOptionStatement()
        Case SyntaxKind.OutKeyword : Return ParseOutStatement()
        Case SyntaxKind.PaintKeyword : Return ParsePaintStatement()
        Case SyntaxKind.PaletteKeyword : Return ParsePaletteStatement()
        Case SyntaxKind.PCopyKeyword : Return ParsePcopyStatement()
        Case SyntaxKind.PenKeyword : Return ParsePenStatement()
        Case SyntaxKind.PlayKeyword : Return ParsePlayStatement()
        Case SyntaxKind.PokeKeyword : Return ParsePokeStatement()
        Case SyntaxKind.PrintKeyword : Return ParsePrintStatement()
        Case SyntaxKind.PsetKeyword : Return ParsePsetStatement()
        Case SyntaxKind.PresetKeyword : Return ParsePresetStatement()
        Case SyntaxKind.PutKeyword : Return ParsePutStatement()
        Case SyntaxKind.RandomizeKeyword : Return ParseRandomizeStatement()
        Case SyntaxKind.ReadKeyword : Return ParseReadStatement()
        Case SyntaxKind.RedimKeyword : Return ParseRedimStatement()
        Case SyntaxKind.RemKeyword : Return ParseRemStatement()
        Case SyntaxKind.ResetKeyword : Return ParseResetStatement()
        Case SyntaxKind.RestoreKeyword : Return ParseRestoreStatement()
        Case SyntaxKind.ResumeKeyword : Return ParseResumeStatement()
        Case SyntaxKind.ReturnKeyword : Return ParseReturnStatement(isTopLevel)
        Case SyntaxKind.RmDirKeyword : Return ParseRmDirStatement()
        Case SyntaxKind.RSetKeyword : Return ParseRSetStatement()
        Case SyntaxKind.RunKeyword : Return ParseRunStatement()
        Case SyntaxKind.ScreenKeyword : Return ParseScreenStatement()
        Case SyntaxKind.SeekKeyword : Return ParseSeekStatement()
        Case SyntaxKind.SelectKeyword : Return ParseSelectCaseStatement()
        Case SyntaxKind.SharedKeyword : Return ParseSharedStatement()
        Case SyntaxKind.ShellKeyword : Return ParseShellStatement()
        Case SyntaxKind.SleepKeyword : Return ParseSleepStatement()
        Case SyntaxKind.SoundKeyword : Return ParseSoundStatement()
        Case SyntaxKind.StaticKeyword : Return ParseStaticStatement()
        Case SyntaxKind.StopKeyword : Return ParseStopStatement()
        Case SyntaxKind.StrigKeyword : Return ParseStrigStatement()
        Case SyntaxKind.SubKeyword : Return ParseSubStatement()
        Case SyntaxKind.SwapKeyword : Return ParseSwapStatement()
        Case SyntaxKind.SystemKeyword : Return ParseSystemStatement()
        Case SyntaxKind.TimeKeyword : Return ParseTimeStatement()
        Case SyntaxKind.TimerKeyword : Return ParseTimerStatement()
        Case SyntaxKind.TroffKeyword : Return ParseTroffStatement()
        Case SyntaxKind.TronKeyword : Return ParseTronStatement()
        Case SyntaxKind.TypeKeyword : Return ParseTypeStatement()
        Case SyntaxKind.UnlockKeyword : Return ParseUnlockStatement()
        Case SyntaxKind.ViewKeyword : Return ParseViewStatement()
        Case SyntaxKind.WaitKeyword : Return ParseWaitStatement()
        Case SyntaxKind.WhileKeyword : Return ParseWhileStatement(isTopLevel)
        Case SyntaxKind.WidthKeyword : Return ParseWidthStatement()
        Case SyntaxKind.WindowKeyword : Return ParseWindowStatement()
        Case SyntaxKind.WriteKeyword : Return ParseWriteStatement()
#End Region

        Case Else

          If Peek(0).Kind = SyntaxKind.IdentifierToken Then
            Dim identifier = ParseIdentifier() 'MatchToken(SyntaxKind.IdentifierToken)
            If Current.Kind = SyntaxKind.EqualToken Then
              ' *identifier* = *expression*
              Dim equalsToken = MatchToken(SyntaxKind.EqualToken)
              Dim expression = ParseExpression()
              Return New ExpressionStatementSyntax(m_syntaxTree, New AssignmentExpressionSyntax(m_syntaxTree, identifier, equalsToken, expression))
            Else
              ' *identifier* [ *expression1* [, *expression2*] ... ]
              Dim expressions = If(Not IsEndOfStatement(), ParseArguments(), Nothing)
              Return New CallStatementSyntax(m_syntaxTree, Nothing, identifier.Identifier, Nothing, expressions, Nothing)
            End If
          End If

          Return ParseExpressionStatement()

      End Select

    End Function

#Region "SpecBAS"

    Private Function ParseSbAtStatement() As AtStatementSyntax

      ' ... AT *row*, *col* ; ...

      Dim atKeyword = MatchToken(SyntaxKind.AtKeyword)
      Dim rowExpression = ParseExpression()
      Dim commaToken = MatchToken(SyntaxKind.CommaToken)
      Dim colExpression = ParseExpression()
      Return New AtStatementSyntax(m_syntaxTree, atKeyword, rowExpression, commaToken, colExpression)

    End Function

    Private Function ParseSbAutoStatement() As AutoStatementSyntax

      ' AUTO *value*

      Dim autoKeyword = MatchToken(SyntaxKind.AutoKeyword)
      Dim expression = ParseExpression()
      Return New AutoStatementSyntax(m_syntaxTree, autoKeyword, expression)

    End Function

    Private Function ParseSbBorderStatement() As BorderStatementSyntax

      ' BORDER *value*

      Dim borderKeyword = MatchToken(SyntaxKind.BorderKeyword)
      Dim expression = ParseExpression()
      Return New BorderStatementSyntax(m_syntaxTree, borderKeyword, expression)

    End Function

    Private Function ParseSbCircleStatement() As SbCircleStatementSyntax

      ' CIRCLE [colour-item;]x,y,r[,fill$]

      Dim circleKeyword = MatchToken(SyntaxKind.CircleKeyword)
      Dim optionalInkStatement As InkStatementSyntax = Nothing
      Dim optionalSemicolonToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.InkKeyword Then
        optionalInkStatement = ParseSbInkStatement()
        optionalSemicolonToken = MatchToken(SyntaxKind.SemicolonToken)
      End If
      Dim xExpression = ParseExpression()
      Dim commaToken1 = MatchToken(SyntaxKind.CommaToken)
      Dim yExpression = ParseExpression()
      Dim commaToken2 = MatchToken(SyntaxKind.CommaToken)
      Dim rExpression = ParseExpression()
      Dim optionalFillKeyword As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.FillKeyword Then
        optionalFillKeyword = MatchToken(SyntaxKind.FillKeyword)
      End If
      Return New SbCircleStatementSyntax(m_syntaxTree,
                                       circleKeyword,
                                       optionalInkStatement,
                                       optionalSemicolonToken,
                                       xExpression,
                                       commaToken1,
                                       yExpression,
                                       commaToken2,
                                       rExpression,
                                       optionalFillKeyword)

    End Function

    Private Function ParseSbGoStatement() As StatementSyntax
      Dim goKeyword = MatchToken(SyntaxKind.GoKeyword)
      If Current.Kind = SyntaxKind.SubKeyword Then
        Dim subKeyword = MatchToken(SyntaxKind.SubKeyword)
        If Current.Kind = SyntaxKind.NumberToken Then
          Dim numberToken = MatchToken(SyntaxKind.NumberToken)
          Return New SbGosubStatementSyntax(m_syntaxTree, goKeyword, subKeyword, numberToken)
        Else
          Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
          Return New SbGosubStatementSyntax(m_syntaxTree, goKeyword, subKeyword, identifierToken)
        End If
      Else 'If Current.Kind = SyntaxKind.ToKeyword Then
        Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
        If Current.Kind = SyntaxKind.NumberToken Then
          Dim numberToken = MatchToken(SyntaxKind.NumberToken)
          Return New SbGotoStatementSyntax(m_syntaxTree, goKeyword, toKeyword, numberToken)
        Else
          Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
          Return New SbGotoStatementSyntax(m_syntaxTree, goKeyword, toKeyword, identifierToken)
        End If
      End If
    End Function

    Private Function ParseIncSbStatement() As IncStatementSyntax

      ' INC numvar[,amount[,start TO end]]

      ' INC pozx,b
      ' INC ov,0 TO 10
      ' INC bsize

      Dim incKeyword = MatchToken(SyntaxKind.IncKeyword)
      Dim identifier = ParseIdentifier()
      Dim optionalComma1 As SyntaxToken = Nothing
      Dim optionalAmount As ExpressionSyntax = Nothing
      Dim optionalComma2 As SyntaxToken = Nothing
      Dim optionalStart As ExpressionSyntax = Nothing
      Dim optionalToKeyword As SyntaxToken = Nothing
      Dim optionalEnd As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalComma1 = MatchToken(SyntaxKind.CommaToken)
        If Not Current.Kind = SyntaxKind.CommaToken Then
          optionalAmount = ParseExpression()
        End If
        If Current.Kind = SyntaxKind.CommaToken Then
          optionalComma2 = MatchToken(SyntaxKind.CommaToken)
          optionalStart = ParseExpression()
          optionalToKeyword = MatchToken(SyntaxKind.ToKeyword)
          optionalEnd = ParseExpression()
        ElseIf Current.Kind = SyntaxKind.ToKeyword Then
          optionalComma2 = optionalComma1 : optionalComma1 = Nothing
          optionalStart = optionalAmount : optionalAmount = Nothing
          optionalToKeyword = MatchToken(SyntaxKind.ToKeyword)
          optionalEnd = ParseExpression()
        End If
      End If

      Return New IncStatementSyntax(m_syntaxTree,
                                     incKeyword,
                                     identifier,
                                     optionalComma1,
                                     optionalAmount,
                                     optionalComma2,
                                     optionalStart,
                                     optionalToKeyword,
                                     optionalEnd)

    End Function

    Private Function ParseSbInkStatement() As InkStatementSyntax

      ' INK *value*

      Dim inkKeyword = MatchToken(SyntaxKind.InkKeyword)
      Dim expression = ParseExpression()
      Return New InkStatementSyntax(m_syntaxTree, inkKeyword, expression)

    End Function

    Private Function ParseSbOverStatement() As OverStatementSyntax

      ' OVER *value*

      Dim overKeyword = MatchToken(SyntaxKind.OverKeyword)
      Dim expression = ParseExpression()
      Return New OverStatementSyntax(m_syntaxTree, overKeyword, expression)

    End Function

    Private Function ParseSbPaletteStatement() As StatementSyntax

      Dim paletteKeyword = MatchToken(SyntaxKind.PaletteKeyword)
      Select Case Peek(0).Kind
        Case SyntaxKind.HsvKeyword
          ' PALETTE HSV index,<h,s,v|HSV>
          Dim hsvKeyword = MatchToken(SyntaxKind.HsvKeyword)
          Dim indexExpression = ParseExpression()
          Dim commaToken1 = MatchToken(SyntaxKind.CommaToken)
          Dim hExpression = ParseExpression()
          Dim optionalCommaToken2 As SyntaxToken = Nothing
          Dim optionalSExpression As ExpressionSyntax = Nothing
          Dim optionalCommaToken3 As SyntaxToken = Nothing
          Dim optionalVExpression As ExpressionSyntax = Nothing
          If Current.Kind = SyntaxKind.CommaToken Then optionalCommaToken2 = MatchToken(SyntaxKind.CommaToken)
          If optionalCommaToken2 IsNot Nothing Then
            optionalSExpression = ParseExpression()
            optionalCommaToken3 = MatchToken(SyntaxKind.CommaToken)
            optionalVExpression = ParseExpression()
          End If
          Return New PaletteHsvStatementSyntax(m_syntaxTree,
                                               paletteKeyword,
                                               hsvKeyword,
                                               indexExpression,
                                               commaToken1,
                                               hExpression,
                                               optionalCommaToken2,
                                               optionalSExpression,
                                               optionalCommaToken3,
                                               optionalVExpression)
        Case SyntaxKind.ShlKeyword, SyntaxKind.ShrKeyword
          ' PALETTE <SHL|SHR> amount[,start TO end]
          Dim shiftKeyword = If(Peek(0).Kind = SyntaxKind.ShlKeyword, MatchToken(SyntaxKind.ShlKeyword), MatchToken(SyntaxKind.ShrKeyword))
          Dim amountExpression = ParseExpression()
          Dim optionalCommaToken As SyntaxToken = Nothing
          Dim optionalStartExpression As ExpressionSyntax = Nothing
          Dim optionalToKeyword As SyntaxToken = Nothing
          Dim optionalEndExpression As ExpressionSyntax = Nothing
          If Current.Kind = SyntaxKind.CommaToken Then optionalCommaToken = MatchToken(SyntaxKind.CommaToken)
          If optionalCommaToken IsNot Nothing Then
            optionalStartExpression = ParseExpression()
            optionalToKeyword = MatchToken(SyntaxKind.ToKeyword)
            optionalEndExpression = ParseExpression()
          End If
          Return New PaletteShiftStatementSyntax(m_syntaxTree,
                                                 paletteKeyword,
                                                 shiftKeyword,
                                                 amountExpression,
                                                 optionalCommaToken,
                                                 optionalStartExpression,
                                                 optionalToKeyword,
                                                 optionalEndExpression)
        Case SyntaxKind.DefaultKeyword
          ' PALETTE DEFAULT
          Dim defaultKeyword = MatchToken(SyntaxKind.DefaultKeyword)
          Return New PaletteDefaultStatementSyntax(m_syntaxTree,
                                                   paletteKeyword,
                                                   defaultKeyword)
        Case Else
          ' PALETTE index,<r,g,b|RGB>
          Dim indexExpression = ParseExpression()
          Dim commaToken1 = MatchToken(SyntaxKind.CommaToken)
          Dim rExpression = ParseExpression()
          Dim optionalCommaToken2 As SyntaxToken = Nothing
          Dim optionalGExpression As ExpressionSyntax = Nothing
          Dim optionalCommaToken3 As SyntaxToken = Nothing
          Dim optionalBExpression As ExpressionSyntax = Nothing
          If Current.Kind = SyntaxKind.CommaToken Then optionalCommaToken2 = MatchToken(SyntaxKind.CommaToken)
          If optionalCommaToken2 IsNot Nothing Then
            optionalGExpression = ParseExpression()
            optionalCommaToken3 = MatchToken(SyntaxKind.CommaToken)
            optionalBExpression = ParseExpression()
          End If
          Return New SbPaletteStatementSyntax(m_syntaxTree,
                                            paletteKeyword,
                                            indexExpression,
                                            commaToken1,
                                            rExpression,
                                            optionalCommaToken2,
                                            optionalGExpression,
                                            optionalCommaToken3,
                                            optionalBExpression)
      End Select

    End Function

    Private Function ParseSbPaperStatement() As PaperStatementSyntax

      ' PAPER *value*

      Dim paperKeyword = MatchToken(SyntaxKind.PaperKeyword)
      Dim expression = ParseExpression()
      Return New PaperStatementSyntax(m_syntaxTree, paperKeyword, expression)

    End Function

    Private Function ParseSbInverseStatement() As InverseStatementSyntax

      Dim inverseKeyword = MatchToken(SyntaxKind.InverseKeyword)
      Dim expression = ParseExpression()
      Return New InverseStatementSyntax(m_syntaxTree, inverseKeyword, expression)

    End Function

    Private Function ParseSbMoveStatement() As MoveStatementSyntax

      Dim moveKeyword = MatchToken(SyntaxKind.MoveKeyword)
      Dim expression = ParseExpression()
      Return New MoveStatementSyntax(m_syntaxTree, moveKeyword, expression)

    End Function

    Private Function ParseSbProgStatement() As ProgStatementSyntax

      ' PROG *name*

      Dim progKeyword = MatchToken(SyntaxKind.ProgKeyword)
      Dim progLine = m_text.GetLineIndex(progKeyword.Span.Start)

      Dim position = Current.Position

      Dim name As String = ""

      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        If currentLine <> progLine Then Exit While

        Dim token = NextToken()
        For Each entry In token.LeadingTrivia
          name &= entry.Text
        Next
        name &= token.Text
        For Each entry In token.TrailingTrivia
          name &= entry.Text
        Next
      End While

      Dim trailingTrivia = New List(Of SyntaxTrivia)
      For Each entry In progKeyword.TrailingTrivia
        trailingTrivia.Add(entry)
      Next
      trailingTrivia.Add(New SyntaxTrivia(m_syntaxTree, SyntaxKind.SingleLineCommentTrivia, position, name))
      Dim tkn = New SyntaxToken(progKeyword.SyntaxTree,
                                progKeyword.Kind,
                                progKeyword.Position,
                                progKeyword.Text,
                                progKeyword.Value,
                                progKeyword.LeadingTrivia,
                                trailingTrivia.ToImmutableArray)


      Return New ProgStatementSyntax(m_syntaxTree, tkn)

    End Function

    Private Function ParseSbScreenStatement() As SbScreenStatementSyntax

      ' SCREEN LOCK | UNLOCK | UPDATE
      ' SCREEN WINDOW | FULL ...
      ' SCREEN GRAB ...

      Dim screenKeyword = MatchToken(SyntaxKind.ScreenKeyword)
      Dim actionKeyword As SyntaxToken
      Select Case Peek(0).Kind
        Case SyntaxKind.WindowKeyword : actionKeyword = MatchToken(SyntaxKind.WindowKeyword)
        Case SyntaxKind.FullKeyword : actionKeyword = MatchToken(SyntaxKind.FullKeyword)
        Case SyntaxKind.LockKeyword : actionKeyword = MatchToken(SyntaxKind.LockKeyword)
        Case SyntaxKind.UnlockKeyword : actionKeyword = MatchToken(SyntaxKind.UnlockKeyword)
        Case SyntaxKind.UpdateKeyword : actionKeyword = MatchToken(SyntaxKind.UpdateKeyword)
          'Case SyntaxToken.GrabKeyword
        Case Else
          actionKeyword = MatchToken(SyntaxKind.GrabKeyword)
      End Select
      Return New SbScreenStatementSyntax(m_syntaxTree, screenKeyword, actionKeyword)

    End Function

    Private Function ParseSbWaitStatement() As SbWaitStatementSyntax

      ' WAIT [SCREEN] numexpr

      Dim waitKeyword = MatchToken(SyntaxKind.WaitKeyword)
      Dim optionalScreenKeyword As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.ScreenKeyword Then optionalScreenKeyword = MatchToken(SyntaxKind.ScreenKeyword)
      Dim optionalExpression As ExpressionSyntax = Nothing
      If Not IsEndOfStatement() Then
        optionalExpression = ParseExpression()
      End If

      Return New SbWaitStatementSyntax(m_syntaxTree,
                                     waitKeyword,
                                     optionalScreenKeyword,
                                     optionalExpression)

    End Function

    Private Function ParseSbZxAsciiStatement() As ZxAsciiStatementSyntax

      ' ZXASCII

      Dim zxAsciiKeyword = MatchToken(SyntaxKind.ZxAsciiKeyword)

      Return New ZxAsciiStatementSyntax(m_syntaxTree, zxAsciiKeyword)

    End Function

#End Region

    Private Function ParseBeepStatement() As BeepStatementSyntax

      'QBasic: BEEP

      Dim beepKeyword = MatchToken(SyntaxKind.BeepKeyword)

      Return New BeepStatementSyntax(m_syntaxTree, beepKeyword)

    End Function

    Private Function ParseBloadStatement() As BloadStatementSyntax

      'QBasic: BLOAD filespec$[,offset%]

      Dim bloadKeyword = MatchToken(SyntaxKind.BLoadKeyword)
      Dim filespec = ParseExpression()
      Dim comma = TryMatchToken(SyntaxKind.CommaToken)
      Dim offset As ExpressionSyntax = Nothing
      If comma IsNot Nothing Then
        offset = ParseExpression()
      End If

      Return New BloadStatementSyntax(m_syntaxTree, bloadKeyword, filespec, comma, offset)

    End Function

    Private Function ParseBsaveStatement() As BsaveStatementSyntax

      'QBasic: BSAVE filespec$, offset%, length&

      Dim bsaveKeyword = MatchToken(SyntaxKind.BSaveKeyword)
      Dim filespec = ParseExpression()
      Dim comma1 = MatchToken(SyntaxKind.CommaToken)
      Dim offset = ParseExpression()
      Dim comma2 = MatchToken(SyntaxKind.CommaToken)
      Dim length = ParseExpression()

      Return New BsaveStatementSyntax(m_syntaxTree, bsaveKeyword, filespec, comma1, offset, comma2, length)

    End Function

    Private Function ParseCallStatement() As CallStatementSyntax

      'QBasic: [CALL] name [([argumentlist])]

      'TODO:

      'QBasic: CALL ABSOLUTE ([argumentlist,] offset%)

      Dim callKeyword = MatchToken(SyntaxKind.CallKeyword)

      If Current.Kind = SyntaxKind.AbsoluteKeyword Then
        Stop
      End If

      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParen As SyntaxToken = Nothing
      Dim expressions As SeparatedSyntaxList(Of ExpressionSyntax) = Nothing
      Dim closeParen As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.OpenParenToken Then
        openParen = MatchToken(SyntaxKind.OpenParenToken)
        If Current.Kind <> SyntaxKind.CloseParenToken Then expressions = ParseArguments()
        closeParen = MatchToken(SyntaxKind.CloseParenToken)
      End If
      Return New CallStatementSyntax(m_syntaxTree, callKeyword, identifier, openParen, expressions, closeParen)

    End Function

    Private Function ParseChainStatement() As ChainStatementSyntax

      'QBasic: CHAIN filespec$

      Dim chainKeyword = MatchToken(SyntaxKind.ChainKeyword)
      Dim filename = ParseExpression()
      Dim optionalCommaToken As SyntaxToken = Nothing
      Dim optionalLine As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalCommaToken = MatchToken(SyntaxKind.CommaToken)
        optionalLine = ParseExpression()
      End If
      Return New ChainStatementSyntax(m_syntaxTree, chainKeyword, filename, optionalCommaToken, optionalLine)
    End Function

    Private Function ParseChDirStatement() As ChDirStatementSyntax

      'QBasic: CHDIR pathname$

      Dim chDirKeyword = MatchToken(SyntaxKind.ChDirKeyword)
      Dim path = ParseExpression()
      Return New ChDirStatementSyntax(m_syntaxTree, chDirKeyword, path)

    End Function

    Private Function ParseCircleStatement() As StatementSyntax

      If Peek(1).Kind = SyntaxKind.StepKeyword OrElse
         Peek(1).Kind = SyntaxKind.OpenParenToken Then

        'QBasic: CIRCLE [STEP] (x!,y!),radius![,[color%] [,[start!] [,[end!] [,aspect!]]]]

        Dim circleKeyword = MatchToken(SyntaxKind.CircleKeyword)
        Dim optionalStepKeyword = TryMatchToken(SyntaxKind.StepKeyword)
        Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
        Dim x = ParseExpression()
        Dim comma1 = MatchToken(SyntaxKind.CommaToken)
        Dim y = ParseExpression()
        Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
        Dim comma2 = MatchToken(SyntaxKind.CommaToken)
        Dim radius = ParseExpression()

        Dim optionalComma3 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalColor As ExpressionSyntax = Nothing
        If optionalComma3 IsNot Nothing AndAlso Not Current.Kind = SyntaxKind.CommaToken Then optionalColor = ParseExpression()
        Dim optionalComma4 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalStart As ExpressionSyntax = Nothing
        If optionalComma4 IsNot Nothing AndAlso Not Current.Kind = SyntaxKind.CommaToken Then optionalStart = ParseExpression()
        Dim optionalComma5 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalEnd As ExpressionSyntax = Nothing
        If optionalComma5 IsNot Nothing AndAlso Not Current.Kind = SyntaxKind.CommaToken Then optionalEnd = ParseExpression()
        Dim optionalComma6 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalAspect As ExpressionSyntax = Nothing
        If optionalComma6 IsNot Nothing AndAlso Not Current.Kind = SyntaxKind.CommaToken Then optionalAspect = ParseExpression()

        Return New CircleStatementSyntax(m_syntaxTree,
                                         circleKeyword,
                                         optionalStepKeyword,
                                         openParen,
                                         x,
                                         comma1,
                                         y,
                                         closeParen,
                                         comma2,
                                         radius,
                                         optionalComma3,
                                         optionalColor,
                                         optionalComma4,
                                         optionalStart,
                                         optionalComma5,
                                         optionalEnd,
                                         optionalComma6,
                                         optionalAspect)

      Else

        'SpecBAS: CIRCLE [colour-item;]x,y,r[,fill$]
        Return ParseSbCircleStatement()

      End If

    End Function

    Private Function ParseClearStatement() As ClearStatementSyntax

      'QBasic: CLEAR [,,stack&]

      Dim clearKeyword = MatchToken(SyntaxKind.ClearKeyword)
      Dim maxBytesCommaToken As SyntaxToken = Nothing
      Dim stackSpaceCommaToken As SyntaxToken = Nothing
      Dim maxBytesExpression As ExpressionSyntax = Nothing
      Dim stackSpaceExpression As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        maxBytesCommaToken = MatchToken(SyntaxKind.CommaToken)
      End If
      If maxBytesCommaToken IsNot Nothing Then
        If IsPossibleExpression() Then
          maxBytesExpression = ParseExpression()
        End If
        If Current.Kind = SyntaxKind.CommaToken Then
          stackSpaceCommaToken = MatchToken(SyntaxKind.CommaToken)
          stackSpaceExpression = ParseExpression()
        End If
      End If
      Return New ClearStatementSyntax(m_syntaxTree, clearKeyword, maxBytesCommaToken, maxBytesExpression, stackSpaceCommaToken, stackSpaceExpression)

    End Function

#Region "Close"

    Private Function ParseCloseStatement() As CloseStatementSyntax

      'QBasic: CLOSE [[#]filenumber%[,[#]filenumber%]...]

      Dim closeKeyword = MatchToken(SyntaxKind.CloseKeyword)
      Dim expressions = If(Not IsEndOfStatement(), ParseFileNumbers(), Nothing)

      Return New CloseStatementSyntax(m_syntaxTree, closeKeyword, expressions)

    End Function

    Private Function ParseFileNumbers() As SeparatedSyntaxList(Of ExpressionSyntax)

      ' ... [[#]filenumber%[,[#]filenumber%]...] ...

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)

      Dim parseNextArgument = Not IsEndOfStatement()
      While parseNextArgument AndAlso Not IsEndOfStatement()
        Dim pound = TryMatchToken(SyntaxKind.PoundToken)
        If pound IsNot Nothing Then nodesAndSeparators.Add(pound)
        Dim expression = ParseExpression()
        nodesAndSeparators.Add(expression)
        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextArgument = False
        End If
      End While
      Return New SeparatedSyntaxList(Of ExpressionSyntax)(nodesAndSeparators.ToImmutable)

    End Function

#End Region

    Private Function ParseClsStatement() As ClsStatementSyntax

      'QBasic: CLS [{0 | 1 | 2}]

      Dim clsKeyword = MatchToken(SyntaxKind.ClsKeyword)
      Dim optionalExpression As ExpressionSyntax = Nothing
      Return New ClsStatementSyntax(m_syntaxTree, clsKeyword, optionalExpression)

    End Function

    Private Function ParseColorStatement() As ColorStatementSyntax

      'QBasic: COLOR [foreground%] [,[background%] [,border%]]    Screen mode 0 (text only)
      'QBasic: COLOR [background%] [,palette%]                    Screen mode 1
      'QBasic: COLOR [foreground%]                                Screen modes 4, 12, 13
      'QBasic: COLOR [foreground%] [,background&]                 Screen modes 7-10

      Dim colorKeyword = MatchToken(SyntaxKind.ColorKeyword)
      Dim argument1Expression As ExpressionSyntax = Nothing
      Dim commaToken1 As SyntaxToken = Nothing
      Dim argument2Expression As ExpressionSyntax = Nothing
      Dim commaToken2 As SyntaxToken = Nothing
      Dim argument3Expression As ExpressionSyntax = Nothing
      If IsPossibleExpression() Then
        argument1Expression = ParseExpression()
      End If
      If Current.Kind = SyntaxKind.CommaToken Then
        commaToken1 = MatchToken(SyntaxKind.CommaToken)
        If IsPossibleExpression() Then
          argument2Expression = ParseExpression()
        End If
        If Current.Kind = SyntaxKind.CommaToken Then
          commaToken2 = MatchToken(SyntaxKind.CommaToken)
          argument3Expression = ParseExpression()
        End If
      End If
      Return New ColorStatementSyntax(m_syntaxTree, colorKeyword, argument1Expression, commaToken1, argument2Expression, commaToken2, argument3Expression)

    End Function

    Private Function ParseComStatement() As ComStatementSyntax

      'QBasic: COM(n%) ON
      'QBasic: COM(n%) OFF
      'QBasic: COM(n%) STOP
      'QBasic: ON COM(n%) GOSUB line

      Dim comKeyword = MatchToken(SyntaxKind.ComKeyword)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim n = ParseExpression()
      Dim closeParent = MatchToken(SyntaxKind.CloseParenToken)
      Dim verb As SyntaxNode
      Select Case Current.Kind
        Case SyntaxKind.OnKeyword : verb = MatchToken(SyntaxKind.OnKeyword)
        Case SyntaxKind.OffKeyword : verb = MatchToken(SyntaxKind.OffKeyword)
        Case Else : verb = MatchToken(SyntaxKind.StopKeyword)
      End Select

      Return New ComStatementSyntax(m_syntaxTree, comKeyword, openParen, n, closeParent, verb)

    End Function

    Private Function ParseCommonStatement() As StatementSyntax

      'QBasic: COMMON [SHARED] variable[( )] [AS type] [, variable[( )] [AS type]]...

      Dim commonKeyword = MatchToken(SyntaxKind.CommonKeyword)
      Dim sharedKeyword = TryMatchToken(SyntaxKind.SharedKeyword)

      Dim variables As New List(Of SyntaxNode)

      Do

        Dim variable = MatchToken(SyntaxKind.IdentifierToken)
        Dim boundsClause = ParseOptionalBoundsClause()
        Dim asClause = ParseOptionalAsClause()

        variables.Add(New VariableDeclarationSyntax(m_syntaxTree,
                                                         variable,
                                                         boundsClause,
                                                         asClause,
                                                         Nothing))

        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        variables.Add(MatchToken(SyntaxKind.CommaToken))

      Loop

      Return New CommonStatementSyntax(m_syntaxTree, commonKeyword, sharedKeyword, variables.ToImmutableArray)

    End Function

#Region "Parse CONST/DIM"

    Private Function ParseVariableDeclaration() As StatementSyntax

      'QBasic: CONST constantname = expression [,constantname = expression]...
      'QBasic: DIM [SHARED] variable[(subscripts)] [AS type][,variable[(subscripts)] [AS type]]...

      ' DIM *identifier* [AS *type*] [= *initializer*]
      ' CONST *identifier* [AS *type*] [= *initializer*]

      ' DIM *identifier*(*subscripts*)[,*identifier*(*subscripts*)]...

      ' QBasic

      ' DIM [ SHARED ] variable_name [ ( subscripts )] [ AS type ] [, variable_name [ ( subscripts ) ] [ AS type ] ]…

      ' ** SpecBAS **

      ' DIM *identifier* = "a", "b", "c", "d" BASE 0

      Dim expected = SyntaxKind.DimKeyword
      If Current.Kind = SyntaxKind.ConstKeyword Then expected = SyntaxKind.ConstKeyword
      'If Current.Kind = SyntaxKind.DimKeyword Then expected = SyntaxKind.DimKeyword
      Dim keyword = MatchToken(expected)
      Dim optionalSharedKeyword As SyntaxToken = Nothing
      If expected = SyntaxKind.DimKeyword AndAlso
         Current.Kind = SyntaxKind.SharedKeyword Then
        optionalSharedKeyword = MatchToken(SyntaxKind.SharedKeyword)
      End If

      ' Required. List of variables being declared in this statement.
      '   variable [ , variable ... ]
      ' Each variable has the following syntax And parts
      '   variablename [ ( [ boundslist ] ) ] [ As [ New ] datatype [ With{[ .propertyname = propinitializer [ , ... ] ] } ] ] [ = initializer ]

      Dim variables As New List(Of SyntaxNode)

      Do

        Dim variable = MatchToken(SyntaxKind.IdentifierToken)
        Dim optionalBoundsClause = ParseOptionalBoundsClause()
        Dim optionalAsClause = ParseOptionalAsClause()
        Dim optionalInitClause = ParseOptionalInitClause()

        variables.Add(New VariableDeclarationSyntax(m_syntaxTree,
                                                         variable,
                                                         optionalBoundsClause,
                                                         optionalAsClause,
                                                         optionalInitClause))

        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        variables.Add(MatchToken(SyntaxKind.CommaToken))

      Loop

      If expected = SyntaxKind.DimKeyword Then
        Return New DimStatementSyntax(m_syntaxTree,
                                      keyword,
                                      optionalSharedKeyword,
                                      variables)
      Else
        Return New ConstStatementSyntax(m_syntaxTree,
                                        keyword,
                                        variables)
      End If

    End Function

    Private Function ParseOptionalBoundsClause() As DimensionsClauseSyntax
      If Current.Kind <> SyntaxKind.OpenParenToken Then Return Nothing
      Return ParseDimensionsClause()
    End Function

    Private Function ParseDimensionsClause() As DimensionsClauseSyntax

      ' ... ( [*lower* TO] *upper* [, [*lower* TO] *upper] [, ...]  )

      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim dimensions = New List(Of SyntaxNode)
      Do
        dimensions.Add(ParseDimensionClause())
        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        dimensions.Add(MatchToken(SyntaxKind.CommaToken))
      Loop
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Return New DimensionsClauseSyntax(m_syntaxTree, openParen, dimensions, closeParen)

    End Function

    Private Function ParseDimensionClause() As DimensionClauseSyntax
      Dim upper = ParseExpression()
      Dim optionalLower As ExpressionSyntax = Nothing
      Dim optionalToKeyword As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.ToKeyword Then
        optionalLower = upper
        optionalToKeyword = MatchToken(SyntaxKind.ToKeyword)
        upper = ParseExpression()
      End If
      Return New DimensionClauseSyntax(m_syntaxTree, optionalLower, optionalToKeyword, upper)
    End Function

    Private Function ParseOptionalInitClause() As InitClauseSyntax
      If Current.Kind <> SyntaxKind.EqualToken Then Return Nothing
      Return ParseInitClause()
    End Function

    Private Function ParseInitClause() As InitClauseSyntax

      ' ... = *expression*

      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim initializer As New List(Of SyntaxNode) From {ParseExpression()}
      Dim afterComma = False
      Do
        If afterComma Then
          initializer.Add(ParseExpression()) : afterComma = False
        Else
          If Peek(0).Kind = SyntaxKind.CommaToken Then
            initializer.Add(MatchToken(SyntaxKind.CommaToken))
            afterComma = True
          Else
            Exit Do
          End If
        End If
      Loop
      Dim optionalBaseKeyword As SyntaxToken = Nothing
      Dim optionalBaseExpression As ExpressionSyntax = Nothing
      If Peek(0).Kind = SyntaxKind.BaseKeyword Then
        optionalBaseKeyword = MatchToken(SyntaxKind.BaseKeyword)
        optionalBaseExpression = ParseExpression()
      End If

      Return New InitClauseSyntax(m_syntaxTree,
                                  equalToken,
                                  initializer,
                                  optionalBaseKeyword,
                                  optionalBaseExpression)

    End Function

    Private Function ParseOptionalAsClause() As AsClause
      If Current.Kind <> SyntaxKind.AsKeyword Then Return Nothing
      Return ParseAsClause()
    End Function

    Private Function ParseAsClause() As AsClause

      ' ... AS *type*

      Dim asKeyword = MatchToken(SyntaxKind.AsKeyword)
      Dim identifier As SyntaxToken '= Nothing
      Select Case Current.Kind
        Case SyntaxKind.DoubleKeyword, SyntaxKind.IntegerKeyword, SyntaxKind.LongKeyword, SyntaxKind.SingleKeyword, SyntaxKind.StringKeyword, SyntaxKind.AnyKeyword
          identifier = NextToken()
        Case Else
          identifier = MatchToken(SyntaxKind.IdentifierToken)
      End Select
      Return New AsClause(m_syntaxTree, asKeyword, identifier)

    End Function

#End Region

    Private Function ParseContinueStatement() As ContinueStatementSyntax

      ' Continue Do
      ' Continue For
      ' Continue While

      Dim continueKeyword = MatchToken(SyntaxKind.ContinueKeyword)
      Dim kind As SyntaxKind = SyntaxKind.ForKeyword
      Select Case Current.Kind
        Case SyntaxKind.DoKeyword,
             SyntaxKind.ForKeyword,
             SyntaxKind.WhileKeyword
          kind = Current.Kind
        Case Else
      End Select
      Dim scopeKeyword = MatchToken(kind)
      Return New ContinueStatementSyntax(m_syntaxTree, continueKeyword, scopeKeyword)

    End Function

    Private Function ParseDataStatement() As DataStatementSyntax

      'QBasic: DATA constant[,constant]...

      Dim dataKeyword = MatchToken(SyntaxKind.DataKeyword)

      Dim constantsAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxToken)()

      Dim parseNextConstant = True
      While parseNextConstant AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        If Current.Kind = SyntaxKind.MinusToken Then
          constantsAndSeparators.Add(MatchToken(SyntaxKind.MinusToken))
        End If

        Dim kind = SyntaxKind.NumberToken
        If Current.Kind = SyntaxKind.StringToken Then
          kind = Current.Kind
        End If
        Dim constant = MatchToken(kind)
        constantsAndSeparators.Add(constant)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          constantsAndSeparators.Add(comma)
        Else
          parseNextConstant = False
        End If

      End While

      Return New DataStatementSyntax(m_syntaxTree, dataKeyword, constantsAndSeparators.ToImmutable)

    End Function

    Public Function ParseDateStatement() As DateStatementSyntax

      'QBasic: DATE$ = stringexpression$

      Dim dateKeyword = MatchToken(SyntaxKind.DateKeyword)
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()

      Return New DateStatementSyntax(m_syntaxTree, dateKeyword, equalToken, expression)

    End Function

    Private Function ParseDeclareStatement() As DeclareStatementSyntax

      'QBasic: DECLARE {FUNCTION | SUB} name [([parameterlist])]

      Dim declareKeyword = MatchToken(SyntaxKind.DeclareKeyword)
      Dim typeKeyword = If(TryMatchToken(SyntaxKind.FunctionKeyword), TryMatchToken(SyntaxKind.SubKeyword))
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)

      Dim openParen As SyntaxToken = Nothing
      Dim parameters As SeparatedSyntaxList(Of ParameterSyntax) = Nothing
      Dim closeParen As SyntaxToken = Nothing

      If Current.Kind = SyntaxKind.OpenParenToken Then
        openParen = MatchToken(SyntaxKind.OpenParenToken)
        parameters = ParseParameterList()
        closeParen = MatchToken(SyntaxKind.CloseParenToken)
      End If

      Return New DeclareStatementSyntax(m_syntaxTree,
                                        declareKeyword,
                                        typeKeyword,
                                        identifier,
                                        openParen,
                                        parameters,
                                        closeParen)

    End Function

    Private Function ParseDefFnDeclaration() As MemberSyntax

      'QBasic: 
      '
      '  DEF FNname[(parameterlist)] = expression
      '  
      '  DEF FNname[(parameterlist)]
      '    [statementblock]
      '    FNname = expression
      '    [statementblock]
      '    [EXIT DEF]
      '    [statementblock]
      '  END DEF

      ' DEF FN*identifier*[(*parameters*)]
      '   *statements*
      ' END DEF

      Dim defKeyword = MatchToken(SyntaxKind.DefKeyword)
      Dim identifier = ParseIdentifier() 'MatchToken(SyntaxKind.IdentifierToken)
      'TODO: Are the first two characters FN?  If not, error!
      Dim openParenToken As SyntaxToken = Nothing
      Dim parameters As SeparatedSyntaxList(Of ParameterSyntax) = Nothing
      Dim closeParenToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.OpenParenToken Then
        openParenToken = MatchToken(SyntaxKind.OpenParenToken)
        If Current.Kind <> SyntaxKind.CloseParenToken Then parameters = ParseParameterList()
        closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      End If

      Dim value = identifier?.Identifier?.Text
      If Not value?.StartsWith("fn", StringComparison.InvariantCultureIgnoreCase) Then
        m_diagnostics.ReportInvalidFunctionName(Current.Location, value)
      End If

      If Current.Kind = SyntaxKind.EqualToken Then
        ' single line version
        Dim equalToken = MatchToken(SyntaxKind.EqualToken)
        Dim expression = ParseExpression()
        Return New SingleLineDefDeclarationSyntax(m_syntaxTree, defKeyword, identifier, openParenToken, parameters, closeParenToken, equalToken, expression)
      Else
        ' multi line version
        'Dim asClause = ParseOptionalAsClause()
        Dim statements = ParseBlockStatement(False)
        Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
        Dim endDefKeyword = MatchToken(SyntaxKind.DefKeyword)
        Return New DefDeclarationSyntax(m_syntaxTree, defKeyword, identifier.Identifier, openParenToken, parameters, closeParenToken, statements, endKeyword, endDefKeyword)
      End If

    End Function

    Private Function ParseDefStatement() As DefSegStatementSyntax

      'QBasic: DEF SEG [=address]

      Dim defKeyword = MatchToken(SyntaxKind.DefKeyword)

      Dim segKeyword = MatchToken(SyntaxKind.SegKeyword)

      Dim optionalEqualToken As SyntaxToken = Nothing
      Dim optionalAddress As ExpressionSyntax = Nothing

      If Current.Kind = SyntaxKind.EqualToken Then
        optionalEqualToken = MatchToken(SyntaxKind.EqualToken)
        optionalAddress = ParseExpression()
      End If

      Return New DefSegStatementSyntax(m_syntaxTree, defKeyword, segKeyword, optionalEqualToken, optionalAddress)

    End Function

#Region "ParseDefDbl/DefInt/DefLng/DefSng/DefStr"

    Private Function ParseDefTypeStatement() As DefTypeStatementSyntax

      'QBasic: DEFINT letterrange [,letterrange]...
      'QBasic: DEFLNG letterrange [,letterrange]...
      'QBasic: DEFSNG letterrange [,letterrange]...
      'QBasic: DEFDBL letterrange [,letterrange]...
      'QBasic: DEFSTR letterrange [,letterrange]...

      Dim keyword As SyntaxToken
      Select Case Current.Kind
        Case SyntaxKind.DefDblKeyword : keyword = MatchToken(SyntaxKind.DefDblKeyword)
        Case SyntaxKind.DefIntKeyword : keyword = MatchToken(SyntaxKind.DefIntKeyword)
        Case SyntaxKind.DefLngKeyword : keyword = MatchToken(SyntaxKind.DefLngKeyword)
        Case SyntaxKind.DefSngKeyword : keyword = MatchToken(SyntaxKind.DefSngKeyword)
        Case Else ' DefStrKeyword
          keyword = MatchToken(SyntaxKind.DefStrKeyword)
      End Select
      Dim nodes As New List(Of SyntaxNode)
      Do
        nodes.Add(ParseDefVarRange)
        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        nodes.Add(MatchToken(SyntaxKind.CommaToken))
      Loop
      Return New DefTypeStatementSyntax(m_syntaxTree, keyword, nodes)
    End Function

    Private Function ParseDefVarRange() As DefVarRangeClause
      Dim lower = MatchToken(SyntaxKind.IdentifierToken)
      Dim optionalDashToken As SyntaxToken = Nothing
      Dim optionalUpper As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.MinusToken Then
        optionalDashToken = MatchToken(SyntaxKind.MinusToken)
        optionalUpper = MatchToken(SyntaxKind.IdentifierToken)
      End If
      Return New DefVarRangeClause(m_syntaxTree, lower, optionalDashToken, optionalUpper)
    End Function

#End Region

#Region "ParseDo"

    Private Function ParseDoStatement(isTopLevel As Boolean) As StatementSyntax

      'QBasic:
      '
      '  DO [{WHILE | UNTIL} condition]
      '      [statementblock]
      '  LOOP
      '  
      '  DO
      '      [statementblock]
      '  LOOP [{WHILE | UNTIL} condition]

      Dim whileClause As WhileClauseSyntax = Nothing
      Dim untilClause As UntilClauseSyntax = Nothing

      Dim doKeyword = MatchToken(SyntaxKind.DoKeyword)

      Select Case Current.Kind
        Case SyntaxKind.WhileKeyword : whileClause = ParseOptionalWhileClause(True)
        Case SyntaxKind.UntilKeyword : untilClause = ParseOptionalUntilClause(True)
        Case Else
      End Select

      Dim body = ParseBlockStatement(isTopLevel)

      Dim loopKeyword = MatchToken(SyntaxKind.LoopKeyword)

      If whileClause Is Nothing AndAlso untilClause Is Nothing Then
        Select Case Current.Kind
          Case SyntaxKind.WhileKeyword : whileClause = ParseOptionalWhileClause(False)
          Case SyntaxKind.UntilKeyword : untilClause = ParseOptionalUntilClause(False)
          Case Else
        End Select
      End If

      If untilClause Is Nothing Then
        Return New DoWhileStatementSyntax(m_syntaxTree, doKeyword, whileClause, body, loopKeyword)
      Else
        Return New DoUntilStatementSyntax(m_syntaxTree, doKeyword, untilClause, body, loopKeyword)
      End If

    End Function

    Private Function ParseOptionalUntilClause(atBeginning As Boolean) As UntilClauseSyntax

      ' ... UNTIL *expression*

      If Current.Kind <> SyntaxKind.UntilKeyword Then Return Nothing
      Dim untilKeyword = MatchToken(SyntaxKind.UntilKeyword)
      Dim expression = ParseExpression()
      Return New UntilClauseSyntax(m_syntaxTree, untilKeyword, expression, atBeginning)

    End Function

    Private Function ParseOptionalWhileClause(atBeginning As Boolean) As WhileClauseSyntax

      ' ... WHILE *expression*

      If Current.Kind <> SyntaxKind.WhileKeyword Then Return Nothing
      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim expression = ParseExpression()
      Return New WhileClauseSyntax(m_syntaxTree, whileKeyword, expression, atBeginning)

    End Function

#End Region

    Private Function ParseDrawStatement() As DrawStatementSyntax

      'QBasic: DRAW commandstring$

      Dim drawKeyword = MatchToken(SyntaxKind.DrawKeyword)
      Dim command = ParseExpression()
      Return New DrawStatementSyntax(m_syntaxTree, drawKeyword, command)

    End Function

    Private Function ParseEndStatement() As EndStatementSyntax

      'QBasic: END [{DEF | FUNCTION | IF | SELECT | SUB | TYPE}]

      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Return New EndStatementSyntax(m_syntaxTree, endKeyword)

    End Function

    Private Function ParseEnvironStatement() As EnvironStatementSyntax

      'QBasic: ENVIRON stringexpression$

      Dim environKeyword = MatchToken(SyntaxKind.EnvironKeyword)
      Dim expression = ParseExpression()

      Return New EnvironStatementSyntax(m_syntaxTree, environKeyword, expression)

    End Function

    Private Function ParseEraseStatement() As EraseStatementSyntax

      'QBasic: ERASE arrayname [,arrayname]...

      Dim eraseKeyword = MatchToken(SyntaxKind.EraseKeyword)

      Dim variables As New List(Of SyntaxNode)

      Do
        variables.Add(MatchToken(SyntaxKind.IdentifierToken))
        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        variables.Add(MatchToken(SyntaxKind.CommaToken))
      Loop

      Return New EraseStatementSyntax(m_syntaxTree, eraseKeyword, variables)

    End Function

    Private Function ParseErrorStatement() As ErrorStatementSyntax

      'QBasic: ERROR expression%

      Dim errorKeyword = MatchToken(SyntaxKind.ErrorKeyword)
      Dim expression = ParseExpression()
      Return New ErrorStatementSyntax(m_syntaxTree, errorKeyword, expression)

    End Function

    Private Function ParseExitStatement() As ExitStatementSyntax

      'QBasic: EXIT {DEF | DO | FOR | FUNCTION | SUB}

      Dim exitKeyword = MatchToken(SyntaxKind.ExitKeyword)
      Dim kind As SyntaxKind = SyntaxKind.ForKeyword
      Select Case Current.Kind
        Case SyntaxKind.DefKeyword,
             SyntaxKind.DoKeyword,
             SyntaxKind.ForKeyword,
             SyntaxKind.FunctionKeyword,
             SyntaxKind.SubKeyword,
             SyntaxKind.WhileKeyword
          kind = Current.Kind
        Case Else
      End Select
      Dim scopeKeyword = MatchToken(kind)
      Return New ExitStatementSyntax(m_syntaxTree, exitKeyword, scopeKeyword)

    End Function

    Private Function ParseFieldStatement() As FieldStatementSyntax

      'QBasic: FIELD [#]filenumber%, fieldwidth% AS stringvariable$[,fieldwidth% AS stringvariable$] ...

      Dim fieldKeyword = MatchToken(SyntaxKind.FieldKeyword)
      Dim poundToken = TryMatchToken(SyntaxKind.PoundToken)
      Dim filenumber = ParseExpression()
      Dim comma = MatchToken(SyntaxKind.CommaToken)

      'fieldwidth% AS stringvariable$[,

      Dim identifiers As New List(Of SyntaxNode)
      Do
        Dim fieldWidth = ParseExpression() : identifiers.Add(fieldWidth)
        Dim asKeyword = MatchToken(SyntaxKind.AsKeyword) : identifiers.Add(asKeyword)
        Dim identifier = MatchToken(SyntaxKind.IdentifierToken) : identifiers.Add(identifier)
        If Not Current.Kind = SyntaxKind.CommaToken Then Exit Do
        Dim comma2 = MatchToken(SyntaxKind.CommaToken) : identifiers.Add(comma2)
      Loop

      Return New FieldStatementSyntax(m_syntaxTree, fieldKeyword, poundToken, filenumber, comma, identifiers.ToImmutableArray)

    End Function

    Private Function ParseFilesStatement() As FilesStatementSyntax

      'QBasic: FILES [filespec$]

      Dim filesKeyword = MatchToken(SyntaxKind.FilesKeyword)
      Dim expression = ParseExpression()
      Return New FilesStatementSyntax(m_syntaxTree, filesKeyword, expression)

    End Function

    Private Function ParseForStatement(isTopLevel As Boolean) As StatementSyntax

      'QBasic:
      '
      '  FOR counter = start TO end [STEP increment]
      '      [statementblock]
      '  NEXT [counter [,counter]...]

      'If Current.Kind = SyntaxKind.ForKeyword AndAlso Peek(1).Kind = SyntaxKind.EachKeyword Then
      '  Return ParseForEachStatement(isTopLevel)
      'End If

      Dim forKeyword = MatchToken(SyntaxKind.ForKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim startValue = ParseExpression()
      Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
      Dim endValue = ParseExpression()
      Dim stepKeyword As SyntaxToken = Nothing
      Dim increment As ExpressionSyntax = Nothing
      If Peek(0).Kind = SyntaxKind.StepKeyword Then
        stepKeyword = MatchToken(SyntaxKind.StepKeyword)
        increment = ParseExpression()
      End If
      Dim statements = ParseBlockStatement(isTopLevel)
      Dim nextKeyword As SyntaxToken '= Nothing
      Dim optionalIdentifier As SyntaxToken = Nothing
      If Peek(0).Kind = SyntaxKind.CommaToken Then
        ' In theory, should be able to help handle: NEXT a, b, c
        nextKeyword = MatchToken(SyntaxKind.CommaToken)
        optionalIdentifier = MatchToken(SyntaxKind.IdentifierToken)
      Else
        nextKeyword = MatchToken(SyntaxKind.NextKeyword)
        If Not IsEndOfStatement() Then
          optionalIdentifier = MatchToken(SyntaxKind.IdentifierToken)
        End If
      End If
      Return New ForStatementSyntax(m_syntaxTree, forKeyword,
                                    identifier,
                                    equalToken,
                                    startValue,
                                    toKeyword,
                                    endValue,
                                    stepKeyword,
                                    increment,
                                    statements,
                                    nextKeyword,
                                    optionalIdentifier)
    End Function

    Private Function ParseFunctionDeclaration() As MemberSyntax

      ' FUNCTION *identifier*([*parameters*])[ AS *type*]
      '   *statements*
      ' END FUNCTION

      Dim functionKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim parameters = ParseParameterList()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Dim asClause = ParseOptionalAsClause()
      Dim statements = ParseBlockStatement(False)
      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Dim endFunctionKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Return New FunctionDeclarationSyntax(m_syntaxTree, functionKeyword, identifier, openParenToken, parameters, closeParenToken, asClause, statements, endKeyword, endFunctionKeyword)

    End Function

    Private Function ParseFunctionStatement() As FunctionStatementSyntax

      'QBasic:
      '
      '  FUNCTION name [(parameterlist)] [STATIC]
      '    [statementblock]
      '    name = expression
      '    [statementblock]
      '  END FUNCTION

      ' FUNCTION *identifier*([*parameters*])[ AS *type*]
      '   *statements*
      ' END FUNCTION

      Dim functionKeyword = MatchToken(SyntaxKind.FunctionKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)

      Dim openParenToken As SyntaxToken = Nothing
      Dim parameters As SeparatedSyntaxList(Of ParameterSyntax) = Nothing
      Dim closeParenToken As SyntaxToken = Nothing

      If Current.Kind = SyntaxKind.OpenParenToken Then
        openParenToken = MatchToken(SyntaxKind.OpenParenToken)
        parameters = ParseParameterList()
        closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      End If
      Dim staticKeyword = TryMatchToken(SyntaxKind.StaticKeyword)

      Dim asClause As AsClause = Nothing
      If Current.Kind = SyntaxKind.AsKeyword Then asClause = ParseOptionalAsClause()

      Dim statements = ParseBlockStatement(False)

      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Dim endFunctionKeyword = MatchToken(SyntaxKind.FunctionKeyword)

      Return New FunctionStatementSyntax(m_syntaxTree,
                                         functionKeyword,
                                         identifier,
                                         openParenToken,
                                         parameters,
                                         closeParenToken,
                                         asClause,
                                         staticKeyword,
                                         statements,
                                         endKeyword,
                                         endFunctionKeyword)

    End Function

    Private Function ParseGetStatement() As StatementSyntax

      'QBasic: GET [#]filenumber%[,[recordnumber&][,variable]]
      'QBasic: GET [STEP](x1!,y1!)-[STEP](x2!,y2!), arrayname[(index%)]

      Dim getKeyword = MatchToken(SyntaxKind.GetKeyword)

      If Current.Kind = SyntaxKind.StepKeyword OrElse
         Current.Kind = SyntaxKind.OpenParenToken Then
        ' GET [STEP](x1!,y1!)-[STEP](x2!,y2!), arrayname[(index%)]
        Dim optionalStepKeyword1 = TryMatchToken(SyntaxKind.StepKeyword)
        Dim openParen1 = MatchToken(SyntaxKind.OpenParenToken)
        Dim x1 = ParseExpression()
        Dim comma1 = MatchToken(SyntaxKind.CommaToken)
        Dim y1 = ParseExpression()
        Dim closeParen1 = MatchToken(SyntaxKind.CloseParenToken)
        Dim dashToken = MatchToken(SyntaxKind.MinusToken)
        Dim optionalStepKeyword2 = TryMatchToken(SyntaxKind.StepKeyword)
        Dim openParen2 = MatchToken(SyntaxKind.OpenParenToken)
        Dim x2 = ParseExpression()
        Dim comma2 = MatchToken(SyntaxKind.CommaToken)
        Dim y2 = ParseExpression()
        Dim closeParen2 = MatchToken(SyntaxKind.CloseParenToken)
        Dim comma3 = MatchToken(SyntaxKind.CommaToken)
        Dim buffer = ParseIdentifier()
        Return New GetStatementSyntax(m_syntaxTree,
                                      getKeyword,
                                      optionalStepKeyword1,
                                      openParen1,
                                      x1,
                                      comma1,
                                      y1,
                                      closeParen1,
                                      dashToken,
                                      optionalStepKeyword2,
                                      openParen2,
                                      x2,
                                      comma2,
                                      y2,
                                      closeParen2,
                                      comma3,
                                      buffer)
      Else
        ' GET [#]filenumber%[,[recordnumber&][,variable]]
        Dim optionalPoundToken = TryMatchToken(SyntaxKind.PoundToken)
        Dim fileNumber = ParseExpression()
        Dim optionalComma1 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalRecord As ExpressionSyntax = Nothing
        If optionalComma1 IsNot Nothing AndAlso
           Not Current.Kind = SyntaxKind.CommaToken Then
          optionalRecord = ParseExpression()
        End If
        Dim optionalComma2 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalVariable As SyntaxToken = Nothing
        If optionalComma2 IsNot Nothing Then
          optionalVariable = MatchToken(SyntaxKind.IdentifierToken)
        End If
        Return New GetFileStatementSyntax(m_syntaxTree, getKeyword, optionalPoundToken, fileNumber, optionalComma1, optionalRecord, optionalComma2, optionalVariable)
      End If

    End Function

    Private Function ParseGosubStatement() As GosubStatementSyntax

      'QBasic: GOSUB line1

      ' GOSUB *line number*

      ' GOSUB *label*

      Dim gosubKeyword = MatchToken(SyntaxKind.GosubKeyword)

      If Current.Kind = SyntaxKind.NumberToken Then
        Dim numberToken = MatchToken(SyntaxKind.NumberToken)
        Return New GosubStatementSyntax(m_syntaxTree, gosubKeyword, numberToken)
      Else
        Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
        Return New GosubStatementSyntax(m_syntaxTree, gosubKeyword, identifierToken)
      End If

    End Function

    Private Function ParseGotoStatement() As GotoStatementSyntax

      'QBasic: GOTO line

      ' GOTO *line number*

      ' GOTO *label*

      Dim gotoKeyword = MatchToken(SyntaxKind.GotoKeyword)

      If Current.Kind = SyntaxKind.NumberToken Then
        Dim numberToken = MatchToken(SyntaxKind.NumberToken)
        Return New GotoStatementSyntax(m_syntaxTree, gotoKeyword, numberToken)
      Else
        Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
        Return New GotoStatementSyntax(m_syntaxTree, gotoKeyword, identifierToken)
      End If

    End Function

#Region "ParseIf"

    Private Function ParseIfStatement(isTopLevel As Boolean) As StatementSyntax

      'QBasic:
      '  
      '  IF condition1 THEN
      '     [statementblock-1]
      '  [ELSEIF condition2 THEN
      '     [statementblock-2]]...
      '  [ELSE
      '     [statementblock-n]]
      '  END IF
      '
      '  IF condition THEN statements [ELSE statements]

      Dim ifKeyword = MatchToken(SyntaxKind.IfKeyword)
      Dim expression = ParseExpression()
      Dim thenKeyword = MatchToken(SyntaxKind.ThenKeyword)

      Dim thenLine = m_text.GetLineIndex(thenKeyword.Span.Start)
      Dim peekLine = m_text.GetLineIndex(Peek(0).Span.Start)
      Dim multiLine = Current.Kind = SyntaxKind.EndOfFileToken OrElse peekLine > thenLine

      Dim statements = ParseBlockStatement(isTopLevel, Not multiLine)

      If multiLine Then
        Dim elseIfClauses As New List(Of ElseIfClause)
        Dim elseClause As ElseClause = Nothing
        Do
          If Current.Kind = SyntaxKind.ElseIfKeyword Then
            elseIfClauses.Add(ParseElseIfClause(isTopLevel))
          ElseIf Current.Kind = SyntaxKind.ElseKeyword Then
            elseClause = ParseElseClause(isTopLevel)
            Exit Do
          Else
            Exit Do
          End If
        Loop
        Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
        Dim endIfKeyword = MatchToken(SyntaxKind.IfKeyword)
        Return New IfStatementSyntax(m_syntaxTree,
                                     ifKeyword,
                                     expression,
                                     thenKeyword,
                                     statements,
                                     elseIfClauses.ToImmutableArray,
                                     elseClause,
                                     endKeyword,
                                     endIfKeyword)
      Else
        Dim elseClause = ParseOptionalSingleLineElseClause(isTopLevel)
        Return New SingleLineIfStatementSyntax(m_syntaxTree,
                                               ifKeyword,
                                               expression,
                                               thenKeyword,
                                               statements,
                                               elseClause)
      End If

    End Function

    Private Function ParseOptionalSingleLineElseClause(isTopLevel As Boolean) As SingleLineElseClauseSyntax

      ' ... ELSE *statements*

      If Current.Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseBlockStatement(isTopLevel, True)
      Return New SingleLineElseClauseSyntax(m_syntaxTree, elseKeyword, statements)

    End Function

    Private Function ParseElseIfClause(isTopLevel As Boolean) As ElseIfClause

      ' ...
      ' ELSEIF expression THEN
      '   *statements*
      ' ...

      If Current.Kind <> SyntaxKind.ElseIfKeyword Then Return Nothing
      Dim elseIfKeyword = MatchToken(SyntaxKind.ElseIfKeyword)
      Dim expression = ParseExpression()
      Dim thenKeyword = MatchToken(SyntaxKind.ThenKeyword)
      Dim statements = ParseBlockStatement(isTopLevel)
      Return New ElseIfClause(m_syntaxTree, elseIfKeyword, expression, thenKeyword, statements)

    End Function

    Private Function ParseElseClause(isTopLevel As Boolean) As ElseClause

      ' ...
      ' ELSE
      '   *statements*
      ' ...

      If Current.Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statements = ParseBlockStatement(isTopLevel)
      Return New ElseClause(m_syntaxTree, elseKeyword, statements)

    End Function

#End Region

    Private Function ParseInputStatement() As InputStatementSyntax

      'QBasic: INPUT [;] ["prompt"{; | ,}] variablelist
      'QBasic: INPUT #filenumber%, variablelist

      Dim inputKeyword = MatchToken(SyntaxKind.InputKeyword)
      Dim optionalSemiColonToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.SemicolonToken Then optionalSemiColonToken = MatchToken(SyntaxKind.SemicolonToken)
      Dim optionalPromptExpression As ExpressionSyntax = Nothing
      Dim semiColonOrCommaToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.StringToken Then
        optionalPromptExpression = ParseExpression()
        Dim kind = SyntaxKind.SemicolonToken
        If Current.Kind = SyntaxKind.CommaToken Then kind = SyntaxKind.CommaToken
        semiColonOrCommaToken = MatchToken(kind)
      End If

      Dim identifiersAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxToken)()

      Dim parseNextIdentifier = True
      While parseNextIdentifier AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
        identifiersAndSeparators.Add(identifier)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          identifiersAndSeparators.Add(comma)
        Else
          parseNextIdentifier = False
        End If

      End While

      Return New InputStatementSyntax(m_syntaxTree, inputKeyword, optionalSemiColonToken, optionalPromptExpression, semiColonOrCommaToken, identifiersAndSeparators.ToImmutable())

    End Function

    Private Function ParseIoCtlStatement() As IoCtlStatementSyntax

      'QBasic: IOCTL [#]filenumber%, string$

      Dim ioctlKeyword = MatchToken(SyntaxKind.IoCtlKeyword)
      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber = ParseExpression()
      Dim comma = MatchToken(SyntaxKind.CommaToken)
      Dim expression = ParseExpression()

      Return New IoCtlStatementSyntax(m_syntaxTree, ioctlKeyword, pound, fileNumber, comma, expression)

    End Function

    Private Function ParseKeyStatement() As StatementSyntax

      'QBasic: KEY key%, stringexpression$
      'QBasic: KEY LIST
      'QBasic: KEY ON
      'QBasic: KEY OFF
      '
      'QBasic: KEY(n%) ON
      'QBasic: KEY(n%) OFF
      'QBasic: KEY(n%) STOP

      Dim keyKeyword = MatchToken(SyntaxKind.KeyKeyword)
      Select Case Current.Kind
        Case SyntaxKind.OpenParenToken
          Stop
        Case SyntaxKind.OffKeyword
          Dim offKeyword = MatchToken(SyntaxKind.OffKeyword)
          Return New KeyOffStatementSyntax(m_syntaxTree, keyKeyword, offKeyword)
        Case SyntaxKind.OnKeyword
          Dim onKeyword = MatchToken(SyntaxKind.OffKeyword)
          Return New KeyOnStatementSyntax(m_syntaxTree, keyKeyword, onKeyword)
        Case SyntaxKind.ListKeyword
          Dim listKeyword = MatchToken(SyntaxKind.ListKeyword)
          Return New KeyListStatementSyntax(m_syntaxTree, keyKeyword, listKeyword)
        Case Else
          Dim key = ParseExpression()
          Dim commaToken = MatchToken(SyntaxKind.CommaToken)
          Dim label = ParseExpression()
          Return New KeyStatementSyntax(m_syntaxTree, keyKeyword, key, commaToken, label)
      End Select

    End Function

    Private Function ParseKillStatement() As KillStatementSyntax

      'QBasic: KILL filespec$

      Dim killKeyword = MatchToken(SyntaxKind.KillKeyword)
      Dim path = ParseExpression()
      Return New KillStatementSyntax(m_syntaxTree, killKeyword, path)

    End Function

    Private Function ParseLabelStatement() As LabelStatementSyntax

      ' LabelName:

      Dim label = MatchToken(SyntaxKind.Label)
      Return New LabelStatementSyntax(m_syntaxTree, label)

    End Function

    Private Function ParseLetStatement() As StatementSyntax

      'QBasic: [LET] variable=expression

      'SpecBAS: LET *identifier*, *identifier* = *expression*

      Dim letKeywordToken = MatchToken(SyntaxKind.LetKeyword)

      Dim identifiers = New List(Of SyntaxNode)
repeat:
      identifiers.Add(ParseIdentifier)
      If Current.Kind = SyntaxKind.CommaToken Then
        identifiers.Add(MatchToken(SyntaxKind.CommaToken))
        GoTo repeat
      End If

      'Dim identifierToken = NextToken()
      'Dim openParenToken As SyntaxToken = Nothing
      'Dim arguments As SeparatedSyntaxList(Of ExpressionSyntax) = Nothing
      'Dim closeParenToken As SyntaxToken = Nothing
      'If Current.Kind = SyntaxKind.OpenParenToken Then
      '  openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      '  arguments = ParseArguments()
      '  closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      'End If

      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()
      Return New LetStatementSyntax(m_syntaxTree,
                                    letKeywordToken,
                                    identifiers,
                                    equalToken,
                                    expression)

    End Function

    Private Function ParseLineStatement() As StatementSyntax

      'QBasic: LINE [[STEP](x1!,y1!)]-[STEP](x2!,y2!) [,[color%] [,[B | BF] [,style%]]]

      'QBasic: LINE INPUT [;] ["prompt";] variable$
      'QBasic: LINE INPUT #filenumber%, variable$

      Dim lineKeyword = MatchToken(SyntaxKind.LineKeyword)

      Dim inputKeyword As SyntaxToken '= Nothing
      If Current.Kind = SyntaxKind.InputKeyword Then
        inputKeyword = MatchToken(SyntaxKind.InputKeyword)
        If Current.Kind = SyntaxKind.PoundToken Then
          Dim pound = MatchToken(SyntaxKind.PoundToken)
          Dim fileNumber = ParseExpression()
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          Dim identifier = ParseIdentifier()
          Return New LineInputFileStatementSyntax(m_syntaxTree, lineKeyword, inputKeyword,
                                                  pound, fileNumber, comma, identifier)
        Else
          Dim prompt As SyntaxToken = Nothing
          Dim separator As SyntaxToken = Nothing
          If Current.Kind = SyntaxKind.StringToken Then
            prompt = MatchToken(SyntaxKind.StringToken)
            If Current.Kind = SyntaxKind.CommaToken Then
              separator = MatchToken(SyntaxKind.CommaToken)
            Else
              separator = MatchToken(SyntaxKind.SemicolonToken)
            End If
          ElseIf Current.Kind = SyntaxKind.SemicolonToken Then
            separator = TryMatchToken(SyntaxKind.SemicolonToken)
          End If
          Dim identifier = ParseIdentifier()
          Return New LineInputStatementSyntax(m_syntaxTree,
                                              lineKeyword,
                                              inputKeyword,
                                              prompt,
                                              separator,
                                              identifier)
        End If
      End If

      Dim optionalStep1 = TryMatchToken(SyntaxKind.StepKeyword)
      Dim optionalOpenParen1 As SyntaxToken = Nothing
      Dim optionalX1 As ExpressionSyntax = Nothing
      Dim optionalComma1 As SyntaxToken = Nothing
      Dim optionalY1 As ExpressionSyntax = Nothing
      Dim optionalCloseParen1 As SyntaxToken = Nothing

      If Current.Kind = SyntaxKind.OpenParenToken Then
        optionalOpenParen1 = MatchToken(SyntaxKind.OpenParenToken)
        optionalX1 = ParseExpression()
        optionalComma1 = MatchToken(SyntaxKind.CommaToken)
        optionalY1 = ParseExpression()
        optionalCloseParen1 = MatchToken(SyntaxKind.CloseParenToken)
      End If

      Dim dashToken = MatchToken(SyntaxKind.MinusToken)

      Dim optionalStep2 = TryMatchToken(SyntaxKind.StepKeyword)
      Dim openParen2 = MatchToken(SyntaxKind.OpenParenToken)
      Dim x2 = ParseExpression()
      Dim comma2 = MatchToken(SyntaxKind.CommaToken)
      Dim y2 = ParseExpression()
      Dim closeParen2 = MatchToken(SyntaxKind.CloseParenToken)

      Dim optionalComma3 As SyntaxToken = Nothing
      Dim optionalAttribute As ExpressionSyntax = Nothing

      Dim optionalComma4 As SyntaxToken = Nothing
      Dim optionalMode As SyntaxToken = Nothing

      Dim optionalComma5 As SyntaxToken = Nothing
      Dim optionalStyle As ExpressionSyntax = Nothing

      If Current.Kind = SyntaxKind.CommaToken Then
        optionalComma3 = MatchToken(SyntaxKind.CommaToken)
        If Not IsEndOfStatement() AndAlso Not Current.Kind = SyntaxKind.CommaToken Then optionalAttribute = ParseExpression()
      End If

      If Current.Kind = SyntaxKind.CommaToken Then
        optionalComma4 = MatchToken(SyntaxKind.CommaToken)
        If Current.Text?.ToLower = "B" OrElse Current.Text?.ToLower = "BF" Then optionalMode = NextToken()
      End If

      If Current.Kind = SyntaxKind.CommaToken Then
        optionalComma5 = MatchToken(SyntaxKind.CommaToken)
        optionalStyle = ParseExpression()
      End If

      Return New LineStatementSyntax(m_syntaxTree,
                                     lineKeyword,
                                     optionalStep1,
                                     optionalOpenParen1,
                                     optionalX1,
                                     optionalComma1,
                                     optionalY1,
                                     optionalCloseParen1,
                                     dashToken,
                                     optionalStep2,
                                     openParen2,
                                     x2,
                                     comma2,
                                     y2,
                                     closeParen2,
                                     optionalComma3,
                                     optionalAttribute,
                                     optionalComma4,
                                     optionalMode,
                                     optionalComma5,
                                     optionalStyle)

    End Function

    Private Function ParseLocateStatement() As LocateStatementSyntax

      'QBasic: LOCATE  [row%] [,[column%] [,[cursor%] [,start% [,stop%]]]]

      Dim locateKeyword = MatchToken(SyntaxKind.LocateKeyword)
      Dim row As ExpressionSyntax = Nothing
      Dim commaToken1 As SyntaxToken = Nothing
      Dim col As ExpressionSyntax = Nothing
      Dim commaToken2 As SyntaxToken = Nothing
      Dim visible As ExpressionSyntax = Nothing
      Dim commaToken3 As SyntaxToken = Nothing
      Dim scanStart As ExpressionSyntax = Nothing
      Dim commaToken4 As SyntaxToken = Nothing
      Dim scanStop As ExpressionSyntax = Nothing

      If Not IsEndOfStatement() AndAlso Current.Kind <> SyntaxKind.CommaToken Then row = ParseExpression()
      If Not IsEndOfStatement() Then commaToken1 = MatchToken(SyntaxKind.CommaToken)
      If Not IsEndOfStatement() AndAlso Current.Kind <> SyntaxKind.CommaToken Then col = ParseExpression()
      If Not IsEndOfStatement() Then commaToken2 = MatchToken(SyntaxKind.CommaToken)
      If Not IsEndOfStatement() AndAlso Current.Kind <> SyntaxKind.CommaToken Then visible = ParseExpression()
      If Not IsEndOfStatement() Then commaToken3 = MatchToken(SyntaxKind.CommaToken)
      If Not IsEndOfStatement() AndAlso Current.Kind <> SyntaxKind.CommaToken Then scanStart = ParseExpression()
      If Not IsEndOfStatement() Then commaToken4 = MatchToken(SyntaxKind.CommaToken)
      If Not IsEndOfStatement() Then scanStop = ParseExpression()

      Return New LocateStatementSyntax(m_syntaxTree,
                                       locateKeyword,
                                       row,
                                       commaToken1,
                                       col,
                                       commaToken2,
                                       visible,
                                       commaToken3,
                                       scanStart,
                                       commaToken4,
                                       scanStop)

    End Function

    Private Function ParseLockStatement() As StatementSyntax

      'QBasic: LOCK [#]filenumber% [,{record& | [start&] TO end&}]

      Dim lockKeyword = MatchToken(SyntaxKind.LockKeyword)
      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber = ParseExpression()

      Dim comma = TryMatchToken(SyntaxKind.CommaToken)
      Dim record = If(comma IsNot Nothing, ParseExpression(), Nothing)
      If Current.Kind = SyntaxKind.ToKeyword Then
        Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
        Dim [end] = ParseExpression()
        Return New LockRangeStatementSyntax(m_syntaxTree, lockKeyword, pound, fileNumber, comma, record, toKeyword, [end])
      Else
        Return New LockStatementSyntax(m_syntaxTree, lockKeyword, pound, fileNumber, comma, record)
      End If


    End Function

    Private Function ParseMidStatement() As MidStatementSyntax

      'QBasic: MID$(stringvariable$,start%[,length%])=stringexpression$

      Dim midKeyword = MatchToken(SyntaxKind.MidKeyword)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim identifierToken = MatchToken(SyntaxKind.IdentifierToken)
      Dim positionCommaToken = MatchToken(SyntaxKind.CommaToken)
      Dim position = ParseExpression()
      Dim lengthCommaToken As SyntaxToken = Nothing
      Dim length As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        lengthCommaToken = MatchToken(SyntaxKind.CommaToken)
        length = ParseExpression()
      End If
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Dim equalToken = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()
      Return New MidStatementSyntax(m_syntaxTree, midKeyword, openParen, identifierToken, positionCommaToken, position, lengthCommaToken, length, closeParen, equalToken, expression)
    End Function

    Private Function ParseLprintStatement() As LprintStatementSyntax

      'QBasic: LPRINT [expressionlist] [{; | ,}]
      'QBasic: LPRINT USING formatstring$; expressionlist [{; | ,}]

      Dim lprintKeyword = MatchToken(SyntaxKind.LPrintKeyword)

      Dim nodes = ImmutableArray.CreateBuilder(Of SyntaxNode)()

      Dim usingKeyword As SyntaxToken = Nothing
      Dim usingFormat As ExpressionSyntax = Nothing
      Dim usingSemicolon As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.UsingKeyword Then
        usingKeyword = MatchToken(SyntaxKind.UsingKeyword)
        usingFormat = ParseExpression()
        usingSemicolon = MatchToken(SyntaxKind.SemicolonToken)
      End If

      Dim lastPosition = Current.Position
      While Not IsEndOfStatement()

        Select Case Current.Kind
          Case SyntaxKind.SpcKeyword
            Dim spcKeyword = MatchToken(SyntaxKind.SpcKeyword)
            Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
            Dim valueExpression = ParseExpression()
            Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
            Dim spcFunction = New SpcFunctionSyntax(m_syntaxTree, spcKeyword, openParen, valueExpression, closeParen)
            nodes.Add(spcFunction)
          Case SyntaxKind.TabKeyword
            Dim tabKeyword = MatchToken(SyntaxKind.TabKeyword)
            Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
            Dim valueExpression = ParseExpression()
            Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
            Dim tabFunction = New TabFunctionSyntax(m_syntaxTree, tabKeyword, openParen, valueExpression, closeParen)
            nodes.Add(tabFunction)
          Case SyntaxKind.CommaToken
            Dim commaToken = MatchToken(SyntaxKind.CommaToken)
            nodes.Add(commaToken)
          Case SyntaxKind.SemicolonToken
            Dim semiColonToken = MatchToken(SyntaxKind.SemicolonToken)
            nodes.Add(semiColonToken)
          Case Else
            Dim expression = ParseExpression()
            If expression IsNot Nothing Then
              nodes.Add(expression)
            Else
              Stop
            End If
        End Select

        If Current.Position = lastPosition Then
          ' stuck?
          Exit While
        Else
          lastPosition = Current.Position
        End If

      End While

      Return New LprintStatementSyntax(m_syntaxTree,
                                       lprintKeyword,
                                       usingKeyword,
                                       usingFormat,
                                       usingSemicolon,
                                       If(nodes IsNot Nothing, nodes.ToImmutable, Nothing))

    End Function

    Private Function ParseLSetStatement() As LSetStatementSyntax

      'QBasic: LSET stringvariable$=stringexpression$
      'QBasic: LSET recordvariable1=recordvariable2

      Dim lsetKeyword = MatchToken(SyntaxKind.LSetKeyword)
      Dim identifier = ParseIdentifier()
      Dim equal = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()
      'TODO: How to determine if what is after the equal
      '      is a variable reference or an expression syntax?
      '      Follow up question: Does it really matter?
      Return New LSetStatementSyntax(m_syntaxTree, lsetKeyword, identifier, equal, expression)

    End Function

    Private Function ParseMkDirStatement() As MkDirStatementSyntax

      'QBasic: MKDIR pathname$

      Dim mkDirKeyword = MatchToken(SyntaxKind.MkDirKeyword)
      Dim path = ParseExpression()
      Return New MkDirStatementSyntax(m_syntaxTree, mkDirKeyword, path)

    End Function

    Private Function ParseNameStatement() As NameStatementSyntax

      'QBasic: NAME oldspec$ AS newspec$

      Dim nameKeyword = MatchToken(SyntaxKind.NameKeyword)
      Dim originalPath = ParseExpression()
      Dim asKeyword = MatchToken(SyntaxKind.AsKeyword)
      Dim destinationPath = ParseExpression()
      Return New NameStatementSyntax(m_syntaxTree, nameKeyword, originalPath, asKeyword, destinationPath)

    End Function

    Private Function ParseOnStatement() As StatementSyntax

      'QBasic: ON COM(n%) GOSUB line
      'QBasic: ON ERROR {GOTO line | RESUME NEXT}
      'QBasic: ON KEY(n%) GOSUB line
      'QBasic: ON PEN GOSUB line
      'QBasic: ON PLAY(queuelimit%) GOSUB line
      'QBasic: ON TIMER(n%) GOSUB line
      'QBasic: ON expression% GOSUB line-list
      'QBasic: ON expression% GOTO line-list

      Dim onKeyword = MatchToken(SyntaxKind.OnKeyword)

      Select Case Current.Kind
        Case SyntaxKind.ErrorKeyword
          Dim errorKeyword = MatchToken(SyntaxKind.ErrorKeyword)
          Dim gotoKeyword = MatchToken(SyntaxKind.GotoKeyword)
          Dim target = ParseExpression()
          Return New OnErrorGotoStatementSyntax(m_syntaxTree,
                                                onKeyword,
                                                errorKeyword,
                                                gotoKeyword,
                                                target)
        Case Else ' Expression

          Dim expression = ParseExpression()
          Select Case Current.Kind
            'Case SyntaxKind.GosubKeyword
            '  Stop
            Case Else 'SyntaxKind.GotoKeyword
              Dim gotoKeyword = MatchToken(SyntaxKind.GotoKeyword)
              Dim targets = New List(Of SyntaxToken)
              Do
                targets.Add(MatchToken(SyntaxKind.NumberToken))
                If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
                targets.Add(MatchToken(SyntaxKind.CommaToken))
              Loop
              Return New OnGotoStatementSyntax(m_syntaxTree,
                                               onKeyword,
                                               expression,
                                               gotoKeyword,
                                               targets)
          End Select

      End Select

    End Function

    Private Function ParseOpenStatement() As OpenStatementSyntax

      'QBasic: OPEN file$ [FOR mode] [ACCESS access] [lock] AS [#]filenumber% [LEN=reclen%]
      'QBasic: OPEN "COMn: optlist1 optlist2" [FOR mode] AS [#]filenum% [LEN=reclen%]

      Dim openKeyword = MatchToken(SyntaxKind.OpenKeyword)
      Dim file = ParseExpression()

      'TODO: If file is a string expression and
      '      starts with "COMn: " then this an
      '      COM related statement.

      Dim forKeyword = TryMatchToken(SyntaxKind.ForKeyword)
      ' APPEND, BINARY, INPUT, OUTPUT, RANDOM
      Dim modeKeyword As SyntaxToken = Nothing
      If forKeyword IsNot Nothing Then
        Select Case Current.Kind
          Case SyntaxKind.AppendKeyword,
               SyntaxKind.BinaryKeyword,
               SyntaxKind.InputKeyword,
               SyntaxKind.OutputKeyword
            modeKeyword = NextToken()
          Case Else 'SyntaxKind.RandomKeyword
            modeKeyword = MatchToken(SyntaxKind.RandomKeyword)
        End Select
      End If

      Dim accessKeyword = TryMatchToken(SyntaxKind.AccessKeyword)
      ' READ, WRITE, READ WRITE
      Dim access As New List(Of SyntaxNode)
      If accessKeyword IsNot Nothing Then
        Select Case Current.Kind
          Case SyntaxKind.ReadKeyword
            access.Add(MatchToken(SyntaxKind.ReadKeyword))
            If Current.Kind = SyntaxKind.WriteKeyword Then
              access.Add(MatchToken(SyntaxKind.WriteKeyword))
            End If
          Case Else
            access.Add(MatchToken(SyntaxKind.WriteKeyword))
        End Select
      End If

      'SHARED, LOCK READ, LOCK WRITE, LOCK READ WRITE
      Dim lock As New List(Of SyntaxNode)
      Select Case Current.Kind
        Case SyntaxKind.SharedKeyword : lock.Add(MatchToken(SyntaxKind.SharedKeyword))
        Case SyntaxKind.LockKeyword
          lock.Add(MatchToken(SyntaxKind.LockKeyword))
          Select Case Current.Kind
            Case SyntaxKind.ReadKeyword
              lock.Add(MatchToken(SyntaxKind.ReadKeyword))
              If Current.Kind = SyntaxKind.WriteKeyword Then
                lock.Add(MatchToken(SyntaxKind.WriteKeyword))
              End If
            Case Else
              lock.Add(MatchToken(SyntaxKind.WriteKeyword))
          End Select
        Case Else
      End Select

      Dim asKeyword = MatchToken(SyntaxKind.AsKeyword)
      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber = ParseExpression()

      Dim lenKeyword As SyntaxToken = Nothing
      Dim equal As SyntaxToken = Nothing
      Dim recLen As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.LenKeyword Then
        lenKeyword = MatchToken(SyntaxKind.LenKeyword)
        equal = MatchToken(SyntaxKind.EqualToken)
        recLen = ParseExpression()
      End If

      Return New OpenStatementSyntax(m_syntaxTree,
                                     openKeyword,
                                     file,
                                     forKeyword,
                                     modeKeyword,
                                     accessKeyword,
                                     access.ToImmutableArray,
                                     lock.ToImmutableArray,
                                     asKeyword,
                                     pound,
                                     fileNumber,
                                     lenKeyword,
                                     equal,
                                     recLen)

    End Function

    Private Function ParseOptionStatement() As StatementSyntax

      'QBasic: OPTION BASE {0 | 1}

      Dim optionKeyword = MatchToken(SyntaxKind.OptionKeyword)
      Dim baseKeyword = MatchToken(SyntaxKind.BaseKeyword)
      If Not (Current.Kind = SyntaxKind.NumberToken AndAlso
              (Current.Text = "0" OrElse Current.Text = "1")) Then
        m_diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, SyntaxKind.NumberToken)
      End If
      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New OptionStatementSyntax(m_syntaxTree, optionKeyword, baseKeyword, numberToken)

    End Function

    Private Function ParseOutStatement() As OutStatementSyntax
      'QBasic: OUT port%, data%
      Dim outKeyword = MatchToken(SyntaxKind.OutKeyword)
      Dim port = ParseExpression()
      Dim comma = MatchToken(SyntaxKind.CommaToken)
      Dim data = ParseExpression()
      Return New OutStatementSyntax(m_syntaxTree, outKeyword, port, comma, data)
    End Function

    Private Function ParsePaintStatement() As PaintStatementSyntax

      'QBasic: PAINT [STEP] (x!,y!)[,[{color% | tile$}] [,[bordercolor%] [,background$]]]

      Dim paintKeyword = MatchToken(SyntaxKind.PaintKeyword)
      Dim stepKeyword = TryMatchToken(SyntaxKind.StepKeyword)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim x = ParseExpression()
      Dim comma1 = MatchToken(SyntaxKind.CommaToken)
      Dim y = ParseExpression()
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Dim comma2 = TryMatchToken(SyntaxKind.CommaToken)
      Dim colorOrTile As ExpressionSyntax = Nothing
      If comma2 IsNot Nothing AndAlso
         Current.Kind <> SyntaxKind.CommaToken Then
        colorOrTile = ParseExpression()
      End If
      Dim comma3 = TryMatchToken(SyntaxKind.CommaToken)
      Dim borderColor As ExpressionSyntax = Nothing
      If comma3 IsNot Nothing AndAlso
         Current.Kind <> SyntaxKind.CommaToken Then
        borderColor = ParseExpression()
      End If
      Dim comma4 = TryMatchToken(SyntaxKind.CommaToken)
      Dim background As ExpressionSyntax = Nothing
      If comma4 IsNot Nothing Then
        background = ParseExpression()
      End If

      Return New PaintStatementSyntax(m_syntaxTree,
                                      paintKeyword,
                                      stepKeyword,
                                      openParen,
                                      x,
                                      comma1,
                                      y,
                                      closeParen,
                                      comma2,
                                      colorOrTile,
                                      comma3,
                                      borderColor,
                                      comma4,
                                      background)

    End Function

    Private Function ParsePaletteStatement() As StatementSyntax

      'QBasic: PALETTE [attribute%,color&]
      'QBasic: PALETTE USING arrayname#[(index%)]

      Return ParseSbPaletteStatement()

    End Function

    Private Function ParsePcopyStatement() As PcopyStatementSyntax

      'QBasic: PCOPY sourcepage%,destinationpage%

      Dim pcopyKeyword = MatchToken(SyntaxKind.PCopyKeyword)
      Dim sourcePage = ParseExpression()
      Dim comma = MatchToken(SyntaxKind.CommaToken)
      Dim destinationPage = ParseExpression()

      Return New PcopyStatementSyntax(m_syntaxTree, pcopyKeyword, sourcePage, comma, destinationPage)

    End Function

    Private Function ParsePenStatement() As PenStatementSyntax

      'QBasic: PEN ON
      'QBasic: PEN OFF
      'QBasic: PEN STOP

      Dim penKeyword = MatchToken(SyntaxKind.PenKeyword)
      Dim verbKeyword As SyntaxToken
      Select Case Current.Kind
        Case SyntaxKind.OnKeyword, SyntaxKind.OffKeyword
          verbKeyword = NextToken()
        Case Else
          verbKeyword = MatchToken(SyntaxKind.StopKeyword)
      End Select

      Return New PenStatementSyntax(m_syntaxTree, penKeyword, verbKeyword)

    End Function

    Private Function ParsePlayStatement() As PlayStatementSyntax

      'QBasic: PLAY commandstring$
      'QBasic: PLAY ON
      'QBasic: PLAY OFF
      'QBasic: PLAY STOP

      Dim playKeyword = MatchToken(SyntaxKind.PlayKeyword)

      Select Case Current.Kind
        Case SyntaxKind.OnKeyword
          Stop
        Case SyntaxKind.OffKeyword
          Stop
        Case SyntaxKind.StopKeyword
          Stop
        Case Else
          Dim command = ParseExpression()
          Return New PlayStatementSyntax(m_syntaxTree, playKeyword, command)
      End Select

    End Function

    Private Function ParsePokeStatement() As PokeStatementSyntax

      'QBasic: POKE address,byte%

      Dim pokeKeyword = MatchToken(SyntaxKind.PokeKeyword)
      Dim offset = ParseExpression()
      Dim commaToken = MatchToken(SyntaxKind.CommaToken)
      Dim value = ParseExpression()

      Return New PokeStatementSyntax(m_syntaxTree, pokeKeyword, offset, commaToken, value)

    End Function

    Private Function ParsePresetStatement() As PresetStatementSyntax

      'QBasic: PRESET [STEP] (x!,y!) [,color%]

      Dim presetKeyword = MatchToken(SyntaxKind.PresetKeyword)
      Dim optionalStepKeyword As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.StepKeyword Then optionalStepKeyword = MatchToken(SyntaxKind.StepKeyword)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim xExpression = ParseExpression()
      Dim commaToken = MatchToken(SyntaxKind.CommaToken)
      Dim yExpression = ParseExpression()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Dim optionalCommaToken As SyntaxToken = Nothing
      Dim optionalColorExpression As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalCommaToken = MatchToken(SyntaxKind.CommaToken)
        optionalColorExpression = ParseExpression()
      End If

      Return New PresetStatementSyntax(m_syntaxTree,
                                       presetKeyword,
                                       optionalStepKeyword,
                                       openParenToken,
                                       xExpression,
                                       commaToken,
                                       yExpression,
                                       closeParenToken,
                                       optionalCommaToken,
                                       optionalColorExpression)

    End Function

    Private Function ParsePrintStatement() As PrintStatementSyntax

      'QBasic: PRINT [#filenumber%,] [expressionlist] [{; | ,}]
      'QBasic: PRINT [#filenumber%,] USING formatstring$; expressionlist [{; | ,}]

      'SpecBAS: PRINT AT 0,0;OVER 0;"OVER ";ov;" - ";ov$(ov)

      Dim printKeyword = MatchToken(SyntaxKind.PrintKeyword)

      Dim nodes = ImmutableArray.CreateBuilder(Of SyntaxNode)()

      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber As ExpressionSyntax = Nothing
      Dim comma As SyntaxToken = Nothing
      If pound IsNot Nothing Then
        fileNumber = ParseExpression()
        comma = MatchToken(SyntaxKind.CommaToken)
        nodes.Add(pound)
        nodes.Add(fileNumber)
        nodes.Add(comma)
      End If

      Dim usingKeyword As SyntaxToken = Nothing
      Dim usingFormat As ExpressionSyntax = Nothing
      Dim usingSemicolon As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.UsingKeyword Then
        usingKeyword = MatchToken(SyntaxKind.UsingKeyword)
        usingFormat = ParseExpression()
        usingSemicolon = MatchToken(SyntaxKind.SemicolonToken)
      End If

      Dim lastPosition = Current.Position
      While Not IsEndOfStatement()

        Select Case Current.Kind
          Case SyntaxKind.AtKeyword : nodes.Add(ParseSbAtStatement())
          Case SyntaxKind.InkKeyword : nodes.Add(ParseSbInkStatement())
          Case SyntaxKind.PaperKeyword : nodes.Add(ParseSbPaperStatement())
          Case SyntaxKind.OverKeyword : nodes.Add(ParseSbOverStatement())
          Case SyntaxKind.InverseKeyword : nodes.Add(ParseSbInverseStatement())
          Case SyntaxKind.MoveKeyword : nodes.Add(ParseSbMoveStatement())
          Case SyntaxKind.SpcKeyword
            Dim spcKeyword = MatchToken(SyntaxKind.SpcKeyword)
            Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
            Dim valueExpression = ParseExpression()
            Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
            Dim spcFunction = New SpcFunctionSyntax(m_syntaxTree, spcKeyword, openParen, valueExpression, closeParen)
            nodes.Add(spcFunction)
          Case SyntaxKind.TabKeyword
            Dim tabKeyword = MatchToken(SyntaxKind.TabKeyword)
            Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
            Dim valueExpression = ParseExpression()
            Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
            Dim tabFunction = New TabFunctionSyntax(m_syntaxTree, tabKeyword, openParen, valueExpression, closeParen)
            nodes.Add(tabFunction)
          Case SyntaxKind.CommaToken
            Dim commaToken = MatchToken(SyntaxKind.CommaToken)
            nodes.Add(commaToken)
          Case SyntaxKind.SemicolonToken
            Dim semiColonToken = MatchToken(SyntaxKind.SemicolonToken)
            nodes.Add(semiColonToken)
          Case Else
            Dim expression = ParseExpression()
            If expression IsNot Nothing Then
              nodes.Add(expression)
            Else
              Stop
            End If
        End Select

        If Current.Position = lastPosition Then
          ' stuck?
          Exit While
        Else
          lastPosition = Current.Position
        End If

      End While

      Return New PrintStatementSyntax(m_syntaxTree,
                                      printKeyword,
                                      pound,
                                      fileNumber,
                                      comma,
                                      usingKeyword,
                                      usingFormat,
                                      usingSemicolon,
                                      If(nodes IsNot Nothing, nodes.ToImmutable, Nothing))

    End Function

    Private Function ParsePsetStatement() As PsetStatementSyntax

      'QBasic: PSET [STEP] (x!,y!) [,color%]

      Dim psetKeyword = MatchToken(SyntaxKind.PsetKeyword)
      Dim optionalStepKeyword As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.StepKeyword Then optionalStepKeyword = MatchToken(SyntaxKind.StepKeyword)
      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim xExpression = ParseExpression()
      Dim commaToken = MatchToken(SyntaxKind.CommaToken)
      Dim yExpression = ParseExpression()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Dim optionalCommaToken As SyntaxToken = Nothing
      Dim optionalColorExpression As ExpressionSyntax = Nothing
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalCommaToken = MatchToken(SyntaxKind.CommaToken)
        optionalColorExpression = ParseExpression()
      End If

      Return New PsetStatementSyntax(m_syntaxTree,
                                     psetKeyword,
                                     optionalStepKeyword,
                                     openParenToken,
                                     xExpression,
                                     commaToken,
                                     yExpression,
                                     closeParenToken,
                                     optionalCommaToken,
                                     optionalColorExpression)

    End Function

    Private Function ParsePutStatement() As StatementSyntax

      'QBasic: PUT [#]filenumber%[,[recordnumber&][,variable]]
      'QBasic: PUT [STEP] (x1!,y1!), arrayname[(index%)] [,actionverb]

      Dim putKeyword = MatchToken(SyntaxKind.PutKeyword)

      If Current.Kind = SyntaxKind.StepKeyword OrElse
         Current.Kind = SyntaxKind.OpenParenToken Then
        ' PUT [STEP] (x,y), array[(index)] [, display_verb]
        Dim optionalStepKeyword = TryMatchToken(SyntaxKind.StepKeyword)
        Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
        Dim x = ParseExpression()
        Dim comma1 = MatchToken(SyntaxKind.CommaToken)
        Dim y = ParseExpression()
        Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
        Dim comma2 = MatchToken(SyntaxKind.CommaToken)
        Dim buffer = ParseIdentifier()
        Dim optionalComma3 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalDisplayVerb As SyntaxToken = Nothing
        If optionalComma3 IsNot Nothing Then
          Select Case Current.Kind
            Case SyntaxKind.PsetKeyword : optionalDisplayVerb = MatchToken(SyntaxKind.PsetKeyword)
            Case SyntaxKind.PresetKeyword : optionalDisplayVerb = MatchToken(SyntaxKind.PresetKeyword)
            Case SyntaxKind.AndKeyword : optionalDisplayVerb = MatchToken(SyntaxKind.AndKeyword)
            Case SyntaxKind.OrKeyword : optionalDisplayVerb = MatchToken(SyntaxKind.OrKeyword)
            Case Else ' XorKeyword
              optionalDisplayVerb = MatchToken(SyntaxKind.XorKeyword)
          End Select
        End If
        Return New PutStatementSyntax(m_syntaxTree,
                                      putKeyword,
                                      optionalStepKeyword,
                                      openParen,
                                      x,
                                      comma1,
                                      y,
                                      closeParen,
                                      comma2,
                                      buffer,
                                      optionalComma3,
                                      optionalDisplayVerb)
      Else
        ' PUT [#]filenumber%[,[recordnumber&][,variable]]
        Dim optionalPoundToken = TryMatchToken(SyntaxKind.PoundToken)
        Dim fileNumber = ParseExpression()
        Dim optionalComma1 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalRecord As ExpressionSyntax = Nothing
        If optionalComma1 IsNot Nothing AndAlso
           Not Current.Kind = SyntaxKind.CommaToken Then
          optionalRecord = ParseExpression()
        End If
        Dim optionalComma2 = TryMatchToken(SyntaxKind.CommaToken)
        Dim optionalVariable As IdentifierSyntax = Nothing
        If optionalComma2 IsNot Nothing Then
          optionalVariable = ParseIdentifier()
        End If
        Return New PutFileStatementSyntax(m_syntaxTree, putKeyword, optionalPoundToken, fileNumber, optionalComma1, optionalRecord, optionalComma2, optionalVariable)
      End If

    End Function

    Private Function ParseRandomizeStatement() As RandomizeStatementSyntax

      'QBasic: RANDOMIZE [seed%]

      Dim randomizeKeyword = MatchToken(SyntaxKind.RandomizeKeyword)
      Dim seed As ExpressionSyntax = Nothing
      If Not IsEndOfStatement() Then
        seed = ParseExpression()
      End If

      Return New RandomizeStatementSyntax(m_syntaxTree,
                                          randomizeKeyword,
                                          seed)

    End Function

    Private Function ParseReadStatement() As ReadStatementSyntax

      'QBasic: READ variablelist

      Dim readKeyword = MatchToken(SyntaxKind.ReadKeyword)

      Dim identifiersAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxToken)()

      Dim parseNextIdentifier = True
      While parseNextIdentifier AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
        identifiersAndSeparators.Add(identifier)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          identifiersAndSeparators.Add(comma)
        Else
          parseNextIdentifier = False
        End If

      End While

      Return New ReadStatementSyntax(m_syntaxTree, readKeyword, identifiersAndSeparators.ToImmutable())

    End Function

    Private Function ParseRedimStatement() As RedimStatementSyntax

      'QBasic: REDIM [SHARED] variable(subscripts) [AS type][,variable(subscripts) [AS type]]...

      'Note: Might be able to merge this with DIM.

      Dim redimKeyword = MatchToken(SyntaxKind.RedimKeyword)
      Dim sharedKeyword = TryMatchToken(SyntaxKind.SharedKeyword)

      Dim variables As New List(Of SyntaxNode)

      Do

        Dim variable = MatchToken(SyntaxKind.IdentifierToken)
        Dim optionalBoundsClause = ParseOptionalBoundsClause()
        Dim optionalAsClause = ParseOptionalAsClause()

        variables.Add(New VariableDeclarationSyntax(m_syntaxTree,
                                                         variable,
                                                         optionalBoundsClause,
                                                         optionalAsClause,
                                                         Nothing))

        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        variables.Add(MatchToken(SyntaxKind.CommaToken))

      Loop

      Return New RedimStatementSyntax(m_syntaxTree,
                                      redimKeyword,
                                      sharedKeyword,
                                      variables)

    End Function

    Private Function ParseRemStatement() As RemStatementSyntax

      'QBasic: REM remark
      'QBasic: ' remark

      Dim remKeyword = MatchToken(SyntaxKind.RemKeyword)
      Dim remLine = m_text.GetLineIndex(remKeyword.Span.Start)

      Dim position = Current.Position

      Dim comment As String = ""

      While Current.Kind <> SyntaxKind.EndOfFileToken

        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        If currentLine <> remLine Then Exit While

        Dim token = NextToken()
        For Each entry In token.LeadingTrivia
          comment &= entry.Text
        Next
        comment &= token.Text
        For Each entry In token.TrailingTrivia
          comment &= entry.Text
        Next

      End While

      Dim trailingTrivia = New List(Of SyntaxTrivia)
      For Each entry In remKeyword.TrailingTrivia
        trailingTrivia.Add(entry)
      Next
      trailingTrivia.Add(New SyntaxTrivia(m_syntaxTree, SyntaxKind.SingleLineCommentTrivia, position, comment))
      Dim tkn = New SyntaxToken(remKeyword.SyntaxTree,
                                remKeyword.Kind,
                                remKeyword.Position,
                                remKeyword.Text,
                                remKeyword.Value,
                                remKeyword.LeadingTrivia,
                                trailingTrivia.ToImmutableArray)

      Return New RemStatementSyntax(m_syntaxTree, tkn)

    End Function

    Private Function ParseResetStatement() As ResetStatementSyntax

      'QBasic: RESET

      Dim resetKeyword = MatchToken(SyntaxKind.ResetKeyword)

      Return New ResetStatementSyntax(m_syntaxTree, resetKeyword)

    End Function

    Private Function ParseRestoreStatement() As RestoreStatementSyntax

      'QBasic: RESTORE [line]

      Dim restoreKeyword = MatchToken(SyntaxKind.RestoreKeyword)

      Dim numberToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.NumberToken Then
        numberToken = MatchToken(SyntaxKind.NumberToken)
      End If

      Return New RestoreStatementSyntax(m_syntaxTree, restoreKeyword, numberToken)

    End Function

    Private Function ParseResumeStatement() As StatementSyntax

      'QBasic: RESUME [{line | NEXT}]

      Dim resumeKeyword = MatchToken(SyntaxKind.ResumeKeyword)

      If Current.Kind = SyntaxKind.NumberToken Then
        Dim line = ParseExpression()
        Return New ResumeStatementSyntax(m_syntaxTree, resumeKeyword, line)
      Else
        Dim nextKeyword = MatchToken(SyntaxKind.NextKeyword)
        Return New ResumeNextStatementSyntax(m_syntaxTree, resumeKeyword, nextKeyword)
      End If

    End Function

    Private Function ParseReturnStatement(isTopLevel As Boolean) As StatementSyntax

      'QBasic: RETURN [line2]

      If isTopLevel Then

        ' RETURN [*label*]

        ' RETURN [*line number*]

        Dim returnKeyword = MatchToken(SyntaxKind.ReturnKeyword)
        Dim keywordLine = m_text.GetLineIndex(returnKeyword.Span.Start)
        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        Dim isEof = Current.Kind = SyntaxKind.EndOfFileToken
        Dim sameLine = Not isEof AndAlso keywordLine = currentLine

        Dim token As SyntaxToken = Nothing
        If sameLine Then
          Dim kind = SyntaxKind.NumberToken
          If Current.Kind = SyntaxKind.IdentifierToken Then
            kind = SyntaxKind.IdentifierToken
          End If
          token = MatchToken(kind)
        End If
        Return New ReturnGosubStatementSyntax(m_syntaxTree, returnKeyword, token)
      Else

        ' RETURN [*expression*]

        Dim returnKeyword = MatchToken(SyntaxKind.ReturnKeyword)
        Dim keywordLine = m_text.GetLineIndex(returnKeyword.Span.Start)
        Dim currentLine = m_text.GetLineIndex(Current.Span.Start)
        Dim isEof = Current.Kind = SyntaxKind.EndOfFileToken
        Dim sameLine = Not isEof AndAlso keywordLine = currentLine
        Dim expression = If(sameLine, ParseExpression(), Nothing)
        Return New ReturnStatementSyntax(m_syntaxTree, returnKeyword, expression)

      End If

    End Function

    Private Function ParseRmDirStatement() As RmDirStatementSyntax

      'QBasic: RMDIR pathname$

      Dim rmDirKeyword = MatchToken(SyntaxKind.RmDirKeyword)
      Dim path = ParseExpression()
      Return New RmDirStatementSyntax(m_syntaxTree, rmDirKeyword, path)

    End Function

    Private Function ParseRSetStatement() As RSetStatementSyntax

      'QBasic: RSET stringvariable$=stringexpression$

      Dim rsetKeyword = MatchToken(SyntaxKind.RSetKeyword)
      Dim identifier = ParseIdentifier()
      Dim equal = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()

      Return New RSetStatementSyntax(m_syntaxTree, rsetKeyword, identifier, equal, expression)
    End Function

    Private Function ParseRunStatement() As RunStatementSyntax

      'QBasic: RUN [{linenumber | file$}]

      Dim runKeyword = MatchToken(SyntaxKind.RunKeyword)
      Dim expression As ExpressionSyntax = Nothing
      If Not IsEndOfStatement() Then
        expression = ParseExpression()
      End If

      Return New RunStatementSyntax(m_syntaxTree, runKeyword, expression)

    End Function

    Private Function ParseScreenStatement() As StatementSyntax

      'QBasic: SCREEN mode% [,[colorswitch%] [,[activepage%] [,visualpage%]]]

      Select Case Current.Kind
        Case SyntaxKind.LockKeyword, SyntaxKind.UnlockKeyword, SyntaxKind.UpdateKeyword,
             SyntaxKind.WindowKeyword, SyntaxKind.FullKeyword, SyntaxKind.GrabKeyword
          Return ParseSbScreenStatement()
        Case Else
      End Select

      ' SCREEN [mode] [, [colorburst] [, [apage] [, [vpage] [, erase]]]]

      Dim screenKeyword = MatchToken(SyntaxKind.ScreenKeyword)
      Dim optionalModeExpression As ExpressionSyntax = Nothing
      Dim optionalColorBurstCommaToken As SyntaxToken = Nothing
      Dim optionalColorBurstExpression As ExpressionSyntax = Nothing
      Dim optionalApageCommaToken As SyntaxToken = Nothing
      Dim optionalApageExpression As ExpressionSyntax = Nothing
      Dim optionalVpageCommaToken As SyntaxToken = Nothing
      Dim optionalVpageExpression As ExpressionSyntax = Nothing
      Dim optionalEraseCommaToken As SyntaxToken = Nothing
      Dim optionalEraseExpression As ExpressionSyntax = Nothing

      If Current.Kind <> SyntaxKind.CommaToken Then optionalModeExpression = ParseExpression()
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalColorBurstCommaToken = MatchToken(SyntaxKind.CommaToken)
        If Current.Kind <> SyntaxKind.CommaToken Then optionalColorBurstExpression = ParseExpression()
      End If
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalApageCommaToken = MatchToken(SyntaxKind.CommaToken)
        If Current.Kind <> SyntaxKind.CommaToken Then optionalApageExpression = ParseExpression()
      End If
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalVpageCommaToken = MatchToken(SyntaxKind.CommaToken)
        If Current.Kind <> SyntaxKind.CommaToken Then optionalVpageExpression = ParseExpression()
      End If
      If Current.Kind = SyntaxKind.CommaToken Then
        optionalEraseCommaToken = MatchToken(SyntaxKind.CommaToken)
        optionalEraseExpression = ParseExpression()
      End If

      Return New ScreenStatementSyntax(m_syntaxTree,
                                         screenKeyword,
                                         optionalModeExpression,
                                         optionalColorBurstCommaToken,
                                         optionalColorBurstExpression,
                                         optionalApageCommaToken,
                                         optionalApageExpression,
                                         optionalVpageCommaToken,
                                         optionalVpageExpression,
                                         optionalEraseCommaToken,
                                         optionalEraseExpression)

    End Function

    Private Function ParseSeekStatement() As SeekStatementSyntax

      'QBasic: SEEK [#]filenumber%, position&

      Dim seekKeyword = MatchToken(SyntaxKind.SeekKeyword)
      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber = ParseExpression()
      Dim comma = MatchToken(SyntaxKind.CommaToken)
      Dim position = ParseExpression()

      Return New SeekStatementSyntax(m_syntaxTree, seekKeyword, pound, fileNumber, comma, position)
    End Function

#Region "ParseSelect"

    Private Function ParseSelectCaseStatement() As StatementSyntax

      'QBasic:
      '
      '  SELECT CASE testexpression
      '    CASE expressionlist1
      '      [statementblock-1]
      '    [CASE expressionlist2
      '      [statementblock-2]]...
      '    [CASE ELSE
      '      [statementblock-n]]
      '  END SELECT

      Dim selectKeyword = MatchToken(SyntaxKind.SelectKeyword)
      Dim selectCaseKeyword = MatchToken(SyntaxKind.CaseKeyword)
      Dim test = ParseExpression()

      Dim cases = New List(Of CaseClauseSyntax)
      Do
        If Current.Kind = SyntaxKind.CaseKeyword Then
          If Peek(1).Kind = SyntaxKind.ElseKeyword Then Exit Do
          Dim ck = MatchToken(SyntaxKind.CaseKeyword)
          Dim matches = New List(Of ExpressionSyntax)
          Do
            If Current.Kind = SyntaxKind.IsKeyword Then
              ' Case Is > 5
              Dim isKeyword = MatchToken(SyntaxKind.IsKeyword)
              Dim comparisonKind = SyntaxKind.EqualToken
              Select Case Current.Kind
                Case SyntaxKind.LessThanToken,
                     SyntaxKind.LessThanGreaterThanToken,
                     SyntaxKind.LessThanEqualToken,
                     SyntaxKind.EqualToken,
                     SyntaxKind.GreaterThanEqualToken,
                     SyntaxKind.GreaterThanToken,
                     SyntaxKind.LessThanGreaterThanToken
                  comparisonKind = Current.Kind
                Case Else
              End Select
              Dim comparison = MatchToken(comparisonKind)
              Dim expression = ParseExpression()
              matches.Add(New CaseIsMatchExpressionSyntax(m_syntaxTree, isKeyword, comparison, expression))
            Else
              ' Case 1
              ' Case variable
              ' Case 1 To 10
              Dim expression = ParseExpression()
              If Current.Kind = SyntaxKind.ToKeyword Then
                Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
                Dim expressionTo = ParseExpression()
                matches.Add(New CaseMatchRangeExpressionSyntax(m_syntaxTree, expression, toKeyword, expressionTo))
              Else
                matches.Add(New CaseMatchExpressionSyntax(m_syntaxTree, expression))
              End If
            End If
            If Current.Kind <> SyntaxKind.CommaToken Then
              Exit Do
            Else
              Dim c = MatchToken(SyntaxKind.CommaToken)
            End If
          Loop
          Dim statement = ParseBlockStatement(False) 'ParseStatement()
          cases.Add(New CaseClauseSyntax(m_syntaxTree, ck, matches.ToImmutableArray, statement))
        Else
          Exit Do
        End If
      Loop

      Dim caseElseBlock = ParseCaseElseClause()

      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Dim endSelectKeyword = MatchToken(SyntaxKind.SelectKeyword)

      Return New SelectCaseStatementSyntax(m_syntaxTree,
                                           selectKeyword,
                                           selectCaseKeyword,
                                           test,
                                           cases.ToImmutableArray,
                                           caseElseBlock,
                                           endKeyword,
                                           endSelectKeyword)

    End Function

    Private Function ParseCaseElseClause() As CaseElseClauseSyntax
      If Current.Kind <> SyntaxKind.CaseKeyword Then Return Nothing
      If Peek(1).Kind <> SyntaxKind.ElseKeyword Then Return Nothing
      Dim caseKeyword = MatchToken(SyntaxKind.CaseKeyword)
      Dim elseKeyword = MatchToken(SyntaxKind.ElseKeyword)
      Dim statement = ParseBlockStatement(False) 'ParseStatement()
      Return New CaseElseClauseSyntax(m_syntaxTree, caseKeyword, elseKeyword, statement)
    End Function

#End Region

    Private Function ParseSharedStatement() As SharedStatementSyntax

      'QBasic: SHARED variable[()] [AS type] [,variable[()] [AS type]]...

      Dim sharedKeyword = MatchToken(SyntaxKind.SharedKeyword)
      Dim variables As New List(Of SyntaxNode)

      Do

        Dim variable = MatchToken(SyntaxKind.IdentifierToken)
        Dim optionalBoundsClause = ParseOptionalBoundsClause()
        Dim optionalAsClause = ParseOptionalAsClause()

        variables.Add(New VariableDeclarationSyntax(m_syntaxTree,
                                                         variable,
                                                         optionalBoundsClause,
                                                         optionalAsClause,
                                                         Nothing))

        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        variables.Add(MatchToken(SyntaxKind.CommaToken))

      Loop

      Return New SharedStatementSyntax(m_syntaxTree,
                                       sharedKeyword,
                                       variables)

    End Function

    Private Function ParseShellStatement() As ShellStatementSyntax
      'QBasic: SHELL [commandstring$]
      Dim shellKeyword = MatchToken(SyntaxKind.ShellKeyword)
      Dim commandString = TryParseExpression()
      Return New ShellStatementSyntax(m_syntaxTree, shellKeyword, commandString)
    End Function

    Private Function ParseSleepStatement() As SleepStatementSyntax

      'QBasic: SLEEP [seconds&]

      Dim sleepKeyword = MatchToken(SyntaxKind.SleepKeyword)
      Dim seconds = If(Not IsEndOfStatement(), ParseExpression(), Nothing)

      Return New SleepStatementSyntax(m_syntaxTree, sleepKeyword, seconds)

    End Function

    Private Function ParseSoundStatement() As SoundStatementSyntax

      'QBasic: SOUND frequency, duration

      Dim soundKeyword = MatchToken(SyntaxKind.SoundKeyword)
      Dim frequency = ParseExpression()
      Dim comma = MatchToken(SyntaxKind.CommaToken)
      Dim duration = ParseExpression()

      Return New SoundStatementSyntax(m_syntaxTree, soundKeyword, frequency, comma, duration)

    End Function

    Private Function ParseStopStatement() As StatementSyntax

      'QBasic: STOP

      Dim stopKeyword = MatchToken(SyntaxKind.StopKeyword)
      Return New StopStatementSyntax(m_syntaxTree, stopKeyword)

    End Function

    Private Function ParseStaticStatement() As StaticStatementSyntax

      'QBasic: STATIC variable[()] [AS type] [,variable[()] [AS type]]...

      Dim staticKeyword = MatchToken(SyntaxKind.StaticKeyword)
      Dim variables As New List(Of SyntaxNode)

      Do

        Dim variable = MatchToken(SyntaxKind.IdentifierToken)
        Dim optionalBoundsClause = ParseOptionalBoundsClause()
        Dim optionalAsClause = ParseOptionalAsClause()

        variables.Add(New VariableDeclarationSyntax(m_syntaxTree,
                                                         variable,
                                                         optionalBoundsClause,
                                                         optionalAsClause,
                                                         Nothing))

        If Current.Kind <> SyntaxKind.CommaToken Then Exit Do
        variables.Add(MatchToken(SyntaxKind.CommaToken))

      Loop

      Return New StaticStatementSyntax(m_syntaxTree,
                                       staticKeyword,
                                       variables)

    End Function

    Private Function ParseStrigStatement() As StrigStatementSyntax

      'QBasic: STRIG(n%) ON
      'QBasic: STRIG(n%) OFF
      'QBasic: STRIG(n%) STOP

      Dim strigKeyword = MatchToken(SyntaxKind.StrigKeyword)
      Dim openParen = MatchToken(SyntaxKind.OpenParenToken)
      Dim n = ParseExpression()
      Dim closeParen = MatchToken(SyntaxKind.CloseParenToken)
      Dim verb = MatchTokens({SyntaxKind.OnKeyword, SyntaxKind.OffKeyword, SyntaxKind.StopKeyword})

      Return New StrigStatementSyntax(m_syntaxTree, strigKeyword, openParen, n, closeParen, verb)

    End Function

    Private Function ParseSubStatement() As SubStatementSyntax

      'QBasic:
      '
      '  SUB name[(parameterlist)] [STATIC]
      '    [statementblock]
      '  END SUB
      Dim subKeyword = MatchToken(SyntaxKind.SubKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)

      Dim openParenToken As SyntaxToken = Nothing
      Dim parameters As SeparatedSyntaxList(Of ParameterSyntax) = Nothing
      Dim closeParenToken As SyntaxToken = Nothing

      If Current.Kind = SyntaxKind.OpenParenToken Then
        openParenToken = MatchToken(SyntaxKind.OpenParenToken)
        parameters = ParseParameterList()
        closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      End If
      Dim staticKeyword = TryMatchToken(SyntaxKind.StaticKeyword)

      Dim statements = ParseBlockStatement(False)

      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Dim endSubKeyword = MatchToken(SyntaxKind.SubKeyword)

      Return New SubStatementSyntax(m_syntaxTree, subKeyword, identifier, openParenToken, parameters, closeParenToken, staticKeyword, statements, endKeyword, endSubKeyword)

    End Function

    Private Function ParseSwapStatement() As SwapStatementSyntax

      'QBasic: SWAP variable1, variable2

      Dim swapKeyword = MatchToken(SyntaxKind.SwapKeyword)
      Dim variable1 = ParseIdentifier()
      Dim comma = MatchToken(SyntaxKind.CommaToken)
      Dim variable2 = ParseIdentifier()

      Return New SwapStatementSyntax(m_syntaxTree, swapKeyword, variable1, comma, variable2)

    End Function

    Private Function ParseSystemStatement() As SystemStatementSyntax

      'QBasic: SYSTEM

      Dim systemKeyword = MatchToken(SyntaxKind.SystemKeyword)
      Return New SystemStatementSyntax(m_syntaxTree, systemKeyword)

    End Function

    Private Function ParseTimeStatement() As TimeStatementSyntax
      'QBasic: TIME$ = stringexpression$
      Dim timeKeyword = MatchToken(SyntaxKind.TimeKeyword)
      Dim equal = MatchToken(SyntaxKind.EqualToken)
      Dim expression = ParseExpression()
      Return New TimeStatementSyntax(m_syntaxTree, timeKeyword, equal, expression)
    End Function

    Private Function ParseTimerStatement() As TimerStatementSyntax

      'QBasic: TIMER ON
      'QBasic: TIMER OFF
      'QBasic: TIMER STOP

      Dim timerKeyword = MatchToken(SyntaxKind.TimerKeyword)
      Dim verb = MatchTokens({SyntaxKind.OnKeyword, SyntaxKind.OffKeyword, SyntaxKind.StopKeyword})

      Return New TimerStatementSyntax(m_syntaxTree, timerKeyword, verb)

    End Function

    Private Function ParseTroffStatement() As TroffStatementSyntax
      'QBasic: TROFF
      Dim troffKeyword = MatchToken(SyntaxKind.TroffKeyword)
      Return New TroffStatementSyntax(m_syntaxTree, troffKeyword)
    End Function

    Private Function ParseTronStatement() As TronStatementSyntax
      'QBasic: TRON
      Dim tronKeyword = MatchToken(SyntaxKind.TronKeyword)
      Return New TronStatementSyntax(m_syntaxTree, tronKeyword)
    End Function

    Private Function ParseTypeStatement() As TypeStatementSyntax

      'QBasic:
      '
      '  TYPE usertype
      '    elementname AS typename
      '    [elementname AS typename]
      '    .
      '    .
      '    .
      '  END TYPE

      Dim typeKeyword = MatchToken(SyntaxKind.TypeKeyword)
      Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim properties As New List(Of ParameterSyntax)
      Do
        If IsEndOfBlock() Then Exit Do
        properties.Add(ParseParameter)
      Loop
      Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      Dim endTypeKeyword = MatchToken(SyntaxKind.TypeKeyword)

      Return New TypeStatementSyntax(m_syntaxTree, typeKeyword, identifier, properties, endKeyword, endTypeKeyword)

    End Function

    Private Function ParseUnlockStatement() As StatementSyntax

      'QBasic: UNLOCK [#]filenumber% [,{record& | [start&] TO end&}]

      Dim unlockKeyword = MatchToken(SyntaxKind.UnlockKeyword)
      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber = ParseExpression()

      Dim comma = TryMatchToken(SyntaxKind.CommaToken)
      Dim record = If(comma IsNot Nothing, ParseExpression(), Nothing)
      If Current.Kind = SyntaxKind.ToKeyword Then
        Dim toKeyword = MatchToken(SyntaxKind.ToKeyword)
        Dim [end] = ParseExpression()
        Return New UnlockRangeStatementSyntax(m_syntaxTree, unlockKeyword, pound, fileNumber, comma, record, toKeyword, [end])
      Else
        Return New UnlockStatementSyntax(m_syntaxTree, unlockKeyword, pound, fileNumber, comma, record)
      End If

    End Function

    Private Function ParseViewStatement() As StatementSyntax

      'QBasic: VIEW [[SCREEN] (x1!,y1!)-(x2!,y2!) [,[color%] [,border%]]]
      'QBasic: VIEW PRINT [toprow% TO bottomrow%]

      Dim viewKeyword = MatchToken(SyntaxKind.ViewKeyword)

      If Current.Kind = SyntaxKind.PrintKeyword Then

        ' VIEW PRINT [toprow% TO bottomrow%]

        Dim printKeyword = MatchToken(SyntaxKind.PrintKeyword)

        Dim topRow As ExpressionSyntax = Nothing
        Dim toKeyword As SyntaxToken = Nothing
        Dim bottomRow As ExpressionSyntax = Nothing

        If Not IsEndOfStatement() Then
          topRow = ParseExpression()
          toKeyword = MatchToken(SyntaxKind.ToKeyword)
          bottomRow = ParseExpression()
        End If

        Return New ViewPrintStatementSyntax(m_syntaxTree, viewKeyword, printKeyword, topRow, toKeyword, bottomRow)

      Else

        ' VIEW [[SCREEN] (x1!,y1!)-(x2!,y2!) [,[color%] [,border%]]]

        Dim screenKeyword = TryMatchToken(SyntaxKind.ScreenKeyword)

        Dim openParen1 = MatchToken(SyntaxKind.OpenParenToken)
        Dim x1 = ParseExpression()
        Dim comma1 = MatchToken(SyntaxKind.CommaToken)
        Dim y1 = ParseExpression()
        Dim closeParen1 = MatchToken(SyntaxKind.CloseParenToken)
        Dim dash = MatchToken(SyntaxKind.MinusToken)
        Dim openParen2 = MatchToken(SyntaxKind.OpenParenToken)
        Dim x2 = ParseExpression()
        Dim comma2 = MatchToken(SyntaxKind.CommaToken)
        Dim y2 = ParseExpression()
        Dim closeParen2 = MatchToken(SyntaxKind.CloseParenToken)

        Dim comma3 = TryMatchToken(SyntaxKind.CommaToken)
        Dim color As ExpressionSyntax = Nothing
        If comma3 IsNot Nothing AndAlso
           Not Current.Kind = SyntaxKind.CommaToken Then
          color = ParseExpression()
        End If
        Dim comma4 = TryMatchToken(SyntaxKind.CommaToken)
        Dim border As ExpressionSyntax = Nothing
        If comma4 IsNot Nothing Then
          border = ParseExpression()
        End If

        Return New ViewStatementSyntax(m_syntaxTree,
                                       viewKeyword,
                                       screenKeyword,
                                       openParen1,
                                       x1,
                                       comma1,
                                       y1,
                                       closeParen1,
                                       dash,
                                       openParen2,
                                       x2,
                                       comma2,
                                       y2,
                                       closeParen2,
                                       comma3,
                                       color,
                                       comma4,
                                       border)

      End If

    End Function

    Private Function ParseWaitStatement() As StatementSyntax

      'QBasic: WAIT portnumber%, AND-expression% [,XOR-expression%]

      'SpecBAS: WAIT [SCREEN] numexpr

      If Peek(1).Kind = SyntaxKind.ScreenKeyword Then
        Return ParseSbWaitStatement()
      End If

      Dim waitKeyword = MatchToken(SyntaxKind.WaitKeyword)
      Dim portNumber = ParseExpression()
      Dim comma1 = MatchToken(SyntaxKind.CommaToken)
      Dim andExpression = ParseExpression()
      Dim comma2 = TryMatchToken(SyntaxKind.CommaToken)
      Dim xorExpression = If(comma2 IsNot Nothing, ParseExpression(), Nothing)
      Return New WaitStatementSyntax(m_syntaxTree, waitKeyword, portNumber, comma1, andExpression, comma2, xorExpression)

    End Function

    Private Function ParseWhileStatement(isTopLevel As Boolean) As StatementSyntax

      'QBasic:
      '
      '  WHILE condition
      '    . 
      '    .
      '    .
      '  WEND

      Dim whileKeyword = MatchToken(SyntaxKind.WhileKeyword)
      Dim expression = ParseExpression()
      Dim body = ParseBlockStatement(isTopLevel)
      'If Peek(0).Kind = SyntaxKind.WendKeyword Then
      Dim wendKeyword = MatchToken(SyntaxKind.WendKeyword)
      'Else
      '  Dim endKeyword = MatchToken(SyntaxKind.EndKeyword)
      '  Dim endingWhilekeyword = MatchToken(SyntaxKind.WhileKeyword)
      'End If
      'TODO: Need to decide how to handle WEND and END WHILE...
      Return New WhileStatementSyntax(m_syntaxTree, whileKeyword, expression, body, wendKeyword)

    End Function

    Private Function ParseWidthStatement() As StatementSyntax

      'QBasic: WIDTH [columns%] [,rows%]
      'QBasic: WIDTH {#filenumber% | device$}, columns%
      'QBasic: WIDTH LPRINT columns%

      Dim widthKeyword = MatchToken(SyntaxKind.WidthKeyword)

      If Current.Kind = SyntaxKind.PoundToken Then
        Dim poundKeyword = MatchToken(SyntaxKind.PoundToken)
        Dim fileNumber = ParseExpression()
        Dim commaToken = MatchToken(SyntaxKind.CommaToken)
        Dim columns = ParseExpression()
        Return New WidthFileStatementSyntax(m_syntaxTree, widthKeyword, poundKeyword, fileNumber, commaToken, columns)
      ElseIf Current.Kind = SyntaxKind.LPrintKeyword Then
        Dim lprintKeyword = MatchToken(SyntaxKind.LPrintKeyword)
        Dim columns = ParseExpression()
        Return New WidthLprintStatementSyntax(m_syntaxTree, widthKeyword, lprintKeyword, columns)
      Else
        'TODO: Need to determine if there is an expression and whether or not it is a string or numeric...
        Dim optionalColumns As ExpressionSyntax = Nothing
        Dim optionalCommaToken As SyntaxToken = Nothing
        Dim optionalLines As ExpressionSyntax = Nothing
        If Not IsEndOfStatement() AndAlso Not Current.Kind = SyntaxKind.CommaToken Then
          optionalColumns = ParseExpression()
        End If
        If Current.Kind = SyntaxKind.CommaToken Then
          optionalCommaToken = MatchToken(SyntaxKind.CommaToken)
          If Not IsEndOfStatement() Then
            optionalLines = ParseExpression()
          End If
        End If
        Return New WidthStatementSyntax(m_syntaxTree, widthKeyword, optionalColumns, optionalCommaToken, optionalLines)
      End If

    End Function

    Private Function ParseWindowStatement() As WindowStatementSyntax

      'QBasic: WINDOW [[SCREEN] (x1!,y1!)-(x2!,y2!)]

      Dim windowKeyword = MatchToken(SyntaxKind.WindowKeyword)
      Dim screenKeyword = TryMatchToken(SyntaxKind.ScreenKeyword)
      Dim openParen1 = TryMatchToken(SyntaxKind.OpenParenToken)
      Dim x1 As ExpressionSyntax = Nothing
      Dim comma1 As SyntaxToken = Nothing
      Dim y1 As ExpressionSyntax = Nothing
      Dim closeparen1 As SyntaxToken = Nothing
      Dim dash As SyntaxToken = Nothing
      Dim openParen2 As SyntaxToken = Nothing
      Dim x2 As ExpressionSyntax = Nothing
      Dim comma2 As SyntaxToken = Nothing
      Dim y2 As ExpressionSyntax = Nothing
      Dim closeparen2 As SyntaxToken = Nothing
      If screenKeyword IsNot Nothing OrElse
         openParen1 IsNot Nothing Then
        x1 = ParseExpression()
        comma1 = MatchToken(SyntaxKind.CommaToken)
        y1 = ParseExpression()
        closeparen1 = MatchToken(SyntaxKind.CloseParenToken)
        dash = MatchToken(SyntaxKind.MinusToken)
        openParen2 = MatchToken(SyntaxKind.OpenParenToken)
        x2 = ParseExpression()
        comma2 = MatchToken(SyntaxKind.CommaToken)
        y2 = ParseExpression()
        closeparen2 = MatchToken(SyntaxKind.CloseParenToken)
      End If

      Return New WindowStatementSyntax(m_syntaxTree,
                                       windowKeyword,
                                       screenKeyword,
                                       openParen1,
                                       x1,
                                       comma1,
                                       y1,
                                       closeparen1,
                                       dash,
                                       openParen2,
                                       x2,
                                       comma2,
                                       y2,
                                       closeparen2)

    End Function

    Private Function ParseWriteStatement() As WriteStatementSyntax

      'QBasic: WRITE [[#]filenumber%,] expressionlist

      Dim writeKeyword = MatchToken(SyntaxKind.WriteKeyword)
      Dim pound = TryMatchToken(SyntaxKind.PoundToken)
      Dim fileNumber As ExpressionSyntax = Nothing
      Dim comma As SyntaxToken = Nothing
      If pound IsNot Nothing OrElse
         Peek(1).Kind = SyntaxKind.CommaToken Then
        fileNumber = ParseExpression()
        comma = MatchToken(SyntaxKind.CommaToken)
      End If
      Dim expressions = ParseExpressionList()

      Return New WriteStatementSyntax(m_syntaxTree,
                                      writeKeyword,
                                      pound,
                                      fileNumber,
                                      comma,
                                      expressions)

    End Function

    Private Function ParseExpressionList() As ImmutableArray(Of SyntaxNode)

      Dim nodes As New List(Of SyntaxNode)

      Do
        nodes.Add(ParseExpression)
        If Not Current.Kind = SyntaxKind.CommaToken Then Exit Do
        nodes.Add(MatchToken(SyntaxKind.CommaToken))
      Loop

      Return nodes.ToImmutableArray

    End Function

#Region "Expressions"

    Private Function ParseBinaryExpression(Optional parentPrecedence As Integer = 0) As ExpressionSyntax
      Dim left As ExpressionSyntax
      Dim unaryOperatorPrecedence = SyntaxFacts.GetUnaryOperatorPrecedence(Current.Kind)
      If unaryOperatorPrecedence <> 0 AndAlso unaryOperatorPrecedence > parentPrecedence Then
        Dim operatorToken = NextToken()
        Dim operand = ParseBinaryExpression(unaryOperatorPrecedence)
        left = New UnaryExpressionSyntax(m_syntaxTree, operatorToken, operand)
      Else
        left = ParsePrimaryExpression()
      End If
      Do
        Dim precedence = SyntaxFacts.GetBinaryOperatorPrecedence(Current.Kind)
        If precedence = 0 OrElse precedence <= parentPrecedence Then Exit Do
        Dim operatorToken = NextToken()
        Dim right = ParseBinaryExpression(precedence)
        left = New BinaryExpressionSyntax(m_syntaxTree, left, operatorToken, right)
      Loop
      Return left
    End Function

    'Private Function ParseCallExpression() As ExpressionSyntax

    '  ' *identifier*(*arguments*)

    '  Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
    '  Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
    '  Dim arguments = ParseArguments()
    '  Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
    '  Return New CallExpressionSyntax(m_syntaxTree, identifier, openParenToken, arguments, closeParenToken)

    'End Function

    Private Function ParseExpression() As ExpressionSyntax
      Return ParseBinaryExpression()
    End Function

    Private Function ParseExpressionStatement() As ExpressionStatementSyntax
      Dim expression = ParseExpression()
      Return New ExpressionStatementSyntax(m_syntaxTree, expression)
    End Function

    'Private Function ParseNameOrCallExpression() As ExpressionSyntax
    '  If Peek(0).Kind = SyntaxKind.IdentifierToken AndAlso
    '     Peek(1).Kind = SyntaxKind.OpenParenToken Then
    '    Return ParseCallExpression()
    '  End If
    '  Return ParseNameExpression()
    'End Function

    Private Function ParsePrimaryExpression() As ExpressionSyntax
      Select Case Current.Kind
        Case SyntaxKind.OpenParenToken : Return ParseParenExpression()
        'Case SyntaxKind.FalseKeyword, SyntaxKind.TrueKeyword : Return ParseBooleanLiteral()
        Case SyntaxKind.NumberToken : Return ParseNumberLiteral()
        Case SyntaxKind.StringToken : Return ParseStringLiteral()
          'Case SyntaxKind.IdentifierToken : ParseNameOrCallExpression()
        Case Else
          Return ParseIdentifierExpression() 'ParseNameOrCallExpression()
      End Select
    End Function

#End Region

#Region "Sub-Parsers, Clauses, etc."

    Private Function ParseArguments() As SeparatedSyntaxList(Of ExpressionSyntax)

      ' ... *expression*[, *expression*] ...

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)

      Dim parseNextArgument = True
      While parseNextArgument AndAlso
            Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken
        Dim expression = ParseExpression()
        nodesAndSeparators.Add(expression)
        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextArgument = False
        End If
      End While
      Return New SeparatedSyntaxList(Of ExpressionSyntax)(nodesAndSeparators.ToImmutable)

    End Function

    Private Function ParseBlockStatement(isTopLevel As Boolean, Optional singleLine As Boolean = False) As BlockStatementSyntax
      Dim statements = ImmutableArray.CreateBuilder(Of StatementSyntax)
      Dim openBraceToken As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.OpenBraceToken Then
        openBraceToken = MatchToken(SyntaxKind.OpenBraceToken)
      End If
      Dim startToken As SyntaxToken '= Current()

      Dim beginLine = m_text.GetLineIndex(Current.Span.Start)

      While Current.Kind <> SyntaxKind.EndOfFileToken AndAlso
            Not IsEndOfBlock()
        'Current.Kind <> SyntaxKind.CloseBraceToken

        If singleLine Then
          Dim peekLine = m_text.GetLineIndex(Current.Span.Start)
          If peekLine > beginLine Then
            Exit While
          End If
        End If

        startToken = Current()
        Dim statement = ParseStatement(isTopLevel)
        statements.Add(statement)

        ' If ParseStatement did not consume any tokens,
        ' let's skip the current token and continue in 
        ' order to avoid an infinite loop.
        '
        ' We do not need to report an error, because we've
        ' already tried to parse an expression statement
        ' and reported one.
        If Current() Is startToken Then
          NextToken()
        End If

      End While
      Dim closeBraceToken As SyntaxToken = Nothing
      If openBraceToken IsNot Nothing Then
        closeBraceToken = MatchToken(SyntaxKind.CloseBraceToken)
      End If
      Return New BlockStatementSyntax(m_syntaxTree, openBraceToken, statements.ToImmutable, closeBraceToken)
    End Function

    'Private Function ParseBooleanLiteral() As ExpressionSyntax

    '  ' ... true ...

    '  ' or

    '  ' ... false ...

    '  Dim isTrue = (Current.Kind = SyntaxKind.TrueKeyword)
    '  Dim keywordToken = MatchToken(If(isTrue, SyntaxKind.TrueKeyword, SyntaxKind.FalseKeyword))
    '  Return New LiteralExpressionSyntax(m_syntaxTree, keywordToken, isTrue)

    'End Function

    Private Function ParseIdentifier() As IdentifierSyntax
      Dim identifierToken = NextToken()
      Dim openParen As SyntaxToken = Nothing
      Dim arguments As SeparatedSyntaxList(Of ExpressionSyntax) = Nothing
      Dim closeParen As SyntaxToken = Nothing
      If Current.Kind = SyntaxKind.OpenParenToken Then
        openParen = MatchToken(SyntaxKind.OpenParenToken)
        arguments = ParseArguments()
        closeParen = MatchToken(SyntaxKind.CloseParenToken)
      End If
      Return New IdentifierSyntax(m_syntaxTree,
                                  identifierToken,
                                  openParen,
                                  arguments,
                                  closeParen)
    End Function

    Public Function ParseIdentifierExpression() As IdentifierExpressionSyntax

      Dim identifier = ParseIdentifier()

      Return New IdentifierExpressionSyntax(m_syntaxTree, identifier)

    End Function

    'Private Function ParseNameExpression() As ExpressionSyntax

    '  ' ... *identifier* ...

    '  Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
    '  Return New NameExpressionSyntax(m_syntaxTree, identifier)

    'End Function

    Private Function ParseNumberLiteral() As ExpressionSyntax

      ' ... *number* ...

      Dim numberToken = MatchToken(SyntaxKind.NumberToken)
      Return New LiteralExpressionSyntax(m_syntaxTree, numberToken)

    End Function

    Private Function ParseParameter() As ParameterSyntax

      ' ... *identifier* AS *type* ...

      'Dim identifier = MatchToken(SyntaxKind.IdentifierToken)
      Dim identifier = ParseIdentifier()
      Dim asClause As AsClause = Nothing
      If Current.Kind = SyntaxKind.AsKeyword Then asClause = ParseAsClause()
      Return New ParameterSyntax(m_syntaxTree, identifier, asClause)

    End Function

    Private Function ParseParameterList() As SeparatedSyntaxList(Of ParameterSyntax)

      ' ...(... *identifier* AS *type*[, *identifier* AS *type*] ...)...

      Dim nodesAndSeparators = ImmutableArray.CreateBuilder(Of SyntaxNode)()

      Dim parseNextParameter = True
      While parseNextParameter AndAlso
            Current.Kind <> SyntaxKind.CloseParenToken AndAlso
            Current.Kind <> SyntaxKind.EndOfFileToken

        Dim parameter = ParseParameter()
        nodesAndSeparators.Add(parameter)

        If Current.Kind = SyntaxKind.CommaToken Then
          Dim comma = MatchToken(SyntaxKind.CommaToken)
          nodesAndSeparators.Add(comma)
        Else
          parseNextParameter = False
        End If

      End While

      Return New SeparatedSyntaxList(Of ParameterSyntax)(nodesAndSeparators.ToImmutable())

    End Function

    Private Function ParseParenExpression() As ExpressionSyntax

      ' ... (*expression*) ...

      Dim openParenToken = MatchToken(SyntaxKind.OpenParenToken)
      Dim expression = ParseExpression()
      Dim closeParenToken = MatchToken(SyntaxKind.CloseParenToken)
      Return New ParenExpressionSyntax(m_syntaxTree, openParenToken, expression, closeParenToken)

    End Function

    Private Function ParseStatementSeparatorSyntax() As StatementSeparatorStatementSyntax
      Dim colonToken = MatchToken(SyntaxKind.ColonToken)
      Return New StatementSeparatorStatementSyntax(m_syntaxTree, colonToken)
    End Function

    Private Function ParseStringLiteral() As ExpressionSyntax

      ' ... "*string*" ...

      Dim stringToken = MatchToken(SyntaxKind.StringToken)
      Return New LiteralExpressionSyntax(m_syntaxTree, stringToken)

    End Function

#End Region

#Region "Internal Support Methods"

    Private Function Current() As SyntaxToken
      Return Peek(0)
    End Function

    Private Function IsEndOfBlock() As Boolean
      ' If/ElseIf/End If
      ' For/Next
      ' While/End While
      ' While/Wend
      ' Do/Loop
      ' Function/End Function
      ' Sub/End Sub
      ' Type/End Type
      ' Def/End Def
      ' Select Case/Case/End Select
      ' ----
      ' Struct/End Struct
      ' Try/Catch/Finally/End Try
      ' Namespace/End Namespace
      ' Module/End Module
      ' Class/End Class
      ' Interface/End Interface
      ' Enum/End Enum
      ' Using/End Using
      ' SyncLock/End SyncLock
      Select Case Current.Kind
        'Case SyntaxKind.EndDefKeyword,
        '     SyntaxKind.EndFunctionKeyword,
        '     SyntaxKind.EndIfKeyword,
        '     SyntaxKind.EndSelectKeyword,
        '     SyntaxKind.EndSubKeyword,
        '     SyntaxKind.EndTypeKeyword
        '  Stop
        Case SyntaxKind.EndKeyword
          Select Case Peek(1).Kind
            Case SyntaxKind.DefKeyword,
                 SyntaxKind.FunctionKeyword,
                 SyntaxKind.IfKeyword,
                 SyntaxKind.SelectKeyword,
                 SyntaxKind.SubKeyword,
                 SyntaxKind.TypeKeyword
              Return True
            Case Else
              Return False
          End Select
        Case SyntaxKind.CaseKeyword,
             SyntaxKind.ElseKeyword,
             SyntaxKind.ElseIfKeyword,
             SyntaxKind.LoopKeyword,
             SyntaxKind.NextKeyword,
             SyntaxKind.WendKeyword
          Return True
        Case Else
          Return False
      End Select
    End Function

    Private Function IsEndOfStatement() As Boolean
      Select Case Current.Kind
        Case SyntaxKind.ColonToken,
             SyntaxKind.EndOfFileToken
          Return True
        Case SyntaxKind.LineBreakTrivia
          Return True
        Case Else
          ' This here might seem a bit strange,
          ' but the logic seems sound. We wouldn't
          ' be asking is we are at the end of a
          ' statement at the beginning of a statement
          ' as we would be calling this method within
          ' the process of parsing a current statement.
          ' So the logic here is that while we are
          ' parsing the current statement, we are
          ' trying to determine that whether or not
          ' we have reached the completion of the 
          ' current statement and this might take
          ' place after we've "popped" the last item
          ' out of the token stream. This last "popped"
          ' token might contain a "line break" trivia
          ' element; thus giving us an indication that
          ' we've reached the end of a line and need
          ' to return that (because of the end of line)
          ' we've, thus, reached the end of the current
          ' statement.
          Return IsEndOfBlock() OrElse
                 If(Peek(-1)?.TrailingTrivia.LastOrDefault?.Kind = SyntaxKind.LineBreakTrivia, False)
      End Select
    End Function

    Private Function IsPossibleExpression() As Boolean
      ' 1
      ' 1 + 1
      ' 1 + a
      ' a
      ' a + a
      ' a + 1
      ' (a)
      ' (1)
      ' int(value)
      For Each entry In Peek(-1).TrailingTrivia
        If entry.Kind = SyntaxKind.LineBreakTrivia Then Return False
      Next
      Return Current.Kind = SyntaxKind.NumberToken OrElse
             Current.Kind = SyntaxKind.IdentifierToken OrElse
             Current.Kind = SyntaxKind.OpenParenToken OrElse
             Current.Kind.Is_Keyword
    End Function

    Private Function MatchToken(kind As SyntaxKind) As SyntaxToken
      If Current.Kind = kind Then
        Return NextToken()
      End If
      m_diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, kind)
      Return New SyntaxToken(m_syntaxTree, kind, Current.Position, Nothing, Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
    End Function

    Private Function MatchTokens(kinds() As SyntaxKind) As SyntaxToken
      For Each kind In kinds
        If Current.Kind = kind Then
          Return NextToken()
        End If
      Next
      m_diagnostics.ReportUnexpectedToken(Current.Location, Current.Kind, kinds.Last)
      Return New SyntaxToken(m_syntaxTree, kinds.Last, Current.Position, Nothing, Nothing, ImmutableArray(Of SyntaxTrivia).Empty, ImmutableArray(Of SyntaxTrivia).Empty)
    End Function

    Private Function NextToken() As SyntaxToken
      Dim current = Me.Current
      m_position += 1
      Return current
    End Function

    Private Function Peek(offset As Integer) As SyntaxToken
      Dim index = m_position + offset
      If index >= m_tokens.Length Then
        Return m_tokens(m_tokens.Length - 1)
      End If
      Return m_tokens(index)
    End Function

    Private Function TryMatchToken(kind As SyntaxKind) As SyntaxToken
      If Current.Kind = kind Then Return MatchToken(kind)
      Return Nothing
    End Function

    Private Function TryMatchTokens(kinds() As SyntaxKind) As SyntaxToken
      For Each kind In kinds
        If Current.Kind = kind Then Return MatchToken(kind)
      Next
      Return Nothing
    End Function

    Private Function TryParseExpression() As ExpressionSyntax
      If Not IsEndOfStatement() Then
        Return ParseBinaryExpression()
      End If
      Return Nothing
    End Function

#End Region

  End Class

End Namespace