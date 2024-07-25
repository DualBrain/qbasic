Namespace Global.QB.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
    ChDirStatement
    CircleStatement
    ClearStatement
    ClsStatement
    ColorStatement
    ConditionalGotoStatement
    DoUntilStatement
    DoWhileStatement
    EndStatement
    ElseIfStatement
    ExpressionStatement
    ForStatement
    GosubStatement
    GotoStatement
    HandleCommaStatement
    HandlePrintLineStatement
    HandlePrintStatement
    HandleTabStatement
    HandleSpcStatement
    IfStatement
    InputStatement
    KillStatement
    LabelStatement
    LetStatement
    LocateStatement
    MidStatement
    MkDirStatement
    NameStatement
    NopStatement
    OptionStatement
    PrintStatement
    PsetStatement
    PresetStatement
    RemStatement
    ReturnGosubStatement
    ReturnStatement
    RmDirStatement
    ScreenStatement
    StopStatement
    SystemStatement
    SwapStatement
    VariableDeclaration
    WhileStatement

    ' Expressions
    BinaryExpression
    AssignmentExpression
    CallExpression
    ConversionExpression
    ErrorExpression
    LiteralExpression
    VariableExpression
    UnaryExpression

  End Enum

End Namespace