Namespace Global.QB.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BlockStatement
    CallStatement
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
    LineStatement
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
     DataStatement
     DimStatement
     EraseStatement
     ReadStatement
     RedimStatement
     WhileStatement

    ' Expressions
    ArrayAccessExpression
    BinaryExpression
    AssignmentExpression
    BoundFunctionExpression
    CallExpression
    ConversionExpression
    ErrorExpression
    LiteralExpression
    VariableExpression
    UnaryExpression

  End Enum

End Namespace