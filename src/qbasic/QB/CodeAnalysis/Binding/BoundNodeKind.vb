Namespace Global.QB.CodeAnalysis.Binding

  Friend Enum BoundNodeKind

    Symbol

    ' Functions
    SpcFunction
    TabFunction

    ' Statements
    BeepStatement
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
    OnErrorGotoStatement
    OnErrorGotoZeroStatement
    OnTimerGosubStatement
    OnComGosubStatement
    OnKeyGosubStatement
    OnStrigGosubStatement
    OnPlayGosubStatement
     OnPenGosubStatement
     ComStatement
     KeyEventStatement
     StrigStatement
     PlayEventStatement
     PenStatement
     OpenStatement
     CloseStatement
     LineInputFileStatement
     PrintFileStatement
    OptionStatement
    PrintStatement
    PokeStatement
    OutStatement
    PsetStatement
    PresetStatement
    RemStatement
    ResumeStatement
    ResumeNextStatement
    ReturnGosubStatement
    ReturnStatement
    RmDirStatement
    ScreenStatement
    StopStatement
    SubStatement
    SystemStatement
    SwapStatement
    VariableDeclaration
    DataStatement
    DateStatement
    DimStatement
    EnvironStatement
    EraseStatement
    ErrorStatement
    ReadStatement
    RedimStatement
    SleepStatement
    TimeStatement
    TimerStatement
    WhileStatement
    SelectCaseStatement
    CaseStatement
    CaseMatchStatement

    ' Expressions
    ArrayAccessExpression
    BinaryExpression
    AssignmentExpression
    BoundFunctionExpression
    CallExpression
    ConversionExpression
    ErrorExpression
    LiteralExpression
    ParenExpression
    VariableExpression
    UnaryExpression

  End Enum

End Namespace