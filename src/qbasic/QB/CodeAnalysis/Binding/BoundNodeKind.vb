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
    ChainStatement
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
    FieldStatement
    ForStatement
    GosubStatement
    GetFileStatement
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
    LsetStatement
    RsetStatement
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
    OnGotoStatement
    OnGosubStatement
    ComStatement
    KeyEventStatement
    KeyOffStatement
    StrigStatement
    PlayEventStatement
    PenStatement
    OpenStatement
    CloseStatement
    ResetStatement
    LineInputFileStatement
    PrintFileStatement
    WriteStatement
    SeekStatement
    OptionStatement
    PrintStatement
    PutFileStatement
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
    DefTypeStatement
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
    RestoreStatement
    SleepStatement
    TimeStatement
    TimerStatement
    WhileStatement
    SelectCaseStatement
    CaseStatement
    CaseMatchStatement
    CommonStatement

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