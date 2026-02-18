Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundMemberAccessExpression
    Inherits BoundExpression

    Private ReadOnly m_expression As BoundExpression
    Private ReadOnly m_memberName As String
    Private ReadOnly m_memberType As Symbols.TypeSymbol
    Private ReadOnly m_fixedLength As Integer

    Public Sub New(expression As BoundExpression, memberName As String, memberType As Symbols.TypeSymbol, syntax As Syntax.MemberAccessExpressionSyntax, Optional fixedLength As Integer = 0)
      MyBase.New(syntax)
      m_expression = expression
      m_memberName = memberName
      m_memberType = memberType
      m_fixedLength = fixedLength
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.MemberAccessExpression

    Public ReadOnly Property Expression As BoundExpression
      Get
        Return m_expression
      End Get
    End Property

    Public ReadOnly Property MemberName As String
      Get
        Return m_memberName
      End Get
    End Property

    Public Overrides ReadOnly Property Type As Symbols.TypeSymbol
      Get
        Return m_memberType
      End Get
    End Property

    Public ReadOnly Property FixedLength As Integer
      Get
        Return m_fixedLength
      End Get
    End Property

  End Class

End Namespace