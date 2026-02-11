Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend NotInheritable Class BoundConversionExpression
    Inherits BoundExpression

    Private ReadOnly m_constantValue As BoundConstant

    Sub New([type] As TypeSymbol, expression As BoundExpression)
      Me.Type = [type]
      Me.Expression = expression
      ' Propagate ConstantValue if the inner expression has one
      If expression.ConstantValue IsNot Nothing Then
        Dim constVal = expression.ConstantValue.Value
        Dim convertedValue As Object = Nothing
        Select Case [type].Name
          Case "Single"
            If TypeOf constVal Is Integer Then
              convertedValue = CSng(CInt(constVal))
            ElseIf TypeOf constVal Is Single Then
              convertedValue = constVal
            End If
          Case "Integer"
            If TypeOf constVal Is Integer Then
              convertedValue = constVal
            ElseIf TypeOf constVal Is Single Then
              convertedValue = CInt(CSng(constVal))
            End If
          Case "Double"
            If TypeOf constVal Is Double Then
              convertedValue = constVal
            ElseIf TypeOf constVal Is Single Then
              convertedValue = CDbl(CSng(constVal))
            End If
          Case "Long"
            If TypeOf constVal Is Long Then
              convertedValue = constVal
            End If
        End Select
        If convertedValue IsNot Nothing Then
          m_constantValue = New BoundConstant(convertedValue)
        Else
          m_constantValue = expression.ConstantValue
        End If
      Else
        m_constantValue = Nothing
      End If
    End Sub

    Public Overrides ReadOnly Property Kind As BoundNodeKind = BoundNodeKind.ConversionExpression
    Public Overrides ReadOnly Property Type As TypeSymbol
    Public ReadOnly Property Expression As BoundExpression
    Public Overrides ReadOnly Property ConstantValue As BoundConstant
      Get
        Return m_constantValue
      End Get
    End Property

  End Class

End Namespace