Imports QB.CodeAnalysis.Symbols

Namespace Global.QB.CodeAnalysis.Binding

  Friend Module ConstantFolding

    Public Function ComputeConstant(op As BoundUnaryOperator, operand As BoundExpression) As BoundConstant
      If operand.ConstantValue IsNot Nothing Then
        Dim constVal = operand.ConstantValue.Value
        Select Case op.Kind
          Case BoundUnaryOperatorKind.Identity
            If TypeOf constVal Is Integer Then
              Return New BoundConstant(CInt(constVal))
            ElseIf TypeOf constVal Is Single Then
              Return New BoundConstant(CSng(constVal))
            End If
          Case BoundUnaryOperatorKind.Negation
            If TypeOf constVal Is Integer Then
              Return New BoundConstant(-CInt(constVal))
            ElseIf TypeOf constVal Is Single Then
              Return New BoundConstant(-CSng(constVal))
            End If
          Case BoundUnaryOperatorKind.LogicalNegation
            If TypeOf constVal Is Boolean Then
              Return New BoundConstant(Not CBool(constVal))
            ElseIf TypeOf constVal Is Integer Then
              Return New BoundConstant(Not CInt(constVal))
            End If
          Case BoundUnaryOperatorKind.BitwiseComplement
            If TypeOf constVal Is Integer Then
              Return New BoundConstant(Not CInt(constVal))
            ElseIf TypeOf constVal Is Single Then
              Return New BoundConstant(Not CInt(CSng(constVal)))
            ElseIf TypeOf constVal Is Double Then
              Return New BoundConstant(Not CInt(CDbl(constVal)))
            End If
          Case Else
            Throw New Exception($"Unexpected unary operator {op.Kind}")
        End Select
      End If
      Return Nothing
    End Function

    Public Function Fold(left As BoundExpression, op As BoundBinaryOperator, right As BoundExpression) As BoundConstant

      Dim leftConstant = left.ConstantValue
      Dim rightConstant = right.ConstantValue

      ' Special case `And` and `Or` because there are cases where only need one side needs to be known.

      If op.Kind = BoundBinaryOperatorKind.LogicalAnd Then
        If (leftConstant IsNot Nothing AndAlso Not CBool(leftConstant.Value)) OrElse
           (rightConstant IsNot Nothing AndAlso Not CBool(rightConstant.Value)) Then
          Return New BoundConstant(0) 'False)
        End If
      End If

      If op.Kind = BoundBinaryOperatorKind.LogicalOr Then
        If (leftConstant IsNot Nothing AndAlso CBool(leftConstant.Value)) OrElse
           (rightConstant IsNot Nothing AndAlso CBool(rightConstant.Value)) Then
          Return New BoundConstant(-1) 'True)
        End If
      End If

      If leftConstant Is Nothing OrElse rightConstant Is Nothing Then Return Nothing

      ' compute

      Dim l = leftConstant.Value
      Dim r = rightConstant.Value

      Select Case op.Kind
        Case BoundBinaryOperatorKind.Raise
          ' Check for invalid exponentiation operations that should not be folded
          ' 0 raised to negative power = division by zero (1/0)
          Dim lDouble = CDbl(l)
          Dim rDouble = CDbl(r)

          If lDouble = 0 AndAlso rDouble < 0 Then
            ' Don't fold - should be evaluated at runtime to trigger error
            Return Nothing
          End If

          ' Compute the result
          Dim result = lDouble ^ rDouble

          ' Don't fold if result is infinity or NaN
          If Double.IsInfinity(result) OrElse Double.IsNaN(result) Then
            Return Nothing
          End If

          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(result))
            Case TypeSymbol.Type.Double : Return New BoundConstant(result)
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(result))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(result))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(result))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(result))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(result))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(result))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(result))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(result))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(result))
          End Select
        Case BoundBinaryOperatorKind.Addition
          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) + CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) + CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) + CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) + CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) + CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) + CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) + CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) + CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) + CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) + CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) + CByte(r))
            Case TypeSymbol.Type.String : Return New BoundConstant(CStr(l) & CStr(r))
          End Select
        Case BoundBinaryOperatorKind.Subtraction
          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) - CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) - CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) - CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) - CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) - CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) - CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) - CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) - CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) - CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) - CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) - CByte(r))
          End Select
        Case BoundBinaryOperatorKind.Multiplication
          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) * CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) * CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) * CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) * CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) * CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) * CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) * CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) * CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) * CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) * CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) * CByte(r))
          End Select
        Case BoundBinaryOperatorKind.Division
          ' Don't fold division by zero - it should be evaluated at runtime to trigger error handling
          If CDbl(r) = 0 Then Return Nothing
          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) / CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) / CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CSng(l) / CSng(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) / CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) / CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) / CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) / CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) / CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) / CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) / CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) / CByte(r))
          End Select
        Case BoundBinaryOperatorKind.IntegerDivision
          ' Don't fold integer division by zero
          If CDbl(r) = 0 Then Return Nothing
          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CLng(l) \ CLng(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CLng(l) \ CLng(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CInt(l) \ CInt(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) \ CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) \ CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) \ CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) \ CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) \ CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) \ CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) \ CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) \ CByte(r))
          End Select
        Case BoundBinaryOperatorKind.BitwiseAnd
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) And CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) And CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) And CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) And CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) And CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) And CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) And CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) And CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) And CBool(r))
          End Select
        Case BoundBinaryOperatorKind.BitwiseOr
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) Or CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) Or CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) Or CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) Or CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) Or CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) Or CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) Or CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) Or CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) Or CBool(r))
          End Select
        Case BoundBinaryOperatorKind.BitwiseXor
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) Xor CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) Xor CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) Xor CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) Xor CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) Xor CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) Xor CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) Xor CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) Xor CByte(r))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(CBool(l) Xor CBool(r))
          End Select
        Case BoundBinaryOperatorKind.LogicalAnd
          Return New BoundConstant(CBool(l) And CBool(r))
        Case BoundBinaryOperatorKind.LogicalOr
          Return New BoundConstant(CBool(l) Or CBool(r))
        Case BoundBinaryOperatorKind.LogicalXor
          Return New BoundConstant(CBool(l) Xor CBool(r))
        Case BoundBinaryOperatorKind.LogicalOrElse
          Return New BoundConstant(CBool(l) OrElse CBool(r))
        Case BoundBinaryOperatorKind.LogicalAndAlso
          Return New BoundConstant(CBool(l) AndAlso CBool(r))
        Case BoundBinaryOperatorKind.Equal
          Return New BoundConstant(If(Equals(l, r), -1, 0))
        Case BoundBinaryOperatorKind.NotEqual
          Return New BoundConstant(If(Not Equals(l, r), -1, 0))
        Case BoundBinaryOperatorKind.LessThan
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(If(CDec(l) < CDec(r), -1, 0))
            Case TypeSymbol.Type.Double : Return New BoundConstant(If(CDbl(l) < CDbl(r), -1, 0))
            Case TypeSymbol.Type.Single : Return New BoundConstant(If(CSng(l) < CSng(r), -1, 0))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(If(CULng(l) < CULng(r), -1, 0))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(If(CLng(l) < CLng(r), -1, 0))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(If(CUInt(l) < CUInt(r), -1, 0))
            Case TypeSymbol.Type.Long : Return New BoundConstant(If(CInt(l) < CInt(r), -1, 0))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(If(CUShort(l) < CUShort(r), -1, 0))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(If(CShort(l) < CShort(r), -1, 0))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(If(CSByte(l) < CSByte(r), -1, 0))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(If(CByte(l) < CByte(r), -1, 0))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(If(CBool(l) < CBool(r), -1, 0))
            Case TypeSymbol.Type.String : Return New BoundConstant(If(CStr(l) < CStr(r), -1, 0))
          End Select
        Case BoundBinaryOperatorKind.GreaterThan
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(If(CDec(l) > CDec(r), -1, 0))
            Case TypeSymbol.Type.Double : Return New BoundConstant(If(CDbl(l) > CDbl(r), -1, 0))
            Case TypeSymbol.Type.Single : Return New BoundConstant(If(CSng(l) > CSng(r), -1, 0))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(If(CULng(l) > CULng(r), -1, 0))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(If(CLng(l) > CLng(r), -1, 0))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(If(CUInt(l) > CUInt(r), -1, 0))
            Case TypeSymbol.Type.Long : Return New BoundConstant(If(CInt(l) > CInt(r), -1, 0))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(If(CUShort(l) > CUShort(r), -1, 0))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(If(CShort(l) > CShort(r), -1, 0))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(If(CSByte(l) > CSByte(r), -1, 0))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(If(CByte(l) > CByte(r), -1, 0))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(If(CBool(l) > CBool(r), -1, 0))
            Case TypeSymbol.Type.String : Return New BoundConstant(If(CStr(l) > CStr(r), -1, 0))
          End Select
        Case BoundBinaryOperatorKind.LessThanEqual
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(If(CDec(l) <= CDec(r), -1, 0))
            Case TypeSymbol.Type.Double : Return New BoundConstant(If(CDbl(l) <= CDbl(r), -1, 0))
            Case TypeSymbol.Type.Single : Return New BoundConstant(If(CSng(l) <= CSng(r), -1, 0))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(If(CULng(l) <= CULng(r), -1, 0))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(If(CLng(l) <= CLng(r), -1, 0))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(If(CUInt(l) <= CUInt(r), -1, 0))
            Case TypeSymbol.Type.Long : Return New BoundConstant(If(CInt(l) <= CInt(r), -1, 0))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(If(CUShort(l) <= CUShort(r), -1, 0))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(If(CShort(l) <= CShort(r), -1, 0))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(If(CSByte(l) <= CSByte(r), -1, 0))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(If(CByte(l) <= CByte(r), -1, 0))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(If(CBool(l) <= CBool(r), -1, 0))
            Case TypeSymbol.Type.String : Return New BoundConstant(If(CStr(l) <= CStr(r), -1, 0))
          End Select
        Case BoundBinaryOperatorKind.GreaterThanEqual
          Select Case TypeSymbol.TypeSymbolToType(left.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(If(CDec(l) >= CDec(r), -1, 0))
            Case TypeSymbol.Type.Double : Return New BoundConstant(If(CDbl(l) >= CDbl(r), -1, 0))
            Case TypeSymbol.Type.Single : Return New BoundConstant(If(CSng(l) >= CSng(r), -1, 0))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(If(CULng(l) >= CULng(r), -1, 0))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(If(CLng(l) >= CLng(r), -1, 0))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(If(CUInt(l) >= CUInt(r), -1, 0))
            Case TypeSymbol.Type.Long : Return New BoundConstant(If(CInt(l) >= CInt(r), -1, 0))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(If(CUShort(l) >= CUShort(r), -1, 0))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(If(CShort(l) >= CShort(r), -1, 0))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(If(CSByte(l) >= CSByte(r), -1, 0))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(If(CByte(l) >= CByte(r), -1, 0))
            Case TypeSymbol.Type.Boolean : Return New BoundConstant(If(CBool(l) >= CBool(r), -1, 0))
            Case TypeSymbol.Type.String : Return New BoundConstant(If(CStr(l) >= CStr(r), -1, 0))
            Case Else
          End Select
        Case BoundBinaryOperatorKind.ModOperation
          Select Case TypeSymbol.TypeSymbolToType(op.Type)
            Case TypeSymbol.Type.Decimal : Return New BoundConstant(CDec(l) Mod CDec(r))
            Case TypeSymbol.Type.Double : Return New BoundConstant(CDbl(l) Mod CDbl(r))
            Case TypeSymbol.Type.Single : Return New BoundConstant(CInt(l) Mod CInt(r))
            Case TypeSymbol.Type.ULong64 : Return New BoundConstant(CULng(l) Mod CULng(r))
            Case TypeSymbol.Type.Long64 : Return New BoundConstant(CLng(l) Mod CLng(r))
            Case TypeSymbol.Type.ULong : Return New BoundConstant(CUInt(l) Mod CUInt(r))
            Case TypeSymbol.Type.Long : Return New BoundConstant(CInt(l) Mod CInt(r))
            Case TypeSymbol.Type.UInteger : Return New BoundConstant(CUShort(l) Mod CUShort(r))
            Case TypeSymbol.Type.Integer : Return New BoundConstant(CShort(l) Mod CShort(r))
            Case TypeSymbol.Type.SByte : Return New BoundConstant(CSByte(l) Mod CSByte(r))
            Case TypeSymbol.Type.Byte : Return New BoundConstant(CByte(l) Mod CByte(r))
            Case Else
          End Select
        Case BoundBinaryOperatorKind.LogicalImp
          Return New BoundConstant(CInt(Not CBool(l) Or CBool(r)))
        Case BoundBinaryOperatorKind.BitwiseEqv
          Return New BoundConstant(CInt(CBool(l) = CBool(r)))
        Case BoundBinaryOperatorKind.BitwiseImp
          Return New BoundConstant(CInt(CBool(l) AndAlso Not CBool(r)))
        Case Else
          Throw New Exception($"Unexpected binary operator {op.Kind}")
      End Select

      Throw New Exception($"Invalid binary operator for {TypeSymbol.TypeSymbolToType(left.Type)} {op.Kind} {TypeSymbol.TypeSymbolToType(right.Type)}")

    End Function

  End Module

End Namespace