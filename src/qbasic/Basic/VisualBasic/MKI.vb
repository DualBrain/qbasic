Namespace Global.Basic.VisualBasic

  Friend Module MkCv

    Friend Function CVI(X As String) As Short

      Dim D(16) As Short, B(7) As Short
      Dim i As Short, J As Short, k As Short
      Dim N As Short, Y As Short
      Dim TOT As Short, C As Short

      For i = 1 To 2
        'B(i) = AscW(Mid(X, 3 - i))
        B(i) = CShort(AscW(X.Substring(2 - i)))
      Next

      N = 0
      For J = 1 To 2
        C = B(J)
        For k = 1 To 8
          N += 1S
          Y = CShort(Conversion.Fix(C / (2 ^ (8 - k))))
          If Y = 1 Then
            D(N) = Y ' Binary array
            C -= CShort(2 ^ (8 - k))
          Else : D(N) = 0
          End If

        Next
      Next

      TOT = 0 ' Calculation - Power

      For i = 0 To 14 ' Calculation - Binary
        N = 14S - i
        TOT += CShort(D(i + 2) * (2 ^ N))
      Next
      If D(1) = 1 Then ' Sign of the value
        TOT *= (-1S)
      End If

      Return TOT

    End Function

    Friend Function CVS(X As String) As Single

      Dim D(24) As Short, B(3) As Short
      Dim i As Short, J As Short, k As Short
      Dim N As Short, Y As Short, C As Short
      Dim Expt As Short, PNT As Short, Pwr As Short
      Dim TOT As Single, STOT As Single

      'Expt = AscW(Right(X, 1)) ' Exponent part
      Expt = CShort(AscW(X.Substring(X.Length - 1))) ' Exponent part
      For i = 1 To 3
        'B(i) = AscW(Mid(X, 4 - i)) ' Mantissa part
        B(i) = CShort(AscW(X.Substring(3 - i))) ' Mantissa part
      Next

      N = 0
      For J = 1 To 3
        C = B(J)
        For k = 1 To 8
          N += 1S
          Y = CShort(Conversion.Fix(C / (2 ^ (8 - k))))
          If Y = 1 Then
            D(N) = Y ' Binary array for mantissa
            C -= CShort(2 ^ (8 - k))
          Else : D(N) = 0
          End If

        Next
      Next

      ' For i = 1 To 24
      ' Debug.Print D(i);
      ' Next
      ' Debug.Print


      Pwr = Expt - 129S ' Power of 2
      If Pwr = -129 Then ' Calculation - Power
        TOT = 0
      Else : TOT = CSng(2 ^ Pwr)
      End If

      PNT = Pwr + 2S ' Starting Point of Binary decimal
      STOT = 0 ' sub total Binary array
      k = 1 ' array element number

      For i = 0 To Pwr - 1S ' Calculation - Binary
        N = (Pwr - 1S) - i
        k += 1S
        If i < 23S Then
          STOT += CSng(D(i + 2) * (2 ^ N))
        End If
      Next

      If PNT < 0 Then ' set - power binary decimal
        N = PNT - 2S
      End If

      For J = k + 1S To 24 ' Calculation - Binary Decimal
        If PNT < 0 Then
          N -= 1S
        Else : N = PNT - (J + 1S)
        End If
        STOT += CSng(D(J) * (2 ^ N))
      Next

      TOT += STOT ' Full Total

      If D(1) = 1 Then ' Sign of the value
        TOT *= (-1)
      End If

      Return TOT

    End Function

    Friend Function CVD(X As String) As Double

      Dim D(56) As Short, B(7) As Short
      Dim i As Short, J As Short, k As Short
      Dim N As Short, Y As Short, C As Short
      Dim Expt As Short, PNT As Short, Pwr As Short
      Dim TOT As Double, STOT As Double

      'Expt = AscW(Right(X, 1)) ' Exponent part
      Expt = CShort(AscW(X.Substring(X.Length - 1))) ' Exponent part

      For i = 1 To 7
        'B(i) = AscW(Mid(X, 8 - i)) ' Mantissa part
        B(i) = CShort(AscW(X.Substring(7 - i)))  ' Mantissa part
      Next

      N = 0
      For J = 1 To 7
        C = B(J)
        For k = 1 To 8
          N += 1S
          Y = CShort(Conversion.Fix(C / (2 ^ (8 - k))))
          If Y = 1 Then
            D(N) = Y ' Binary array for mantissa
            C -= CShort(2 ^ (8 - k))
          Else : D(N) = 0
          End If

        Next
      Next

      ' For i = 1 To 56
      ' Debug.Print D(i);
      ' Next
      ' Debug.Print


      Pwr = Expt - 129S ' Power of 2
      If Pwr = -129 Then ' Calculation - Power
        TOT = 0
      Else : TOT = 2 ^ Pwr
      End If

      PNT = Pwr + 2S ' Starting Point of Binary decimal
      STOT = 0 ' sub total Binary array
      k = 1 ' array element number

      For i = 0 To Pwr - 1S ' Calculation - Binary
        N = (Pwr - 1S) - i
        k += 1S
        If i < 55 Then
          STOT += D(i + 2) * (2 ^ N)
        End If
      Next

      If PNT < 0 Then ' set - power binary decimal
        N = PNT - 2S
      End If

      For J = k + 1S To 56 ' Calculation - Binary Decimal
        If PNT < 0 Then
          N -= 1S
        Else : N = PNT - (J + 1S)
        End If
        STOT += D(J) * (2 ^ N)
      Next

      TOT += STOT ' Full Total

      If D(1) = 1 Then ' Sign of the value
        TOT *= (-1)
      End If

      Return TOT
    End Function

    Friend Function CVII$(X As String)
      '    Dim S, SS As Short
      '    On Error GoTo Err
      '    S = AscW(Left(X, 1)) + AscW(Right(X, 1)) * 256
      'Err:
      '    If Err.Number = 6 Then
      '      SS = 255 - AscW(Left(X, 1)) + (255 - AscW(Right(X, 1))) * 256 + 1
      '      S = (SS) * -1
      '    End If
      '    CVII$ = S

      Dim S, SS As Short
      Try
        S = CShort(AscW(X.Substring(0, 1)) + AscW(X.Substring(X.Length - 1)) * 256)
      Catch
        SS = CShort(255 - AscW(X.Substring(0, 1)) + (255 - AscW(X.Substring(X.Length - 1))) * 256 + 1)
        S = (SS) * -1S
      End Try
      CVII$ = S.ToString

    End Function

    Friend Function MKI(X As Short) As String

      Dim D(16) As Byte, V(2) As Byte
      Dim i As Short, J As Short, k As Short
      Dim N As Short, C As Short

      If X < 0 Then D(1) = 1 ' Sign of the number

      C = Math.Abs(X) ' Absolute Value
      For i = 0 To 14 ' Set Binary array
        N = 14S - i
        If Conversion.Fix(C / 2 ^ N) = 1 Then
          D(i + 2) = 1
          C -= CShort(2 ^ N)
        Else : D(i + 2) = 0
        End If
      Next

      'For i = 1 To 16
      ' Debug.Print D(i);
      'Next
      'Debug.Print

      N = 0
      For J = 1 To 2 ' Divide - 8 bit bytes
        For k = 1 To 8
          N += 1S
          V(J) = CByte(V(J) + D(N) * 2 ^ (8 - k))
        Next
      Next

      Return ChrW(V(2)) + ChrW(V(1)) ' Setup String

    End Function

    Friend Function MKS(X As Single) As String

      Dim D(24) As Byte, V(3) As Byte
      Dim MxExp As Short, AExp As Short
      Dim i As Short, J As Short, k As Short
      Dim N As Short, E As Single
      Dim Found As Boolean

      If X < 0 Then ' Sign of the number
        D(1) = 1
      Else : D(1) = 0
      End If

      E = CSng(Math.Abs(X)) ' Absolute Value
      Found = False
      For i = 0 To 256 ' Cal Maximum power
        N = 128S - i
        If E >= (2 ^ N) Then
          MxExp = N
          i = 256
          Found = True
        End If
      Next

      If E = 0 Or (Not Found) Then ' Set Max Power for String
        AExp = 0
      Else
        AExp = 129S + MxExp
        E -= CShort(2 ^ MxExp)
      End If

      For k = 1 To 23 ' Set Binary array
        N = MxExp - k
        If Conversion.Fix(E / (2 ^ N)) = 1 Then
          D(k + 1) = 1
          E -= CShort(2 ^ N)
        Else : D(k + 1) = 0
        End If
      Next

      ' For i = 1 To 24
      ' Debug.Print D(i);
      ' Next
      ' Debug.Print

      N = 0
      For J = 1 To 3 ' Divide - 8 bit bytes
        For k = 1 To 8
          N += 1S
          V(J) = CByte(V(J) + D(N) * 2 ^ (8 - k))
        Next
      Next

      ' Set-up String
      Return ChrW(V(3)) + ChrW(V(2)) + ChrW(V(1)) + ChrW(AExp)

    End Function

    Friend Function MKD(X As Double) As String

      Dim D(56) As Byte, V(7) As Byte
      Dim MxExp As Short, AExp As Short ', C As Short
      Dim i As Short, J As Short, k As Short
      Dim N As Short, E As Double
      Dim Found As Boolean
      Dim tMKD As String

      If X < 0 Then ' Sign of the number
        D(1) = 1
      Else : D(1) = 0
      End If

      E = Math.Abs(X) ' Absolute Value
      Found = False
      For i = 0 To 256 ' Cal Maximum power
        N = 128S - i
        If E >= (2 ^ N) Then
          MxExp = N
          i = 256
          Found = True
        End If
      Next

      If E = 0 Or (Not Found) Then ' Set Max Power for String
        AExp = 0
      Else
        AExp = 129S + MxExp
        E -= (2 ^ MxExp)
      End If

      For k = 1 To 55 ' Set Binary array
        N = MxExp - k
        If Conversion.Fix(E / (2 ^ N)) = 1 Then
          D(k + 1) = 1
          E -= (2 ^ N)
        Else : D(k + 1) = 0
        End If
      Next

      ' For i = 1 To 24
      ' Debug.Print D(i);
      ' Next
      ' Debug.Print

      N = 0
      For J = 1 To 7 ' Divide - 8 bit bytes
        For k = 1 To 8
          N += 1S
          V(J) = CByte(V(J) + D(N) * 2 ^ (8 - k))
        Next
      Next

      tMKD = ""
      For i = 1 To 7 ' Set-up String
        k = 8S - i
        tMKD += ChrW(V(k))
      Next
      tMKD += ChrW(AExp)

      Return tMKD
    End Function

  End Module

End Namespace