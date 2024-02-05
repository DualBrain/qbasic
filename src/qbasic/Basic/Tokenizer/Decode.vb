Namespace Global.Basic.Tokenizer

  Public Class Decode

    Private Sub New()
    End Sub

    Public Shared Function Process(data As Byte()) As String
      Return ProcessInternal(data)
    End Function

    Private Shared Function ProcessInternal(data As Byte()) As String

      'First byte = 0FFh = non-protected program.
      'A different byte specifies a protected program and the rest of the data is pseudo-encrypted by XORing the data with successive bytes from two chunks of BASIC constants (the two chunk lengths being relatively prime to each other). I currently don't have the decoding information available so protected programs are not covered here (but it's coming).

      'Subsequent bytes

      'a.Two bytes giving the offset of the start of the next line (or the end of the BASIC program) stored in Intel binary integer format with the least-significant byte first. The actual value is not terribly important as the offsets vary depending on other data in memory and the offsets are re-calculated at loading time but the relative difference between the offset of one line and the offset of the next may provide some sanity checks on the decoding of the second of the two lines. The important value to remember is that an offset of 0000 indicates the end of the program. In an actual GW-BASIC program being interpreted, the offset is the offset of the next such pointer or the offset of the second of three zero bytes at the end of a tokenised program.
      'b.Two bytes giving the current BASIC line number in Intel integer format with the least-significant byte first.
      'c.A variable number of bytes with the tokenised BASIC program text for one line. When not following a numeric constant token, space (20h) to '~' (7Eh) represent themselves as untokenized ASCII characters and 00, 0Bh to 1Fh and 81h to 0FFh are tokens or parts of tokens as listed below.
      'd.A zero (00h) byte indicating the end of the line. Followed by the next "(a)".
      'e.A final trailing 1A (a control-Z) is tacked onto the end of a saved program. 

      Dim offset As Integer = 1

      'Dim output As String = ""
      Dim sb As New Text.StringBuilder

      'If data(0) >= &H30 AndAlso data(0) <= &H39 Then

      '  ' First character is a 0, 1, 2, 3, 4, 5, 6, 7, 8 or 9.
      '  ' Means this most likely a valid ASCII version of a line number basic file.

      '  For i = 0 To data.Length - 1
      '    sb.Append(ChrW(data(i)))
      '  Next

      If data(0) = &HFF Then

        Do

          ' Two bytes giving the offset of the start of the next line (or the end of the BASIC program) stored in Intel binary integer format with the least-significant byte first. The actual value is not terribly important as the offsets vary depending on other data in memory and the offsets are re-calculated at loading time but the relative difference between the offset of one line and the offset of the next may provide some sanity checks on the decoding of the second of the two lines. The important value to remember is that an offset of 0000 indicates the end of the program. In an actual GW-BASIC program being interpreted, the offset is the offset of the next such pointer or the offset of the second of three zero bytes at the end of a tokenised program.
          Dim value16 = BitConverter.ToInt16(data, offset)
          'sb.append("Offset: " & info)
          If value16 = 0 Then
            ' End of program...
            Exit Do
          End If
          offset += 2

          ' Two bytes giving the current BASIC line number in Intel integer format with the least-significant byte first.
          value16 = BitConverter.ToInt16(data, offset)
          sb.Append(String.Format("{0} ", value16))
          'sb.append(value16 & " ")
          offset += 2

          Dim inQuot As Boolean = False

          ' A variable number of bytes with the tokenised BASIC program text for one line. When not following a numeric constant token, space (20h) to '~' (7Eh) represent themselves as untokenized ASCII characters and 00, 0Bh to 1Fh and 81h to 0FFh are tokens or parts of tokens as listed below.
          Do

            If offset > data.Length - 1 Then
              Exit Do
            End If

            If inQuot Then
              Select Case data(offset)
                Case &H0 ' 00                 The end of the program line.
                  sb.Append(vbCrLf)
                  Exit Do
                Case &H22 ' "
                  sb.Append(ChrW(data(offset)))
                  inQuot = False
                Case Else
                  sb.Append(ChrW(data(offset)))
              End Select
            Else

              Select Case data(offset)

                Case &H3A ' 3A8FD9 - special pattern.

                  If data(offset + 1) = &H8F AndAlso
                     data(offset + 2) = &HD9 Then ' :REM'
                    sb.Append("'"c)
                    offset += 2
                  ElseIf data(offset + 1) = &H8F Then ' :REM
                    sb.Append("'"c)
                    offset += 1
                  ElseIf data(offset + 1) = &HA1 Then ' :ELSE
                    sb.Append("ELSE")
                    offset += 1
                  Else
                    sb.Append(ChrW(data(offset)))
                  End If

                Case &H22 ' Quot
                  sb.Append(ChrW(data(offset)))
                  inQuot = True

                Case &H0 ' 00                 The end of the program line.
                  sb.Append(vbCrLf)
                  'sb.appendLine()
                  Exit Do

                Case &HB
                  ' 0Bxxxx             An octal constant (defined with "&3627" or "&O3627" for example)
                  sb.Append("&O")
                  offset += 1
                  Dim value = BitConverter.ToUInt16(data, offset)
                  sb.Append(VisualBasic.Conversion.Oct(value))
                  offset += 1
                Case &HC
                  ' 0Cxxxx             A hexadecimal constant (defined with "&H2D4F" for example).
                  sb.Append("&H")
                  offset += 1
                  If data(offset + 1) > 0 Then
                    sb.Append(VisualBasic.Conversion.Hex(data(offset + 1)))
                    sb.Append(VisualBasic.Conversion.Hex(data(offset)).PadLeft(2, "0"c))
                  Else
                    sb.Append(VisualBasic.Conversion.Hex(data(offset)))
                  End If
                  offset += 1
                Case &HD
                  ' 0Dxxxx             Line pointer -- a previous line number after being used by GOTO or GOSUB so now it doesn't have to be searched for). It points to the byte just before the line to GOTO (the zero byte that ends the previous line). Only in use in a running program. Saved programs are always stored using 0Exxxx.
                  ' 0 - 65535
                  offset += 1
                  Dim value = BitConverter.ToUInt16(data, offset)
                  offset += 1
                Case &HE
                  ' 0Exxxx             A line number before being used by GOTO or GOSUB or in a saved tokenised program.
                  ' 0 - 65535
                  offset += 1
                  Dim value = BitConverter.ToUInt16(data, offset)
                  'sb.append(value)
                  sb.Append(value)
                  offset += 1
                Case &HF
                  ' 0Fxx               A one-byte integer constant, 11 to 255.
                  offset += 1
                  'sb.append(data(offset))
                  sb.Append(data(offset)) '.ToString)
                Case &H10
                  ' 10                 Never used as a token in a line. Flags constant no longer being processed. See 1E.
                  Stop
                Case &H11 To &H1B
                  ' 11 to 1B           Constants 0 to 10.
                  sb.Append(data(offset) - &H11)
                Case &H1C
                  ' 1Cxxxx             A two-byte integer constant.
                  offset += 1
                  Dim value = BitConverter.ToInt16(data, offset)
                  sb.Append(value)
                  offset += 1
                Case &H1D
                  ' 1Dxxxxxxxx         A four-byte single-precision floating-point constant.

                  offset += 1
                  Dim str As New Text.StringBuilder
                  For i = 0 To 3
                    Dim b = data(offset + i)
                    Dim c = ChrW(b)
                    str.Append(c)
                  Next
                  Dim value = VisualBasic.CVS(str.ToString)

                  sb.Append(value) '.ToString)

                  offset += 3

                Case &H1E
                  ' 1E                 This is not used as a token in a program line. It's used to flag that a numeric constant is being processed. Normally BASIC's program pointer points to the line where processing and interpreting is taking place. The item pointed to is the item currently being interpreted. A subroutine can be called anywhere in the interpreter to ask it to "fetch the current item". Elsewhere, it can be told, "I'm done with the current item so increment the program pointer and fetch the next item." This could cause a problem if the program pointer was pointing to a numeric constant because the next byte would not be a BASIC token but an arbitrary numeric value depending on the constant value. For that reason, whenever BASIC encounters a numeric constant, 0B to 0F or 1C or 1D or 1F, it processes the numeric constant, puts it in a special accumulator stack, advances the BASIC pointer past the constant, pushes that on a stack and points to the first byte of a 1E10 byte string. When 1E is returned by a "fetch the current item" call, the numeric constant type token is returned. When a "I'm done with the current item so increment the program pointer and fetch the next item." call is made, the program counter now points to 10 which prompts the interpreter to discard the constant from the accumulator stack and pop the original program pointer (now pointing past the constant) back off the stack and continue past the constant. 
                Case &H1F
                  ' 1Fxxxxxxxxxxxxxxxx An eight-byte double-precision floating-point constant.

                  offset += 1
                  Dim str As New Text.StringBuilder
                  For i = 0 To 7
                    Dim b = data(offset + i)
                    Dim c = ChrW(b)
                    str.Append(c)
                  Next
                  Dim value = VisualBasic.CVD(str.ToString)

                  sb.Append(value) '.ToString)

                  offset += 7

                  ' 1 Byte tokens...

                Case &H81 ' 81 END
                  sb.Append("END")

                Case &H82 ' 82 FOR
                  sb.Append("FOR")
                Case &H83 ' 83 NEXT
                  sb.Append("NEXT")
                Case &H84 ' 84 DATA
                  sb.Append("DATA")
                Case &H85 ' 85 INPUT
                  sb.Append("INPUT")
                Case &H86 ' 86 DIM
                  sb.Append("DIM")
                Case &H87 ' 87 READ
                  sb.Append("READ")
                Case &H88 ' 88 LET
                  sb.Append("LET")
                Case &H89 ' 89 GOTO
                  sb.Append("GOTO")
                Case &H8A ' 8A RUN
                  sb.Append("RUN")
                Case &H8B ' 8B IF
                  sb.Append("IF")
                Case &H8C ' 8C RESTORE
                  sb.Append("RESTORE")
                Case &H8D ' 8D GOSUB
                  sb.Append("GOSUB")
                Case &H8E ' 8E RETURN
                  sb.Append("RETURN")
                Case &H8F ' 8F REM
                  sb.Append("REM")
                Case &H90 ' 90 STOP
                  sb.Append("STOP")
                Case &H91 ' 91 PRINT
                  sb.Append("PRINT")
                Case &H92 ' 92 CLEAR
                  sb.Append("CLEAR")
                Case &H93 ' 93 LIST
                  sb.Append("LIST")
                Case &H94 ' 94 NEW
                  sb.Append("NEW")
                Case &H95 ' 95 ON
                  sb.Append("ON")
                Case &H96 ' 96 WAIT
                  sb.Append("WAIT")
                Case &H97 ' 97 DEF
                  sb.Append("DEF")
                Case &H98 ' 98 POKE
                  sb.Append("POKE")
                Case &H99 ' 99 CONT
                  sb.Append("CONT")
                Case &H9A ' 9A (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &H9B ' 9B (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &H9C ' 9C OUT
                  sb.Append("OUT")
                Case &H9D ' 9D LPRINT
                  sb.Append("LPRINT")
                Case &H9E ' 9E LLIST
                  sb.Append("LLIST")
                Case &H9F ' 9F (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HA0 ' A0 WIDTH
                  sb.Append("WIDTH")
                Case &HA1 ' A1 ELSE   (stored as 3AA1, ":ELSE" but the ":" is suppressed when the program is listed.)
                  sb.Append("ELSE")
                Case &HA2 ' A2 TRON
                  sb.Append("TRON")
                Case &HA3 ' A3 TROFF
                  sb.Append("TROFF")
                Case &HA4 ' A4 SWAP
                  sb.Append("SWAP")
                Case &HA5 ' A5 ERASE
                  sb.Append("ERASE")
                Case &HA6 ' A6 EDIT
                  sb.Append("EDIT")
                Case &HA7 ' A7 ERROR
                  sb.Append("ERROR")
                Case &HA8 ' A8 RESUME
                  sb.Append("RESUME")
                Case &HA9 ' A9 DELETE
                  sb.Append("DELETE")
                Case &HAA ' AA AUTO
                  sb.Append("AUTO")
                Case &HAB ' AB RENUM
                  sb.Append("RENUM")
                Case &HAC ' AC DEFSTR
                  sb.Append("DEFSTR")
                Case &HAD ' AD DEFINT
                  sb.Append("DEFSNG")
                Case &HAE ' AE DEFSNG
                  sb.Append("DEFDBL")
                Case &HAF ' AF DEFDBL
                  sb.Append("LINE")
                Case &HB0 ' B0 LINE
                  sb.Append("LINE")
                Case &HB1 ' B1 WHILE   (stored as B1E9, "WHILE+" but the "+" is suppressed when the program is listed.)
                  sb.Append("WHILE")
                  If data(offset + 1) = &HE9 Then ' +
                    offset += 1
                  End If
                Case &HB2 ' B2 WEND
                  sb.Append("WEND")
                Case &HB3 ' B3 CALL
                  sb.Append("CALL")
                Case &HB4 ' B4 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HB5 ' B5 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HB6 ' B6 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HB7 ' B7 WRITE
                  sb.Append("WRITE")
                Case &HB8 ' B8 OPTION
                  sb.Append("OPTION")
                Case &HB9 ' B9 RANDOMIZE
                  sb.Append("RANDOMIZE")
                Case &HBA ' BA OPEN
                  sb.Append("OPEN")
                Case &HBB ' BB CLOSE
                  sb.Append("CLOSE")
                Case &HBC ' BC LOAD
                  sb.Append("LOAD")
                Case &HBD ' BD MERGE
                  sb.Append("MERGE")
                Case &HBE ' BE SAVE
                  sb.Append("SAVE")
                Case &HBF ' BF COLOR
                  sb.Append("COLOR")
                Case &HC0 ' C0 CLS
                  sb.Append("CLS")
                Case &HC1 ' C1 MOTOR
                  sb.Append("MOTOR")
                Case &HC2 ' C2 BSAVE
                  sb.Append("BSAVE")
                Case &HC3 ' C3 BLOAD
                  sb.Append("BLOAD")
                Case &HC4 ' C4 SOUND
                  sb.Append("SOUND")
                Case &HC5 ' C5 BEEP
                  sb.Append("BEEP")
                Case &HC6 ' C6 PSET
                  sb.Append("PSET")
                Case &HC7 ' C7 PRESET
                  sb.Append("PRESET")
                Case &HC8 ' C8 SCREEN
                  sb.Append("SCREEN")
                Case &HC9 ' C9 KEY
                  sb.Append("KEY")
                Case &HCA ' CA LOCATE
                  sb.Append("LOCATE")
                Case &HCB ' CB (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HCC ' CC TO
                  sb.Append("TO")
                Case &HCD ' CD THEN
                  sb.Append("THEN")
                Case &HCE ' CE TAB(   (note that the following "(" is part of the keyword with no intervening space. That's why "TAB   (..." will not work.)
                  sb.Append("TAB(")
                Case &HCF ' CF STEP
                  sb.Append("STEP")
                Case &HD0 ' D0 USR
                  sb.Append("USR")
                Case &HD1 ' D1 FN
                  sb.Append("FN")
                Case &HD2 ' D2 SPC(   (note that the following "(" is part of the keyword with no intervening space. That's why "SPC   (..." will not work.)
                  sb.Append("SPC(")
                Case &HD3 ' D3 NOT
                  sb.Append("NOT")
                Case &HD4 ' D4 ERL
                  sb.Append("ERL")
                Case &HD5 ' D5 ERR
                  sb.Append("ERR")
                Case &HD6 ' D6 STRING$
                  sb.Append("STRING$")
                Case &HD7 ' D7 USING
                  sb.Append("USING")
                Case &HD8 ' D8 INSTR
                  sb.Append("INSTR")
                Case &HD9 ' D9 '   (stored as 3A8FD9, ":REM'" but the ":REM" is suppressed when the program is listed.)
                  sb.Append("'"c)
                Case &HDA ' DA VARPTR
                  sb.Append("VARPTR")
                Case &HDB ' DB CSRLIN
                  sb.Append("CSRLIN")
                Case &HDC ' DC POINT
                  sb.Append("POINT")
                Case &HDD ' DD OFF
                  sb.Append("OFF")
                Case &HDE ' DE INKEY$
                  sb.Append("INKEY$")
                Case &HDF ' DF (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE0 ' E0 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE1 ' E1 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE2 ' E2 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE3 ' E3 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE4 ' E4 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE5 ' E5 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HE6 ' E6 >
                  sb.Append(">"c)
                Case &HE7 ' E7 =
                  sb.Append("="c)
                Case &HE8 ' E8 <
                  sb.Append("<"c)
                Case &HE9 ' E9 +
                  sb.Append("+"c)
                Case &HEA ' EA -
                  sb.Append("-"c)
                Case &HEB ' EB *
                  sb.Append("*"c)
                Case &HEC ' EC /
                  sb.Append("/"c)
                Case &HED ' ED ^
                  sb.Append("^"c)
                Case &HEE ' EE AND
                  sb.Append("AND")
                Case &HEF ' EF OR
                  sb.Append("OR")
                Case &HF0 ' F0 XOR
                  sb.Append("XOR")
                Case &HF1 ' F1 EQV
                  sb.Append("EQV")
                Case &HF2 ' F2 IMP
                  sb.Append("IMP")
                Case &HF3 ' F3 MOD
                  sb.Append("MOD")
                Case &HF4 ' F4 \
                  sb.Append("\"c)
                Case &HF5 ' F5 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HF6 ' F6 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HF7 ' F7 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HF8 ' F8 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HF9 ' F9 (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HFA ' FA (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HFB ' FB (Undefined)
                  sb.Append("(Undefined)")
                  Stop
                Case &HFC ' FC (Undefined)
                  sb.Append("(Undefined)")
                  Stop

                  ' Double byte tokens...

                Case &HFD

                  offset += 1

                  Select Case data(offset)
                    Case &H81 ' FD81 CVI
                      sb.Append("CVI")
                    Case &H82 ' FD82 CVS
                      sb.Append("CVS")
                    Case &H83 ' FD83 CVD
                      sb.Append("CVD")
                    Case &H84 ' FD84 MKI$
                      sb.Append("MKI$")
                    Case &H85 ' FD85 MKS$
                      sb.Append("MKS$")
                    Case &H86 ' FD86 MKD$
                      sb.Append("MKD$")
                    Case &H8B ' FD8B EXTERR
                      sb.Append("EXTERR")
                    Case Else
                      sb.Append("(Undefined)")
                      Stop
                  End Select

                Case &HFE
                  offset += 1

                  Select Case data(offset)
                    Case &H81 ' FE81 FILES
                      sb.Append("FILES")
                    Case &H82 ' FE82 FIELD
                      sb.Append("FIELD")
                    Case &H83 ' FE83 SYSTEM
                      sb.Append("SYSTEM")
                    Case &H84 ' FE84 NAME
                      sb.Append("NAME")
                    Case &H85 ' FE85 LSET
                      sb.Append("LSET")
                    Case &H86 ' FE86 RSET
                      sb.Append("RSET")
                    Case &H87 ' FE87 KILL
                      sb.Append("KILL")
                    Case &H88 ' FE88 PUT
                      sb.Append("PUT")
                    Case &H89 ' FE89 GET
                      sb.Append("GET")
                    Case &H8A ' FE8A RESET
                      sb.Append("RESET")
                    Case &H8B ' FE8B COMMON
                      sb.Append("COMMON")
                    Case &H8C ' FE8C CHAIN
                      sb.Append("CHAIN")
                    Case &H8D ' FE8D DATE$
                      sb.Append("DATE$")
                    Case &H8E ' FE8E TIME$
                      sb.Append("TIME$")
                    Case &H8F ' FE8F PAINT
                      sb.Append("PAINT")
                    Case &H90 ' FE90 COM
                      sb.Append("COM")
                    Case &H91 ' FE91 CIRCLE
                      sb.Append("CIRCLE")
                    Case &H92 ' FE92 DRAW
                      sb.Append("DRAW")
                    Case &H93 ' FE93 PLAY
                      sb.Append("PLAY")
                    Case &H94 ' FE94 TIMER
                      sb.Append("TIMER")
                    Case &H95 ' FE95 ERDEV
                      sb.Append("ERDEV")
                    Case &H96 ' FE96 IOCTL
                      sb.Append("IOCTL")
                    Case &H97 ' FE97 CHDIR
                      sb.Append("CHDIR")
                    Case &H98 ' FE98 MKDIR
                      sb.Append("MKDIR")
                    Case &H99 ' FE99 RMDIR
                      sb.Append("RMDIR")
                    Case &H9A ' FE9A SHELL
                      sb.Append("SHELL")
                    Case &H9B ' FE9B ENVIRON
                      sb.Append("ENVIRON")
                    Case &H9C ' FE9C VIEW
                      sb.Append("VIEW")
                    Case &H9D ' FE9D WINDOW
                      sb.Append("WINDOW")
                    Case &H9E ' FE9E PMAP
                      sb.Append("PMAP")
                    Case &H9F ' FE9F PALETTE
                      sb.Append("PALETTE")
                    Case &HA0 ' FEA0 LCOPY
                      sb.Append("LCOPY")
                    Case &HA1 ' FEA1 CALLS
                      sb.Append("CALLS")
                    Case &HA4 ' FEA4 NOISE   (PCjr only) or DEBUG   (Sperry PC only)
                      sb.Append("NOISE")
                    Case &HA5 ' FEA5 PCOPY   (PCjr or EGA system only)
                      sb.Append("PCOPY")
                    Case &HA6 ' FEA6 TERM   (PCjr only)
                      sb.Append("TERM")
                    Case &HA7 ' FEA7 LOCK
                      sb.Append("LOCK")
                    Case &HA8 ' FEA8 UNLOCK
                      sb.Append("UNLOCK")
                    Case Else
                      sb.Append("(Undefined)")
                      Stop
                  End Select

                Case &HFF
                  offset += 1

                  Select Case data(offset)
                    Case &H81 ' FF81 LEFT$
                      sb.Append("LEFT$")
                    Case &H82 ' FF82 RIGHT$
                      sb.Append("RIGHT$")
                    Case &H83 ' FF83 MID$
                      sb.Append("MID$")
                    Case &H84 ' FF84 SGN
                      sb.Append("SGN")
                    Case &H85 ' FF85 INT
                      sb.Append("INT")
                    Case &H86 ' FF86 ABS
                      sb.Append("ABS")
                    Case &H87 ' FF87 SQR
                      sb.Append("SQR")
                    Case &H88 ' FF88 RND
                      sb.Append("RND")
                    Case &H89 ' FF89 SIN
                      sb.Append("SIN")
                    Case &H8A ' FF8A LOG
                      sb.Append("LOG")
                    Case &H8B ' FF8B EXP
                      sb.Append("EXP")
                    Case &H8C ' FF8C COS
                      sb.Append("COS")
                    Case &H8D ' FF8D TAN
                      sb.Append("TAN")
                    Case &H8E ' FF8E ATN
                      sb.Append("ATN")
                    Case &H8F ' FF8F FRE
                      sb.Append("FRE")
                    Case &H90 ' FF90 INP
                      sb.Append("INP")
                    Case &H91 ' FF91 POS
                      sb.Append("POS")
                    Case &H92 ' FF92 LEN
                      sb.Append("LEN")
                    Case &H93 ' FF93 STR$
                      sb.Append("STR$")
                    Case &H94 ' FF94 VAL
                      sb.Append("VAL")
                    Case &H95 ' FF95 ASC
                      sb.Append("ASC")
                    Case &H96 ' FF96 CHR$
                      sb.Append("CHR$")
                    Case &H97 ' FF97 PEEK
                      sb.Append("PEEK")
                    Case &H98 ' FF98 SPACE$
                      sb.Append("SPACE$")
                    Case &H99 ' FF99 OCT$
                      sb.Append("OCT$")
                    Case &H9A ' FF9A HEX$
                      sb.Append("HEX$")
                    Case &H9B ' FF9B LPOS
                      sb.Append("LPOS")
                    Case &H9C ' FF9C CINT
                      sb.Append("CINT")
                    Case &H9D ' FF9D CSNG
                      sb.Append("CSNG")
                    Case &H93 ' FF9E CDBL
                      sb.Append("CDBL")
                    Case &H9F ' FF9F FIX
                      sb.Append("FIX")
                    Case &HA0 ' FFA0 PEN
                      sb.Append("PEN")
                    Case &HA1 ' FFA1 STICK
                      sb.Append("STICK")
                    Case &HA2 ' FFA2 STRIG
                      sb.Append("STRIG")
                    Case &HA3 ' FFA3 EOF
                      sb.Append("EOF")
                    Case &HA4 ' FFA4 LOC
                      sb.Append("LOC")
                    Case &HA5 ' FFA5 LOF
                      sb.Append("LOF")
                    Case Else
                      sb.Append("(Undefined)")
                      Stop
                  End Select

                Case Else

                  sb.Append(ChrW(data(offset)))

              End Select

            End If

            offset += 1

            If offset > data.Length - 1 Then
              Exit Do
            End If

          Loop

          ' A zero (00h) byte indicating the end of the line. Followed by the next "(a)".
          offset += 1

          ' A final trailing 1A (a control-Z) is tacked onto the end of a saved program. 

          'If offset < data.Length Then
          '  If data(offset) = &H1A Then
          '    offset += 1
          '  End If
          'End If

          If offset + 2 > data.Length Then
            sb.Append("Missing CTRL+Z.")
            Exit Do
          End If

        Loop

        ' A final trailing 1A (a control-Z) is tacked onto the end of a saved program. 

      Else

        '' Either not tokenized or possibly protected?
        'sb.Append("Invalid.")

        ' Just try to load as a text document.

        Dim start = 0

        If data.Length >= 3 Then

          'UTF-8
          '0xEF,0xBB,0xBF

          If data(0) = &HEF AndAlso
             data(1) = &HBB AndAlso
             data(2) = &HBF Then
            ' UTF8
            start = 3
          End If

        End If

        For i = start To data.Length - 1
          sb.Append(ChrW(data(i)))
        Next

      End If

      Return sb.ToString

    End Function

  End Class

End Namespace