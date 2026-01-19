SCREEN 0: CLS
PI = 3.14159
A = 30: B = 10: CX = 40: CY = 12
T = 0
WHILE INKEY$ = ""
  'CLS
  X = CX + A * SIN(2 * T)
  Y = CY + B * SIN(3 * T)
  'LOCATE INT(Y), INT(X)
  PRINT "*"
  T = T + 0.1
  IF T > 2 * PI THEN T = 0
  FOR I = 1 TO 100: NEXT I ' DELAY
WEND


