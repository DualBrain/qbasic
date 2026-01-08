DEFINT A-Z
SCREEN 13
OUT &H3C8, 0
FOR i = 0 TO 255: c = ABS(32 - (i AND 63)) * 2: OUT &H3C9, c: OUT &H3C9, c \ 2: OUT &H3C9, c \ 2: NEXT
t = 0
DO
    ox = SIN(t * .03) * 60: oy = COS(t * .02) * 40
    FOR y = 0 TO 49: yy = y * 4 - 100: FOR x = 0 TO 79: xx = x * 4 - 160
        d1 = SQR(CLNG(xx) * xx + CLNG(yy) * yy)
        d2 = SQR(CLNG(xx - ox) * (xx - ox) + CLNG(yy - oy) * (yy - oy))
        LINE (x * 4, y * 4)-STEP(3, 3), (d1 + d2) AND 255, BF
    NEXT: NEXT
    t = t + 1
LOOP WHILE INKEY$ = ""