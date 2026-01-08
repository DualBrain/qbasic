DEFINT A-Z
SCREEN 13
DIM s%(255), r%(159, 99)
FOR i = 0 TO 255: s%(i) = 32 * SIN(i * .0245436): NEXT
FOR y = 0 TO 99: d = y * 2 - 100: FOR x = 0 TO 159: r%(x, y) = SQR((x * 2 - 160) ^ 2 + d * d) AND 255: NEXT: NEXT
OUT &H3C8, 0
FOR i = 0 TO 255: OUT &H3C9, (128 + 127 * SIN(i * .021)) \ 4: OUT &H3C9, (128 + 127 * SIN(i * .033 + 2)) \ 4: OUT &H3C9, (128 + 127 * SIN(i * .017 + 4)) \ 4: NEXT
t = 0
DO
    tx = t AND 255: ty = t * 2 AND 255
    FOR y = 0 TO 99
        yy = y * 2: yt = (yy + ty) AND 255: xy = yy + tx: sy = s%(yt)
        FOR x = 0 TO 159
            xx = x * 2: xt = (xx + tx) AND 255
            LINE (xx, yy)-STEP(1, 1), (s%(xt) + sy + s%((xy + xx) AND 255) + s%((r%(x, y) + t) AND 255) + 128) AND 255, BF
        NEXT
    NEXT
    t = (t + 1) AND 255
LOOP WHILE INKEY$ = ""