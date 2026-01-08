DEFINT A-Z
SCREEN 13

OUT &H3C8, 0
FOR i = 0 TO 63: OUT &H3C9, i: OUT &H3C9, i: OUT &H3C9, i: NEXT

LINE (0, 0)-(319, 199), 0, BF
COLOR 63
LOCATE 7, 5: PRINT "WebQB.org"

DIM txt%(159, 99)
FOR y = 0 TO 99: FOR x = 0 TO 159
    txt%(x, y) = POINT(x, y)
NEXT: NEXT

r = 22
r2 = r * r

DIM pathx%(255), pathy%(255)
FOR i = 0 TO 255
    a! = i * .0245436926
    pathx%(i) = 80 + SIN(a!) * 45
    pathy%(i) = 50 + SIN(a! * 2) * 25
NEXT

LINE (0, 0)-(319, 199), 0, BF

t = 64

DO
    sx = pathx%(t)
    sy = pathy%(t)
    
    x1 = sx - r - 1: IF x1 < 0 THEN x1 = 0
    x2 = sx + r + 1: IF x2 > 159 THEN x2 = 159
    y1 = sy - r - 1: IF y1 < 0 THEN y1 = 0
    y2 = sy + r + 1: IF y2 > 99 THEN y2 = 99
    
    FOR y = y1 TO y2
        dy = y - sy
        dy2 = dy * dy
        FOR x = x1 TO x2
            dx = x - sx
            d2 = dx * dx + dy2
            IF d2 < r2 THEN
                ds! = SQR(d2)
                b = 63 - ds! * 50 / r
                IF txt%(x, y) > 0 THEN
                    c = b
                ELSE
                    c = b \ 4
                END IF
            ELSE
                c = 0
            END IF
            LINE (x * 2, y * 2)-STEP(1, 1), c, BF
        NEXT
    NEXT
    
    t = (t + 1) AND 255
LOOP WHILE INKEY$ = ""