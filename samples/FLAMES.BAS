DEFINT A-Z
SCREEN 13
DIM f%(81, 51)
OUT &H3C8, 0
FOR i = 0 TO 255
    r = i \ 3: IF r > 63 THEN r = 63
    g = (i - 80) \ 2: IF g < 0 THEN g = 0
    IF g > 63 THEN g = 63
    b = i - 200: IF b < 0 THEN b = 0
    OUT &H3C9, r: OUT &H3C9, g: OUT &H3C9, b
NEXT
DO
    FOR x = 1 TO 79: f%(x, 51) = RND * 255: NEXT
    FOR y = 1 TO 50
        FOR x = 1 TO 79
            f%(x, y) = (f%(x - 1, y + 1) + f%(x, y + 1) + f%(x + 1, y + 1) + f%(x, y)) \ 4 - 2
            IF f%(x, y) < 0 THEN f%(x, y) = 0
            LINE (x * 4, y * 4)-STEP(3, 3), f%(x, y), BF
        NEXT
    NEXT
LOOP WHILE INKEY$ = ""