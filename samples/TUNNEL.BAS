DEFINT A-Z
SCREEN 13
DIM an%(159, 99), de%(159, 99)
FOR y = 0 TO 99: yy! = y * 2 - 99: FOR x = 0 TO 159: xx! = x * 2 - 159
    an%(x, y) = ATN2!(yy!, xx!) * 40.74 AND 255
    d! = SQR(xx! * xx! + yy! * yy!): IF d! < 1 THEN d! = 1
    de%(x, y) = (2000 / d!) AND 255
NEXT: NEXT
OUT &H3C8, 0
FOR i = 0 TO 255
    IF (i AND 63) < 32 THEN
        r = 50 + (i AND 7): g = 50 + (i AND 7): b = 63  
    ELSE
        r = 63: g = 42 + (i AND 7): b = 56 + (i AND 7) 
    END IF
    OUT &H3C9, r: OUT &H3C9, g: OUT &H3C9, b
NEXT
t = 0
DO
    FOR y = 0 TO 99: y2 = y * 2: FOR x = 0 TO 159
        LINE (x * 2, y2)-STEP(1, 1), ((an%(x, y) + t) XOR (de%(x, y) + t * 3)) AND 255, BF
    NEXT: NEXT
    t = (t + 1) AND 255
LOOP WHILE INKEY$ = ""

FUNCTION ATN2! (y!, x!)
    IF x! = 0 AND y! = 0 THEN ATN2! = 0: EXIT FUNCTION
    ATN2! = ATN(y! / (x! + .0001)) + 3.14159 * (x! < 0) + 6.28318 * (x! >= 0 AND y! < 0)
END FUNCTION