DEFINT A-Z
SCREEN 13
OUT &H3C8, 0
FOR i = 0 TO 255
    c = i \ 4: IF c > 63 THEN c = 63
    OUT &H3C9, c: OUT &H3C9, c \ 2: OUT &H3C9, c * 2 \ 3
NEXT
t! = 0
DO
    x1 = 80 + SIN(t!) * 50: y1 = 50 + COS(t! * 1.3) * 30
    x2 = 80 + SIN(t! * .7 + 2) * 50: y2 = 50 + COS(t! * 1.1) * 30
    x3 = 80 + SIN(t! * 1.2 + 4) * 50: y3 = 50 + COS(t! * .8 + 1) * 30
    FOR y = 0 TO 99: FOR x = 0 TO 159
        d1 = 800 / (1 + (x - x1) ^ 2 + (y - y1) ^ 2)
        d2 = 800 / (1 + (x - x2) ^ 2 + (y - y2) ^ 2)
        d3 = 800 / (1 + (x - x3) ^ 2 + (y - y3) ^ 2)
        c = (d1 + d2 + d3) * 8: IF c > 255 THEN c = 255
        LINE (x * 2, y * 2)-STEP(1, 1), c, BF
    NEXT: NEXT
    t! = t! + .08
LOOP WHILE INKEY$ = ""