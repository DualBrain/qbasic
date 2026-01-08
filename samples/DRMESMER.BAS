DEFINT A-Z
SCREEN 7, , 0, 0
pg = 0
t! = 0
DO
    SCREEN 7, , pg, 1 - pg
    LINE (0, 0)-(319, 199), 0, BF
    
    r1! = 3 + SIN(t! * .1) * 2
    r2! = 2 + COS(t! * .07) * 1.5
    r3! = 5 + SIN(t! * .13) * 2
    r4! = 4 + COS(t! * .09) * 1.5
    r5! = 2.5 + SIN(t! * .11) * 1.5
    
    cx = 160: cy = 100
    
    'outer ring - bright cyan
    FOR j = 0 TO 5
        phase! = j * 1.047
        FOR i = 0 TO 300
            a! = t! + i * .01 + phase!
            x = cx + SIN(a! * r1!) * 155
            y = cy + SIN(a! * r2!) * 95
            PSET (x, y), 11
        NEXT
    NEXT
    
    'second ring - dark cyan
    FOR j = 0 TO 3
        phase! = j * 1.57
        FOR i = 0 TO 250
            a! = -t! * 1.5 + i * .012 + phase!
            x = cx + SIN(a! * r3!) * 90
            y = cy + SIN(a! * 2) * 55
            PSET (x, y), 3
        NEXT
    NEXT
    
    'middle ring - bright white
    FOR j = 0 TO 3
        phase! = j * 1.57
        FOR i = 0 TO 220
            a! = t! * 1.2 + i * .013 + phase!
            x = cx + SIN(a! * r5!) * 70
            y = cy + SIN(a! * 3.5) * 42
            PSET (x, y), 15
        NEXT
    NEXT
    
    'inner ring - bright red
    FOR j = 0 TO 2
        phase! = j * 2.09
        FOR i = 0 TO 200
            a! = t! * 2 + i * .015 + phase!
            x = cx + SIN(a! * r4!) * 50
            y = cy + SIN(a! * 3) * 30
            PSET (x, y), 12
        NEXT
    NEXT
    
    'orbiting dots - dark red
    FOR j = 0 TO 7
        da! = t! * 2 + j * .785
        dx = cx + COS(da!) * (110 + SIN(t! + j) * 30)
        dy = cy + SIN(da!) * (70 + COS(t! + j) * 20)
        CIRCLE (dx, dy), 2, 4
    NEXT
    
    pg = 1 - pg
    t! = t! + .02
LOOP WHILE INKEY$ = ""