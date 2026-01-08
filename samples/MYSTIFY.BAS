DEFINT A-Z
SCREEN 12

DIM x!(2, 3), y!(2, 3), dx!(2, 3), dy!(2, 3), ph!(2, 3)
FOR p = 0 TO 2
    FOR i = 0 TO 3
        x!(p, i) = RND * 560 + 40: y!(p, i) = RND * 400 + 40
        dx!(p, i) = (RND * 2 + .5) * (1 - 2 * (RND > .5))
        dy!(p, i) = (RND * 2 + .5) * (1 - 2 * (RND > .5))
        ph!(p, i) = RND * 6.28
    NEXT
NEXT

DIM hx(2, 3, 5), hy(2, 3, 5)
FOR h = 0 TO 5: FOR p = 0 TO 2: FOR i = 0 TO 3
    hx(p, i, h) = x!(p, i): hy(p, i, h) = y!(p, i)
NEXT: NEXT: NEXT
h = 0
t! = 0

DIM c(2)
c(0) = 11: c(1) = 12: c(2) = 15

DO
    oh = (h + 1) MOD 6
    
    'erase oldest
    FOR p = 0 TO 2
        FOR i = 0 TO 3
            LINE (hx(p, i, oh), hy(p, i, oh))-(hx(p, (i + 1) MOD 4, oh), hy(p, (i + 1) MOD 4, oh)), 0
        NEXT
    NEXT
    
    'store current
    FOR p = 0 TO 2: FOR i = 0 TO 3
        hx(p, i, h) = x!(p, i): hy(p, i, h) = y!(p, i)
    NEXT: NEXT
    
    'draw new
    FOR p = 0 TO 2
        FOR i = 0 TO 3
            LINE (x!(p, i), y!(p, i))-(x!(p, (i + 1) MOD 4), y!(p, (i + 1) MOD 4)), c(p)
        NEXT
    NEXT
    
    'move with independent swing
    FOR p = 0 TO 2: FOR i = 0 TO 3
        dx!(p, i) = dx!(p, i) + SIN(t! * (.8 + p * .2) + ph!(p, i)) * .25
        dy!(p, i) = dy!(p, i) + COS(t! * (.7 + p * .15) + ph!(p, i) * 1.3) * .25
        
        IF dx!(p, i) > 4 THEN dx!(p, i) = 4
        IF dx!(p, i) < -4 THEN dx!(p, i) = -4
        IF dy!(p, i) > 4 THEN dy!(p, i) = 4
        IF dy!(p, i) < -4 THEN dy!(p, i) = -4
        
        x!(p, i) = x!(p, i) + dx!(p, i): y!(p, i) = y!(p, i) + dy!(p, i)
        IF x!(p, i) < 0 OR x!(p, i) > 639 THEN dx!(p, i) = -dx!(p, i): x!(p, i) = x!(p, i) + dx!(p, i) * 2
        IF y!(p, i) < 0 OR y!(p, i) > 479 THEN dy!(p, i) = -dy!(p, i): y!(p, i) = y!(p, i) + dy!(p, i) * 2
    NEXT: NEXT
    
    h = (h + 1) MOD 6
    t! = t! + .07
LOOP WHILE INKEY$ = ""