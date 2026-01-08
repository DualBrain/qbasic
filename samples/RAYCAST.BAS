'===============================================
' FAST QBASIC RAYCASTER v2.0
' Uses DDA algorithm, fixed-point math, pixel doubling
'===============================================
DEFINT A-Z

DECLARE SUB MoveIt (va%)
DECLARE FUNCTION CanMove% (va%)
DECLARE SUB Engine ()
DECLARE SUB RayCast ()
DECLARE SUB DrawMiniMap ()

TYPE PlayerType
    x AS SINGLE       ' World position (fractional for smooth movement)
    y AS SINGLE
    va AS INTEGER     ' View angle 0-359
END TYPE

'$DYNAMIC
SCREEN 7, 0, 1, 0

' Lookup tables - precalculate everything
DIM SHARED sintab(359) AS SINGLE, costab(359) AS SINGLE
DIM SHARED plr AS PlayerType, map(15, 15) AS INTEGER
DIM SHARED mapW AS INTEGER, mapH AS INTEGER

CONST PI! = 3.14159265#
CONST MOVESPEED! = .15
CONST TURNSPEED = 8
CONST FOV = 60
CONST HALFH = 100        ' Half screen height for wall projection

mapW = 16: mapH = 16

' Build trig tables
FOR i = 0 TO 359
    r! = i * (PI! / 180!)
    sintab(i) = SIN(r!)
    costab(i) = COS(r!)
NEXT

' Read map - 2 = player start
FOR y = 0 TO mapH - 1
    FOR x = 0 TO mapW - 1
        READ map(x, y)
        IF map(x, y) = 9 THEN
            map(x, y) = 0
            plr.x = x + .5
            plr.y = y + .5
            plr.va = 90
        END IF
    NEXT
NEXT

Engine
END

' Map data: 0=empty, 1-7=wall colors, 9=player start
MapData:
DATA 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
DATA 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
DATA 1, 0, 0, 2, 2, 2, 0, 0, 0, 3, 3, 3, 0, 0, 0, 1
DATA 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1
DATA 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 4, 4, 4, 0, 0, 0, 0, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 4, 0, 4, 0, 0, 0, 0, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 4, 0, 4, 0, 0, 0, 9, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
DATA 1, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 1
DATA 1, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 1
DATA 1, 0, 0, 5, 5, 5, 0, 0, 0, 6, 6, 6, 0, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
DATA 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
DATA 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1


SUB RayCast
    DIM rayDirX AS SINGLE, rayDirY AS SINGLE
    DIM deltaDistX AS SINGLE, deltaDistY AS SINGLE
    DIM sideDistX AS SINGLE, sideDistY AS SINGLE
    DIM perpWallDist AS SINGLE
    DIM mapX AS INTEGER, mapY AS INTEGER
    DIM stepX AS INTEGER, stepY AS INTEGER
    DIM hit AS INTEGER, side AS INTEGER
    DIM lineHeight AS INTEGER, drawStart AS INTEGER, drawEnd AS INTEGER
    DIM col AS INTEGER, rayAngle AS INTEGER
    
    ' Draw ceiling (dark blue) and floor (dark gray)
    LINE (0, 0)-(319, HALFH - 1), 1, BF
    LINE (0, HALFH)-(319, 199), 8, BF
    
    ' Cast a ray for each column (pixel doubling = 2 pixels per ray)
    FOR col = 0 TO 159
        ' Calculate ray angle
        rayAngle = plr.va - (FOV \ 2) + (col * FOV) \ 160
        IF rayAngle < 0 THEN rayAngle = rayAngle + 360
        IF rayAngle > 359 THEN rayAngle = rayAngle - 360
        
        rayDirX = sintab(rayAngle)
        rayDirY = costab(rayAngle)
        
        ' Current map cell
        mapX = INT(plr.x)
        mapY = INT(plr.y)
        
        ' Length of ray from one x/y side to next
        IF rayDirX = 0 THEN deltaDistX = 1E+10 ELSE deltaDistX = ABS(1 / rayDirX)
        IF rayDirY = 0 THEN deltaDistY = 1E+10 ELSE deltaDistY = ABS(1 / rayDirY)
        
        ' Calculate step and initial sideDist
        IF rayDirX < 0 THEN
            stepX = -1
            sideDistX = (plr.x - mapX) * deltaDistX
        ELSE
            stepX = 1
            sideDistX = (mapX + 1 - plr.x) * deltaDistX
        END IF
        
        IF rayDirY < 0 THEN
            stepY = -1
            sideDistY = (plr.y - mapY) * deltaDistY
        ELSE
            stepY = 1
            sideDistY = (mapY + 1 - plr.y) * deltaDistY
        END IF
        
        ' DDA algorithm
        hit = 0
        DO WHILE hit = 0
            IF sideDistX < sideDistY THEN
                sideDistX = sideDistX + deltaDistX
                mapX = mapX + stepX
                side = 0
            ELSE
                sideDistY = sideDistY + deltaDistY
                mapY = mapY + stepY
                side = 1
            END IF
            IF mapX < 0 OR mapX >= mapW OR mapY < 0 OR mapY >= mapH THEN
                hit = 1
            ELSEIF map(mapX, mapY) > 0 THEN
                hit = 1
            END IF
        LOOP
        
        ' Calculate perpendicular wall distance (avoids fisheye)
        IF side = 0 THEN
            perpWallDist = sideDistX - deltaDistX
        ELSE
            perpWallDist = sideDistY - deltaDistY
        END IF
        
        IF perpWallDist < .1 THEN perpWallDist = .1
        
        ' Calculate wall height
        lineHeight = INT(200 / perpWallDist)
        IF lineHeight > 200 THEN lineHeight = 200
        
        drawStart = HALFH - lineHeight \ 2
        drawEnd = HALFH + lineHeight \ 2
        IF drawStart < 0 THEN drawStart = 0
        IF drawEnd > 199 THEN drawEnd = 199
        
        ' Get wall color, darken if Y-side hit (fake shading)
        wallColor = map(mapX, mapY)
        IF side = 1 THEN
            ' Darken: 1->8, 2->4, 3->9, 4->12, 5->13, 6->14, 7->8
            SELECT CASE wallColor
                CASE 1: wallColor = 8
                CASE 2: wallColor = 4
                CASE 3: wallColor = 9
                CASE 4: wallColor = 12
                CASE 5: wallColor = 5
                CASE 6: wallColor = 6
                CASE 7: wallColor = 7
            END SELECT
        END IF
        
        ' Draw wall slice (pixel doubled)
        LINE (col * 2, drawStart)-(col * 2 + 1, drawEnd), wallColor, BF
    NEXT
    
    ' Draw minimap
    DrawMiniMap
    
    ' Flip buffer
    PCOPY 1, 0
END SUB
SUB DrawMiniMap
    DIM mx AS INTEGER, my AS INTEGER
    DIM px AS INTEGER, py AS INTEGER
    
    ' Map offset in screen coords
    CONST MAPOFS = 2
    CONST MAPSCALE = 3
    
    FOR my = 0 TO mapH - 1
        FOR mx = 0 TO mapW - 1
            IF map(mx, my) > 0 THEN
                LINE (MAPOFS + mx * MAPSCALE, MAPOFS + my * MAPSCALE)-(MAPOFS + mx * MAPSCALE + MAPSCALE - 1, MAPOFS + my * MAPSCALE + MAPSCALE - 1), map(mx, my), BF
            END IF
        NEXT
    NEXT
    
    ' Draw player position
    px = MAPOFS + INT(plr.x * MAPSCALE)
    py = MAPOFS + INT(plr.y * MAPSCALE)
    CIRCLE (px, py), 2, 15
    
    ' Draw view direction line
    LINE (px, py)-(px + sintab(plr.va) * 5, py + costab(plr.va) * 5), 14
END SUB
FUNCTION CanMove% (va AS INTEGER)
    DIM newX AS SINGLE, newY AS SINGLE
    DIM a AS INTEGER
    
    a = va
    IF a < 0 THEN a = a + 360
    IF a > 359 THEN a = a - 360
    
    newX = plr.x + sintab(a) * MOVESPEED
    newY = plr.y + costab(a) * MOVESPEED
    
    ' Check collision with wall (with small margin)
    IF map(INT(newX + .2), INT(plr.y)) > 0 THEN CanMove% = 0: EXIT FUNCTION
    IF map(INT(newX - .2), INT(plr.y)) > 0 THEN CanMove% = 0: EXIT FUNCTION
    IF map(INT(plr.x), INT(newY + .2)) > 0 THEN CanMove% = 0: EXIT FUNCTION
    IF map(INT(plr.x), INT(newY - .2)) > 0 THEN CanMove% = 0: EXIT FUNCTION
    IF map(INT(newX), INT(newY)) > 0 THEN CanMove% = 0: EXIT FUNCTION
    
    CanMove% = 1
END FUNCTION
SUB MoveIt (va AS INTEGER)
    DIM a AS INTEGER
    a = va
    IF a < 0 THEN a = a + 360
    IF a > 359 THEN a = a - 360
    
    plr.x = plr.x + sintab(a) * MOVESPEED
    plr.y = plr.y + costab(a) * MOVESPEED
END SUB
SUB Engine
    DIM k$
    
    ' Key codes
    u$ = CHR$(0) + "H": d$ = CHR$(0) + "P"
    l$ = CHR$(0) + "K": r$ = CHR$(0) + "M"
    q$ = CHR$(0) + "I": a$ = CHR$(0) + "Q"  ' PgUp/PgDn for strafe
    
    DO
        RayCast
        
        ' Process all pending keys for smoother input
        DO
            k$ = INKEY$
            IF k$ = "" THEN EXIT DO
            
            SELECT CASE k$
                CASE CHR$(27)
                    END
                CASE u$  ' Forward
                    IF CanMove%(plr.va) THEN MoveIt plr.va
                CASE d$  ' Backward
                    IF CanMove%(plr.va + 180) THEN MoveIt plr.va + 180
                CASE l$  ' Turn left
                    plr.va = plr.va - TURNSPEED
                    IF plr.va < 0 THEN plr.va = plr.va + 360
                CASE r$  ' Turn right
                    plr.va = plr.va + TURNSPEED
                    IF plr.va > 359 THEN plr.va = plr.va - 360
                CASE q$  ' Strafe left
                    IF CanMove%(plr.va - 90) THEN MoveIt plr.va - 90
                CASE a$  ' Strafe right
                    IF CanMove%(plr.va + 90) THEN MoveIt plr.va + 90
            END SELECT
        LOOP
    LOOP
END SUB