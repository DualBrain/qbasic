DEFINT A-Z
CONST PI! = 3.14159265!

CONST MAXV = 60, MAXE = 60, MAXINST = 10, NEARZ! = .15!
CONST YAWACC! = .004!, MOVEACC! = .03!
CONST YAWDAMP! = .965!, MOVEDAMP! = .94!
CONST MAXYAWV! = .06!, MAXMOVEV! = 1.0!
CONST MAXSTARS = 400

SCREEN 7

workPage = 1
viewPage = 0
SCREEN , , workPage, viewPage


FOR i = 0 TO 9
    OUT &H3C8, i
    OUT &H3C9, i * 6
    OUT &H3C9, i * 6
    OUT &H3C9, i * 6
NEXT i

OUT &H3C8, 10
OUT &H3C9, 60
OUT &H3C9, 60
OUT &H3C9, 60
OUT &H3C8, 11
OUT &H3C9, 63
OUT &H3C9, 63
OUT &H3C9, 63

OUT &H3C8, 12
OUT &H3C9, 63
OUT &H3C9, 63
OUT &H3C9, 0

OUT &H3C8, 13
OUT &H3C9, 63
OUT &H3C9, 0
OUT &H3C9, 0

OUT &H3C8, 14
OUT &H3C9, 0
OUT &H3C9, 20
OUT &H3C9, 63

OUT &H3C8, 15
OUT &H3C9, 0
OUT &H3C9, 63
OUT &H3C9, 0

' ====== Model data ======
DIM vx(3, MAXV) AS SINGLE, vy(3, MAXV) AS SINGLE, vz(3, MAXV) AS SINGLE
DIM e0(3, MAXE) AS INTEGER, e1(3, MAXE) AS INTEGER
DIM nv(3) AS INTEGER, ne(3) AS INTEGER

' ====== Runtime arrays ======
DIM sX(MAXINST, MAXV) AS INTEGER, sY(MAXINST, MAXV) AS INTEGER
DIM visible(MAXINST, MAXV) AS INTEGER

DIM instType(MAXINST) AS INTEGER
DIM instX!(MAXINST), instY!(MAXINST), instZ!(MAXINST)
DIM instYaw!(MAXINST), instPitch!(MAXINST)
DIM instSpeed!(MAXINST), instTurn!(MAXINST), instPitchTurn!(MAXINST), instTimer!(MAXINST)

DIM starX!(MAXSTARS), starY!(MAXSTARS), starZ!(MAXSTARS)

' Multiple planets
DIM planetX!(3), planetY!(3), planetZ!(3), planetRadius!(3), planetColor(3)

' ====== Init models ======
GOSUB InitModels

' ====== Camera ======
camX! = 0!: camY! = 1.0!: camZ! = -8!
camYaw! = 0!: camPitch! = 0!
camVelX! = 0!: camVelY! = 0!: camVelZ! = 0!
pitchV! = 0!: yawV! = 0!
fov! = 200!

' Extended lighting range - objects stay lit longer
minDist! = 2!: maxDist! = 120!

' ====== Init star field ======
FOR i = 0 TO MAXSTARS - 1
    theta! = RND * 2! * PI!
    phi! = (RND - .5) * PI!
    starX!(i) = COS(phi!) * COS(theta!) * 1000!
    starY!(i) = SIN(phi!) * 1000!
    starZ!(i) = COS(phi!) * SIN(theta!) * 1000!
NEXT i

' ====== Init planets ======
' Yellow planet
planetX!(0) = 80!: planetY!(0) = 20!: planetZ!(0) = 150!
planetRadius!(0) = 30!: planetColor(0) = 12

' Red planet
planetX!(1) = -120!: planetY!(1) = -15!: planetZ!(1) = 200!
planetRadius!(1) = 25!: planetColor(1) = 13

' Blue planet
planetX!(2) = 50!: planetY!(2) = 60!: planetZ!(2) = 180!
planetRadius!(2) = 20!: planetColor(2) = 14

' ====== Init ships ======
RANDOMIZE TIMER
FOR i = 0 TO MAXINST - 1
    instType(i) = INT(RND * 2)
    instX!(i) = (RND * 40!) - 20!
    instZ!(i) = (RND * 60!) - 10!
    instY!(i) = (RND * 20!) - 5!
    instYaw!(i) = RND * 2! * PI!
    instPitch!(i) = (RND - .5) * .3!
    instSpeed!(i) = .06! + RND * .04!
    instTurn!(i) = (RND - .5!) * .0005!
    instPitchTurn!(i) = (RND - .5!) * .0003!
    instTimer!(i) = 120! + RND * 300!
NEXT i

' ================================================================
' MAIN LOOP
' ================================================================
DO
    ' Draw on hidden page
    SCREEN , , workPage, viewPage
    
    ' ---- Input ----
    k$ = INKEY$
    
    ' Rotation controls
    IF k$ = CHR$(0) + "K" THEN yawV! = yawV! - .005!
    IF k$ = CHR$(0) + "M" THEN yawV! = yawV! + .005!
    IF k$ = CHR$(0) + "H" THEN pitchV! = pitchV! - .005!
    IF k$ = CHR$(0) + "P" THEN pitchV! = pitchV! + .005!
    
    ' Velocity controls
    IF k$ = "a" OR k$ = "A" THEN
        camVelX! = camVelX! + SIN(camYaw!) * .02!
        camVelZ! = camVelZ! + COS(camYaw!) * .02!
    END IF
    IF k$ = "s" OR k$ = "S" THEN
        camVelX! = camVelX! - SIN(camYaw!) * .02!
        camVelZ! = camVelZ! - COS(camYaw!) * .02!
    END IF
    
    ' Emergency stop
    IF k$ = "x" OR k$ = "X" THEN
        yawV! = 0!: pitchV! = 0!
        camVelX! = 0!: camVelY! = 0!: camVelZ! = 0!
    END IF

    ' Apply damping to rotation
    yawV! = yawV! * .95!
    pitchV! = pitchV! * .95!
    
    ' Apply rotation
    camYaw! = camYaw! + yawV!
    camPitch! = camPitch! + pitchV!
    
    ' Apply velocity (NO damping - space physics)
    camX! = camX! + camVelX!
    camZ! = camZ! + camVelZ!
    camY! = camY! + camVelY!
    IF k$ = CHR$(27) THEN EXIT DO

    ' ---- Update ships ----
    FOR i = 0 TO MAXINST - 1
        instTimer!(i) = instTimer!(i) - 1!
        IF instTimer!(i) <= 0! THEN
            instTimer!(i) = 120! + RND * 300!
            instTurn!(i) = (RND - .5!) * .001!
            instPitchTurn!(i) = (RND - .5!) * .0006!
            instSpeed!(i) = .05! + RND * .05!
        END IF
        instTurn!(i) = instTurn!(i) * .95!
        instPitchTurn!(i) = instPitchTurn!(i) * .95!
        instYaw!(i) = instYaw!(i) + instTurn!(i)
        instPitch!(i) = instPitch!(i) + instPitchTurn!(i)
        
        instX!(i) = instX!(i) + SIN(instYaw!(i)) * COS(instPitch!(i)) * instSpeed!(i)
        instY!(i) = instY!(i) + SIN(instPitch!(i)) * instSpeed!(i)
        instZ!(i) = instZ!(i) + COS(instYaw!(i)) * COS(instPitch!(i)) * instSpeed!(i)
        
        IF instX!(i) > 60! THEN instTurn!(i) = instTurn!(i) + .0005!
        IF instX!(i) < -60! THEN instTurn!(i) = instTurn!(i) - .0005!
        IF instZ!(i) > 90! THEN instTurn!(i) = instTurn!(i) + .0005!
        IF instZ!(i) < -40! THEN instTurn!(i) = instTurn!(i) - .0005!
        IF instY!(i) > 30! THEN instPitchTurn!(i) = instPitchTurn!(i) - .0003!
        IF instY!(i) < -10! THEN instPitchTurn!(i) = instPitchTurn!(i) + .0003!
    NEXT i

    ' Clear background
    LINE (0, 0)-(319, 199), 0, BF

    ' ---- Pre-compute trig ----
    cyaw! = COS(camYaw!): syaw! = SIN(camYaw!)
    cp! = COS(camPitch!): sp! = SIN(camPitch!)

    ' ---- Draw stars ----
    FOR i = 0 TO MAXSTARS - 1
        x1! = starX!(i) * cyaw! - starZ!(i) * syaw!
        z1! = starX!(i) * syaw! + starZ!(i) * cyaw!
        y2! = starY!(i) * cp! - z1! * sp!
        z2! = starY!(i) * sp! + z1! * cp!
        
        IF z2! > 0! THEN
            tx! = x1! / z2! * fov!
            ty! = y2! / z2! * fov!
            IF tx! > -200! AND tx! < 200! AND ty! > -150! AND ty! < 150! THEN
                sx = 160 + INT(tx!)
                sy = 100 - INT(ty!)
                IF sx >= 0 AND sx <= 319 AND sy >= 0 AND sy <= 174 THEN
                    PSET (sx, sy), 11
                END IF
            END IF
        END IF
    NEXT i
    
    ' ---- Draw planets ----
    FOR p = 0 TO 2
        dx! = planetX!(p) - camX!
        dy! = planetY!(p) - camY!
        dz! = planetZ!(p) - camZ!
        
        x1! = dx! * cyaw! - dz! * syaw!
        z1! = dx! * syaw! + dz! * cyaw!
        y2! = dy! * cp! - z1! * sp!
        z2! = dy! * sp! + z1! * cp!
        
        IF z2! > NEARZ! THEN
            tx! = x1! / z2! * fov!
            ty! = y2! / z2! * fov!
            IF tx! > -500! AND tx! < 500! AND ty! > -500! AND ty! < 500! THEN
                px = 160 + INT(tx!)
                py = 100 - INT(ty!)
                prad! = planetRadius!(p) / z2! * fov!
                IF prad! > 500! THEN prad! = 500!
                prad = INT(prad!)
                
                ' Only draw if center is somewhat on screen and above HUD
                IF prad > 1 AND px > -prad AND px < 320 + prad AND py > -prad AND py < 175 + prad THEN
                    CIRCLE (px, py), prad, planetColor(p)
                    ' Only paint if center is safely on screen to avoid PAINT getting lost
                    IF px >= 0 AND px <= 319 AND py >= 0 AND py < 175 THEN
                        PAINT (px, py), planetColor(p), planetColor(p)
                    END IF
                END IF
            END IF
        END IF
    NEXT p

    ' ---- Transform & render ships ----
    FOR i = 0 TO MAXINST - 1
        tt = instType(i)
        
        dx! = instX!(i) - camX!
        dy! = instY!(i) - camY!
        dz! = instZ!(i) - camZ!
        dist! = SQR(dx! * dx! + dy! * dy! + dz! * dz!)
        
        IF dist! < minDist! THEN dist! = minDist!
        IF dist! > maxDist! THEN dist! = maxDist!
        
        ' Shading: far = darkest, medium = mid-tone, close = brightest
        ' distFactor: 0 (close) to 1 (far)
        distFactor! = (dist! - minDist!) / (maxDist! - minDist!)
        
        IF distFactor! < .4 THEN
            ' Close range: 0-40% = brightest (colors 9-11)
            shade = 11 - INT(distFactor! / .4 * 2!)
        ELSEIF distFactor! < .7 THEN
            ' Mid range: 40-70% = mid-tone (colors 4-9)
            midFactor! = (distFactor! - .4) / .3
            shade = 9 - INT(midFactor! * 5!)
        ELSE
            ' Far range: 70-100% = darkest (colors 0-4)
            farFactor! = (distFactor! - .7) / .3
            shade = 4 - INT(farFactor! * 4!)
        END IF
        
        IF shade < 0 THEN shade = 0
        IF shade > 11 THEN shade = 11

        mcy! = COS(instYaw!(i)): msy! = SIN(instYaw!(i))
        numV = nv(tt)

        FOR v = 0 TO numV - 1
            wx! = vx(tt, v) * mcy! - vz(tt, v) * msy! + instX!(i)
            wy! = vy(tt, v) + instY!(i)
            wz! = vx(tt, v) * msy! + vz(tt, v) * mcy! + instZ!(i)
            
            dx! = wx! - camX!: dy! = wy! - camY!: dz! = wz! - camZ!
            x1! = dx! * cyaw! - dz! * syaw!
            z1! = dx! * syaw! + dz! * cyaw!
            cY! = dy! * cp! - z1! * sp!
            cZ! = dy! * sp! + z1! * cp!

            IF cZ! > NEARZ! THEN
                tx! = x1! / cZ! * fov!
                ty! = cY! / cZ! * fov!
                IF tx! > -500! AND tx! < 500! AND ty! > -500! AND ty! < 500! THEN
                    sX(i, v) = 160 + INT(tx!)
                    sY(i, v) = 100 - INT(ty!)
                    visible(i, v) = 1
                ELSE
                    visible(i, v) = 0
                END IF
            ELSE
                visible(i, v) = 0
            END IF
        NEXT v

        numE = ne(tt)
        FOR e = 0 TO numE - 1
            v0 = e0(tt, e): v1 = e1(tt, e)
            IF visible(i, v0) AND visible(i, v1) THEN
                IF sY(i, v0) < 175 AND sY(i, v1) < 175 THEN
                    LINE (sX(i, v0), sY(i, v0))-(sX(i, v1), sY(i, v1)), shade
                END IF
            END IF
        NEXT e
    NEXT i

    ' ---- Draw HUD ----
    ' HUD background bar (bottom 25 pixels)
    LINE (0, 175)-(319, 199), 0, BF
    LINE (0, 175)-(319, 176), 11
    
    ' Radar display (left side, circular)
    radarCX = 30: radarCY = 187: radarR = 10
    CIRCLE (radarCX, radarCY), radarR, 11
    
    ' Draw ship blips on radar (green dots)
    FOR i = 0 TO MAXINST - 1
        dx! = instX!(i) - camX!
        dz! = instZ!(i) - camZ!
        dist2D! = SQR(dx! * dx! + dz! * dz!)
        
        IF dist2D! < 80! THEN
            ' Rotate to camera space
            rx! = dx! * cyaw! - dz! * syaw!
            rz! = dx! * syaw! + dz! * cyaw!
            
            ' Scale to radar
            scale! = radarR / 80!
            rdx = INT(rx! * scale!)
            rdz = INT(-rz! * scale!)  ' Negative because forward is +Z
            
            IF rdx * rdx + rdz * rdz <= radarR * radarR THEN
                PSET (radarCX + rdx, radarCY + rdz), 15
            END IF
        END IF
    NEXT i
    
    ' Player indicator (center dot)
    PSET (radarCX, radarCY), 11
    
    ' Velocity indicator
    velMag! = SQR(camVelX! * camVelX! + camVelZ! * camVelZ!)
    LINE (55, 180)-(95, 185), 0, BF
    LINE (55, 180)-(95, 185), 11, B
    LOCATE 24, 8: PRINT "VEL";
    barLen = INT(velMag! * 20!)
    IF barLen > 40 THEN barLen = 40
    IF barLen > 0 THEN LINE (56, 181)-(55 + barLen, 184), 15, BF
    
    ' Coordinates
    LOCATE 24, 19
    PRINT USING "X:###"; INT(camX!);
    LOCATE 25, 19
    PRINT USING "Z:###"; INT(camZ!);
    
    ' Ship count
    LOCATE 24, 32
    PRINT "SHIPS:"; MAXINST + 1

    ' Flip pages
    PCOPY workPage, viewPage
    SWAP workPage, viewPage

LOOP

SCREEN 0
END

' ================================================================
InitModels:
' ================================================================
' Starfighter (0) - swept wings + engine stubs + dorsal fin
' ------------------------------------------------
nv(0) = 18: ne(0) = 40

vx(0, 0) = 0:     vy(0, 0) = 0:     vz(0, 0) = 2.6
vx(0, 1) = 0:     vy(0, 1) = .25:   vz(0, 1) = 1.6

vx(0, 2) = -.40:  vy(0, 2) = -.22:  vz(0, 2) = 1.40
vx(0, 3) =  .40:  vy(0, 3) = -.22:  vz(0, 3) = 1.40
vx(0, 4) =  .45:  vy(0, 4) =  .22:  vz(0, 4) = 1.10
vx(0, 5) = -.45:  vy(0, 5) =  .22:  vz(0, 5) = 1.10

vx(0, 6) = -.25:  vy(0, 6) = -.16:  vz(0, 6) = -.80
vx(0, 7) =  .25:  vy(0, 7) = -.16:  vz(0, 7) = -.80
vx(0, 8) =  .28:  vy(0, 8) =  .16:  vz(0, 8) = -1.00
vx(0, 9) = -.28:  vy(0, 9) =  .16:  vz(0, 9) = -1.00

vx(0, 10) = 0:    vy(0, 10) = 0:    vz(0, 10) = -2.00

vx(0, 11) = -1.20: vy(0, 11) = 0:    vz(0, 11) =  .40
vx(0, 12) = -.90:  vy(0, 12) = 0:    vz(0, 12) = -.60
vx(0, 13) =  1.20: vy(0, 13) = 0:    vz(0, 13) =  .40
vx(0, 14) =  .90:  vy(0, 14) = 0:    vz(0, 14) = -.60

vx(0, 15) = -.55:  vy(0, 15) = -.25: vz(0, 15) = -1.80
vx(0, 16) =  .55:  vy(0, 16) = -.25: vz(0, 16) = -1.80

vx(0, 17) = 0:     vy(0, 17) = .60:  vz(0, 17) = -1.20

e0(0, 0) = 0:  e1(0, 0) = 2
e0(0, 1) = 0:  e1(0, 1) = 3
e0(0, 2) = 0:  e1(0, 2) = 4
e0(0, 3) = 0:  e1(0, 3) = 5
e0(0, 4) = 0:  e1(0, 4) = 1

e0(0, 5) = 2:  e1(0, 5) = 3
e0(0, 6) = 3:  e1(0, 6) = 4
e0(0, 7) = 4:  e1(0, 7) = 5
e0(0, 8) = 5:  e1(0, 8) = 2

e0(0, 9)  = 2: e1(0, 9)  = 6
e0(0, 10) = 3: e1(0, 10) = 7
e0(0, 11) = 4: e1(0, 11) = 8
e0(0, 12) = 5: e1(0, 12) = 9

e0(0, 13) = 6: e1(0, 13) = 7
e0(0, 14) = 7: e1(0, 14) = 8
e0(0, 15) = 8: e1(0, 15) = 9
e0(0, 16) = 9: e1(0, 16) = 6

e0(0, 17) = 6: e1(0, 17) = 10
e0(0, 18) = 7: e1(0, 18) = 10
e0(0, 19) = 8: e1(0, 19) = 10
e0(0, 20) = 9: e1(0, 20) = 10

e0(0, 21) = 11: e1(0, 21) = 2
e0(0, 22) = 11: e1(0, 22) = 5
e0(0, 23) = 12: e1(0, 23) = 6
e0(0, 24) = 12: e1(0, 24) = 9
e0(0, 25) = 11: e1(0, 25) = 12

e0(0, 26) = 13: e1(0, 26) = 3
e0(0, 27) = 13: e1(0, 27) = 4
e0(0, 28) = 14: e1(0, 28) = 7
e0(0, 29) = 14: e1(0, 29) = 8
e0(0, 30) = 13: e1(0, 30) = 14

e0(0, 31) = 15: e1(0, 31) = 6
e0(0, 32) = 15: e1(0, 32) = 9
e0(0, 33) = 15: e1(0, 33) = 12
e0(0, 34) = 16: e1(0, 34) = 7
e0(0, 35) = 16: e1(0, 35) = 8
e0(0, 36) = 16: e1(0, 36) = 14

e0(0, 37) = 17: e1(0, 37) = 8
e0(0, 38) = 17: e1(0, 38) = 9
e0(0, 39) = 17: e1(0, 39) = 10

' ------------------------------------------------
' Cruiser (1) - tapered hull + bridge tower + side nacelles
' ------------------------------------------------
nv(1) = 21: ne(1) = 46

vx(1, 0) = 0:     vy(1, 0) = 0:     vz(1, 0) = 3.2

vx(1, 1) = -.70:  vy(1, 1) = -.35:  vz(1, 1) = 2.0
vx(1, 2) =  .70:  vy(1, 2) = -.35:  vz(1, 2) = 2.0
vx(1, 3) =  .80:  vy(1, 3) =  .35:  vz(1, 3) = 1.8
vx(1, 4) = -.80:  vy(1, 4) =  .35:  vz(1, 4) = 1.8

vx(1, 5) = -.90:  vy(1, 5) = -.45:  vz(1, 5) =  .5
vx(1, 6) =  .90:  vy(1, 6) = -.45:  vz(1, 6) =  .5
vx(1, 7) =  1.00: vy(1, 7) =  .45:  vz(1, 7) =  .3
vx(1, 8) = -1.00: vy(1, 8) =  .45:  vz(1, 8) =  .3

vx(1, 9)  = -1.20: vy(1, 9)  = -.55: vz(1, 9)  = -1.8
vx(1, 10) =  1.20: vy(1, 10) = -.55: vz(1, 10) = -1.8
vx(1, 11) =  1.20: vy(1, 11) =  .55: vz(1, 11) = -2.0
vx(1, 12) = -1.20: vy(1, 12) =  .55: vz(1, 12) = -2.0

vx(1, 13) = 0:     vy(1, 13) = 0:     vz(1, 13) = -3.0

vx(1, 14) = -.25:  vy(1, 14) =  .55:  vz(1, 14) =  .9
vx(1, 15) =  .25:  vy(1, 15) =  .55:  vz(1, 15) =  .9
vx(1, 16) = 0:     vy(1, 16) = 1.10:  vz(1, 16) =  .7

vx(1, 17) = -1.80: vy(1, 17) = -.20:  vz(1, 17) = -2.6
vx(1, 18) =  1.80: vy(1, 18) = -.20:  vz(1, 18) = -2.6
vx(1, 19) = -1.30: vy(1, 19) = -.20:  vz(1, 19) = -1.0
vx(1, 20) =  1.30: vy(1, 20) = -.20:  vz(1, 20) = -1.0

e0(1, 0) = 0:  e1(1, 0) = 1
e0(1, 1) = 0:  e1(1, 1) = 2
e0(1, 2) = 0:  e1(1, 2) = 3
e0(1, 3) = 0:  e1(1, 3) = 4

e0(1, 4) = 1:  e1(1, 4) = 2
e0(1, 5) = 2:  e1(1, 5) = 3
e0(1, 6) = 3:  e1(1, 6) = 4
e0(1, 7) = 4:  e1(1, 7) = 1

e0(1, 8)  = 1: e1(1, 8)  = 5
e0(1, 9)  = 2: e1(1, 9)  = 6
e0(1, 10) = 3: e1(1, 10) = 7
e0(1, 11) = 4: e1(1, 11) = 8

e0(1, 12) = 5: e1(1, 12) = 6
e0(1, 13) = 6: e1(1, 13) = 7
e0(1, 14) = 7: e1(1, 14) = 8
e0(1, 15) = 8: e1(1, 15) = 5

e0(1, 16) = 5: e1(1, 16) = 9
e0(1, 17) = 6: e1(1, 17) = 10
e0(1, 18) = 7: e1(1, 18) = 11
e0(1, 19) = 8: e1(1, 19) = 12

e0(1, 20) = 9:  e1(1, 20) = 10
e0(1, 21) = 10: e1(1, 21) = 11
e0(1, 22) = 11: e1(1, 22) = 12
e0(1, 23) = 12: e1(1, 23) = 9

e0(1, 24) = 9:  e1(1, 24) = 13
e0(1, 25) = 10: e1(1, 25) = 13
e0(1, 26) = 11: e1(1, 26) = 13
e0(1, 27) = 12: e1(1, 27) = 13

e0(1, 28) = 14: e1(1, 28) = 15
e0(1, 29) = 14: e1(1, 29) = 16
e0(1, 30) = 15: e1(1, 30) = 16
e0(1, 31) = 14: e1(1, 31) = 8
e0(1, 32) = 15: e1(1, 32) = 7

e0(1, 33) = 0:  e1(1, 33) = 16
e0(1, 34) = 16: e1(1, 34) = 3
e0(1, 35) = 16: e1(1, 35) = 4

e0(1, 36) = 19: e1(1, 36) = 9
e0(1, 37) = 19: e1(1, 37) = 12
e0(1, 38) = 19: e1(1, 38) = 17
e0(1, 39) = 17: e1(1, 39) = 13
e0(1, 40) = 19: e1(1, 40) = 5

e0(1, 41) = 20: e1(1, 41) = 10
e0(1, 42) = 20: e1(1, 42) = 11
e0(1, 43) = 20: e1(1, 43) = 18
e0(1, 44) = 18: e1(1, 44) = 13
e0(1, 45) = 20: e1(1, 45) = 6

RETURN