' ================================================================
' 3D Wireframe Demo - SCREEN 7 with PCOPY
' - 320x200, 16 colors, page flipping for speed
' - Distance-based grayscale shading
'
' Controls: Arrows=move/turn, Z/X=height, ESC=quit
' ================================================================

DEFINT A-Z
CONST PI! = 3.14159265!

CONST MAXV = 60, MAXE = 60, MAX_INST = 10, NEARZ! = .15!
CONST YAW_ACC! = .004!, MOVE_ACC! = .03!
CONST YAW_DAMP! = .965!, MOVE_DAMP! = .94!
CONST MAX_YAW_V! = .06!, MAX_MOVE_V! = 1.0!
CONST MAX_STARS = 400

' ====== SCREEN 7: 320x200, 16 colors, page flipping ======
SCREEN 7

' Use two video pages
workPage = 1
viewPage = 0
SCREEN , , workPage, viewPage

' ====== Palette: 16 shades of gray ======
FOR i = 0 TO 15
    OUT &H3C8, i
    OUT &H3C9, i * 4
    OUT &H3C9, i * 4
    OUT &H3C9, i * 4
NEXT i

' ====== Model data ======
DIM vx(3, MAXV) AS SINGLE, vy(3, MAXV) AS SINGLE, vz(3, MAXV) AS SINGLE
DIM e0(3, MAXE) AS INTEGER, e1(3, MAXE) AS INTEGER
DIM nv(3) AS INTEGER, ne(3) AS INTEGER

' ====== Runtime arrays ======
DIM sX(MAX_INST, MAXV) AS INTEGER, sY(MAX_INST, MAXV) AS INTEGER
DIM visible(MAX_INST, MAXV) AS INTEGER

DIM instType(MAX_INST) AS INTEGER
DIM instX!(MAX_INST), instY!(MAX_INST), instZ!(MAX_INST)
DIM instYaw!(MAX_INST), instPitch!(MAX_INST)
DIM instSpeed!(MAX_INST), instTurn!(MAX_INST), instPitchTurn!(MAX_INST), instTimer!(MAX_INST)

DIM starX!(MAX_STARS), starY!(MAX_STARS), starZ!(MAX_STARS)
DIM planetX!, planetY!, planetZ!, planetRadius!

' ====== Init models ======
GOSUB InitModels

' ====== Camera ======
camX! = 0!: camY! = 1.0!: camZ! = -8!
camYaw! = 0!: camPitch! = 0!
camVelX! = 0!: camVelY! = 0!: camVelZ! = 0!
pitchV! = 0!: yawV! = 0!
fov! = 200!

minDist! = 2!: maxDist! = 80!

' ====== Init star field ======
' Stars are on a sphere at infinite distance (directional only)
FOR i = 0 TO MAX_STARS - 1
    ' Random point on unit sphere
    theta! = RND * 2! * PI!
    phi! = (RND - .5) * PI!
    starX!(i) = COS(phi!) * COS(theta!) * 1000!
    starY!(i) = SIN(phi!) * 1000!
    starZ!(i) = COS(phi!) * SIN(theta!) * 1000!
NEXT i

' ====== Init planet ======
planetX! = 80!: planetY! = 20!: planetZ! = 150!
planetRadius! = 30!

' ====== Init ships ======
RANDOMIZE TIMER
FOR i = 0 TO MAX_INST - 1
    instType(i) = INT(RND * 2)  ' Only 0 (starfighter) and 1 (cruiser)
    instX!(i) = (RND * 40!) - 20!
    instZ!(i) = (RND * 60!) - 10!
    instY!(i) = (RND * 20!) - 5!  ' Start at varied heights
    instYaw!(i) = RND * 2! * PI!
    instPitch!(i) = (RND - .5) * .3!  ' Some vertical angle
    instSpeed!(i) = .06! + RND * .04!
    instTurn!(i) = (RND - .5!) * .0005!
    instPitchTurn!(i) = (RND - .5!) * .0003!  ' Vertical turning
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
    IF k$ = CHR$(0) + "K" THEN yawV! = yawV! - .005!     ' Left arrow
    IF k$ = CHR$(0) + "M" THEN yawV! = yawV! + .005!     ' Right arrow
    IF k$ = CHR$(0) + "H" THEN pitchV! = pitchV! - .005! ' Up arrow
    IF k$ = CHR$(0) + "P" THEN pitchV! = pitchV! + .005! ' Down arrow
    
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
    FOR i = 0 TO MAX_INST - 1
        instTimer!(i) = instTimer!(i) - 1!
        IF instTimer!(i) <= 0! THEN
            instTimer!(i) = 120! + RND * 300!
            instTurn!(i) = (RND - .5!) * .001!
            instPitchTurn!(i) = (RND - .5!) * .0006!
            instSpeed!(i) = .05! + RND * .05!
        END IF
        ' Strong damping on turns to prevent spinning
        instTurn!(i) = instTurn!(i) * .95!
        instPitchTurn!(i) = instPitchTurn!(i) * .95!
        instYaw!(i) = instYaw!(i) + instTurn!(i)
        instPitch!(i) = instPitch!(i) + instPitchTurn!(i)
        
        ' Move in 3D based on yaw and pitch
        instX!(i) = instX!(i) + SIN(instYaw!(i)) * COS(instPitch!(i)) * instSpeed!(i)
        instY!(i) = instY!(i) + SIN(instPitch!(i)) * instSpeed!(i)
        instZ!(i) = instZ!(i) + COS(instYaw!(i)) * COS(instPitch!(i)) * instSpeed!(i)
        
        ' Very gentle boundary corrections
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
    ' Stars are at infinite distance, so we ignore camera position
    FOR i = 0 TO MAX_STARS - 1
        ' Only rotate, don't translate (infinite distance)
        x1! = starX!(i) * cyaw! - starZ!(i) * syaw!
        z1! = starX!(i) * syaw! + starZ!(i) * cyaw!
        y2! = starY!(i) * cp! - z1! * sp!
        z2! = starY!(i) * sp! + z1! * cp!
        
        IF z2! > 0! THEN
            sx = 160 + INT(x1! / z2! * fov!)
            sy = 100 - INT(y2! / z2! * fov!)
            IF sx >= 0 AND sx <= 319 AND sy >= 0 AND sy <= 199 THEN
                PSET (sx, sy), 15
            END IF
        END IF
    NEXT i
    
    ' ---- Draw planet ----
    dx! = planetX! - camX!
    dy! = planetY! - camY!
    dz! = planetZ! - camZ!
    
    x1! = dx! * cyaw! - dz! * syaw!
    z1! = dx! * syaw! + dz! * cyaw!
    y2! = dy! * cp! - z1! * sp!
    z2! = dy! * sp! + z1! * cp!
    
    IF z2! > NEARZ! THEN
        px = 160 + INT(x1! / z2! * fov!)
        py = 100 - INT(y2! / z2! * fov!)
        prad = INT(planetRadius! / z2! * fov!)
        IF prad > 1 THEN
            CIRCLE (px, py), prad, 7
            PAINT (px, py), 7, 7
        END IF
    END IF

    ' ---- Transform & render ships ----
    FOR i = 0 TO MAX_INST - 1
        tt = instType(i)
        
        ' Distance & color calc
        dx! = instX!(i) - camX!
        dy! = instY!(i) - camY!
        dz! = instZ!(i) - camZ!
        dist! = SQR(dx! * dx! + dy! * dy! + dz! * dz!)
        
        IF dist! < minDist! THEN dist! = minDist!
        IF dist! > maxDist! THEN dist! = maxDist!
        
        ' Non-linear falloff: square the distance factor for slower darkening near camera
        distFactor! = (dist! - minDist!) / (maxDist! - minDist!)
        distFactor! = distFactor! * distFactor!  ' Square it for non-linear falloff
        shade = 15 - INT(distFactor! * 15!)
        IF shade < 0 THEN shade = 0
        IF shade > 15 THEN shade = 15

        mcy! = COS(instYaw!(i)): msy! = SIN(instYaw!(i))
        numV = nv(tt)

        ' Transform all vertices
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
                sX(i, v) = 160 + INT(x1! / cZ! * fov!)
                sY(i, v) = 100 - INT(cY! / cZ! * fov!)
                visible(i, v) = 1
            ELSE
                visible(i, v) = 0
            END IF
        NEXT v

        ' Draw edges
        numE = ne(tt)
        FOR e = 0 TO numE - 1
            v0 = e0(tt, e): v1 = e1(tt, e)
            IF visible(i, v0) AND visible(i, v1) THEN
                LINE (sX(i, v0), sY(i, v0))-(sX(i, v1), sY(i, v1)), shade
            END IF
        NEXT e
    NEXT i

    ' Flip pages
    PCOPY workPage, viewPage
    SWAP workPage, viewPage

LOOP

SCREEN 0
END

' ================================================================
InitModels:
  ' ================================================================
'  Improved Wireframe Models (drop-in replacements)
'  Keep axes consistent with your existing renderer:
'    +Z = forward (nose), -Z = back (engines)
' ================================================================

' ------------------------------------------------
' Starfighter (0) - swept wings + engine stubs + dorsal fin
' ------------------------------------------------
nv(0) = 18: ne(0) = 40

' Nose + canopy
vx(0, 0) = 0:     vy(0, 0) = 0:     vz(0, 0) = 2.6     ' nose tip
vx(0, 1) = 0:     vy(0, 1) = .25:   vz(0, 1) = 1.6     ' canopy bump

' Front hull ring (slightly tapered / asymmetric in Z)
vx(0, 2) = -.40:  vy(0, 2) = -.22:  vz(0, 2) = 1.40
vx(0, 3) =  .40:  vy(0, 3) = -.22:  vz(0, 3) = 1.40
vx(0, 4) =  .45:  vy(0, 4) =  .22:  vz(0, 4) = 1.10
vx(0, 5) = -.45:  vy(0, 5) =  .22:  vz(0, 5) = 1.10

' Rear hull ring
vx(0, 6) = -.25:  vy(0, 6) = -.16:  vz(0, 6) = -.80
vx(0, 7) =  .25:  vy(0, 7) = -.16:  vz(0, 7) = -.80
vx(0, 8) =  .28:  vy(0, 8) =  .16:  vz(0, 8) = -1.00
vx(0, 9) = -.28:  vy(0, 9) =  .16:  vz(0, 9) = -1.00

vx(0, 10) = 0:    vy(0, 10) = 0:    vz(0, 10) = -2.00  ' tail tip

' Wings (tips + rear tips)
vx(0, 11) = -1.20: vy(0, 11) = 0:    vz(0, 11) =  .40  ' left tip
vx(0, 12) = -.90:  vy(0, 12) = 0:    vz(0, 12) = -.60  ' left rear
vx(0, 13) =  1.20: vy(0, 13) = 0:    vz(0, 13) =  .40  ' right tip
vx(0, 14) =  .90:  vy(0, 14) = 0:    vz(0, 14) = -.60  ' right rear

' Engine stubs (reads like twin engines without building full boxes)
vx(0, 15) = -.55:  vy(0, 15) = -.25: vz(0, 15) = -1.80 ' left engine end
vx(0, 16) =  .55:  vy(0, 16) = -.25: vz(0, 16) = -1.80 ' right engine end

' Dorsal fin
vx(0, 17) = 0:     vy(0, 17) = .60:  vz(0, 17) = -1.20

' ---- edges ----
' nose spokes + canopy
e0(0, 0) = 0:  e1(0, 0) = 2
e0(0, 1) = 0:  e1(0, 1) = 3
e0(0, 2) = 0:  e1(0, 2) = 4
e0(0, 3) = 0:  e1(0, 3) = 5
e0(0, 4) = 0:  e1(0, 4) = 1

' front ring
e0(0, 5) = 2:  e1(0, 5) = 3
e0(0, 6) = 3:  e1(0, 6) = 4
e0(0, 7) = 4:  e1(0, 7) = 5
e0(0, 8) = 5:  e1(0, 8) = 2

' front -> rear
e0(0, 9)  = 2: e1(0, 9)  = 6
e0(0, 10) = 3: e1(0, 10) = 7
e0(0, 11) = 4: e1(0, 11) = 8
e0(0, 12) = 5: e1(0, 12) = 9

' rear ring
e0(0, 13) = 6: e1(0, 13) = 7
e0(0, 14) = 7: e1(0, 14) = 8
e0(0, 15) = 8: e1(0, 15) = 9
e0(0, 16) = 9: e1(0, 16) = 6

' rear -> tail
e0(0, 17) = 6: e1(0, 17) = 10
e0(0, 18) = 7: e1(0, 18) = 10
e0(0, 19) = 8: e1(0, 19) = 10
e0(0, 20) = 9: e1(0, 20) = 10

' wings (left)
e0(0, 21) = 11: e1(0, 21) = 2
e0(0, 22) = 11: e1(0, 22) = 5
e0(0, 23) = 12: e1(0, 23) = 6
e0(0, 24) = 12: e1(0, 24) = 9
e0(0, 25) = 11: e1(0, 25) = 12

' wings (right)
e0(0, 26) = 13: e1(0, 26) = 3
e0(0, 27) = 13: e1(0, 27) = 4
e0(0, 28) = 14: e1(0, 28) = 7
e0(0, 29) = 14: e1(0, 29) = 8
e0(0, 30) = 13: e1(0, 30) = 14

' engines
e0(0, 31) = 15: e1(0, 31) = 6
e0(0, 32) = 15: e1(0, 32) = 9
e0(0, 33) = 15: e1(0, 33) = 12
e0(0, 34) = 16: e1(0, 34) = 7
e0(0, 35) = 16: e1(0, 35) = 8
e0(0, 36) = 16: e1(0, 36) = 14

' dorsal fin
e0(0, 37) = 17: e1(0, 37) = 8
e0(0, 38) = 17: e1(0, 38) = 9
e0(0, 39) = 17: e1(0, 39) = 10


' ------------------------------------------------
' Cruiser (1) - tapered hull + bridge tower + side nacelles
' ------------------------------------------------
nv(1) = 21: ne(1) = 46

vx(1, 0) = 0:     vy(1, 0) = 0:     vz(1, 0) = 3.2     ' nose tip

' front ring (z ~ 2.0)
vx(1, 1) = -.70:  vy(1, 1) = -.35:  vz(1, 1) = 2.0
vx(1, 2) =  .70:  vy(1, 2) = -.35:  vz(1, 2) = 2.0
vx(1, 3) =  .80:  vy(1, 3) =  .35:  vz(1, 3) = 1.8
vx(1, 4) = -.80:  vy(1, 4) =  .35:  vz(1, 4) = 1.8

' mid ring (z ~ 0.5)
vx(1, 5) = -.90:  vy(1, 5) = -.45:  vz(1, 5) =  .5
vx(1, 6) =  .90:  vy(1, 6) = -.45:  vz(1, 6) =  .5
vx(1, 7) =  1.00: vy(1, 7) =  .45:  vz(1, 7) =  .3
vx(1, 8) = -1.00: vy(1, 8) =  .45:  vz(1, 8) =  .3

' aft ring (z ~ -1.8)
vx(1, 9)  = -1.20: vy(1, 9)  = -.55: vz(1, 9)  = -1.8
vx(1, 10) =  1.20: vy(1, 10) = -.55: vz(1, 10) = -1.8
vx(1, 11) =  1.20: vy(1, 11) =  .55: vz(1, 11) = -2.0
vx(1, 12) = -1.20: vy(1, 12) =  .55: vz(1, 12) = -2.0

vx(1, 13) = 0:     vy(1, 13) = 0:     vz(1, 13) = -3.0 ' tail center

' bridge tower
vx(1, 14) = -.25:  vy(1, 14) =  .55:  vz(1, 14) =  .9
vx(1, 15) =  .25:  vy(1, 15) =  .55:  vz(1, 15) =  .9
vx(1, 16) = 0:     vy(1, 16) = 1.10:  vz(1, 16) =  .7

' nacelles + mounts
vx(1, 17) = -1.80: vy(1, 17) = -.20:  vz(1, 17) = -2.6 ' left nacelle end
vx(1, 18) =  1.80: vy(1, 18) = -.20:  vz(1, 18) = -2.6 ' right nacelle end
vx(1, 19) = -1.30: vy(1, 19) = -.20:  vz(1, 19) = -1.0 ' left mount
vx(1, 20) =  1.30: vy(1, 20) = -.20:  vz(1, 20) = -1.0 ' right mount

' ---- edges ----
' nose to front ring
e0(1, 0) = 0:  e1(1, 0) = 1
e0(1, 1) = 0:  e1(1, 1) = 2
e0(1, 2) = 0:  e1(1, 2) = 3
e0(1, 3) = 0:  e1(1, 3) = 4

' front ring
e0(1, 4) = 1:  e1(1, 4) = 2
e0(1, 5) = 2:  e1(1, 5) = 3
e0(1, 6) = 3:  e1(1, 6) = 4
e0(1, 7) = 4:  e1(1, 7) = 1

' front -> mid
e0(1, 8)  = 1: e1(1, 8)  = 5
e0(1, 9)  = 2: e1(1, 9)  = 6
e0(1, 10) = 3: e1(1, 10) = 7
e0(1, 11) = 4: e1(1, 11) = 8

' mid ring
e0(1, 12) = 5: e1(1, 12) = 6
e0(1, 13) = 6: e1(1, 13) = 7
e0(1, 14) = 7: e1(1, 14) = 8
e0(1, 15) = 8: e1(1, 15) = 5

' mid -> aft
e0(1, 16) = 5: e1(1, 16) = 9
e0(1, 17) = 6: e1(1, 17) = 10
e0(1, 18) = 7: e1(1, 18) = 11
e0(1, 19) = 8: e1(1, 19) = 12

' aft ring
e0(1, 20) = 9:  e1(1, 20) = 10
e0(1, 21) = 10: e1(1, 21) = 11
e0(1, 22) = 11: e1(1, 22) = 12
e0(1, 23) = 12: e1(1, 23) = 9

' aft -> tail center
e0(1, 24) = 9:  e1(1, 24) = 13
e0(1, 25) = 10: e1(1, 25) = 13
e0(1, 26) = 11: e1(1, 26) = 13
e0(1, 27) = 12: e1(1, 27) = 13

' bridge tower
e0(1, 28) = 14: e1(1, 28) = 15
e0(1, 29) = 14: e1(1, 29) = 16
e0(1, 30) = 15: e1(1, 30) = 16
e0(1, 31) = 14: e1(1, 31) = 8
e0(1, 32) = 15: e1(1, 32) = 7

' spine / tower tie-in
e0(1, 33) = 0:  e1(1, 33) = 16
e0(1, 34) = 16: e1(1, 34) = 3
e0(1, 35) = 16: e1(1, 35) = 4

' nacelles (left)
e0(1, 36) = 19: e1(1, 36) = 9
e0(1, 37) = 19: e1(1, 37) = 12
e0(1, 38) = 19: e1(1, 38) = 17
e0(1, 39) = 17: e1(1, 39) = 13
e0(1, 40) = 19: e1(1, 40) = 5

' nacelles (right)
e0(1, 41) = 20: e1(1, 41) = 10
e0(1, 42) = 20: e1(1, 42) = 11
e0(1, 43) = 20: e1(1, 43) = 18
e0(1, 44) = 18: e1(1, 44) = 13
e0(1, 45) = 20: e1(1, 45) = 6

RETURN