' 3D Flat Shaded Vehicle Display
' Scanline triangle filling
DEFINT A-Z
CONST PI! = 3.14159265!

SCREEN 7
workPage = 1: viewPage = 0
SCREEN , , workPage, viewPage

' Set 16 shades of gray using VGA DAC
FOR i = 0 TO 15
    gray = i * 4      ' 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60
    OUT &H3C8, i      ' Select palette entry i
    OUT &H3C9, gray   ' Red component (0-63)
    OUT &H3C9, gray   ' Green component (0-63)  
    OUT &H3C9, gray   ' Blue component (0-63)
NEXT i

' Now draw something to test
FOR i = 0 TO 15
    LINE (i * 20, 0)-(i * 20 + 19, 199), i, BF
NEXT i

DIM vx(50) AS SINGLE, vy(50) AS SINGLE, vz(50) AS SINGLE
DIM sx(50) AS INTEGER, sy(50) AS INTEGER
DIM cx(50) AS SINGLE, cy(50) AS SINGLE, cz(50) AS SINGLE
DIM fv(60, 3) AS INTEGER, fc(60) AS INTEGER
DIM fz(60) AS SINGLE, fo(60) AS INTEGER

' Scanline buffer
DIM scanL(199) AS INTEGER, scanR(199) AS INTEGER
DIM scanZL(199) AS SINGLE, scanZR(199) AS SINGLE  ' depth at each edge

numVerts = 0: numFaces = 0
vehicle = 0: numVehicles = 4

GOSUB LoadVehicle

centerX = 160: centerY = 100
fov! = 200!
camOrbitY! = .5!
camOrbitX! = .4!
camRadius! = 10!

' Depth range for shading
minDepth! = 3!
maxDepth! = 18!

DO
    SCREEN , , workPage, viewPage
    LINE (0, 0)-(319, 199), 0, BF
    
    SELECT CASE vehicle
        CASE 0: t$ = "STARFIGHTER"
        CASE 1: t$ = "SPACE CRUISER"
        CASE 2: t$ = "RACE CAR"
        CASE 3: t$ = "HELICOPTER"
    END SELECT
    LOCATE 1, 20 - LEN(t$) \ 2: COLOR 14: PRINT t$;
    LOCATE 25, 3: COLOR 8: PRINT "Arrows=Orbit  Z/X=Zoom  SPACE=Next";
    
    cosY! = COS(camOrbitY!): sinY! = SIN(camOrbitY!)
    cosX! = COS(camOrbitX!): sinX! = SIN(camOrbitX!)
    camPosX! = camRadius! * cosX! * sinY!
    camPosY! = camRadius! * sinX!
    camPosZ! = camRadius! * cosX! * cosY!
    
    fwdX! = -camPosX! / camRadius!
    fwdY! = -camPosY! / camRadius!
    fwdZ! = -camPosZ! / camRadius!
    rightX! = fwdZ!: rightZ! = -fwdX!: rightY! = 0!
    rl! = SQR(rightX! * rightX! + rightZ! * rightZ!)
    IF rl! > .001! THEN rightX! = rightX! / rl!: rightZ! = rightZ! / rl!
    upX! = fwdY! * rightZ! - fwdZ! * rightY!
    upY! = fwdZ! * rightX! - fwdX! * rightZ!
    upZ! = fwdX! * rightY! - fwdY! * rightX!
    
    FOR i = 0 TO numVerts - 1
        dx! = vx(i) - camPosX!
        dy! = vy(i) - camPosY!
        dz! = vz(i) - camPosZ!
        cx(i) = dx! * rightX! + dy! * rightY! + dz! * rightZ!
        cy(i) = dx! * upX! + dy! * upY! + dz! * upZ!
        cz(i) = dx! * fwdX! + dy! * fwdY! + dz! * fwdZ!
        IF cz(i) > .1! THEN
            sx(i) = centerX + INT(cx(i) / cz(i) * fov!)
            sy(i) = centerY - INT(cy(i) / cz(i) * fov!)
        END IF
    NEXT i
    
    FOR f = 0 TO numFaces - 1
        v0 = fv(f, 0): v1 = fv(f, 1): v2 = fv(f, 2): v3 = fv(f, 3)
        IF v3 >= 0 THEN
            fz(f) = (cz(v0) + cz(v1) + cz(v2) + cz(v3)) / 4!
        ELSE
            fz(f) = (cz(v0) + cz(v1) + cz(v2)) / 3!
        END IF
        fo(f) = f
    NEXT f
    
    FOR i = 0 TO numFaces - 2
        FOR j = i + 1 TO numFaces - 1
            IF fz(fo(i)) < fz(fo(j)) THEN SWAP fo(i), fo(j)
        NEXT j
    NEXT i
    
    FOR i = 0 TO numFaces - 1
        f = fo(i)
        v0 = fv(f, 0): v1 = fv(f, 1): v2 = fv(f, 2): v3 = fv(f, 3)
        
        IF v3 >= 0 THEN
            IF cz(v0) < .1! OR cz(v1) < .1! OR cz(v2) < .1! OR cz(v3) < .1! THEN GOTO SkipFace
        ELSE
            IF cz(v0) < .1! OR cz(v1) < .1! OR cz(v2) < .1! THEN GOTO SkipFace
        END IF
        
        ux = sx(v1) - sx(v0): uy = sy(v1) - sy(v0)
        wx = sx(v2) - sx(v0): wy = sy(v2) - sy(v0)
        cross& = CLNG(ux) * wy - CLNG(uy) * wx
        IF cross& >= 0 THEN GOTO SkipFace
        
        e1x! = vx(v1) - vx(v0): e1y! = vy(v1) - vy(v0): e1z! = vz(v1) - vz(v0)
        e2x! = vx(v2) - vx(v0): e2y! = vy(v2) - vy(v0): e2z! = vz(v2) - vz(v0)
        nx! = e1y! * e2z! - e1z! * e2y!
        ny! = e1z! * e2x! - e1x! * e2z!
        nz! = e1x! * e2y! - e1y! * e2x!
        nl! = SQR(nx! * nx! + ny! * ny! + nz! * nz!)
        IF nl! > .001! THEN nx! = nx! / nl!: ny! = ny! / nl!: nz! = nz! / nl!
        
        diff! = -(nx! * lx! + ny! * ly! + nz! * lz!)
        IF diff! < .15! THEN diff! = .15!
        IF diff! > 1! THEN diff! = 1!
        
        bc = fc(f)
        IF diff! < .4! THEN
            col = bc - 8: IF col < 0 THEN col = 0
        ELSEIF diff! > .7! THEN
            col = bc: IF bc < 8 THEN col = bc + 8
        ELSE
            col = bc
        END IF
        
        ' Draw triangle(s) with scanline fill
        x0 = sx(v0): y0 = sy(v0)
        x1 = sx(v1): y1 = sy(v1)
        x2 = sx(v2): y2 = sy(v2)
        ' Get depths for triangle verts
        tz0! = cz(v0): tz1! = cz(v1): tz2! = cz(v2)
        GOSUB FillTriangle
        
        IF v3 >= 0 THEN
            x0 = sx(v0): y0 = sy(v0)
            x1 = sx(v2): y1 = sy(v2)
            x2 = sx(v3): y2 = sy(v3)
            tz0! = cz(v0): tz1! = cz(v2): tz2! = cz(v3)
            GOSUB FillTriangle
        END IF
SkipFace:
    NEXT i
    
    PCOPY workPage, viewPage
    SWAP workPage, viewPage
    
    k$ = INKEY$
    SELECT CASE k$
        CASE " "
            vehicle = (vehicle + 1) MOD numVehicles
            GOSUB LoadVehicle
        CASE CHR$(0) + "H": IF camOrbitX! < 1.3! THEN camOrbitX! = camOrbitX! + .1!
        CASE CHR$(0) + "P": IF camOrbitX! > -.5! THEN camOrbitX! = camOrbitX! - .1!
        CASE CHR$(0) + "K": camOrbitY! = camOrbitY! - .1!
        CASE CHR$(0) + "M": camOrbitY! = camOrbitY! + .1!
        CASE "z", "Z": IF camRadius! > 4! THEN camRadius! = camRadius! - .5!
        CASE "x", "X": IF camRadius! < 25! THEN camRadius! = camRadius! + .5!
        CASE CHR$(27): EXIT DO
    END SELECT
LOOP

SCREEN 0: END

'--- Scanline triangle filler with depth shading ---
FillTriangle:
    ' Sort vertices by Y: y0 <= y1 <= y2
    IF y0 > y1 THEN SWAP x0, x1: SWAP y0, y1: SWAP tz0!, tz1!
    IF y1 > y2 THEN SWAP x1, x2: SWAP y1, y2: SWAP tz1!, tz2!
    IF y0 > y1 THEN SWAP x0, x1: SWAP y0, y1: SWAP tz0!, tz1!
    
    yStart = y0: IF yStart < 0 THEN yStart = 0
    yEnd = y2: IF yEnd > 199 THEN yEnd = 199
    IF yStart > yEnd THEN RETURN
    
    FOR yy = yStart TO yEnd
        scanL(yy) = 32767: scanR(yy) = -32768
    NEXT yy
    
    ' Edge: v0 -> v1
    IF y1 <> y0 THEN
        dy = y1 - y0: dx = x1 - x0: dz! = tz1! - tz0!
        FOR yy = y0 TO y1
            IF yy >= 0 AND yy <= 199 THEN
                t! = (yy - y0) / dy
                xx = x0 + INT(dx * t!)
                zz! = tz0! + dz! * t!
                IF xx < scanL(yy) THEN scanL(yy) = xx: scanZL(yy) = zz!
                IF xx > scanR(yy) THEN scanR(yy) = xx: scanZR(yy) = zz!
            END IF
        NEXT yy
    END IF
    
    ' Edge: v1 -> v2
    IF y2 <> y1 THEN
        dy = y2 - y1: dx = x2 - x1: dz! = tz2! - tz1!
        FOR yy = y1 TO y2
            IF yy >= 0 AND yy <= 199 THEN
                t! = (yy - y1) / dy
                xx = x1 + INT(dx * t!)
                zz! = tz1! + dz! * t!
                IF xx < scanL(yy) THEN scanL(yy) = xx: scanZL(yy) = zz!
                IF xx > scanR(yy) THEN scanR(yy) = xx: scanZR(yy) = zz!
            END IF
        NEXT yy
    END IF
    
    ' Edge: v0 -> v2
    IF y2 <> y0 THEN
        dy = y2 - y0: dx = x2 - x0: dz! = tz2! - tz0!
        FOR yy = y0 TO y2
            IF yy >= 0 AND yy <= 199 THEN
                t! = (yy - y0) / dy
                xx = x0 + INT(dx * t!)
                zz! = tz0! + dz! * t!
                IF xx < scanL(yy) THEN scanL(yy) = xx: scanZL(yy) = zz!
                IF xx > scanR(yy) THEN scanR(yy) = xx: scanZR(yy) = zz!
            END IF
        NEXT yy
    END IF
    
    ' Fill scanlines with depth-based shading
    FOR yy = yStart TO yEnd
        lxx = scanL(yy): rxx = scanR(yy)
        IF lxx < 0 THEN lxx = 0
        IF rxx > 319 THEN rxx = 319
        IF lxx <= rxx THEN
            spanW = rxx - lxx
            zL! = scanZL(yy): zR! = scanZR(yy)
            IF spanW = 0 THEN
                ' Single pixel
                zAvg! = zL!
                shade = 15 - INT((zAvg! - minDepth!) / (maxDepth! - minDepth!) * 15!)
                IF shade < 1 THEN shade = 1
                IF shade > 15 THEN shade = 15
                PSET (lxx, yy), shade
            ELSE
                ' Interpolate Z across span
                dz! = (zR! - zL!) / spanW
                zCur! = zL!
                FOR px = lxx TO rxx
                    shade = 15 - INT((zCur! - minDepth!) / (maxDepth! - minDepth!) * 15!)
                    IF shade < 1 THEN shade = 1
                    IF shade > 15 THEN shade = 15
                    PSET (px, yy), shade
                    zCur! = zCur! + dz!
                NEXT px
            END IF
        END IF
    NEXT yy
RETURN

LoadVehicle:
SELECT CASE vehicle

CASE 0 ' STARFIGHTER
    numVerts = 16: numFaces = 14
    vx(0) = 0: vy(0) = 0: vz(0) = 3
    vx(1) = -.5: vy(1) = -.3: vz(1) = 1
    vx(2) = .5: vy(2) = -.3: vz(2) = 1
    vx(3) = .5: vy(3) = .3: vz(3) = 1
    vx(4) = -.5: vy(4) = .3: vz(4) = 1
    vx(5) = -.5: vy(5) = -.3: vz(5) = -1.5
    vx(6) = .5: vy(6) = -.3: vz(6) = -1.5
    vx(7) = .5: vy(7) = .3: vz(7) = -1.5
    vx(8) = -.5: vy(8) = .3: vz(8) = -1.5
    vx(9) = -2.5: vy(9) = 0: vz(9) = -.5
    vx(10) = -.6: vy(10) = 0: vz(10) = .5
    vx(11) = -.6: vy(11) = 0: vz(11) = -1
    vx(12) = 2.5: vy(12) = 0: vz(12) = -.5
    vx(13) = .6: vy(13) = 0: vz(13) = .5
    vx(14) = .6: vy(14) = 0: vz(14) = -1
    vx(15) = 0: vy(15) = .6: vz(15) = .5
    
    fv(0, 0) = 0: fv(0, 1) = 2: fv(0, 2) = 1: fv(0, 3) = -1: fc(0) = 7
    fv(1, 0) = 0: fv(1, 1) = 3: fv(1, 2) = 2: fv(1, 3) = -1: fc(1) = 7
    fv(2, 0) = 0: fv(2, 1) = 4: fv(2, 2) = 3: fv(2, 3) = -1: fc(2) = 15
    fv(3, 0) = 0: fv(3, 1) = 1: fv(3, 2) = 4: fv(3, 3) = -1: fc(3) = 8
    fv(4, 0) = 1: fv(4, 1) = 2: fv(4, 2) = 6: fv(4, 3) = 5: fc(4) = 8
    fv(5, 0) = 2: fv(5, 1) = 3: fv(5, 2) = 7: fv(5, 3) = 6: fc(5) = 7
    fv(6, 0) = 3: fv(6, 1) = 4: fv(6, 2) = 8: fv(6, 3) = 7: fc(6) = 15
    fv(7, 0) = 4: fv(7, 1) = 1: fv(7, 2) = 5: fv(7, 3) = 8: fc(7) = 7
    fv(8, 0) = 5: fv(8, 1) = 6: fv(8, 2) = 7: fv(8, 3) = 8: fc(8) = 4
    fv(9, 0) = 9: fv(9, 1) = 10: fv(9, 2) = 11: fv(9, 3) = -1: fc(9) = 11
    fv(10, 0) = 9: fv(10, 1) = 11: fv(10, 2) = 10: fv(10, 3) = -1: fc(10) = 3
    fv(11, 0) = 12: fv(11, 1) = 14: fv(11, 2) = 13: fv(11, 3) = -1: fc(11) = 11
    fv(12, 0) = 12: fv(12, 1) = 13: fv(12, 2) = 14: fv(12, 3) = -1: fc(12) = 3
    fv(13, 0) = 15: fv(13, 1) = 3: fv(13, 2) = 4: fv(13, 3) = -1: fc(13) = 14

CASE 1 ' SPACE CRUISER
    numVerts = 18: numFaces = 16
    vx(0) = 0: vy(0) = 0: vz(0) = 4
    vx(1) = -.8: vy(1) = -.5: vz(1) = 2
    vx(2) = .8: vy(2) = -.5: vz(2) = 2
    vx(3) = .8: vy(3) = .5: vz(3) = 2
    vx(4) = -.8: vy(4) = .5: vz(4) = 2
    vx(5) = -1: vy(5) = -.6: vz(5) = -2
    vx(6) = 1: vy(6) = -.6: vz(6) = -2
    vx(7) = 1: vy(7) = .6: vz(7) = -2
    vx(8) = -1: vy(8) = .6: vz(8) = -2
    vx(9) = -.4: vy(9) = .6: vz(9) = 1
    vx(10) = .4: vy(10) = .6: vz(10) = 1
    vx(11) = .4: vy(11) = .6: vz(11) = -.5
    vx(12) = -.4: vy(12) = .6: vz(12) = -.5
    vx(13) = 0: vy(13) = 1.1: vz(13) = .25
    vx(14) = -.7: vy(14) = 0: vz(14) = -2.8
    vx(15) = .7: vy(15) = 0: vz(15) = -2.8
    vx(16) = -.7: vy(16) = -.4: vz(16) = -2
    vx(17) = .7: vy(17) = -.4: vz(17) = -2
    
    fv(0, 0) = 0: fv(0, 1) = 2: fv(0, 2) = 1: fv(0, 3) = -1: fc(0) = 8
    fv(1, 0) = 0: fv(1, 1) = 3: fv(1, 2) = 2: fv(1, 3) = -1: fc(1) = 7
    fv(2, 0) = 0: fv(2, 1) = 4: fv(2, 2) = 3: fv(2, 3) = -1: fc(2) = 15
    fv(3, 0) = 0: fv(3, 1) = 1: fv(3, 2) = 4: fv(3, 3) = -1: fc(3) = 7
    fv(4, 0) = 1: fv(4, 1) = 2: fv(4, 2) = 6: fv(4, 3) = 5: fc(4) = 8
    fv(5, 0) = 2: fv(5, 1) = 3: fv(5, 2) = 7: fv(5, 3) = 6: fc(5) = 7
    fv(6, 0) = 3: fv(6, 1) = 4: fv(6, 2) = 8: fv(6, 3) = 7: fc(6) = 15
    fv(7, 0) = 4: fv(7, 1) = 1: fv(7, 2) = 5: fv(7, 3) = 8: fc(7) = 7
    fv(8, 0) = 5: fv(8, 1) = 6: fv(8, 2) = 7: fv(8, 3) = 8: fc(8) = 8
    fv(9, 0) = 9: fv(9, 1) = 10: fv(9, 2) = 13: fv(9, 3) = -1: fc(9) = 14
    fv(10, 0) = 10: fv(10, 1) = 11: fv(10, 2) = 13: fv(10, 3) = -1: fc(10) = 6
    fv(11, 0) = 11: fv(11, 1) = 12: fv(11, 2) = 13: fv(11, 3) = -1: fc(11) = 14
    fv(12, 0) = 12: fv(12, 1) = 9: fv(12, 2) = 13: fv(12, 3) = -1: fc(12) = 6
    fv(13, 0) = 5: fv(13, 1) = 16: fv(13, 2) = 14: fv(13, 3) = -1: fc(13) = 4
    fv(14, 0) = 6: fv(14, 1) = 15: fv(14, 2) = 17: fv(14, 3) = -1: fc(14) = 4
    fv(15, 0) = 14: fv(15, 1) = 16: fv(15, 2) = 17: fv(15, 3) = 15: fc(15) = 12

CASE 2 ' RACE CAR
    numVerts = 20: numFaces = 16
    vx(0) = -.7: vy(0) = 0: vz(0) = 2
    vx(1) = .7: vy(1) = 0: vz(1) = 2
    vx(2) = .9: vy(2) = 0: vz(2) = 0
    vx(3) = .9: vy(3) = 0: vz(3) = -1.5
    vx(4) = .5: vy(4) = 0: vz(4) = -2.5
    vx(5) = -.5: vy(5) = 0: vz(5) = -2.5
    vx(6) = -.9: vy(6) = 0: vz(6) = -1.5
    vx(7) = -.9: vy(7) = 0: vz(7) = 0
    vx(8) = -.5: vy(8) = .3: vz(8) = 1.5
    vx(9) = .5: vy(9) = .3: vz(9) = 1.5
    vx(10) = .5: vy(10) = .6: vz(10) = .3
    vx(11) = -.5: vy(11) = .6: vz(11) = .3
    vx(12) = .5: vy(12) = .6: vz(12) = -.8
    vx(13) = -.5: vy(13) = .6: vz(13) = -.8
    vx(14) = .5: vy(14) = .3: vz(14) = -1.2
    vx(15) = -.5: vy(15) = .3: vz(15) = -1.2
    vx(16) = -1: vy(16) = .9: vz(16) = -2.2
    vx(17) = 1: vy(17) = .9: vz(17) = -2.2
    vx(18) = 1: vy(18) = .9: vz(18) = -2.6
    vx(19) = -1: vy(19) = .9: vz(19) = -2.6
    
    fv(0, 0) = 0: fv(0, 1) = 8: fv(0, 2) = 9: fv(0, 3) = 1: fc(0) = 12
    fv(1, 0) = 8: fv(1, 1) = 11: fv(1, 2) = 10: fv(1, 3) = 9: fc(1) = 14
    fv(2, 0) = 11: fv(2, 1) = 13: fv(2, 2) = 12: fv(2, 3) = 10: fc(2) = 9
    fv(3, 0) = 13: fv(3, 1) = 15: fv(3, 2) = 14: fv(3, 3) = 12: fc(3) = 14
    fv(4, 0) = 15: fv(4, 1) = 6: fv(4, 2) = 3: fv(4, 3) = 14: fc(4) = 12
    fv(5, 0) = 1: fv(5, 1) = 9: fv(5, 2) = 10: fv(5, 3) = 2: fc(5) = 4
    fv(6, 0) = 2: fv(6, 1) = 10: fv(6, 2) = 12: fv(6, 3) = 3: fc(6) = 4
    fv(7, 0) = 3: fv(7, 1) = 12: fv(7, 2) = 14: fv(7, 3) = -1: fc(7) = 4
    fv(8, 0) = 0: fv(8, 1) = 7: fv(8, 2) = 11: fv(8, 3) = 8: fc(8) = 4
    fv(9, 0) = 7: fv(9, 1) = 6: fv(9, 2) = 13: fv(9, 3) = 11: fc(9) = 4
    fv(10, 0) = 6: fv(10, 1) = 15: fv(10, 2) = 13: fv(10, 3) = -1: fc(10) = 4
    fv(11, 0) = 4: fv(11, 1) = 5: fv(11, 2) = 6: fv(11, 3) = 3: fc(11) = 8
    fv(12, 0) = 16: fv(12, 1) = 17: fv(12, 2) = 18: fv(12, 3) = 19: fc(12) = 15
    fv(13, 0) = 19: fv(13, 1) = 18: fv(13, 2) = 17: fv(13, 3) = 16: fc(13) = 7
    fv(14, 0) = 5: fv(14, 1) = 16: fv(14, 2) = 19: fv(14, 3) = -1: fc(14) = 8
    fv(15, 0) = 4: fv(15, 1) = 18: fv(15, 2) = 17: fv(15, 3) = -1: fc(15) = 8

CASE 3 ' HELICOPTER
    numVerts = 20: numFaces = 18
    vx(0) = 0: vy(0) = 0: vz(0) = 2.5
    vx(1) = -.6: vy(1) = -.3: vz(1) = 1
    vx(2) = .6: vy(2) = -.3: vz(2) = 1
    vx(3) = .6: vy(3) = .4: vz(3) = 1
    vx(4) = -.6: vy(4) = .4: vz(4) = 1
    vx(5) = -.6: vy(5) = -.3: vz(5) = -.8
    vx(6) = .6: vy(6) = -.3: vz(6) = -.8
    vx(7) = .6: vy(7) = .4: vz(7) = -.8
    vx(8) = -.6: vy(8) = .4: vz(8) = -.8
    vx(9) = -.2: vy(9) = 0: vz(9) = -3
    vx(10) = .2: vy(10) = 0: vz(10) = -3
    vx(11) = .2: vy(11) = .3: vz(11) = -3
    vx(12) = -.2: vy(12) = .3: vz(12) = -3
    vx(13) = 0: vy(13) = 1: vz(13) = -3
    vx(14) = 0: vy(14) = .7: vz(14) = .1
    vx(15) = 0: vy(15) = .5: vz(15) = .1
    vx(16) = -.7: vy(16) = -.6: vz(16) = 1.2
    vx(17) = -.7: vy(17) = -.6: vz(17) = -.8
    vx(18) = .7: vy(18) = -.6: vz(18) = 1.2
    vx(19) = .7: vy(19) = -.6: vz(19) = -.8
    
    fv(0, 0) = 0: fv(0, 1) = 2: fv(0, 2) = 1: fv(0, 3) = -1: fc(0) = 8
    fv(1, 0) = 0: fv(1, 1) = 3: fv(1, 2) = 2: fv(1, 3) = -1: fc(1) = 2
    fv(2, 0) = 0: fv(2, 1) = 4: fv(2, 2) = 3: fv(2, 3) = -1: fc(2) = 10
    fv(3, 0) = 0: fv(3, 1) = 1: fv(3, 2) = 4: fv(3, 3) = -1: fc(3) = 2
    fv(4, 0) = 1: fv(4, 1) = 2: fv(4, 2) = 6: fv(4, 3) = 5: fc(4) = 8
    fv(5, 0) = 2: fv(5, 1) = 3: fv(5, 2) = 7: fv(5, 3) = 6: fc(5) = 2
    fv(6, 0) = 3: fv(6, 1) = 4: fv(6, 2) = 8: fv(6, 3) = 7: fc(6) = 10
    fv(7, 0) = 4: fv(7, 1) = 1: fv(7, 2) = 5: fv(7, 3) = 8: fc(7) = 2
    fv(8, 0) = 5: fv(8, 1) = 6: fv(8, 2) = 10: fv(8, 3) = 9: fc(8) = 8
    fv(9, 0) = 6: fv(9, 1) = 7: fv(9, 2) = 11: fv(9, 3) = 10: fc(9) = 10
    fv(10, 0) = 7: fv(10, 1) = 8: fv(10, 2) = 12: fv(10, 3) = 11: fc(10) = 10
    fv(11, 0) = 8: fv(11, 1) = 5: fv(11, 2) = 9: fv(11, 3) = 12: fc(11) = 2
    fv(12, 0) = 11: fv(12, 1) = 12: fv(12, 2) = 13: fv(12, 3) = -1: fc(12) = 12
    fv(13, 0) = 12: fv(13, 1) = 11: fv(13, 2) = 13: fv(13, 3) = -1: fc(13) = 4
    fv(14, 0) = 3: fv(14, 1) = 15: fv(14, 2) = 14: fv(14, 3) = -1: fc(14) = 7
    fv(15, 0) = 4: fv(15, 1) = 14: fv(15, 2) = 15: fv(15, 3) = -1: fc(15) = 7
    fv(16, 0) = 16: fv(16, 1) = 17: fv(16, 2) = 5: fv(16, 3) = 1: fc(16) = 8
    fv(17, 0) = 18: fv(17, 1) = 2: fv(17, 2) = 6: fv(17, 3) = 19: fc(17) = 8

END SELECT
RETURN