'Oval Racer
'Version 2.0
'Programmed by Young Timberwolf and Timberwolf
'1996/2022
'
DECLARE SUB PrintAt (ln%, col%, c%, text$)
DECLARE SUB DrawTrack ()
DECLARE SUB PrintLapDisplay (lap%, lastLapTime!, bestLapTime!)
DECLARE SUB DoCrashScreen (x%, y%)
DECLARE SUB PopulateTrigTables ()
DECLARE SUB SetCarVertices ()
DECLARE SUB DrawWheel (x%, y%, side%, s%, c%, ws%, wc%)
DECLARE SUB DrawTriangle (x1%, y1%, x2%, y2%, x3%, y3%, c%)
DECLARE SUB DrawCar (x%, y%, angle%, steer%)
DECLARE SUB ScanEndingEdge (x1%, y1%, x2%, y2%)
DECLARE SUB ScanStartingEdge (x1%, y1%, x2%, y2%)
DECLARE SUB ScanEdge (x1%, y1%, x2%, y2%, setStart%)

' Working edge arrays
DIM SHARED workingEdgeStart%(350), workingEdgeEnd%(350)

' Define car
DIM SHARED ox%(20), oy%(20), wx%(6), wy%(6)
SetCarVertices

' Set up sin/cos tables
DIM SHARED st%(256), ct%(256)
PopulateTrigTables

' Sprites for logo and screen clearer
DIM logo%(705), square%(2523)

SCREEN 9
LINE (0, 0)-(639, 300), 1, BF
LINE (2, 2)-(83, 33), 0, BF
LINE (4, 4)-(81, 31), 7, BF

PrintAt 2, 2, 15, "T W Games"
GET (2, 2)-(83, 33), logo%

FOR i% = 0 TO 250
        xi% = (RND * 553) + 2
        yi% = (RND * 263) + 2
        PUT (xi%, yi%), logo%, PSET
        IF INKEY$ = CHR$(27) THEN EXIT FOR
        IF i% MOD 5 = 0 THEN
                PLAY "T255MLL64N" + STR$(50 - (i% / 5))
        END IF
NEXT i%

PrintAt 22, 35, 15, "Timberwolf"
PrintAt 23, 32, 15, "Written for fun"

PrintAt 10, 12, 12, "  ŪŪŪ  Ū   Ū  ŪŪŪ  Ū        ŪŪŪŪ   ŪŪŪ   ŪŪŪ  ŪŪŪŪ ŪŪŪŪ  "
PrintAt 11, 12, 12, " Ū   Ū Ū   Ū Ū   Ū Ū        Ū   Ū Ū   Ū Ū   Ū Ū    Ū   Ū "
PrintAt 12, 12, 12, " Ū   Ū Ū   Ū Ū   Ū Ū        ŪŪŪŪ  Ū   Ū Ū     ŪŪŪ  ŪŪŪŪ  "
PrintAt 13, 12, 12, " Ū   Ū Ū   Ū ŪŪŪŪŪ Ū        Ū  Ū  ŪŪŪŪŪ Ū   Ū Ū    Ū  Ū  "
PrintAt 14, 12, 12, "  ŪŪŪ   ŪŪŪ  Ū   Ū ŪŪŪŪŪ    Ū   Ū Ū   Ū  ŪŪŪ  ŪŪŪŪ Ū   Ū "
PLAY "T255MLL64N50N49n48n47n46n45n44n43n42n41n40n39n38n37n36n35n34n33n32n31n30n29n28n27n26n25n24n23n22n21n20n19n18n17n16n15n14n13n12n11n10n9n8n7n6n5n4n3n2n1N50O3MNT128"

WHILE INKEY$ = "": WEND

SCREEN 9, 0, 1, 0
DrawTrack

DIM xv%, yv%, angle%, v%, x%, y%
DIM sx1%, sx2%, sy1%, sy2%
DIM checkpoint%, quit%, hasCrashed%, hasFinished%
DIM startTime!, currentLapStartTime!, bestLapTime!, lastLapTime!, endTime!

x% = 2600: y% = 600: lap% = 1

bestLapTime! = 999.99
lastLapTime! = bestLapTime!
startTime! = TIMER
currentLapStartTime! = startTime!

PrintLapDisplay lap%, lastLapTime!, bestLapTime!

WHILE quit% = 0 AND hasFinished% = 0 AND hasCrashed% = 0
        ' Input and physics
        steer% = 0

        DO
            ik$ = INKEY$
            SELECT CASE ik$
            CASE CHR$(27)       ' Escape
                    quit% = 1
            CASE CHR$(0) + "H"  ' Up
                    IF v% < 75 THEN v% = v% + 5
            CASE CHR$(0) + "P"  ' Down
                    IF v% > -25 THEN v% = v% - 5
            CASE CHR$(0) + "K"  ' Left
                    IF v% > 0 THEN angle% = angle% - 4 ELSE IF v% < 0 THEN angle% = angle% + 4
                    steer% = -1
            CASE CHR$(0) + "M"  ' Right
                    IF v% > 0 THEN angle% = angle% + 4 ELSE IF v% < 0 THEN angle% = angle% - 4
                    steer% = 1
            END SELECT
        LOOP UNTIL ik$ = ""

        angle% = (angle% + 256) MOD 256
        xv% = (v% * ct%(angle%)) / 100
        yv% = (v% * st%(angle%)) / 100
        x% = x% + xv%
        y% = y% + yv%
       
        ' Check for crash positions (outer and inner)
        IF x% < 100 OR y% < 100 OR x% > 6200 OR y% > 3400 OR (x% > 1100 AND x% < 5200 AND y% > 1100 AND y% < 2400) THEN hasCrashed% = 1

        ' Check for checkpoints
        IF x% > 5200 AND y% > 1500 AND y% < 2000 AND checkpoint% = 0 THEN checkpoint% = 1
        IF y% > 2400 AND x% > 2700 AND x% < 3200 AND checkpoint% = 1 THEN checkpoint% = 2
        IF x% < 1100 AND y% > 1500 AND y% < 2000 AND checkpoint% = 2 THEN checkpoint% = 3
       
        ' S/F line
        IF y% < 1100 AND x% > 2700 AND x% < 3200 AND checkpoint% = 3 THEN
            checkpoint% = 0
            lastLapTime! = TIMER - currentLapStartTime!
            IF lastLapTime! < bestLapTime! THEN bestLapTime! = lastLapTime!
           
            IF lap% = 10 THEN
                hasFinished% = 1
                endTime! = TIMER
            ELSE
                lap% = lap% + 1
            END IF
      
            PrintLapDisplay lap%, lastLapTime!, bestLapTime!
            currentLapStartTime! = TIMER
        END IF

        sx1% = (x% \ 10) - 48
        sy1% = (y% \ 10) - 48
       
        ' Move the erase square if it escapes the screen bounds
        IF sx1% < 0 THEN sx1% = 0
        IF sy1% < 0 THEN sy1% = 0
        IF sx1% > 543 THEN sx1% = 543
        IF sy1% > 253 THEN sy1% = 253
       
        sx2% = sx1% + 96
        sy2% = sy1% + 96
        GET (sx1%, sy1%)-(sx2%, sy2%), square%

        DrawCar x% \ 10, y% \ 10, angle%, steer%
        SOUND 45 + ABS(v% * 2), .2
        WAIT &H3DA, 8
        PCOPY 1, 0

        PUT (sx1%, sy1%), square%, PSET
WEND

IF hasCrashed% = 1 THEN DoCrashScreen x% \ 10, y% \ 10

IF hasFinished% = 1 THEN
    PLAY "L8GN0L16DD#DN0L8ED"
    DrawTrack
   
    FOR i% = 0 TO 5
        DrawCar 60 + (i% * 100), 60, i% * (256 \ 6), 0
        DrawCar 60 + (i% * 100), 290, ((i% + 2) * (256 \ 6)) MOD 256, 0
    NEXT i%
   
    PrintAt 11, 20, 15, "                                        "
    PrintAt 12, 20, 14, "        *** CONGRATULATIONS! ***        "
    PrintAt 13, 20, 15, "                                        "
    PrintAt 14, 20, 15, " You finished 10 laps in "
    PRINT USING "###.##"; endTime! - startTime!;
    PRINT " seconds "
    PrintAt 15, 20, 15, "    Your best lap was "
    PRINT USING "##.##"; bestLapTime!;
    PRINT " seconds     "
    PrintAt 16, 20, 15, "                                        "
 
    PCOPY 1, 0
    PLAY "N0F#G"
END IF

SUB DoCrashScreen (x%, y%)
    SCREEN 9, 0, 0, 0

    FOR l% = 0 TO 100
        FOR a% = 0 TO 256 STEP 2
            LINE (x%, y%)-(x% + ((l% * ct%(a%)) \ 100), y% + ((l% * (st%(a%)) \ 100))), 4
            LINE (x%, y%)-(x% + ((l% * ct%(a%)) \ 150), y% + ((l% * (st%(a%)) \ 150))), 12
            LINE (x%, y%)-(x% + ((l% * ct%(a%)) \ 200), y% + ((l% * (st%(a%)) \ 200))), 14
            LINE (x%, y%)-(x% + ((l% * ct%(a%)) \ 300), y% + ((l% * (st%(a%)) \ 300))), 15
        NEXT a%
        SOUND (350 - (l% * 3)) + (RND * 20), .4
       
        ' Wait 2 vertical retraces
        WAIT &H3DA, 8
        WAIT &H3DA, 8
    NEXT l%

    SOUND 40, 10

    PrintAt 10, 6, 15, "                                                                    "
    PrintAt 11, 6, 10, " Ū   Ū  ŪŪŪ  Ū   Ū      ŪŪŪ  ŪŪŪŪ   ŪŪŪ   ŪŪŪŪ Ū   Ū ŪŪŪŪ ŪŪŪŪ  Ū Ū "
    PrintAt 12, 6, 11, "  Ū Ū  Ū   Ū Ū   Ū     Ū   Ū Ū   Ū Ū   Ū Ū     Ū   Ū Ū    Ū   Ū Ū Ū "
    PrintAt 13, 6, 12, "   Ū   Ū   Ū Ū   Ū     Ū     ŪŪŪŪ  Ū   Ū  ŪŪŪ  ŪŪŪŪŪ ŪŪŪ  Ū   Ū Ū Ū "
    PrintAt 14, 6, 13, "   Ū   Ū   Ū Ū   Ū     Ū   Ū Ū  Ū  ŪŪŪŪŪ     Ū Ū   Ū Ū    Ū   Ū     "
    PrintAt 15, 6, 14, "   Ū    ŪŪŪ   ŪŪŪ       ŪŪŪ  Ū   Ū Ū   Ū ŪŪŪŪ  Ū   Ū ŪŪŪŪ ŪŪŪŪ  Ū Ū "
    PrintAt 16, 6, 15, "                                                                    "
END SUB

SUB DrawCar (x%, y%, angle%, steer%)
    DIM vx%(20)
    DIM vy%(20)
    DIM ws%, wc%

    s% = st%(angle%)
    c% = ct%(angle%)
    SELECT CASE steer%
        CASE -1
            ws% = st%((angle% + 256 - 32) MOD 256)
            wc% = ct%((angle% + 256 - 32) MOD 256)
        CASE 0
            ws% = s%
            wc% = c%
        CASE 1
            ws% = st%((angle% + 32) MOD 256)
            wc% = ct%((angle% + 32) MOD 256)
    END SELECT

    FOR i% = 0 TO 20
        vx%(i%) = x% + ((ox%(i%) * c%) \ 100) - ((oy%(i%) * s%) \ 100)
        vy%(i%) = y% + ((ox%(i%) * s%) \ 100) + ((oy%(i%) * c%) \ 100)
    NEXT i%

    ' Front wheels
    DrawWheel vx%(17), vy%(17), -1, s%, c%, ws%, wc%
    DrawWheel vx%(18), vy%(18), 1, s%, c%, ws%, wc%

    ' Rear wheels
    DrawWheel vx%(19), vy%(19), -1, s%, c%, s%, c%
    DrawWheel vx%(20), vy%(20), 1, s%, c%, s%, c%

    ' Side pods
    DrawTriangle (vx%(4)), (vy%(4)), (vx%(5)), (vy%(5)), (vx%(6)), (vy%(6)), 9
    DrawTriangle (vx%(5)), (vy%(5)), (vx%(7)), (vy%(7)), (vx%(6)), (vy%(6)), 9
    DrawTriangle (vx%(8)), (vy%(8)), (vx%(9)), (vy%(9)), (vx%(10)), (vy%(10)), 9
    DrawTriangle (vx%(9)), (vy%(9)), (vx%(11)), (vy%(11)), (vx%(10)), (vy%(10)), 9

    ' Body
    DrawTriangle (vx%(0)), (vy%(0)), (vx%(1)), (vy%(1)), (vx%(2)), (vy%(2)), 1
    DrawTriangle (vx%(2)), (vy%(2)), (vx%(1)), (vy%(1)), (vx%(3)), (vy%(3)), 1

    ' Nose cone
    DrawTriangle (vx%(1)), (vy%(1)), (vx%(12)), (vy%(12)), (vx%(3)), (vy%(3)), 1

    ' Rear Wing
    DrawTriangle (vx%(13)), (vy%(13)), (vx%(14)), (vy%(14)), (vx%(15)), (vy%(15)), 9
    DrawTriangle (vx%(13)), (vy%(13)), (vx%(16)), (vy%(16)), (vx%(15)), (vy%(15)), 9
END SUB

SUB DrawTrack
    LINE (0, 0)-(640, 350), 2, BF
    COLOR 12

    CIRCLE (110, 110), 20, 12
    PAINT (111, 111), 12, 12
    CIRCLE (520, 110), 20, 12
    PAINT (521, 111), 12, 12
    CIRCLE (110, 240), 20, 12
    PAINT (111, 241), 12, 12
    CIRCLE (520, 240), 20, 12
    PAINT (521, 241), 12, 12
   
    LINE (10, 10)-(620, 340), 0, B
    LINE (110, 110)-(520, 240), 0, B
    PAINT (12, 12), 8, 0
   
    LINE (300, 11)-(300, 109), 15
    LINE (298, 11)-(298, 109), 15
END SUB

SUB DrawTriangle (x1%, y1%, x2%, y2%, x3%, y3%, c%)
    ' Ensure vertices are in the correct order
    IF y2% < y1% THEN SWAP y2%, y1%: SWAP x2%, x1%
    IF y3% < y1% THEN SWAP y3%, y1%: SWAP x3%, x1%
    IF y3% < y2% THEN SWAP y3%, y2%: SWAP x3%, x2%

    ' Scan convert top triangle
    ScanStartingEdge x1%, y1%, x2%, y2%
    ScanEndingEdge x1%, y1%, x3%, y3%

    height% = y2% - y1%
    FOR i% = 0 TO height%
      y% = i% + y1%
      LINE (workingEdgeStart%(i%), y%)-(workingEdgeEnd%(i%), y%), c%
    NEXT i%

    IF y3% > y2% THEN
        ScanStartingEdge x2%, y2%, x3%, y3%
        height2% = y3% - y1%
        FOR i% = height% TO height2%
                y% = i% + y1%
                LINE (workingEdgeStart%(i% - height%), y%)-(workingEdgeEnd%(i%), y%), c%
        NEXT i%
    END IF
END SUB

SUB DrawWheel (x%, y%, side%, s%, c%, ws%, wc%)
    DIM vx%(6)
    DIM vy%(6)

    ' Suspension vertices which change according to side
    wy%(4) = (4 * side%)
    wy%(5) = (12 * side%)
    wy%(6) = (12 * side%)

    ' Unrolled loop
    vx%(0) = x% + ((wx%(0) * wc%) / 100) - ((wy%(0) * ws%) / 100)
    vy%(0) = y% + ((wx%(0) * ws%) / 100) + ((wy%(0) * wc%) / 100)
    vx%(1) = x% + ((wx%(1) * wc%) / 100) - ((wy%(1) * ws%) / 100)
    vy%(1) = y% + ((wx%(1) * ws%) / 100) + ((wy%(1) * wc%) / 100)
    vx%(2) = x% + ((wx%(2) * wc%) / 100) - ((wy%(2) * ws%) / 100)
    vy%(2) = y% + ((wx%(2) * ws%) / 100) + ((wy%(2) * wc%) / 100)
    vx%(3) = x% + ((wx%(3) * wc%) / 100) - ((wy%(3) * ws%) / 100)
    vy%(3) = y% + ((wx%(3) * ws%) / 100) + ((wy%(3) * wc%) / 100)

    vx%(4) = x% + ((wx%(4) * c%) / 100) - ((wy%(4) * s%) / 100)
    vy%(4) = y% + ((wx%(4) * s%) / 100) + ((wy%(4) * c%) / 100)
    vx%(5) = x% + ((wx%(5) * c%) / 100) - ((wy%(5) * s%) / 100)
    vy%(5) = y% + ((wx%(5) * s%) / 100) + ((wy%(5) * c%) / 100)
    vx%(6) = x% + ((wx%(6) * c%) / 100) - ((wy%(6) * s%) / 100)
    vy%(6) = y% + ((wx%(6) * s%) / 100) + ((wy%(6) * c%) / 100)

    LINE (vx%(4), vy%(4))-(vx%(5), vy%(5)), 7
    LINE (vx%(4), vy%(4))-(vx%(6), vy%(6)), 7

    DrawTriangle (vx%(0)), (vy%(0)), (vx%(1)), (vy%(1)), (vx%(2)), (vy%(2)), 0
    DrawTriangle (vx%(2)), (vy%(2)), (vx%(1)), (vy%(1)), (vx%(3)), (vy%(3)), 0

END SUB

SUB PopulateTrigTables
    FOR i% = 0 TO 256
        st%(i%) = SIN((i% * 3.141 * 2) / 256) * 100
        ct%(i%) = COS((i% * 3.141 * 2) / 256) * 100
    NEXT i%
END SUB

SUB PrintAt (ln%, col%, c%, text$)
    COLOR c%
    LOCATE ln%, col%
    PRINT text$;
END SUB

SUB PrintLapDisplay (lap%, lastLapTime!, bestLapTime!)
    PrintAt 13, 23, 15, "Lap: "
    COLOR 11: PRINT USING "##"; lap%;
    IF lastLapTime! < 100 THEN
        COLOR 15: PRINT " | Last: ";
        COLOR 10: PRINT USING "##.## "; lastLapTime!;
        COLOR 15: PRINT "Best: ";
        COLOR 10: PRINT USING "##.## "; bestLapTime!;
    ELSE
        COLOR 15: PRINT " | Last: ";
        COLOR 7: PRINT " n/a  ";
        COLOR 15: PRINT "Best: ";
        COLOR 7: PRINT " n/a  ";
    END IF
END SUB

SUB ScanEndingEdge (x1%, y1%, x2%, y2%)
        DIM x%, y%, i%, w%
        DIM deltaX%, height%
        DIM advanceAmt%, errorTerm%, errorTermAdv%
        DIM xMajorAdvAmt%

        x% = x1%
        deltaX% = x2% - x1%
        w% = ABS(deltaX%)

        IF deltaX% > 0 THEN advanceAmt% = 1 ELSE advanceAmt% = -1

        height% = y2% - y1%
        IF height% <= 0 THEN
                        workingEdgeEnd%(i%) = x%
                EXIT SUB
        END IF

        ' Case 1: vertical
        IF w% = 0 THEN
                FOR i% = 0 TO height%
                                workingEdgeEnd%(i%) = x%
                     
                NEXT i%
        ' Case 2: y-major
        ELSEIF height% > w% THEN
                IF deltaX% >= 0 THEN errorTerm% = 0 ELSE errorTerm% = 1 - height%

                FOR i% = 0 TO height%
                    workingEdgeEnd%(i%) = x%
                    errorTerm% = errorTerm% + w%
                    IF errorTerm% > 0 THEN
                        x% = x% + advanceAmt%
                        errorTerm% = errorTerm% - height%
                    END IF
                NEXT i%
        ' Case 3: x-major
        ELSE
                xMajorAdvAmt% = (w% \ height%) * advanceAmt%
                errorTermAdv% = w% MOD height%

                IF deltaX% >= 0 THEN errorTerm% = 0 ELSE errorTerm% = 1 - height%

                FOR i% = 0 TO height%
                                workingEdgeEnd%(i%) = x%
                       
                        x% = x% + xMajorAdvAmt%

                        errorTerm% = errorTerm% + errorTermAdv%
                        IF errorTerm% > 0 THEN
                                x% = x% + advanceAmt%
                                errorTerm% = errorTerm% - height%
                        END IF
                NEXT i%
        END IF
END SUB

SUB ScanStartingEdge (x1%, y1%, x2%, y2%)
        DIM x%, y%, i%, w%
        DIM deltaX%, height%
        DIM advanceAmt%, errorTerm%, errorTermAdv%
        DIM xMajorAdvAmt%

        x% = x1%
        deltaX% = x2% - x1%
        w% = ABS(deltaX%)

        IF deltaX% > 0 THEN advanceAmt% = 1 ELSE advanceAmt% = -1

        height% = y2% - y1%
        IF height% <= 0 THEN
            workingEdgeStart%(i%) = x%
            EXIT SUB
        END IF

        ' Case 1: vertical
        IF w% = 0 THEN
            FOR i% = 0 TO height%
                workingEdgeStart%(i%) = x%
            NEXT i%
        ' Case 2: y-major
        ELSEIF height% > w% THEN
            IF deltaX% >= 0 THEN errorTerm% = 0 ELSE errorTerm% = 1 - height%

            FOR i% = 0 TO height%
                workingEdgeStart%(i%) = x%

                errorTerm% = errorTerm% + w%
                IF errorTerm% > 0 THEN
                    x% = x% + advanceAmt%
                    errorTerm% = errorTerm% - height%
                END IF
            NEXT i%
        ' Case 3: x-major
        ELSE
                xMajorAdvAmt% = (w% \ height%) * advanceAmt%
                errorTermAdv% = w% MOD height%

                IF deltaX% >= 0 THEN errorTerm% = 0 ELSE errorTerm% = 1 - height%

                FOR i% = 0 TO height%
                    workingEdgeStart%(i%) = x%
                    x% = x% + xMajorAdvAmt%

                    errorTerm% = errorTerm% + errorTermAdv%
                    IF errorTerm% > 0 THEN
                        x% = x% + advanceAmt%
                        errorTerm% = errorTerm% - height%
                    END IF
                NEXT i%
        END IF
END SUB

SUB SetCarVertices
    ox%(0) = -35
    oy%(0) = -5
    ox%(1) = 15
    oy%(1) = -5
    ox%(2) = -35
    oy%(2) = 5
    ox%(3) = 15
    oy%(3) = 5
    ox%(4) = -18
    oy%(4) = -5
    ox%(5) = -10
    oy%(5) = -15
    ox%(6) = 10
    oy%(6) = -5
    ox%(7) = 2
    oy%(7) = -15
    ox%(8) = -18
    oy%(8) = 5
    ox%(9) = -10
    oy%(9) = 15
    ox%(10) = 10
    oy%(10) = 5
    ox%(11) = 2
    oy%(11) = 15
    ox%(12) = 32
    oy%(12) = 0
    ox%(13) = -44
    oy%(13) = -17
    ox%(14) = -35
    oy%(14) = -17
    ox%(15) = -35
    oy%(15) = 17
    ox%(16) = -44
    oy%(16) = 17
    ox%(17) = 19
    oy%(17) = 14
    ox%(18) = 19
    oy%(18) = -14
    ox%(19) = -25
    oy%(19) = 14
    ox%(20) = -25
    oy%(20) = -14

    ' Wheel vertices
    wx%(0) = -6
    wy%(0) = -4
    wx%(1) = 6
    wy%(1) = -4
    wx%(2) = -6
    wy%(2) = 4
    wx%(3) = 6
    wy%(3) = 4

    ' Suspension vertices which remain constant
    wx%(4) = 0
    wx%(5) = -4
    wx%(6) = 4
END SUB
