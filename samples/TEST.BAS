' ============================================
' GET/PUT Unit Test for All Screen Modes
' Tests: GET, PUT, LONG/INTEGER/SINGLE arrays
' ============================================

DEFINT A-Z

CLS
PRINT "GET/PUT Unit Test Suite"
PRINT "======================"
PRINT
PRINT "This test will cycle through screen modes"
PRINT "and verify GET/PUT with different array types."
PRINT
PRINT "Press any key to start..."
DO: LOOP WHILE INKEY$ = ""

' Test each screen mode
testsPassed = 0
testsFailed = 0

' ---- SCREEN 13 (320x200, 256 colors) ----
SCREEN 13
GOSUB TestScreen13
IF result = 1 THEN testsPassed = testsPassed + 1 ELSE testsFailed = testsFailed + 1

' ---- SCREEN 9 (640x350, 16 colors) ----
SCREEN 9
GOSUB TestScreen9
IF result = 1 THEN testsPassed = testsPassed + 1 ELSE testsFailed = testsFailed + 1

' ---- SCREEN 8 (640x200, 16 colors) ----
SCREEN 8
GOSUB TestScreen8
IF result = 1 THEN testsPassed = testsPassed + 1 ELSE testsFailed = testsFailed + 1

' ---- SCREEN 7 (320x200, 16 colors) ----
SCREEN 7
GOSUB TestScreen7
IF result = 1 THEN testsPassed = testsPassed + 1 ELSE testsFailed = testsFailed + 1

' ---- SCREEN 12 (640x480, 16 colors) ----
SCREEN 12
GOSUB TestScreen12
IF result = 1 THEN testsPassed = testsPassed + 1 ELSE testsFailed = testsFailed + 1

' ---- Test LONG arrays (like GORILLAS) ----
SCREEN 9
GOSUB TestLongArrays
IF result = 1 THEN testsPassed = testsPassed + 1 ELSE testsFailed = testsFailed + 1

' ---- Show results ----
SCREEN 0
WIDTH 80, 25
CLS
PRINT
PRINT "========================================"
PRINT "         TEST RESULTS"
PRINT "========================================"
PRINT
PRINT "Tests Passed: "; testsPassed
PRINT "Tests Failed: "; testsFailed
PRINT
IF testsFailed = 0 THEN
    PRINT "*** ALL TESTS PASSED! ***"
ELSE
    PRINT "*** SOME TESTS FAILED ***"
END IF
PRINT
PRINT "Press any key to exit..."
DO: LOOP WHILE INKEY$ = ""
END

' ============================================
' SCREEN 13 Test (256 colors, linear buffer)
' ============================================
TestScreen13:
    CLS
    LOCATE 1, 1: PRINT "SCREEN 13 Test (INTEGER array)"
    
    ' Draw a colorful box
    FOR y = 10 TO 30
        FOR x = 10 TO 40
            PSET (x, y), (x + y) MOD 256
        NEXT x
    NEXT y
    
    ' Capture with INTEGER array
    DIM s13%(2000)
    GET (10, 10)-(40, 30), s13%
    
    ' Draw at new location
    PUT (100, 50), s13%, PSET
    
    ' Verify by checking a pixel
    c1 = POINT(15, 15)
    c2 = POINT(105, 55)
    
    IF c1 = c2 THEN
        LOCATE 24, 1: PRINT "SCREEN 13: PASS"
        result = 1
    ELSE
        LOCATE 24, 1: PRINT "SCREEN 13: FAIL ("; c1; " vs "; c2; ")"
        result = 0
    END IF
    
    ERASE s13%
    GOSUB WaitKey
RETURN

' ============================================
' SCREEN 9 Test (EGA 640x350, 16 colors)
' ============================================
TestScreen9:
    CLS
    LOCATE 1, 1: PRINT "SCREEN 9 Test (INTEGER array)"
    
    ' Draw a simple pattern
    FOR y = 20 TO 50
        FOR x = 20 TO 60
            PSET (x, y), (x + y) MOD 16
        NEXT x
    NEXT y
    
    ' Capture with INTEGER array
    DIM s9%(500)
    GET (20, 20)-(60, 50), s9%
    
    ' Draw at new location
    PUT (200, 100), s9%, PSET
    
    ' Verify
    c1 = POINT(30, 30)
    c2 = POINT(210, 110)
    
    IF c1 = c2 THEN
        LOCATE 24, 1: PRINT "SCREEN 9: PASS"
        result = 1
    ELSE
        LOCATE 24, 1: PRINT "SCREEN 9: FAIL ("; c1; " vs "; c2; ")"
        result = 0
    END IF
    
    ERASE s9%
    GOSUB WaitKey
RETURN

' ============================================
' SCREEN 8 Test (EGA 640x200, 16 colors)
' ============================================
TestScreen8:
    CLS
    LOCATE 1, 1: PRINT "SCREEN 8 Test (SINGLE array)"
    
    ' Draw pattern
    FOR y = 15 TO 35
        FOR x = 15 TO 45
            PSET (x, y), (x + y) MOD 16
        NEXT x
    NEXT y
    
    ' Capture with SINGLE array (4 bytes per element)
    DIM s8!(300)
    GET (15, 15)-(45, 35), s8!
    
    ' Draw at new location
    PUT (200, 80), s8!, PSET
    
    ' Verify
    c1 = POINT(25, 25)
    c2 = POINT(210, 90)
    
    IF c1 = c2 THEN
        LOCATE 24, 1: PRINT "SCREEN 8: PASS"
        result = 1
    ELSE
        LOCATE 24, 1: PRINT "SCREEN 8: FAIL ("; c1; " vs "; c2; ")"
        result = 0
    END IF
    
    ERASE s8!
    GOSUB WaitKey
RETURN

' ============================================
' SCREEN 7 Test (320x200, 16 colors)
' ============================================
TestScreen7:
    CLS
    LOCATE 1, 1: PRINT "SCREEN 7 Test (INTEGER array)"
    
    ' Draw pattern
    FOR y = 10 TO 25
        FOR x = 10 TO 30
            PSET (x, y), (x + y) MOD 16
        NEXT x
    NEXT y
    
    ' Capture
    DIM s7%(200)
    GET (10, 10)-(30, 25), s7%
    
    ' Draw at new location
    PUT (100, 60), s7%, PSET
    
    ' Verify
    c1 = POINT(15, 15)
    c2 = POINT(105, 65)
    
    IF c1 = c2 THEN
        LOCATE 24, 1: PRINT "SCREEN 7: PASS"
        result = 1
    ELSE
        LOCATE 24, 1: PRINT "SCREEN 7: FAIL ("; c1; " vs "; c2; ")"
        result = 0
    END IF
    
    ERASE s7%
    GOSUB WaitKey
RETURN

' ============================================
' SCREEN 12 Test (640x480, 16 colors)
' ============================================
TestScreen12:
    CLS
    LOCATE 1, 1: PRINT "SCREEN 12 Test (INTEGER array)"
    
    ' Draw pattern
    FOR y = 30 TO 60
        FOR x = 30 TO 70
            PSET (x, y), (x + y) MOD 16
        NEXT x
    NEXT y
    
    ' Capture
    DIM s12%(600)
    GET (30, 30)-(70, 60), s12%
    
    ' Draw at new location
    PUT (250, 150), s12%, PSET
    
    ' Verify
    c1 = POINT(40, 40)
    c2 = POINT(260, 160)
    
    IF c1 = c2 THEN
        LOCATE 29, 1: PRINT "SCREEN 12: PASS"
        result = 1
    ELSE
        LOCATE 29, 1: PRINT "SCREEN 12: FAIL ("; c1; " vs "; c2; ")"
        result = 0
    END IF
    
    ERASE s12%
    GOSUB WaitKey
RETURN

' ============================================
' LONG Array Test (like GORILLAS.BAS uses)
' ============================================
TestLongArrays:
    CLS
    LOCATE 1, 1: PRINT "LONG Array Test (SCREEN 9)"
    
    ' Draw a small sprite
    FOR y = 50 TO 65
        FOR x = 50 TO 65
            PSET (x, y), ((x - 50) + (y - 50)) MOD 16
        NEXT x
    NEXT y
    
    ' Capture with LONG array (4 bytes per element, signed 32-bit)
    DIM sLong&(100)
    GET (50, 50)-(65, 65), sLong&
    
    ' Draw at multiple locations with different PUT modes
    PUT (150, 50), sLong&, PSET
    PUT (250, 50), sLong&, XOR
    PUT (350, 50), sLong&, OR
    
    ' Verify PSET copy matches original
    c1 = POINT(55, 55)
    c2 = POINT(155, 55)
    
    IF c1 = c2 THEN
        LOCATE 24, 1: PRINT "LONG Array: PASS"
        result = 1
    ELSE
        LOCATE 24, 1: PRINT "LONG Array: FAIL ("; c1; " vs "; c2; ")"
        result = 0
    END IF
    
    ERASE sLong&
    GOSUB WaitKey
RETURN

' ============================================
' Wait for keypress
' ============================================
WaitKey:
    LOCATE 25, 1: PRINT "Press any key for next test...";
    DO: LOOP WHILE INKEY$ = ""
RETURN
