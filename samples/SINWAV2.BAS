SCREEN 9

CONST visiblePage = 0
CONST drawPage = 1

amplitude = 80
frequency = 15
centerY = 100
speed = 2

phase = 0

DO
    ' draw to hidden page
    SCREEN 7, , drawPage, visiblePage
    CLS

    oldx = 0
    oldy = centerY + amplitude * SIN((oldx + phase) / frequency)

    FOR x = 1 TO 319
        y = centerY + amplitude * SIN((x + phase) / frequency)
        LINE (oldx, oldy)-(x, y), 10
        oldx = x
        oldy = y
    NEXT

    ' overlay knob values
    LOCATE 1, 1
    COLOR 15
    PRINT "A/Z Amp:"; amplitude;
    PRINT "  S/X Freq:"; frequency;
    PRINT "  D/C Speed:"; speed;
    PRINT "  F/V CenterY:"; centerY;

    ' flip pages
    PCOPY drawPage, visiblePage

    ' animate
    phase = phase + speed

    ' keyboard knobs
    k$ = INKEY$
    SELECT CASE UCASE$(k$)
        CASE "A": amplitude = amplitude + 2
        CASE "Z": amplitude = amplitude - 2
        CASE "S": frequency = frequency + 1
        CASE "X": IF frequency > 1 THEN frequency = frequency - 1
        CASE "D": speed = speed + 1
        CASE "C": IF speed > 0 THEN speed = speed - 1
        CASE "F": centerY = centerY - 2
        CASE "V": centerY = centerY + 2
        CASE CHR$(27): EXIT DO
    END SELECT

    ' clamp values (keep it sane)
    IF amplitude < 1 THEN amplitude = 1
    IF amplitude > 120 THEN amplitude = 120
    IF centerY < 0 THEN centerY = 0
    IF centerY > 199 THEN centerY = 199

    ' frame delay
    FOR d = 1 TO 200: NEXT

LOOP
