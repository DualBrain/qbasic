' Smooth rotating wireframe cube
' SCREEN 7 + page flipping

DEFINT A-Z
CONST PI! = 3.14159265!

SCREEN 7

' Use two video pages
workPage = 1
viewPage = 0
SCREEN , , workPage, viewPage

DIM vx(7) AS SINGLE, vy(7) AS SINGLE, vz(7) AS SINGLE
DIM ex1(11) AS INTEGER, ex2(11) AS INTEGER
DIM sx(7) AS INTEGER, sy(7) AS INTEGER

' Cube vertices
vx(0) = -1: vy(0) = -1: vz(0) = -1
vx(1) =  1: vy(1) = -1: vz(1) = -1
vx(2) =  1: vy(2) =  1: vz(2) = -1
vx(3) = -1: vy(3) =  1: vz(3) = -1
vx(4) = -1: vy(4) = -1: vz(4) =  1
vx(5) =  1: vy(5) = -1: vz(5) =  1
vx(6) =  1: vy(6) =  1: vz(6) =  1
vx(7) = -1: vy(7) =  1: vz(7) =  1

' Edges
DATA 0,1, 1,2, 2,3, 3,0
DATA 4,5, 5,6, 6,7, 7,4
DATA 0,4, 1,5, 2,6, 3,7
FOR i = 0 TO 11
    READ ex1(i), ex2(i)
NEXT i

centerX = 160
centerY = 100
scale! = 70!
camDist! = 4.0!

angX! = 0!
angY! = 0!

DO
    ' draw on hidden page
    SCREEN , , workPage, viewPage

    ' clear page by overwriting background
    LINE (0, 0)-(319, 199), 0, BF

    cx! = COS(angX!): sx! = SIN(angX!)
    cy! = COS(angY!): sy! = SIN(angY!)

    FOR i = 0 TO 7
        x! = vx(i)
        y! = vy(i)
        z! = vz(i)

        ' rotate Y
        x1! = x! * cy! + z! * sy!
        z1! = -x! * sy! + z! * cy!
        y1! = y!

        ' rotate X
        y2! = y1! * cx! - z1! * sx!
        z2! = y1! * sx! + z1! * cx!
        x2! = x1!

        ' perspective projection
        px! = x2! / (z2! + camDist!)
        py! = y2! / (z2! + camDist!)

        sx(i) = centerX + INT(px! * scale!)
        sy(i) = centerY - INT(py! * scale!)
    NEXT i

    ' draw cube edges
    col = 15
    FOR e = 0 TO 11
        a = ex1(e): b = ex2(e)
        LINE (sx(a), sy(a))-(sx(b), sy(b)), col
    NEXT e

    ' flip pages
    PCOPY workPage, viewPage
    SWAP workPage, viewPage

    angX! = angX! + .03!
    angY! = angY! + .02!

LOOP WHILE INKEY$ = ""

SCREEN 0
END
