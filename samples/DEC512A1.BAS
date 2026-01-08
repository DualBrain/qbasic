50 'GOSUB 2900
60 NPCT = 200
70 M = 0
80 M = M + 1 ' Ciclu pentru introducerea datelor intr-un vector a
90 N = 2 ^ M ' carui lungime sa fie de forma 2^M.
100 If N < NPCT Then 80
110 REPS = 1 ' Lungimea mentisei calculatorului
120 REPS = REPS / 2
130 If REPS + 1 <> 1 Then 120
140 REPS = REPS * 2
150 PREC = 2 * M * REPS ' Precizia admisa pt.acumul.erori. num.
160 PI = 4 * Atn(1!)
170 M = M + 1 ' Dublam lungimea vectorului de lucru pentru
180 N = 2 ^ M ' a permite pozitionarea picului la N/2+1.
190 Dim A(N), B(N)
200 Dim M(N), V(N), W(N)
210 Dim K(N), L(N), E(M + 2)
220 ZG1 = 0 ' Initializam suma patratelor zgomotelor.
230 GoSub 2900
240 For I = 1 To NPCT ' Simulam un pic bimodal.
    250 V(I) = 900! * Exp(-(I - 105) ^ 2 / 200)
    260 V(I) = V(I) + 200! * Exp(-(I - 140) ^ 2 / 200)
    270 ZG = 2 * (Rnd - .5) * Sqr(V(I)) ' Zgomotul, sau mai mic !
    280 V(I) = V(I) + ZG
    290 ZG1 = ZG1 + ZG * ZG
300 Next I
310 GoSub 2590 'Aflam minimumul datelor.
320 For I = 1 To NPCT ' Scadem valoarea cea mai mica in
    330 V(I) = V(I) - N0 ' vederea bordarii cu 0.
340 Next I
350 ' Incercati pentru sig^2 etalon valorile 90 si 10 (acum 10).
360 For I = 1 To N ' Simulam functia de rezolutie (etalonul).
    370 A(I) = 500 * Exp(-(I - N / 2) ^ 2 / 72) ' Gaussiana (normala).
    380 'a(i) = a(i)+20/((i - n/2)^2 + 100)' Lorenziana(Cauchy).
390 Next I
400 Cls
410 Screen 10
420 'KEY OFF
430 GoSub 2590 ' Scaleaza vectorul v().
440 GoSub 2820 ' Ploteaza vectorul v().
450 GoSub 590 ' Pozitioneaza picul la pozitia N/2+1 din 2^M.
460 GoSub 910 ' Raporteaza centrul de greutate astfel obtinut.
470 If Abs(CG / (N / 2 + 1) - 1) < .001 Then 510
480 For I = 1 To N '  Daca erorile exp. sunt mari, interpolarea
    490 V(I) = M(I) ' liniara conduce la rezultate aberante. Re-
500 Next I '  facem  datele initiale (caz aproape exclus).
510 Print "POZITIA FINALA A CENTRULUI DE GREUTATE="; CG + L1 - 1
520 Locate 23, 1
530 Print "                         >>>>       APASA ORICE TASTA"
540 If InKey$ = "" Then 540
550 GoSub 2900
560 GoSub 1070 ' Deconvolutia cu transf. Fourier si regularizare.
570 If InKey$ = "" Then 570
580 End
590 '*************************************************************
600 ' SUBRUTINA PENTRU CENTRAREA PICULUI LA POZITIA  N/2+1 DIN N
610 '          v()=VECTOR DE DATE ;  w()=VECTOR DE LUCRU
620 '*************************************************************
630 '******************     CENTRAREA PICULUI LA N/2+1
640 L1 = 1 'L1 si L2 sunt limitele intre care este cuprins picul.
650 L2 = NPCT
660 GoSub 910 ' Calc. centrul de greutate initial al picului.
670 For I = 1 To N
    680 W(I) = 0
690 Next I
700 L5 = N / 2 - Int(CG + .5) + 1
710 For I = 0 To L2 - L1 ' Vectorul v() este transferat in w()
    720 W(I + L5) = V(I + L1) '     cat de cat la N/2+1.
730 Next I
740 L2 = L2 + L5
750 For I = 1 To N
    760 M(I) = W(I) ' Datele initiale sunt stocate in M().
    770 V(I) = W(I) ' Datele sunt readuse din w() in V().
780 Next I
790 GoSub 2730
800 CG = L5 + CG ' Noua pozitie a centrului de greutate.
810 GoSub 1050 ' Raporteaza noua pozitie a centrului de  greutate.
820 C = N / 2 - CG + 2 'Centr.pic.la N/2+1 prin interpol.liniara.
830 For I = 1 To N - 1
    840 V(I) = V(I) - C * (V(I + 1) - V(I))
850 Next I
860 GoSub 2830
870 For I = 1 To N
    880 W(I) = 0
890 Next I
900 Return
910 '*************************************************************
920 '  SUBRUTINA PENTRU CALCULUL CENTRULUI DE GREUTATE AL PICULUI
930 '                 CUPRINS INTRE  L1 si L2.
940 '*************************************************************
950 CG = 0
960 N2 = 0
970 N1 = 0
980 For I = L1 To L2
    990 N1 = N1 + 1
    1000 N2 = N2 + V(I)
    1010 CG = CG + N1 * V(I)
1020 Next I
1030 CG = CG / N2
1040 Locate 20, 1
1050 Print "CENTRUL DE GREUTATE ESTE AL "; CG + L1 - 1; "-LEA PUNCT"
1060 Return
1070 '************************************************************
1080 ' SUBRUTINA  DE  DECONVOLUTIE  IN  IPOTEZA  UNEI   DISTIBUTII
1090 ' POISSON IN VECTORUL V PRIN METODA  STOKES  CU  REGULARIZARE
1100 ' TIHONOV. PARAMETRUL DE REGULARIZARE ESTE AFLAT DUPA REZIDUU
1110 '       VECTORI FOLOSITI :A,B,K,L,M,V,W
1120 '     v()=VECTOR DE DATE ; a()=ETALON(functie de rezolutie)
1130 '************************************************************
1140 Print "********  RAPORT ASUPRA REGULARIZARII  ************"
1150 For I = 1 To N
    1160 W(I) = 0
    1170 B(I) = 0
    1180 M(I) = V(I)
1190 Next I
1200 GoSub 2470 'Transporta centrele de greutate din N/2+1 in 1.
1210 Q = 2 ' Fa transformarea Fourier pentru vectorii  complexi
1220 GoSub 2940 '                  [V(),W()]  si [A(),B()].
1230 GoSub 1950 'Calculeaza parametrul de regularizare .
1240 Print "ALFA LA CALCULUL FINAL ="; Z
1250 Print "                         >>>>       APASA ORICE TASTA"
1260 If InKey$ = "" Then 1260
1270 GoSub 2900
1280 For I = 1 To N
    1290 T = 1 + Z * K(I) 'Nr. cu care sunt diminuati coef. Fourier
    1300 C = A(I) * A(I) + B(I) * B(I) ' Norma vect. complex.
    1310 A = V(I) * A(I) + W(I) * B(I) 'Scalari pentru impartire
    1320 B = W(I) * A(I) - V(I) * B(I) 'a doua numere complexe.
    1330 If Abs(C) > Sqr(PREC) Then 1390
    1340 V(I) = 0
    1350 W(I) = 0
    1360 A(I) = 0
    1370 B(I) = 0
    1380 GoTo 1460
    1390 A(I) = A / C / T ' Deconvolutie prin impartirea vecto-
    1400 B(I) = B / C / T '   rilor comlexi [V,W]/[A,B]
    1410 If Abs(V(I)) > PREC / 1000 Then 1440
    1420 V(I) = 0
    1430 W(I) = 0
    1440 V(I) = V(I) / T ' Regularizare prin impartirea la T.
    1450 W(I) = W(I) / T
1460 Next I
1470 For I = 1 To N
    1480 K(I) = A(I) 'Coeficientii Fourier ai functiei deconv.
    1490 L(I) = B(I) 'sunt stocati in vectorul complex [K(),L()]
    1500 B(I) = -B(I) ' Pregatira pt. TF inversa (reconstructie).
    1510 W(I) = -W(I)
1520 Next I
1530 GoSub 2940 ' Transformarea Fourier  pentru reconstructie.
1540 For I = 1 To N ' Regasirea factorului de scala initial.
    1550 A(I) = A(I) / N
    1560 V(I) = V(I) / N
1570 Next I
1580 Cls
1590 '************************************************************
1600 ' VECTORUL M CONTINE FUNCTIA INITIALA .
1610 ' VECTORUL V CONTINE FUNCTIA RECONSTRUITA .
1620 ' VECTORUL A CONTINE FUNCTIA DECONVOLUTATA .
1630 ' VECTORII (K,L) CONTIN COEF.FOURIER AI FCT. DECONVOLUTATE .
1640 '************************************************************
1650 GoSub 2590 ' Afla factorul de scala pentru plotarea lui V().
1660 GoSub 2470 ' Repozitioneaza centrul de greutate la N/2+1.
1670 Locate 3, 1: Print "... Functia initiala"
1680 Locate 4, 1: Print "__  Functia reconstruita"
1690 Locate 18, 1: Print "__  Functia"
1700 Locate 19, 4: Print "deconvolutata "; ""
1710 GoSub 2820 '  Ploteaza vectorul v() cu linie continua.
1720 GoSub 2720 '  Ploteaza vectorul M() cu puncte.

1730 N0 = A(1) ' Afla factorul de scala pt. plotarea lui A().
1740 N9 = A(1)
1750 For I = 1 To N
    1760 If A(I) >= N0 Then 1780
    1770 N0 = A(I)
    1780 If A(I) <= N9 Then 1800
    1790 N9 = A(I)
1800 Next I
1810 SCALAY = 250 / (N9 - N0)

1820 PSet (300, 300 - SCALAY * (A(1) - N0)) ' Incepe plotarea
1830 For I = 1 To N '                   vectorului A().
    1840 PSet (I, 350 - SCALAY * (A(I) - N0))
1850 Next I


1860 M2 = 0
1870 For I = 1 To N
    1880 A1 = M(I) - V(I)
    1890 M2 = M2 + A1 * A1
1900 Next I
1910 Locate 23, 37
1920 Z$ = "Rez final=##.##^^^^ , Initial=##.##^^^^ "
1930 Print Using Z$; M2; ZG1
1940 Return
1950 '************************************************************
1960 '   SUBRUTINA DE CALCUL AL  PARAMETRULUI DE REGULARIZARE
1970 '************************************************************
1980 K(1) = 0 'Valorile polinomului de regularizare, n^8, sunt
1990 L(1) = 0 'stocate temporar in K(), norma lui [V,W] in L().
2000 M1 = 0
2010 M2 = 0
2020 M3 = 0
2030 For I = 2 To N
    2040 kk = N / 2 - Abs(N / 2 - I + 1)
    2050 D = kk ^ 4
    2060 D = D * D
    2070 K(I) = D
    2080 E = V(I) * V(I) + W(I) * W(I)
    2090 L(I) = E
    2100 M1 = M1 + E / D
    2110 M2 = M2 + E
    2120 M3 = M3 + M(I)
2130 Next I
2140 Z = .000001 ' Aproxim. initiala a param de regularizare.
2150 M3 = M3 * N 'Suma patratelor zgomotelor in ipoteza unei
2160 '              distributii Poisson a erorilor.
2170 '
2180 'M3 = ZG1 * N ' DACA PUNEM ZGOMOTUL INTRODUS DE NOI
2190 '
2200 Print "REZIDUU CALCULAT="; M3 / N;
2210 Print "Alfa la iteratia 0 ="; Z
2220 Z1 = 0 ' Incepe metoda secantei pt param de regularizare.
2230 R1 = -M3
2240 L = 0
2250 GoTo 2330
2260 Z = (Z1 - R1 * (Z1 - Z0) / (R1 - R0))
2270 If Z > 0 Then 2310
2280 Z0 = 0
2290 R0 = -M3
2300 GoTo 2260
2310 L = L + 1
2320 Print "Alfa la iteratia"; L; "="; Z
2330 M1 = 0
2340 For I = 2 To N
    2350 A = 1 / Z / K(I) + 1
    2360 M1 = M1 + L(I) / A / A
2370 Next I
2380 Print "REZIDUU LA ITERATIA "; L; "="; M1 / N
2390 Z0 = Z1
2400 Z1 = Z
2410 R0 = R1
2420 R1 = M1 - M3
2430 If Abs(R1 / M3) <= .1 Then 2450
2440 GoTo 2260
2450 Print "*********   Terminat regularizarea   **************"
2460 Return
2470 '************************************************************
2480 '   PUNE CENTRUL DE GREUTATEAL VECTORILOR A(),V() LA 0*Pi
2490 '************************************************************
2500 For I = 1 To N / 2
    2510 Y2 = V(I)
    2520 V(I) = V(N / 2 + I)
    2530 V(N / 2 + I) = Y2
    2540 Y2 = A(I)
    2550 A(I) = A(N / 2 + I)
    2560 A(N / 2 + I) = Y2
2570 Next I
2580 Return
2590 '************************************************************
2600 '         SUBRUTINA DE SCALARE A VECTORULUI V()
2610 '************************************************************
2620 N9 = -1E+20
2630 N0 = 1E+20
2640 For I = 1 To N
    2650 If V(I) > N0 Then 2670
    2660 N0 = V(I)
    2670 If V(I) < N9 Then 2690
    2680 N9 = V(I)
2690 Next I
2700 SCALAY = 250 / (N9 - N0)
2710 Return
2720 '************************************************************
2730 '      SUBRUTINA DE PLOTARE A VECTORULUI  M()
2740 '************************************************************
2750 For I = 1 To N
    2760 PSet (I + 1, 300 - SCALAY * (M(I) - N0)), 5
    2770 PSet (I - 1, 300 - SCALAY * (M(I) - N0)), 5
    2780 PSet (I, 301 - SCALAY * (M(I) - N0)), 5
    2790 PSet (I, 299 - SCALAY * (M(I) - N0)), 5
2800 Next I
2810 Return
2820 '************************************************************
2830 '     SUBRUTINA DE PLOTARE A VECTORULUI V()
2840 '************************************************************
2850 PSet (1, 300 - SCALAY * (V(1) - N0))
2860 For I = 1 To N
    2870 Line -(I, 300 - SCALAY * (V(I) - N0))
2880 Next I
2890 Return
2900 Cls
2910 Locate 11, 10
2920 Print "PROGRAMUL ESTE IN EXECUTIE NORMALA"
2930 Return
2940 '************************************************************
2950 ' ===   SUBRUTINA PENTRU TRANSFORMAREA FOURIER RAPIDA   ====
2960 '   CALCULEAZA TRANSFORMAREA FOURIER DUPA ALGORITMUL COOLEY-
2970 'TUKEY PENTRU DOUA SETURI DE DATE. UN SINGUR SET,[V(),W()]
2980 'SE OBTINE PENTRU Q<>2.DOUA SETURI,[A(),B()],[[V(),W()])
2990 'SE OBTIN PENTRU Q=2. VECTORII A()  SI V()  CONTIN  PARTEA
3000 'REALA. VECTORII  B() SI  W() CONTIN PARTEA IMAGINARA.
3010 '************************************************************
3020 FLAG = 1
3030 If Q <> 2 Then FLAG = 0
3040 S9 = Sqr(2) / 2
3050 S8 = Sin(PI / 8)
3060 C8 = Cos(PI / 8)
3070 M8 = M + 1
3080 N = 2 ^ M
3090 M5 = Int(M / 2) * 2
3100 E(1) = 1
3110 K8 = N + 1
3120 Rem ************** iNITIALIZAREA VECTORULUI DE LUCRU
3130 For I = 2 To M8
    3140 E(I) = E(I - 1) + E(I - 1)
3150 Next I
3160 U = 2 * PI / N
3170 M7 = M - 4
3180 B = 1
3190 If M5 = M Then 3380
3200 K2 = K8
3210 K0 = E(M5 + 1) + B
3220 K2 = K2 - 1
3230 K0 = K0 - 1
3240 If FLAG = 0 Then 3310
3250 Y2 = A(K2)
3260 Z2 = B(K2)
3270 A(K2) = A(K0) - Y2
3280 B(K2) = B(K0) - Z2
3290 A(K0) = A(K0) + Y2
3300 B(K0) = B(K0) + Z2
3310 Y2 = V(K2)
3320 Z2 = W(K2)
3330 V(K2) = V(K0) - Y2
3340 W(K2) = W(K0) - Z2
3350 V(K0) = V(K0) + Y2
3360 W(K0) = W(K0) + Z2
3370 If K0 > B Then 3220
3380 C1 = 1
3390 S1 = 0
3400 J8 = 0
3410 K = M5 - 1
3420 J = 4
3430 If K >= 1 Then 3550
3440 GoTo 4690
3450 If (E(J) > J8) Then 3530
3460 J8 = J8 - E(J)
3470 J = J - 1
3480 If E(J) > J8 Then 3530
3490 J8 = J8 - E(J)
3500 J = J - 1
3510 K = K + 2
3520 GoTo 3450
3530 J8 = J8 + E(J)
3540 J = 4
3550 I9 = E(K)
3560 If J8 = 0 Then 3650
3570 Rem ******** INITIALIZEAZA PARAMETRII TRIGONOMETRICI
3580 C2 = J8 * I9 * U
3590 C1 = Cos(C2)
3600 S1 = Sin(C2)
3610 C2 = C1 * C1 - S1 * S1
3620 S2 = C1 * (S1 + S1)
3630 C3 = C2 * C1 - S2 * S1
3640 S3 = C2 * S1 + S2 * C1
3650 J9 = I9 + B
3660 Rem ******** CALCULEAZA COEF. FOURIER CATE PATRU.
3670 For I = 1 To I9
    3680 K0 = J9 - I
    3690 K1 = K0 + I9
    3700 K2 = K1 + I9
    3710 K3 = K2 + I9
    3720 If FLAG = 0 Then 4110
    3730 A0 = A(K0)
    3740 B0 = B(K0)
    3750 A1 = A(K1)
    3760 B1 = B(K1)
    3770 A2 = A(K2)
    3780 B2 = B(K2)
    3790 A3 = A(K3)
    3800 B3 = B(K3)
    3810 If S1 = 0 Then 3910
    3820 T = A1
    3830 A1 = A1 * C1 - B1 * S1
    3840 B1 = T * S1 + B1 * C1
    3850 T = A2
    3860 A2 = A2 * C2 - B2 * S2
    3870 B2 = T * S2 + B2 * C2
    3880 T = A3
    3890 A3 = A3 * C3 - B3 * S3
    3900 B3 = T * S3 + B3 * C3
    3910 T = A0 + A2
    3920 A2 = A0 - A2
    3930 A0 = T
    3940 T = A1 + A3
    3950 A3 = A1 - A3
    3960 A1 = T
    3970 T = B0 + B2
    3980 B2 = B0 - B2
    3990 B0 = T
    4000 T = B1 + B3
    4010 B3 = B1 - B3
    4020 B1 = T
    4030 A(K0) = A0 + A1
    4040 B(K0) = B0 + B1
    4050 A(K1) = A0 - A1
    4060 B(K1) = B0 - B1
    4070 A(K2) = A2 - B3
    4080 B(K2) = B2 + A3
    4090 A(K3) = A2 + B3
    4100 B(K3) = B2 - A3
    4110 A0 = V(K0)
    4120 B0 = W(K0)
    4130 A1 = V(K1)
    4140 B1 = W(K1)
    4150 A2 = V(K2)
    4160 B2 = W(K2)
    4170 A3 = V(K3)
    4180 B3 = W(K3)
    4190 If S1 = 0 Then 4290
    4200 T = A1
    4210 A1 = A1 * C1 - B1 * S1
    4220 B1 = T * S1 + B1 * C1
    4230 T = A2
    4240 A2 = A2 * C2 - B2 * S2
    4250 B2 = T * S2 + B2 * C2
    4260 T = A3
    4270 A3 = A3 * C3 - B3 * S3
    4280 B3 = T * S3 + B3 * C3
    4290 T = A0 + A2
    4300 A2 = A0 - A2
    4310 A0 = T
    4320 T = A1 + A3
    4330 A3 = A1 - A3
    4340 A1 = T
    4350 T = B0 + B2
    4360 B2 = B0 - B2
    4370 B0 = T
    4380 T = B1 + B3
    4390 B3 = B1 - B3
    4400 B1 = T
    4410 V(K0) = A0 + A1
    4420 W(K0) = B0 + B1
    4430 V(K1) = A0 - A1
    4440 W(K1) = B0 - B1
    4450 V(K2) = A2 - B3
    4460 W(K2) = B2 + A3
    4470 V(K3) = A2 + B3
    4480 W(K3) = B2 - A3
4490 Next I
4500 If K <= 1 Then 4530
4510 K = K - 2
4520 GoTo 3550
4530 B = K3 + I9
4540 Rem   ********    INCEARCA CRITERIILE DE TERMINARE
4550 If K8 <= B Then 4690
4560 If J <> 1 Then 4600
4570 K = 3
4580 J = M7
4590 GoTo 3450
4600 J = J - 1
4610 C2 = C1
4620 If J <> 2 Then 4660
4630 C1 = C1 * C8 + S1 * S8
4640 S1 = S1 * C8 - C2 * S8
4650 GoTo 3610
4660 C1 = (C1 - S1) * S9
4670 S1 = (C2 + S1) * S9
4680 GoTo 3610
4690 Rem **************PERMUTA VECTORII COMPLEXI IN ORDINE BINARA
4700 '                  INVERSA .
4710 If M <= 1 Then 5450
4720 M8 = M + 1
4730 J8 = 1
4740 E(1) = 1
4750 For I = 2 To M8
    4760 E(I) = 2 * E(I - 1)
4770 Next I
4780 N4 = E(M8 - 2)
4790 If M <= 2 Then 4810
4800 N8 = E(M8 - 3)
4810 N2 = E(M8 - 1)
4820 L8 = N2
4830 N7 = E(M8) + 1
4840 M8 = M8 - 4
4850 J = 2
4860 J7 = J8 + N2
4870 If FLAG = 0 Then 4940
4880 Y2 = A(J)
4890 Z2 = B(J)
4900 A(J) = A(J7)
4910 B(J) = B(J7)
4920 A(J7) = Y2
4930 B(J7) = Z2
4940 Y2 = V(J)
4950 Z2 = W(J)
4960 V(J) = V(J7)
4970 W(J) = W(J7)
4980 V(J7) = Y2
4990 W(J7) = Z2
5000 J = J + 1
5010 If J8 > N4 Then 5040
5020 J8 = J8 + N4
5030 GoTo 5150
5040 J8 = J8 - N4
5050 If J8 > N8 Then 5080
5060 J8 = J8 + N8
5070 GoTo 5150 '2310
5080 J8 = J8 - N8
5090 K = M8
5100 If E(K) >= J8 Then 5140
5110 J8 = J8 - E(K)
5120 K = K - 1
5130 GoTo 5100
5140 J8 = E(K) + J8
5150 If J8 <= J Then 5430
5160 K = N7 - J
5170 J7 = N7 - J8
5180 If FLAG = 0 Then 5310
5190 Y2 = A(J)
5200 Z2 = B(J)
5210 A(J) = A(J8)
5220 B(J) = B(J8)
5230 A(J8) = Y2
5240 B(J8) = Z2
5250 Y2 = A(K)
5260 Z2 = B(K)
5270 A(K) = A(J7)
5280 B(K) = B(J7)
5290 A(J7) = Y2
5300 B(J7) = Z2
5310 Y2 = V(J)
5320 Z2 = W(J)
5330 V(J) = V(J8)
5340 W(J) = W(J8)
5350 V(J8) = Y2
5360 W(J8) = Z2
5370 Y2 = V(K)
5380 Z2 = W(K)
5390 V(K) = V(J7)
5400 W(K) = W(J7)
5410 V(J7) = Y2
5420 W(J7) = Z2
5430 J = J + 1
5440 If J <= L8 Then 4860
5450 Return

