*
*
*  MAIN INSTRUCTION TABLE
*
           ORG 1400B;

1400:  TI:        IR←X←7, .LQY, GOTO FIXTRP
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1046    8,12,15,16,
.C = 7   39,40,41,
CHECKBIT IS 50   
       
1401:          CALL 1776B
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16,
       
1402:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1403:  RES←AR←M
.BL = 14   79,78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
           
1404:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1405:  RES←BR←M
.BL = 14   79,78
.SSP = 2   55
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
           
1406:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1407:  XR←M
.BL = 14   79,78
.SSP = 6   55,54
.LSPX = 1   59
           
1410:  Q←SR, CALL DBL IF AORB
.MCONT = 1   7
.SSP = 11   56,53
.TSPY = 1   45
.LQY = 1   75
.MC = 51   5,2,0
.B = 405    9,15,17,
           
1411:  AR←M, R0←S←R6+1, GOTO LDD1
.RRN = 6   67,66
.BL = 14   79,78
.SSP = 1   56
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 20   1
.B = 417    9,14,15,16,17,
           
1412:  RESETCM, DGOTO NX2
.MS = 24   63,61
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1413:  XR←Z, Z←P←P+1, GOTO BRUA IF A
.RRN = 1   68
.BR = 14   83,82
.SSP = 6   55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 33   5,4,2,1
.B = 412    9,14,16,
CHECKBIT IS 30   
           
1414:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1415:  XR← NOT M+1
.BL = 3   81,80
.SSP = 6   55,54
.LOC = 1   50
.TAX = 1   49
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
1416:  K←Q, DGOTO XMAA
.BL = 12   80,78
.TXW = 1   47
.LRN = 3   71,70
.MC = 1   5
.DGO = 1   87
.B = 425    9,13,15,17,
           
1417:  GOTO TI IF A
.MC = 33   5,4,2,1
.B = 1400    8,9,
CHECKBIT IS 30   
           
1420:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1421:  RES←AR←M AND Q
.BL = 10   78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
CHECKBIT IS 87   
           
1422:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1423:  RES←AR←M OR Q
.BL = 16   80,79,78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
CHECKBIT IS 87   

1424:  EOR:       Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1425:  RES←AR←M EOR Q
.BL = 6   80,79
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
           
1426:  Q←40B, Z←SR, DGOTO STD1
.SSP = 11   56,53
.TCX = 1   43
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 431    9,13,14,17,
.C = 40   36,
           
1427:  K←R6, M←Z AND NOT Q, GOTO CSTORE IF R0[2]
.RRN = 6   67,66
.BR = 4   83
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LRN = 3   71,70
.MC = 24   3,1
.B = 701    9,10,11,17,
CHECKBIT IS 30   

1430:  STFX:      IR←M←BR, Z←Q, SETBB, DGOTO STF1
.BL = 12   80,78
.SSP = 2   55
.MS = 60   61,60
.TSPY = 1   45
.TYW = 1   48
.LMY = 1   73
.LZX = 1   76
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 1177    8,11,12,13,14,15,16,17,
CHECKBIT IS 30   
           
1431:  K←ER, Q←7777B, SETBB, CALL FROUND
.MCONT = 1   7
.SSP = 5   56,54
.MS = 60   61,60
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LQX = 1   74
.LRN = 3   71,70
.MC = 20   1
.B = 1160    8,11,12,13,
.C = 7777   30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
1432:  M←Q, GOTO STRFLD IF ST
.BL = 12   80,78
.LMX = 1   72
.MC = 62   4,1,0
.B = 362    10,11,12,13,16,
           
1433:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
           
1434:  M←BR, GOTO STRFLD IF ST
.SSP = 2   55
.TSPY = 1   45
.LMY = 1   73
.MC = 62   4,1,0
.B = 362    10,11,12,13,16,
           
1435:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
           
1436:  M←XR, GOTO STRFLD IF ST
.SSP = 6   55,54
.TSPY = 1   45
.LMY = 1   73
.MC = 62   4,1,0
.B = 362    10,11,12,13,16,
CHECKBIT IS 30   
           
1437:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
           
1440:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1441:  RES←AR←M+Q, ROV
.BR = 12   84,82
.BL = 14   79,78
.SSP = 1   56
.MS = 66   64,63,61,60
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.VCY = 1   86

1442:  SUCA:      Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1443:  RES←AR←Q-M, ROV
.BR = 12   84,82
.BL = 3   81,80
.SSP = 1   56
.MS = 66   64,63,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
1444:  GOTO ADCA IF CAR
.MC = 46   4,3,0
.B = 1553    8,9,11,12,14,16,17,
CHECKBIT IS 30   
           
1445:  RES←AR←M+Q, ROV, GOTO NX1
.BR = 12   84,82
.BL = 14   79,78
.SSP = 1   56
.MS = 66   64,63,61,60
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.VCY = 1   86
.B = 34    13,14,15,
           
1446:  GOTO SUCA IF CAR
.MC = 46   4,3,0
.B = 1442    8,9,12,16,
           
1447:  RES←AR←NOT M+Q, ROV, GOTO NX1
.BR = 12   84,82
.BL = 3   81,80
.SSP = 1   56
.MS = 66   64,63,61,60
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.VCY = 1   86
.B = 34    13,14,15,
           
1450:  K←M←M+Q, ROV, DGOTO XMAA
.BR = 12   84,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.TXW = 1   47
.TAX = 1   49
.LMX = 1   72
.LRN = 3   71,70
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 425    9,13,15,17,
CHECKBIT IS 30   
           
1451:  GOTO TI IF A
.MC = 33   5,4,2,1
.B = 1400    8,9,
CHECKBIT IS 30   
           
1452:  Q←XR, DGOTO NX2
.SSP = 6   55,54
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1453:  XR←Q+M, Z←P←P+1
.RRN = 1   68
.BR = 12   84,82
.BL = 14   79,78
.SSP = 6   55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.TAX = 1   49
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.VCY = 1   86
           
1454:  AR←RES←M←M+1, ROV, GOTO STRFLD IF ST
.BL = 14   79,78
.SSP = 1   56
.MS = 66   64,63,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LMX = 1   72
.LRN = 5   71,69
.LSPX = 1   59
.MC = 62   4,1,0
.VCY = 1   86
.B = 362    10,11,12,13,16,
           
1455:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
           
1456:  AR←RES←M←M-1, ROV, GOTO STRFLD IF ST
.BR = 17   85,84,83,82
.BL = 14   79,78
.SSP = 1   56
.MS = 66   64,63,61,60
.TXW = 1   47
.TAX = 1   49
.LMX = 1   72
.LRN = 5   71,69
.LSPX = 1   59
.MC = 62   4,1,0
.VCY = 1   86
.B = 362    10,11,12,13,16,
CHECKBIT IS 30   
           
1457:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,

1460:  MUL:           MH1←Q, K←0, DGOTO MUL1, .LSPX←0
.BL = 12   80,78
.SSP = 40   51
.TYW = 1   48
.LRN = 3   71,70
.MC = 1   5
.DGO = 1   87
.B = 557    9,11,12,14,15,16,17,
           
1461:  M, R0←AR, CLEARTOV, GOTO MUL2 IF X<0
.BL = 14   79,78
.SSP = 1   56
.MS = 67   65,64,63,61,60
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 552    9,11,12,14,16,

1462:  DIV:       IR←Q←M, Z←M←AR, ABCD0, DGOTO DIV1
.BL = 14   79,78
.SSP = 1   56
.MS = 70   62,61,60
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LQX = 1   74
.LZY = 1   77
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 600    9,10,
CHECKBIT IS 30   
           
1463:  S←M EOR Q, Z←BR, SETA, GOTO DIV11 IF Z<0
.BL = 6   80,79
.SSP = 2   55
.MS = 30   62,61
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
.MC = 4   3
.B = 672    9,10,12,13,14,16,
CHECKBIT IS 30   
           
1464:  RES←Q←Q-M, M←4B7, HROV, DGOTO NX2
.BR = 12   84,82
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LQX = 1   74
.LRN = 5   71,69
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 35    13,14,15,17,
.C = -40000000   18,
           
1465:  Z←P←P+1, GOTO ICP1 IF OVFLW
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 57   5,4,3,2,0
.B = 671    9,10,12,13,14,17,
           
1466:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1467:  RES←M
.BL = 14   79,78
.TXW = 1   47
.LRN = 5   71,69
           
1470:  Z←P←P+1, DGOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
1471:  RES←M AND Q
.BL = 10   78
.TXW = 1   47
.LRN = 5   71,69
CHECKBIT IS 87   
           
1472:  IR←R0, DGOTO ISD1
.THY = 1   46
.TYW = 1   48
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 623    9,10,13,16,17,
CHECKBIT IS 30   
           
1473:  Q←Z←M, R0←R6+1, CALL DBL IF AORB
.MCONT = 1   7
.RRN = 6   67,66
.BL = 14   79,78
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LZX = 1   76
.LR0 = 1   58
.MC = 51   5,2,0
.B = 405    9,15,17,
           
1474:  R0←R6+1, DGOTO DSD1
.RRN = 6   67,66
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 637    9,10,13,14,15,16,17,
           
1475:  Z←M, CALL DBL IF AORB
.MCONT = 1   7
.BL = 14   79,78
.LZX = 1   76
.MC = 51   5,2,0
.B = 405    9,15,17,
CHECKBIT IS 30   
           
1476:  S←Z, Q←7B7, ABCD0, DGOTO EXUA
.BR = 14   83,82
.MS = 70   62,61,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 2   70
.MC = 1   5
.DGO = 1   87
.B = 415    9,14,15,17,
.C = -10000000   18,19,20,
CHECKBIT IS 50   
           
1477:  ILIM←0, GOTO TOSP IF INTRPT1
.SSP = 20   52
.LSPX = 1   59
.MC = 64   3,1,0
.B = 1057    8,12,14,15,16,17,
           
1500:  Z←M, Q←77B, CLEARA, DGOTO ASDL
.BL = 14   79,78
.MS = 50   62,60
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 511    9,11,14,17,
.C = 77   36,37,38,39,40,41,
           
1501:  M←BR, Z←Q AND M, CLEARTOV, GOTO ASDR IF Z<0
.BL = 10   78
.SSP = 2   55
.MS = 67   65,64,63,61,60
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
.MC = 4   3
.B = 534    9,11,13,14,15,
CHECKBIT IS 30   
           
1502:  Z←M, Q←77B, CLEARA, DGOTO ASAL
.BL = 14   79,78
.MS = 50   62,60
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 466    9,12,13,15,16,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 50   
           
1503:  Z←Q AND M, CLEARTOV, GOTO ASAR IF Z<0
.BL = 10   78
.MS = 67   65,64,63,61,60
.LZX = 1   76
.MC = 4   3
.B = 500    9,11,
CHECKBIT IS 30   
           
1504:  Z←M, Q←77B, SETA, DGOTO ASDL
.BL = 14   79,78
.MS = 30   62,61
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 511    9,11,14,17,
.C = 77   36,37,38,39,40,41,
           
1505:  Z←Q AND M, M←BR, GOTO ASDR IF Z<0
.BL = 10   78
.SSP = 2   55
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
.MC = 4   3
.B = 534    9,11,13,14,15,
           
1506:  Z←M, Q←77B, SETA, DGOTO ASAL
.BL = 14   79,78
.MS = 30   62,61
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 466    9,12,13,15,16,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 50   
           
1507:  Z←Q AND M, GOTO ASAR IF Z<0
.BL = 10   78
.LZX = 1   76
.MC = 4   3
.B = 500    9,11,
           
1510:  Z←M, Q←77B, DGOTO CYDL
.BL = 14   79,78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 452    9,12,14,16,
.C = 77   36,37,38,39,40,41,
           
1511:  Z←Q AND M, GOTO CYDR IF Z<0
.BL = 10   78
.LZX = 1   76
.MC = 4   3
.B = 463    9,12,13,16,17,
CHECKBIT IS 30   
           
1512:  Z←M, Q←77B, DGOTO CYAL
.BL = 14   79,78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 441    9,12,17,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 50   
           
1513:  Z←Q AND M, M←24, GOTO CYAR IF Z<0
.BL = 10   78
.TCY = 1   44
.LMY = 1   73
.LZX = 1   76
.MC = 4   3
.B = 445    9,12,15,17,
.C = 30   37,38,
           
1514:  DGOTO TSB1
.MC = 1   5
.DGO = 1   87
.B = 660    9,10,12,13,
CHECKBIT IS 30   
           
1515:  IR←M, CALL GETSR
.MCONT = 1   7
.BL = 14   79,78
.TXW = 1   47
.LRN = 4   69
.MC = 20   1
.B = 662    9,10,12,13,16,
           
1516:  Q←R6 MRG 4B6, RESETCM, DGOTO NX2
.RRN = 6   67,66
.MS = 24   63,61
.TCY = 1   44
.THY = 1   46
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
.C = 4000000   21,
CHECKBIT IS 50   
           
1517:  XR←Q, Z←P←P+1, GOTO BRUA IF A
.RRN = 1   68
.BL = 12   80,78
.SSP = 6   55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 33   5,4,2,1
.B = 412    9,14,16,
CHECKBIT IS 30   

1520:  BRU:       P←Z, R0+1, TAKE, DGOTO NX3
.BR = 14   83,82
.MS = 75   65,63,62,61,60
.IHR = 1   42
.THY = 1   46
.TXW = 1   47
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 36    13,14,15,16,
           
1521:  R6←M, Q←7B7, SABCDE0, GOTO BRUA IF INTRPT
.BL = 14   79,78
.MS = 33   65,64,62,61
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.MC = 41   5,0
.B = 412    9,14,16,
.C = -10000000   18,19,20,
           
1522:  Z←RES, DGOTO BRU
.RRN = 5   68,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1523:  Z←R6, GOTO SKIP IF Z>=0
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 5   5,3
.B = 410    9,14,
CHECKBIT IS 30   
           
1524:  Z←RES, DGOTO BRU
.RRN = 5   68,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1525:  Z←R6, GOTO SKIP IF Z#0
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 3   5,4
.B = 410    9,14,
CHECKBIT IS 30   
           
1526:  Z←RES, DGOTO BRU
.RRN = 5   68,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1527:  Z←R6, GOTO SKIP IF Z>0
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 6   4,3
.B = 410    9,14,
CHECKBIT IS 30   
           
1530:  Z←RES, DGOTO BRU
.RRN = 5   68,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1531:  Z←R6, GOTO SKIP IF Z<=0
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 13   5,4,2
.B = 410    9,14,
           
1532:  Z←RES, DGOTO BRU
.RRN = 5   68,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1533:  Z←R6, GOTO SKIP IF Z=0
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 2   4
.B = 410    9,14,
           
1534:  Z←RES, DGOTO BRU
.RRN = 5   68,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1535:  Z←R6, GOTO SKIP IF Z<0
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 4   3
.B = 410    9,14,
           
1536:  Q←GR, Z←200B, SETBB, DGOTO BLL1
.SSP = 10   53
.MS = 60   61,60
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 12    14,16,
.C = 200   34,
CHECKBIT IS 50   
           
1537:  NEWG←Q, Q←77B6, CALL DBL IF AORB
.MCONT = 1   7
.BL = 12   80,78
.SSP = 25   56,54,52
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 51   5,2,0
.B = 405    9,15,17,
.C = -1000000   18,19,20,21,22,23,
           
1540:  Q←GR, Z←40000200B, SETBB, DGOTO BLL1
.SSP = 10   53
.MS = 60   61,60
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 12    14,16,
.C = -37777600   18,34,
           
1541:  NEWG←Q, Q←77B6, CALL DBL IF AORB
.MCONT = 1   7
.BL = 12   80,78
.SSP = 25   56,54,52
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 51   5,2,0
.B = 405    9,15,17,
.C = -1000000   18,19,20,21,22,23,
           
1542:  IR←XR, DGOTO BRXA
.SSP = 6   55,54
.TSPY = 1   45
.TYW = 1   48
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 1555    8,9,11,12,14,15,17,
CHECKBIT IS 30   
           
1543:  Z←IR+1, DGOTO BRU
.RRN = 4   66
.IHR = 1   42
.THY = 1   46
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1544:  Z←P+1, DGOTO BRU
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 1520    8,9,11,13,
CHECKBIT IS 30   
           
1545:  XR←Z, Z←R6
.RRN = 6   67,66
.BR = 14   83,82
.SSP = 6   55,54
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
           
1546:  IR←M, Q←1, DGOTO SRS1
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 653    9,10,12,14,16,17,
.C = 1   41,
           
1547:  R0←M AND Q, CALL GETSR
.MCONT = 1   7
.BL = 10   78
.TXW = 1   47
.LR0 = 1   58
.MC = 20   1
.B = 662    9,10,12,13,16,
CHECKBIT IS 30   
           
1550:  IR←XR, Q←7B7, SETBB, DGOTO EAC1
.SSP = 6   55,54
.MS = 60   61,60
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LQX = 1   74
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 716    9,10,11,14,15,16,
.C = -10000000   18,19,20,
CHECKBIT IS 50   
           
1551:  S←Z, CALL DBL IF AORB
.MCONT = 1   7
.BR = 14   83,82
.TXW = 1   47
.LRN = 2   70
.MC = 51   5,2,0
.B = 405    9,15,17,
           
1552:  GOTO TI
.MC = 1   5
.B = 1400    8,9,

1553:  ADCA:      RES←AR←M+Q, .LOC, ROV, GOTO NX1
.BR = 12   84,82
.BL = 14   79,78
.SSP = 1   56
.MS = 66   64,63,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.VCY = 1   86
.B = 34    13,14,15,
CHECKBIT IS 30   
           
1554:  GOTO TI
.MC = 1   5
.B = 1400    8,9,

1555:  BRXA:      Z←R6, XR←Z, GOTO SKIP IF Z>=0
.RRN = 6   67,66
.BR = 14   83,82
.SSP = 6   55,54
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
.MC = 5   5,3
.B = 410    9,14,
           
1556:  GOTO TI
.MC = 1   5
.B = 1400    8,9,

1557:  CALL 1776B
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16,
           
1560:  SETBB, DGOTO FLX1
.MS = 60   61,60
.MC = 1   5
.DGO = 1   87
.B = 1242    8,10,12,16,
CHECKBIT IS 30   
           
1561:  Q←2B7, SETBB, CALL FOP
.MCONT = 1   7
.MS = 60   61,60
.TCX = 1   43
.LQX = 1   74
.MC = 20   1
.B = 1255    8,10,12,14,15,17,
.C = 20000000   19,
           
1562:  Q←2B7, SETBB, CALL FOP
.MCONT = 1   7
.MS = 60   61,60
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.B = 1255    8,10,12,14,15,17,
.C = 20000000   19,
           
1563:  ER←Q, SETBB, GOTO FLD1
.BL = 12   80,78
.SSP = 5   56,54
.MS = 60   61,60
.LSPX = 1   59
.MC = 20   1
.B = 1037    8,13,14,15,16,17,
CHECKBIT IS 30   
           
1564:  IR←Q, Q←2B7, SETBB, CALL FOP
.MCONT = 1   7
.BL = 12   80,78
.MS = 60   61,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1255    8,10,12,14,15,17,
.C = 20000000   19,
           
1565:  RES←Q←IR MRG 1, M←Q, SETBB, GOTO FCP1
.RRN = 4   66
.BL = 12   80,78
.MS = 60   61,60
.TCY = 1   44
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LQY = 1   75
.LRN = 5   71,69
.MC = 20   1
.B = 1146    8,11,12,15,16,
.C = 1   41,
           
1566:  IR←Q, Q←2B7, SETBB, CALL FOP
.MCONT = 1   7
.BL = 12   80,78
.MS = 60   61,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1255    8,10,12,14,15,17,
.C = 20000000   19,
           
1567:  Z←ER, R0←Q, SETBB, GOTO FAD1
.BL = 12   80,78
.SSP = 5   56,54
.MS = 60   61,60
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
.MC = 20   1
.B = 775    9,10,11,12,13,14,15,17,
           
1570:  SETBB, DGOTO FSB1
.MS = 60   61,60
.MC = 1   5
.DGO = 1   87
.B = 1033    8,13,14,16,17,
           
1571:  IR←Q, Q←2B7, SETBB, CALL FOP
.MCONT = 1   7
.BL = 12   80,78
.MS = 60   61,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 1255    8,10,12,14,15,17,
.C = 20000000   19,
           
1572:  Q←2B7, CALL DBL IF AORB
.MCONT = 1   7
.TCX = 1   43
.LQX = 1   74
.MC = 51   5,2,0
.B = 405    9,15,17,
.C = 20000000   19,
CHECKBIT IS 50   
           
1573:  M←M EOR Q, Q←2001B4, SETBB, GOTO FMUL1
.BL = 6   80,79
.MS = 60   61,60
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 20   1
.B = 1041    8,12,17,
.C = 20010000   19,29,
           
1574:  Q←2B7, SETBB, CALL FOP
.MCONT = 1   7
.MS = 60   61,60
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.B = 1255    8,10,12,14,15,17,
.C = 20000000   19,
           
1575:  S←Z, SETBB, GOTO FDIV1
.BR = 14   83,82
.MS = 60   61,60
.TXW = 1   47
.LRN = 2   70
.MC = 20   1
.B = 1077    8,12,13,14,15,16,17,
CHECKBIT IS 30   
           
1576:  R0←NOT M, Q←MAXOPR, SETBB, DGOTO OPR1
.BL = 3   81,80
.MS = 60   61,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 500    9,11,
.C = 55   36,38,39,41,
CHECKBIT IS 50   
           
1577:  R0←M-Q, Q←R0+1, SETBB, GOTO SYSCLL IF R0>=0
.BR = 5   85,83
.BL = 14   79,78
.MS = 60   61,60
.LOC = 1   50
.IHR = 1   42
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.MC = 12   4,2
.VCY = 1   86
.B = 503    9,11,16,17,
CHECKBIT IS 30   
*
*
           ORG 1600B;
*
*  TABLE OF INSTRUCTIONS IN 940 COMPATIBILITY MODE
*    H1 = EFFECTIVE ADDRESS
*    H2 = INSTRUCTION WORD
*    H3 = EFFECTIVE ADDRESS BEFORE X MODIFICATION
*    M = CONTENTS(H1), BUT NO CHECK HAS BEEN EXECUTED
*      FOR PAGE-FAULT
*    Q = A REGISTER, R0 = MAP(H1)
*  A DGOTO ASGNA IS IN PROGRESS
*
           
1600:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1601:  P←Z, Z←H1, GOTO BRU1 IF R0>=0, ABCD0
.RRN = 2   67
.MS = 70   62,61,60
.THY = 1   46
.BR = 14       82,83
.TXW = 1   47
.LZY = 1   77
.LRN = 1   71
.MC = 12   4,2
.B = 1224    8,10,13,15,
CHECKBIT IS 30   
           
1602:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1603:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1604:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1605:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1606:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1607:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1610:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1611:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1612:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1613:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1614:  M←M AND Q
.BL = 10   78
.LMX = 1   72
CHECKBIT IS 87   
           
1615:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1616:  M←M OR Q
.BL = 16   80,79,78
.LMX = 1   72
CHECKBIT IS 87   
           
1617:  M←M EOR Q
.BL = 6   80,79
.LMX = 1   72
           
1620:  Z←P←P+1, GOTO NY2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 1153    8,11,12,14,16,17,
           
1621:  GOTO TI
.MC = 20   1
.B = 1400    8,9,
           
1622:  Q←Z←H2, M←20B, GOTO ROV1
.RRN = 3   68,67
.TCX = 1   43
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
.LZY = 1   77
.MC = 1   5
.B = 1267    8,10,12,13,15,16,17,
.C = 20   37,
CHECKBIT IS 50   
           
1623:  GOTO NY3 IF R0>=0
.MC = 12   4,2
.B = 1154    8,11,12,14,15,
           
1624:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1625:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1626:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1627:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1630:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1631:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1632:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1633:  GOTO TI1
.MC = 20   1
.B = 407    9,15,16,17,
           
1634:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1635:  M←Q,   GOTO ST940 IF R0>=0, RESETCM
.BL = 12   80,78
.MS = 24   63,61
.LMX = 1   72
.MC = 12   4,2
.B = 1230    8,10,13,14,
           
1636:  M←BR,   GOTO ST940 IF R0>=0, RESETCM
.SSP = 2   55
.MS = 24   63,61
.TSPY = 1   45
.LMY = 1   73
.MC = 12   4,2
.B = 1230    8,10,13,14,
           
1637:  M←XR,   GOTO ST940 IF R0>=0, RESETCM
.SSP = 6   55,54
.MS = 24   63,61
.TSPY = 1   45
.LMY = 1   73
.MC = 12   4,2
.B = 1230    8,10,13,14,
CHECKBIT IS 30   
           
1640:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1641:  Z←XR, GOTO BRX1
.SSP = 6   55,54
.TSPY = 1   45
.LZY = 1   77
.MC = 1        5
.B = 1221    8,10,13,17,
           
1642:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1643:  M←P, Q←432B5,  GOTO BRM1 IF R0>=0, RESETCM
.RRN = 1   68
.MS = 24   63,61
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 12   4,2
.B = 1232    8,10,13,14,16,
.C = -34600000   18,22,23,25,
CHECKBIT IS 50   
           
1644:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
           
1645:  GOTO TI
.MC = 20   1
.B = 1400    8,9,

1646:  RCH:       M←Z←H2, Q←R0←H1←0, ABCD0, GOTO RCH1
.RRN = 3   68,67
.MS = 70   62,61,60
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LQX = 1   74
.LZY = 1   77
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 1300    8,10,11,
CHECKBIT IS 30   
           
1647:  GOTO TI
.MC = 20   1
.B = 1400    8,9,
           
1650:  Z←M EOR Q, GOTO SKE1 IF R0>=0
.BL = 6   80,79
.LZX = 1   76
.MC = 12   4,2
.B = 1240    8,10,12,
CHECKBIT IS 30   
           
1651:  Z←M←M+1, Q←7774B4, GOTO BRR1 IF R0>=0
.BL = 14   79,78
.LOC = 1   50
.TCY = 1   44
.TAX = 1   49
.LMX = 1   72
.LQY = 1   75
.LZX = 1   76
.MC = 12   4,2
.VCY = 1   86
.B = 1242    8,10,12,16,
.C = -40000   18,19,20,21,22,23,24,25,26,27,
           
1652:  Q←BR, GOTO SKB1 IF R0>=0
.SSP = 2   55
.TSPY = 1   45
.LQY = 1   75
.MC = 12   4,2
.B = 1237    8,10,13,14,15,16,17,
CHECKBIT IS 30   
           
1653:  Z←M, GOTO SKN1 IF R0>=0
.BL = 14   79,78
.LZX = 1   76
.MC = 12   4,2
.B = 1263    8,10,12,13,16,17,

1654:  SUB:       M←Q-M, ROV, Z←XR, GOTO ADD1 IF R0>=0
.BR = 12   84,82
.BL = 3   81,80
.SSP = 6   55,54
.MS = 66   64,63,61,60
.LOC = 1   50
.TSPY = 1   45
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.MC = 12   4,2
.VCY = 1   86
.B = 1250    8,10,12,14,
CHECKBIT IS 30   

1655:  ADD:       M←Q+M, ROV, Z←XR, GOTO ADD1 IF R0>=0
.BR = 12   84,82
.BL = 14   79,78
.SSP = 6   55,54
.MS = 66   64,63,61,60
.TSPY = 1   45
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.MC = 12   4,2
.VCY = 1   86
.B = 1250    8,10,12,14,
           
1656:  Z←XR, CLEAROV, GOTO SUC1 IF R0>=0
.SSP = 6   55,54
.MS = 73   65,64,62,61,60
.TSPY = 1   45
.LZY = 1   77
.MC = 12   4,2
.B = 1244    8,10,12,15,
           
1657:  Z←XR, CLEAROV, GOTO ADC1 IF R0>=0
.SSP = 6   55,54
.MS = 73   65,64,62,61,60
.TSPY = 1   45
.LZY = 1   77
.MC = 12   4,2
.B = 1246    8,10,12,15,16,
CHECKBIT IS 30   
           
1660:  M←M-1, ROV, GOTO SKR1 IF R0>=0
.BR = 17   85,84,83,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.TAX = 1   49
.LMX = 1   72
.MC = 12   4,2
.VCY = 1   86
.B = 1253    8,10,12,14,16,17,
           
1661:  M←M+1, ROV, GOTO ST940 IF R0>=0
.BL = 14   79,78
.MS = 66   64,63,61,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 12   4,2
.VCY = 1   86
.B = 1230    8,10,13,14,
CHECKBIT IS 30   
           
1662:  H1←Q, GOTO XMA1 IF R0>=0
.BL = 12   80,78
.TXW = 1   47
.LRN = 2   70
.MC = 12   4,2
.B = 1226    8,10,13,15,16,
           
1663:  M←Q+M, ROV, GOTO ST940 IF R0>=0
.BR = 12   84,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.TAX = 1   49
.LMX = 1   72
.MC = 12   4,2
.VCY = 1   86
.B = 1230    8,10,13,14,
           
1664:  GOTO MUL IF R0>=0
.MC = 12   4,2
.B = 1460    8,9,12,13,
CHECKBIT IS 30   
           
1665:  GOTO DIV IF R0>=0
.MC = 12   4,2
.B = 1462    8,9,12,13,16,

1666:  RS:        RESETCM, M←H3, Q←777B, GOTO RS1
.RRN = 4   66
.MS = 24   63,61
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 1   5
.B = 1342    8,10,11,12,16,
.C = 777   33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   

1667:  LS:        RESETCM, M←H3, Q←777B, GOTO LS1
.RRN = 4   66
.MS = 24   63,61
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 20   1
.B = 1352    8,10,11,12,14,16,
.C = 777   33,34,35,36,37,38,39,40,41,
           
1670:  M←M EOR Q, Q←BR, GOTO SKM1 IF R0>=0
.BL = 6   80,79
.SSP = 2   55
.TSPY = 1   45
.LMX = 1   72
.LQY = 1   75
.MC = 12   4,2
.B = 1256    8,10,12,14,15,16,
CHECKBIT IS 30   
           
1671:  Q←0, GOTO LDX1 IF R0>=0
.LQX = 1   74
.MC = 12   4,2
.B = 1260    8,10,12,13,
           
1672:  Z←M AND Q, GOTO SKE1 IF R0>=0
.BL = 10   78
.LZX = 1   76
.MC = 12   4,2
.B = 1240    8,10,12,
           
1673:  Z←M-Q, HROV, GOTO SKG1 IF R0>=0
.BR = 5   85,83
.BL = 14   79,78
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 12   4,2
.VCY = 1   86
.B = 1262    8,10,12,13,16,
CHECKBIT IS 30   
           
1674:  Z←M, Q←777B, GOTO SKD1 IF R0>=0
.BL = 14   79,78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 12   4,2
.B = 1332    8,10,11,13,14,16,
.C = 777   33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
1675:  GOTO LDB1 IF R0>=0
.MC = 12   4,2
.B = 1261    8,10,12,13,17,
           
1676:  R0←R0
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
           
1677:  M←XR, Q←37777B, RESETCM, GOTO EAX1
.SSP = 6   55,54
.MS = 24   63,61
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.MC = 20   1
.B = 1257    8,10,12,14,15,16,17,
.C = 37777   28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
*
* 
*   THIS TEXT IS BEING ADDED TO THE QUASI-COPY OF RECORD
*   FOR THE CPU 1.5, AND INCLUDES ADDITIONAL SOURCE
*   INSTRUCTIONS FOR LOCATIONS 1700B TO 1714B....  JD 4/12/73
           ORG 1700B;

1700:  BREAK:     Q←12343210B, FETCH, DGOTO NEXT
.MS = 44   63,60
.TCX = 1   43
.LQX = 1   74
.MC = 20       1
.DGO = 1   87
.B = 657    9,10,12,14,15,16,17,
.C = 12343210  20,22,25,26,27,31,32,34,38,
CHECKBIT IS 50
           
1701:  Q EOR M, GOTO TOSP IF LB=0
.BL = 6   80,79
.MC = 22   4,1
.B = 1057    8,12,14,15,16,17,
CHECKBIT IS 30   

1702:  INDA:      M←M AND Q ! Z AND NOT Q, GOTO NY4
.BR = 4   83
.BL = 10   78
.LMX = 1   72
.MC = 1   5
.B = 1156    8,11,12,14,15,16,
CHECKBIT IS 30   

1703:  LDM1:      ZTOMAP, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 53   65,64,62,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1        5
.B = 35    13,14,15,17,

1704:  IDLE6:     2, UNPROTECT, GOTO IDLE2
.MS = 20   61
.TCX = 1   43
.MC = 20       1
.B = 1135    8,11,13,14,15,17,
.C = 2   40,
CHECKBIT IS 50   
           
1705:  RES←0, GOTO NX1
.TXW = 1   47
.LRN = 5   71,69
.MC = 20   1
.B = 34    13,14,15,
           
1706:  GOTO NY8 IF XPAGE
.MC = 42   4,0
.B = 1177    8,11,12,13,14,15,16,17,
CHECKBIT IS 30
           
1707:  GOTO TOSP
.MC = 1        5
.B = 1057    8,12,14,15,16,17,
           
1710:  P←Z, GOTO BRU1 IF R0>=0
.BR = 14   83,82
.TXW = 1   47
.LRN = 1   71
.MC = 12   4,2
.B = 1224    8,10,13,15,
CHECKBIT IS 30   
           
1711:  GOTO PAGEF
.MC = 1        5
.B = 700    9,10,11,
CHECKBIT IS 30   
           
1712:  Q←E1, GSB
.MS = 27       61,63,64,65
.TE1Y = 1      88
.LQY = 1   75
CHECKBIT IS 87   
           
1713:  Q←Q LCY 2, R0←P+1, DGOTO NY8
.RRN = 1   68
.BL = 12   80,78
.MS = 2   64
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.B = 1177    8,11,12,13,14,15,16,17,
           
1714:  Q LCY 12, Q←7B7, GOTO NX6 IF X>=0
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.LQY = 1   75
.MC = 16   4,3,2
.VCY = 1   86
.B = 32    13,14,16,
.C = -10000000   18,19,20,
CHECKBIT IS 50   

1715:  PATCH: START/STOP, GOTO IDLE6
.MS = 77       60,61,62,63,64,65
.MC = 20       1
.B = 1704      8,9,10,11,15
CHECKBIT IS 30

1716:  BMACC1: Z←R0, Q←3777B
.LZY = 1   77
.THY = 1   46
.LQX = 1   74
.TCX = 1   43
.C = 3777   31,32,33,34,35,36,37,38,39,40,41

1717:  MAPAD, M←E1, IR←1
.MS = 54   60,62,63
.TE1Y = 1   88
.LMY = 1   73
.LRN = 4   69
.TAX = 1   49
.LOC = 1   50
.TXW = 1   47

1720:  PARAM←Z AND Q!M AND NOT Q, GOTO MACC
.SSP = 26   52,54,55
.LSPX = 1   59
.BL = 4   79
.BR = 10   82
.MC = 1   5
.B = 747   9,10,11,12,15,16,17

1721:  M←PRTINDEX, CALL 1775B
.SSP = 21   52,56
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.MCONT = 1   7
.B = 1775   8,9,10,11,12,13,14,15,17

1722:  M←UN0, CALL 1775B
.SSP = 24   52,54
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.MCONT = 1   7
.B = 1775   8,9,10,11,12,13,14,15,17

1723:  M←UN1, CALL 1775B
.SSP = 25   52,54,56
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.MCONT = 1   7
.B = 1775   8,9,10,11,12,13,14,15,17
CHECKBIT IS 50

1724:  GOTO 744B
.MC = 1   5
.B = 744   9,10,11,12,15
CHECKBIT IS 56

1725:
CHECKBIT IS 30

1726:
CHECKBIT IS  87

1727:
CHECKBIT IS 50

1730:
CHECKBIT IS 56

1731:
CHECKBIT IS 30

1732:
CHECKBIT IS  87

1733:
CHECKBIT IS 50

1734:
CHECKBIT IS 56

1735:
CHECKBIT IS 30

1736:
CHECKBIT IS  87

1737:
CHECKBIT IS 50

1740:
CHECKBIT IS 56

1741:
CHECKBIT IS 30

1742:
CHECKBIT IS  87

1743:
CHECKBIT IS 50

1744:
CHECKBIT IS 56

1745:
CHECKBIT IS 30

1746:
CHECKBIT IS  87

1747:
CHECKBIT IS 50

1750:
CHECKBIT IS 56

1751:
CHECKBIT IS 30

1752:
CHECKBIT IS  87

1753:
CHECKBIT IS 50

1754:
CHECKBIT IS 56

1755:
CHECKBIT IS 30

1756:
CHECKBIT IS  87

1757:
CHECKBIT IS 50

1760:
CHECKBIT IS 56

1761:
CHECKBIT IS 30

1762:
CHECKBIT IS  87

1763:
CHECKBIT IS 50

1764:
CHECKBIT IS 56

1765:
CHECKBIT IS 30

1766:
CHECKBIT IS  87

1767:
CHECKBIT IS 50

1770:
CHECKBIT IS 56

1771:
CHECKBIT IS 30

1772:
CHECKBIT IS  87

1773:
CHECKBIT IS 50

1774:
CHECKBIT IS 56

1775:  R0←R0+1, STORE, RETURN
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MS = 42   60,64
.MC = 20   1
.MCONT = 2   6
CHECKBIT IS 30

1776:  M←OS, R0←2456+4*CPUNO, STORE
.TOSY = 1   57     (FOR CPU 0)         .TOSY = 1   57     (FOR CPU 1)
.LMY = 1   73                          .LMY = 1   73
.TXW = 1   4747                        .TXW = 1   47
.LR0 = 1   58                          .LR0 = 1   58
.MS = 42   60,64                       .MS = 42   60,64
.TCX = 1   43                          .TCX = 1   43
.C = 2456   31,33,36,38,39,40          .C = 2462   31,33,36,37,40
                                       CHECKBIT IS 87

1777:  GOTO SSTATE, STOP, .C←6B7
.MS = 76       60,61,62,63,64,
.MC = 20       1
.B = 775       9,10,11,12,13,14,15,17,
.C  = 60000000 18,19,
CHECKBIT IS 50
