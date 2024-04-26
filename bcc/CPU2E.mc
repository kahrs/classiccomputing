            ORG 400B;
*
*  SUBROUTINE M←CONTENTS(R0)

400:  LODX:      MFETCH, R0←E1, .VCY, DRETURN
.MCONT = 2   6
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.TE1Y = 1   88
CHECKBIT IS 30   
           
401:  GOTO PAGEF IF R0<0
.MC = 11   5,2
.B = 700    9,10,11,
*
*  SUBROUTINE CONTENTS(K←K+1)←M

402:  STORX:     R0←K←K+1
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70
.LR0 = 1   58
CHECKBIT IS 87   

403:  STOR1:     MAP, .VCY, R0←E1, GOTO CSTORE IF STERR
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 32   4,2,1
.VCY = 1   86
.TE1Y = 1   88
.B = 701    9,10,11,17,
CHECKBIT IS 30   
           
404:  STORE, RETURN
.MCONT = 2   6
.MS = 42   64,60
.MC = 1   5
CHECKBIT IS 30   
*
*  CHECK MULTIPLE WORD OPERAND

405:  DBL:       CLEARA, GOTO TI IF A
.MS = 50   62,60
.MC = 33   5,4,2,1
.B = 1400    8,9,
CHECKBIT IS 30   
           
406:  FETCH, RETURN
.MCONT = 2   6
.MS = 44   63,60
.MC = 1   5
CHECKBIT IS 30   
*

407:  TI1:       GOTO TI
.MC = 20   1
.B = 1400    8,9,
*

410:  SKIP:       Z←P←P+1, RESETCM, DGOTO NX3
.RRN = 1   68
.MS = 24   63,61
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 36    13,14,15,16,
CHECKBIT IS 30   
           
411:  R6←M←NI, Q←7B7, GOTO NX4 IF INTRPT, .TSPY←0
.SSP = 50   53,51
.TCX = 1   43
.TYW = 1   48
.LMY = 1   73
.LQX = 1   74
.LRN = 6   70,69
.MC = 41   5,0
.B = 31    13,14,17,
.C = -10000000   18,19,20,
CHECKBIT IS 50
*
*  THE SPECIAL CASES IN BRU ARE HANDLED HERE:
*  K[0]= NOT A AND NOT B, K[1]=A
*  NOTE HOW P IS RESTORED IF A TRAP SHOULD OCCUR
*

412:  BRUA:      Q←K, GOTO NX4 IF Y<0
.RRN = 3   68,67
.THY = 1   46
.LQY = 1   75
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 31    13,14,17,
CHECKBIT IS 30   
           
413:  P←S, Q LCY 1, GOTO TI IF X<0
.RRN = 2   67
.BL = 12   80,78
.MS = 1   65
.THY = 1   46
.TYW = 1   48
.LRN = 1   71
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1400    8,9,
CHECKBIT IS 30   
           
414:  R0←P←Z, GOTO NX5
.BR = 14   83,82
.TXW = 1   47
.LRN = 1   71
.LR0 = 1   58
.MC = 1   5
.B = 350    10,11,12,14,
CHECKBIT IS 30   
*

415:  EXUA:      DGOTO M AND Q LCY 4 MRG BASEAD, IR←XR
.MCONT = 3   7,6
.BL = 10   78
.SSP = 6   55,54
.MS = 4   63
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LRN = 4   69
.MC = 20   1
.DGO = 1   87
.C = 40   36,
           
416:  R6←M, Q←37777B, GOTO POP IF M[9]
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.MC = 71   5,2,1,0
.B = 353    10,11,12,14,16,17,
.C = 37777   28,29,30,31,32,33,34,35,36,37,38,39,40,41,
*
*
*  LDD, Q=SR, S=Q+1
*

417:  LDD1:      MFETCH, R0←E1, Z←40B, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TCX = 1   43
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
.C = 40   36,
           
420:  RES←AR, Z←Q AND Z, DGOTO NX1
.BR = 10   82
.SSP = 1   56
.TSPY = 1   45
.TYW = 1   48
.LZX = 1   76
.LRN = 5   71,69
.MC = 1   5
.DGO = 1   87
.B = 34    13,14,15,
CHECKBIT IS 30   
           
421:  BR←M, R0←S←S+1, GOTO LDD2 IF Z#0
.RRN = 2   67
.BL = 14   79,78
.SSP = 2   55
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 3   5,4
.B = 422    9,13,16,

422:  LDD2:      MFETCH, R0←E1, .VCY, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
           
423:  CR←M, R0←S←S+1, CALL LODX
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 3   56,55
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 20   1
.B = 400    9,
CHECKBIT IS 30   
           
424:  DR←M, Z←P←P+1, GOTO NX2
.RRN = 1   68
.BL = 14   79,78
.SSP = 4   54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 1   5
.B = 35    13,14,15,17,
*
*  XMA

425:  XMAA:      GOTO CSTORE IF R0[2]
.MC = 24   3,1
.B = 701    9,10,11,17,
CHECKBIT IS 30   
           
426:  Z←M, GOTO ROIA IF D
.BL = 14   79,78
.LZX = 1   76
.MC = 47   5,4,3,0
.B = 372    10,11,12,13,14,16,
           
427:  RES←AR←Z, M←K, GOTO STRFLD IF B
.RRN = 3   68,67
.BR = 14   83,82
.SSP = 1   56
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 5   71,69
.LSPX = 1   59
.MC = 34   3,2,1
.B = 362    10,11,12,13,16,
           
430:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 35    13,14,15,17,
*
*
*  STD, M=SR AND NOT TDFLAG, Z[18]=TDFLAG, K=Q+1
*

431:  STD1:      IR←M, Q←20B, GOTO CSTORE IF R0<0
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 11   5,2
.B = 701    9,10,11,17,
.C = 20   37,
           
432:  Q←Q AND M LCY 1, Q←IR, GOTO ROIA IF D
.RRN = 4   66
.BL = 10   78
.MS = 1   65
.THY = 1   46
.LQX = 1   74
.LQY = 1   75
.MC = 47   5,4,3,0
.B = 372    10,11,12,13,14,16,
CHECKBIT IS 30   
           
433:  STORE, M←AR, DGOTO STD2 IF NZ[18]
.SSP = 1   56
.MS = 42   64,60
.TSPY = 1   45
.LMY = 1   73
.MC = 15   5,3,2
.DGO = 1   87
.B = 437    9,13,14,15,16,17,
           
434:  M←BR, CALL STORX
.MCONT = 1   7
.SSP = 2   55
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 402    9,16,
           
435:  M←CR, CALL STORX
.MCONT = 1   7
.SSP = 3   56,55
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.B = 402    9,16,
CHECKBIT IS 30   
           
436:  M←DR, CALL STORX
.MCONT = 1   7
.SSP = 4   54
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 402    9,16,

437:  STD2:      Z←P←P+1, SR←Q, GOTO NX2
.RRN = 1   68
.BL = 12   80,78
.SSP = 11   56,53
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,
CHECKBIT IS 30   
*
*  CYCLIC SINGLE LEFT SHIFT

440:  CYAL1:     Z←R0, M←24
.TCX = 1   43
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
.C = 30   37,38,
CHECKBIT IS 87   

441:  CYAL:      R0←Z-M, Q←AR
.BR = 14   83,82
.BL = 3   81,80
.SSP = 1   56
.LOC = 1   50
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
442:  M←Q LCL Z, GOTO CYAL1 IF R0>=0
.BL = 12   80,78
.MS = 11   65,62
.LMX = 1   72
.MC = 12   4,2
.B = 440    9,12,
           
443:  RES←AR←M LCH Z, GOTO NX1
.BL = 14   79,78
.SSP = 1   56
.MS = 12   64,62
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
*
*  CYCLIC SINGLE RIGHT SHIFT
*

444:  CYAR2:     Z←Z+M, Q←AR, GOTO CYAR1
.BR = 14   83,82
.BL = 14   79,78
.SSP = 1   56
.TSPY = 1   45
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.VCY = 1   86
.B = 446    9,12,15,16,

445:  CYAR:      Z←M+ NOT Q OR Z, Q←AR
.BR = 15   85,83,82
.BL = 14   79,78
.SSP = 1   56
.TSPY = 1   45
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.VCY = 1   86

446:  CYAR1:     Q←Q LCL Z, GOTO CYAR2 IF Z<0
.BL = 12   80,78
.MS = 11   65,62
.LQX = 1   74
.MC = 4   3
.B = 444    9,12,15,
           
447:  RES←AR←Q LCH Z, GOTO NX1
.BL = 12   80,78
.SSP = 1   56
.MS = 12   64,62
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
*
*  CYCLIC DOUBLE LEFT SHIFT
*

450:  CYDL1:     Z←R0, BR←M, DGOTO CYDL3
.BL = 14   79,78
.SSP = 2   55
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 453    9,12,14,16,17,
           
451:  M←Q, Q←24
.BL = 12   80,78
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 30   37,38,

452:  CYDL:      Q←24, M←AR
.SSP = 1   56
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.C = 30   37,38,

453:  CYDL3:     R0←Z-Q
.BR = 14   83,82
.BL = 5   81,79
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.VCY = 1   86
           
454:  R6←M LCL Z, Q←BR, GOTO CYDL1 IF R0>=0
.BL = 14   79,78
.SSP = 2   55
.MS = 11   65,62
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.MC = 12   4,2
.B = 450    9,12,14,
           
455:  M←R6, DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.RRN = 6   67,66
.BR = 14   83,82
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
456:  R6←M LCH Z, M←BR, DGOTO CYDL2
.BL = 14   79,78
.SSP = 2   55
.MS = 12   64,62
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LRN = 6   70,69
.MC = 1   5
.DGO = 1   87
.B = 457    9,12,14,15,16,17,
CHECKBIT IS 30   

457:  CYDL2:     M←M LCL Z
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
           
460:  M←M LCH Z, Z←R6
.RRN = 6   67,66
.BL = 14   79,78
.MS = 12   64,62
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
           
461:  BR←M AND NOT Q ! Q AND Z, DGOTO NX1
.BR = 10   82
.BL = 4   79
.SSP = 2   55
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 34    13,14,15,
           
462:  RES←AR←M AND Q ! NOT Q AND Z
.BR = 4   83
.BL = 10   78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
*
*  CYCLIC DOUBLE RIGHT SHIFT
*    SHIFT LEFT 48-COUNT.
*

463:  CYDR:      M←48, DGOTO CYDL
.TCX = 1   43
.LMX = 1   72
.MC = 20   1
.DGO = 1   87
.B = 452    9,12,14,16,
.C = 60   36,37,
CHECKBIT IS 50   
           
464:  Z←M+NOT Q OR Z
.BR = 15   85,83,82
.BL = 14   79,78
.TAX = 1   49
.LZX = 1   76
.VCY = 1   86
CHECKBIT IS 87   
*
*  ARITHMETIC SINGLE LEFT SHIFT (LOGICAL IF A)
*  Z=SHIFT COUNT

465:  ASAL1:     Z←24
.TCX = 1   43
.LZX = 1   76
.C = 30   37,38,
CHECKBIT IS 87   

466:  ASAL:      M←AR, Q←24
.SSP = 1   56
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.C = 30   37,38,
           
467:  R0←Z-Q
.BR = 14   83,82
.BL = 5   81,79
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.VCY = 1   86
           
470:  M←M LCL Z, GOTO ASAL5 IF R0>=0
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
.MC = 12   4,2
.B = 675    9,10,12,13,14,15,17,
CHECKBIT IS 30   
           
471:  DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.BR = 14   83,82
.TCX = 1   43
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
472:  M←M LCH Z, DGOTO ASAL2
.BL = 14   79,78
.MS = 12   64,62
.LMX = 1   72
.MC = 1   5
.DGO = 1   87
.B = 473    9,12,13,14,16,17,

473:  ASAL2:     RES←AR←M AND NOT Q, GOTO NX1 IF A
.BL = 4   79
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 33   5,4,2,1
.B = 34    13,14,15,

474:  ASAL3:     Q←Q MRG 4B7, DGOTO ASAL4
.BL = 12   80,78
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 673    9,10,12,13,14,16,17,
.C = -40000000       18
CHECKBIT IS 50   
           
475:  NOT M AND Q, GOTO NX1 IF LB=0
.BL = 2   80
.MC = 22   4,1
.B = 34    13,14,15,
CHECKBIT IS 30   

476:  SPILL:     M←Q←4B7, DGOTO NX1
.TCX = 1   43
.LMX = 1   72
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 34    13,14,15,
.C = -40000000   18,
           
477:  M+Q, ROV
.BR = 12   84,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.TAX = 1   49
.VCY = 1   86
CHECKBIT IS 87   
*
*  ARITHMETIC SINGLE RIGHT SHIFT, (LOGICAL IF A)
*

500:  ASAR:      M←24
.TCX = 1   43
.LMX = 1   72
.C = 30   37,38,
CHECKBIT IS 87   
           
501:  Z←M+NOT Q OR Z, R0←M←AR
.BR = 15   85,83,82
.BL = 14   79,78
.SSP = 1   56
.TSPY = 1   45
.TYW = 1   48
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LR0 = 1   58
.VCY = 1   86
           
502:  M←M LCL Z, Q←0, GOTO ASAR1 IF Z<0
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
.LQY = 1   75
.MC = 4   3
.B = 505    9,11,15,17,
           
503:  DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.BR = 14   83,82
.TCX = 1   43
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
504:  M←M LCH Z, DGOTO ASAR1
.BL = 14   79,78
.MS = 12   64,62
.LMX = 1   72
.MC = 1   5
.DGO = 1   87
.B = 505    9,11,15,17,

505:  ASAR1:     RES←AR←M AND Q, DGOTO NX1 IF A
.BL = 10   78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 34    13,14,15,
CHECKBIT IS 30   
           
506:  GOTO NX1 IF R0>=0
.MC = 12   4,2
.B = 34    13,14,15,
           
507:  RES←AR←M OR NOT Q, GOTO NX1
.BL = 15   81,79,78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
CHECKBIT IS 30   
*
*  ARITHMETIC DOUBLE LEFT SHIFT, (LOGICAL IF A)
*  M=BR, Z=SHIFT COUNT
*

510:  LSHDL:     Z←R0, M←0
.THY = 1   46
.LMX = 1   72
.LZY = 1   77

511:  ASDL:      BR←0, Q←24
.SSP = 2   55
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.C = 30   37,38,
CHECKBIT IS 87   

512:  ASDL1:     R0←Z-Q
.BR = 14   83,82
.BL = 5   81,79
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.VCY = 1   86
           
513:  Q←M LCL Z, GOTO ASDL3 IF R0>=0
.BL = 14   79,78
.MS = 11   65,62
.LQX = 1   74
.MC = 12   4,2
.B = 524    9,11,13,15,
           
514:  DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.BR = 14   83,82
.TCX = 1   43
.MC = 1   5
.DGO = 1   87
.C = 140   35,36,
           
515:  M←Q LCH Z, DGOTO ASDL2
.BL = 12   80,78
.MS = 12   64,62
.LMX = 1   72
.MC = 20   1
.DGO = 1   87
.B = 516    9,11,14,15,16,
CHECKBIT IS 30   

516:  ASDL2:     BR←M AND NOT Q
.BL = 4   79
.SSP = 2   55
.LSPX = 1   59
           
517:  R6←M AND Q, M←AR
.BL = 10   78
.SSP = 1   56
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LRN = 6   70,69
           
520:  M←M LCL Z
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
           
521:  M←M LCH Z, Z←R6
.RRN = 6   67,66
.BL = 14   79,78
.MS = 12   64,62
.THY = 1   46
.LMX = 1   72
.LZY = 1   77

522:  DGOTO ASAL3
.MC = 1   5
.DGO = 1   87
.B = 474    9,12,13,14,15,
           
523:  RES←AR←M AND NOT Q+Z, GOTO NX1 IF A
.BR = 14   83,82
.BL = 4   79
.SSP = 1   56
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 33   5,4,2,1
.VCY = 1   86
.B = 34    13,14,15,
*
*  SHIFT COUNT>24, OVERFLOW IF NOT(AR=0 OR
*    AR=-1 AND ((AR EOR BR) AND 4B7=0));
*

524:  ASDL3:     Z←AR
.SSP = 1   56
.TSPY = 1   45
.LZY = 1   77
           
525:  AR←M, GOTO LSHDL IF A
.BL = 14   79,78
.SSP = 1   56
.LSPX = 1   59
.MC = 33   5,4,2,1
.B = 510    9,11,14,
           
526:  Q←Z←NOT Z, GOTO ASDL4 IF Z=0
.BR = 3   85,84
.LQX = 1   74
.LZX = 1   76
.MC = 2   4
.B = 532    9,11,13,14,16,
CHECKBIT IS 30   
           
527:  GOTO ASDL4 IF Z=0
.MC = 2   4
.B = 532    9,11,13,14,16,
CHECKBIT IS 30   

530:  ASDL5:     Z←M←4B7, DGOTO ASDL
.TCX = 1   43
.LMX = 1   72
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 511    9,11,14,17,
.C = -40000000   18,
CHECKBIT IS 50   
           
531:  Z←R0, M←Z+M, ROV
.BR = 14   83,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.THY = 1   46
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.VCY = 1   86

532:  ASDL4:     Z←Q EOR M, DGOTO ASDL
.BL = 6   80,79
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 511    9,11,14,17,
           
533:  Z←R0, M←0, GOTO ASDL5 IF Z>=0
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
.MC = 5   5,3
.B = 530    9,11,13,14,
*
*  ARITHMETIC DOUBLE RIGHT SHIFT
*  M=BR, Z=NEGATIVE SHIFT COUNT AND 77B, Q=77B
*

534:  ASDR:      Z←Z OR NOT Q, Q←24, GOTO NX1 IF Z=0
.BR = 15   85,83,82
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 2   4
.B = 34    13,14,15,
.C = 30   37,38,
CHECKBIT IS 50   
           
535:  Z←Q+Z
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LZX = 1   76
.VCY = 1   86

536:  ASDR1:     M←M LCL Z, GOTO ASDR3 IF Z<=0
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
.MC = 13   5,4,2
.B = 547    9,11,12,15,16,17,
CHECKBIT IS 30   
           
537:  DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.BR = 14   83,82
.TCX = 1   43
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
540:  M←M LCH Z, DGOTO ASDR2
.BL = 14   79,78
.MS = 12   64,62
.LMX = 1   72
.MC = 1   5
.DGO = 1   87
.B = 541    9,11,12,17,

541:  ASDR2:     R6←M AND Q, M←AR
.BL = 10   78
.SSP = 1   56
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LRN = 6   70,69
           
542:  M←M LCL Z, R0←AR
.BL = 14   79,78
.SSP = 1   56
.MS = 11   65,62
.TSPY = 1   45
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
           
543:  M←M LCH Z, Z←R6
.RRN = 6   67,66
.BL = 14   79,78
.MS = 12   64,62
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
           
544:  BR←M AND NOT Q ! Z, DGOTO NX1 IF A
.BR = 14   83,82
.BL = 4   79
.SSP = 2   55
.LSPX = 1   59
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 34    13,14,15,
           
545:  RES←AR←M AND Q, GOTO NX1 IF R0>=0
.BL = 10   78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 12   4,2
.B = 34    13,14,15,
           
546:  RES←AR←M OR NOT Q, GOTO NX1
.BL = 15   81,79,78
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.B = 34    13,14,15,
CHECKBIT IS 30   
*
*  SHIF COUNT>24.  EXTEND THE SIGN OF AR.
*

547:  ASDR3:     R0←M←AR, Z←Q+Z, GOTO *+2 IF A
.BR = 14   83,82
.BL = 12   80,78
.SSP = 1   56
.TSPY = 1   45
.TYW = 1   48
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LR0 = 1   58
.MC = 33   5,4,2,1
.VCY = 1   86
.B = 551    9,11,12,14,17,
           
550:  AR←-1, GOTO ASDR1 IF R0<0
.BL = 17   81,80,79,78
.SSP = 1   56
.LSPX = 1   59
.MC = 11   5,2
.B = 536    9,11,13,14,15,16,
CHECKBIT IS 30   
           
551:  AR←0, GOTO ASDR1
.SSP = 1   56
.LSPX = 1   59
.MC = 20   1
.B = 536    9,11,13,14,15,16,
*  MULTIPLY NEGATIVE MULTIPLIER

552:  MUL2:      M←NOT M+1, HROV, DGOTO MUL1
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 557    9,11,12,14,15,16,17,
CHECKBIT IS 30   
           
553:  K←4B7, GOTO MUL6 IF OVFLW
.TCX = 1   43
.TXW = 1   47
.LRN = 3   71,70
.MC = 57   5,4,3,2,0
.B = 570    9,11,12,13,14,
.C = -40000000   18,
*  MULTIPLY NEGATIVE MULTIPLICAND

554:  MUL3:      K←NOT Z, Q←0, GOTO MUL5 IF OVFLW
.BR = 3   85,84
.TXW = 1   47
.LQY = 1   75
.LRN = 3   71,70
.MC = 57   5,4,3,2,0
.B = 563    9,11,12,13,16,17,
           
555:  Q←R0, DGOTO MUL4
.THY = 1   46
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
.B = 560    9,11,12,13,
CHECKBIT IS 30   
           
556:  MH1←Q, .LSPX←0
.BL = 12   80,78
.SSP = 40   51
*  MULTIPLY POSITIVE MULTIPLIER

557:  MUL1:      Z←K, R0←NOT Q+1, HROV, GOTO MUL3 IF R0<0
.RRN = 3   68,67
.BL = 5   81,79
.MS = 57   65,64,63,62,60
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LR0 = 1   58
.MC = 11   5,2
.B = 554    9,11,12,14,15,
CHECKBIT IS 30   

560:  MUL4:      MH2←M LCY 1, .LSPX←0
.BL = 14   79,78
.SSP = 43   56,55,51
.MS = 1   65
CHECKBIT IS 87
           
561:  M←MHR, .TSPY←0
.SSP = 44   54,51
.LMY = 1   73
           
562:  Q←MMR, .TSPY←0
.SSP = 45   56,54,51
.LQY = 1   75
CHECKBIT IS 87   

563:  MUL5:      IR←AR←M, K, GOTO MUL10 IF Y<0
.RRN = 3   68,67
.BL = 14   79,78
.SSP = 1   56
.THY = 1   46
.TXW = 1   47
.LRN = 4   69
.LSPX = 1   59
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 571    9,11,12,13,14,17,
CHECKBIT IS 30   

564:  MUL7:      BR←Q, M←IR
.RRN = 4   66
.BL = 12   80,78
.SSP = 2   55
.THY = 1   46
.LMY = 1   73
.LSPX = 1   59
           
565:  Q←Q LCY 20
.BL = 12   80,78
.MS = 10   62
.LQX = 1   74
CHECKBIT IS 87   
           
566:  Q←Q LCY 3, Z←P←P+1, DGOTO NX2
.RRN = 1   68
.BL = 12   80,78
.MS = 3   65,64
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
           
567:  RES←M OR Q
.BL = 16   80,79,78
.TXW = 1   47
.LRN = 5   71,69
CHECKBIT IS 87   
*

570:  MUL6:      M←Q, Q←0, GOTO MUL5
.BL = 12   80,78
.LMX = 1   72
.LQY = 1   75
.MC = 1   5
.B = 563    9,11,12,13,16,17,

571:  MUL10:     Q←NOT Q+1, HROV
.BL = 5   81,79
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LQX = 1   74
CHECKBIT IS 87   
           
572:  AR←IR←NOT M+1, ROV, GOTO MUL7 IF CARRY
.BL = 3   81,80
.SSP = 1   56
.MS = 66   64,63,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 4   69
.LSPX = 1   59
.MC = 61   5,1,0
.VCY = 1   86
.B = 564    9,11,12,13,15,
           
573:  AR←IR←NOT M, GOTO MUL7
.BL = 3   81,80
.SSP = 1   56
.TXW = 1   47
.LRN = 4   69
.LSPX = 1   59
.MC = 20   1
.B = 564    9,11,12,13,15,
CHECKBIT IS 30   
*

574:  DIV4:      IR←Q←NOT Q+1, GOTO DIV5
.BL = 5   81,79
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LQX = 1   74
.LRN = 4   69
.MC = 1   5
.B = 602    9,10,16,

575:  DIV2:      Z←Z+1, HROV, DGOTO DIV1
.BR = 14   83,82
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 600    9,10,
           
576:  M←NOT M, GOTO DIV3 IF CARRY
.BL = 3   81,80
.LMX = 1   72
.MC = 61   5,1,0
.B = 577    9,11,12,13,14,15,16,17,
CHECKBIT IS 30   

577:  DIV3:      M←M+1
.BL = 14   79,78
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
CHECKBIT IS 87   

600:  DIV1:      IR, Q←40000026B, DGOTO DIV4 IF Y<0
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.MC = 53   5,4,2,0
.DGO = 1   87
.VCY = 1   86
.B = 574    9,11,12,13,14,15,
.C = -37777752   18,37,39,40,
CHECKBIT IS 50   
           
601:  R0←NOT Q, Q←IR
.RRN = 4   66
.BL = 5   81,79
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58

602:  DIV5:      R6←M-Q, Q←1, GOTO DIV6 IF X<0
.BR = 5   85,83
.BL = 14   79,78
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 6   70,69
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 607    9,10,15,16,17,
.C = 1   41,
CHECKBIT IS 56   
           
603:  M←R6, Q←Z MRG 1, GOTO DIV7 IF A
.RRN = 6   67,66
.BR = 14   83,82
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 33   5,4,2,1
.B = 617    9,10,14,15,16,17,
.C = 1   41,
           
604:  Z←Q LCY 1, R0←R0+1, GOTO DIV8 IF R0<0
.BL = 12   80,78
.MS = 1   65
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 11   5,2
.B = 613    9,10,14,16,17,
           
605:  M←M LCY 1, Q←1, DGOTO DIV5
.BL = 14   79,78
.MS = 1   65
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
.B = 602    9,10,16,
.C = 1   41,
CHECKBIT IS 50   
           
606:  M←M AND NOT Q ! Z AND Q, Q←IR
.RRN = 4   66
.BR = 10   82
.BL = 4   79
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
CHECKBIT IS 87   

607:  DIV6:      Q←Z AND NOT Q, CLEARA, R0←R0+1, GOTO DIV8 IF R0<0
.BR = 4   83
.MS = 50   62,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 11   5,2
.B = 613    9,10,14,16,17,
           
610:  M←M LCY 1
.BL = 14   79,78
.MS = 1   65
.LMX = 1   72
CHECKBIT IS 87   
           
611:  Z←Q LCY 1, Q←1, DGOTO DIV5
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.B = 602    9,10,16,
.C = 1   41,
CHECKBIT IS 50   
           
612:  M←M AND NOT Q ! Z AND Q, Q←IR
.RRN = 4   66
.BR = 10   82
.BL = 4   79
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
CHECKBIT IS 87   

613:  DIV8:      AR←RES←Q, S, GOTO DIV9 IF Y<0
.RRN = 2   67
.BL = 12   80,78
.SSP = 1   56
.THY = 1   46
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 616    9,10,14,15,16,

614:  DIV10:     BR←NOT M+1, GOTO NX1 IF D
.BL = 3   81,80
.SSP = 2   55
.LOC = 1   50
.TAX = 1   49
.LSPX = 1   59
.MC = 47   5,4,3,0
.VCY = 1   86
.B = 34    13,14,15,
CHECKBIT IS 30   
           
615:  BR←M, Z←P←P+1, GOTO NX2
.RRN = 1   68
.BL = 14   79,78
.SSP = 2   55
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,

616:  DIV9:      AR←RES←NOT Q+1, GOTO DIV10
.BL = 5   81,79
.SSP = 1   56
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.B = 614    9,10,14,15,
CHECKBIT IS 30   

617:  DIV7:      Q←Q-1
.BR = 17   85,84,83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
620:  M OR Q, GOTO SPILL IF LB#0, Z←S
.RRN = 2   67
.BL = 16   80,79,78
.THY = 1   46
.LZY = 1   77
.MC = 23   5,4,1
.B = 476    9,12,13,14,15,16,
           
621:  GOTO SPILL IF Z>=0, M←S←4B7
.TCX = 1   43
.TXW = 1   47
.LMX = 1   72
.LRN = 2   70
.MC = 5   5,3
.B = 476    9,12,13,14,15,16,
.C = -40000000   18,
           
622:  RES←AR←M, M←0, GOTO DIV10
.BL = 14   79,78
.SSP = 1   56
.TXW = 1   47
.LMY = 1   73
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.B = 614    9,10,14,15,
*
*  ISD, IR=MAPPED Q, R0=Q+1
*    Q=Z=CONTENTS(Q)
*

623:  ISD1:      MFETCH, .VCY, R0←E1, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
           
624:  R6←Z
.BR = 14   83,82
.TXW = 1   47
.LRN = 6   70,69
           
625:  Q←NOT Q LCY 2 MRG 63777777B
.BL = 5   81,79
.MS = 2   64
.TCX = 1   43
.LQX = 1   74
.C = -14000001   18,19,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
626:  Q←Z+NOT Q, Z←54B6
.BR = 14   83,82
.BL = 5   81,79
.TCY = 1   44
.TAX = 1   49
.LQX = 1   74
.LZY = 1   77
.VCY = 1   86
.C = -24000000   18,20,21,
CHECKBIT IS 87   
           
627:  R0←Q-Z, Q←Z←R6
.RRN = 6   67,66
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LZY = 1   77
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
630:  RES←Z←M-Z, M←1B6, DGOTO ISD2 IF R0<0
.BR = 3   85,84
.BL = 14   79,78
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LRN = 5   71,69
.MC = 11   5,2
.DGO = 1   87
.VCY = 1   86
.B = 635    9,10,13,14,15,17,
.C = 1000000   23,
           
631:  Z←Q, GOTO NX1 IF Z=0
.BL = 12   80,78
.LZX = 1   76
.MC = 2   4
.B = 34    13,14,15,
           
632:  R0←IR, Q←3B6, GOTO CSTORE IF STERR, .VCY
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
.C = 3000000   22,23,
CHECKBIT IS 50

633:  M←Z AND NOT Q+1, DGOTO NX2
.BR = 4   83
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 20   1
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
634:  STORE, Z←P←P+1
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
CHECKBIT IS 87   

635:  ISD2:      R0←IR, M←M+Z, GOTO CSTORE IF STERR
.RRN = 4   66
.BR = 14   83,82
.BL = 14   79,78
.THY = 1   46
.TYW = 1   48
.TAX = 1   49
.LMX = 1   72
.LR0 = 1   58
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
CHECKBIT IS 30   
           
636:  STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 35    13,14,15,17,
*
*  DSD, R0=Q+1, Z=CONTENTS(Q)
*

637:  DSD1:      MFETCH, .VCY, R0←E1, Q←3B6, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TCX = 1   43
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
.C = 3000000   22,23,
CHECKBIT IS 50   
           
640:  R6←Z
.BR = 14   83,82
.TXW = 1   47
.LRN = 6   70,69
           
641:  Z←M AND Q, Q←R6
.RRN = 6   67,66
.BL = 10   78
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
CHECKBIT IS 87   
           
642:  RES←Z←Q-M, Q←14B6, DGOTO DSD2 IF Z=0
.BR = 12   84,82
.BL = 3   81,80
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.LRN = 5   71,69
.MC = 2   4
.DGO = 1   87
.VCY = 1   86
.B = 646    9,10,12,15,16,
.C = 14000000   20,21,
CHECKBIT IS 56   
           
643:  Z←1B6, GOTO NX1 IF Z=0
.TCX = 1   43
.LZX = 1   76
.MC = 2   4
.B = 34    13,14,15,
.C = 1000000   23,
           
644:  M←M-Z, R0, GOTO CSTORE IF STERR
.BR = 3   85,84
.BL = 14   79,78
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMX = 1   72
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
CHECKBIT IS 30   

645:  DSD3:      STORE, Z←P←P+1, GOTO NX2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,

646:  DSD2:      Z←M AND Q, R0, GOTO CSTORE IF STERR, .VCY
.BL = 10   78
.THY = 1   46
.LZX = 1   76
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
           
647:  Q←Q-Z
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
CHECKBIT IS 87   
           
650:  Q←Q LCY 20
.BL = 12   80,78
.MS = 10   62
.LQX = 1   74
CHECKBIT IS 87   
           
651:  Q←Q LCY 2, DGOTO DSD3
.BL = 12   80,78
.MS = 2   64
.LQX = 1   74
.MC = 20   1
.DGO = 1   87
.B = 645    9,10,12,15,17,
           
652:  M←M OR Q-1
.BR = 17   85,84,83,82
.BL = 16   80,79,78
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
CHECKBIT IS 87   
*

653:  SRS1:      Z←R0
.THY = 1   46
.LZY = 1   77
CHECKBIT IS 87   
           
654:  M←M, M←Q←IR, GOTO SRS2 IF Z#0
.RRN = 4   66
.BL = 14   79,78
.THY = 1   46
.LMX = 1   72
.LMY = 1   73
.LQY = 1   75
.MC = 3   5,4
.B = 656    9,10,12,14,15,16,
           
655:  M←M AND NOT Q
.BL = 4   79
.LMX = 1   72
CHECKBIT IS 87   

656:  SRS2:      CALL LOADSR
.MCONT = 1   7
.MC = 1   5
.B = 666    9,10,12,13,15,16,
CHECKBIT IS 30   

657:  NEXT:      Z←P←P+1, GOTO NX2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
*

660:  TSB1:      Q←IR, DGOTO NX1
.RRN = 4   66
.THY = 1   46
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 34    13,14,15,
CHECKBIT IS 30   
           
661:  RES←M AND Q
.BL = 10   78
.TXW = 1   47
.LRN = 5   71,69
CHECKBIT IS 87   
*
*

662:  GETSR:     Q←4B7, M←Z←RES
.RRN = 5   68,66
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.LZY = 1   77
.C = -40000000   18,
CHECKBIT IS 87   

663:  GETS1:     M←Z←NOT M AND Q LCY 12, Q←2B7, GOTO GETS1 IF Z=0
.BL = 2   80
.MS = 6   64,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.LZX = 1   76
.MC = 2   4
.B = 663    9,10,12,13,16,17,
.C = 20000000   19,
           
664:  M←SR, Q←7017B, DRETURN
.MCONT = 2   6
.SSP = 11   56,53
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.C = 7017   30,31,32,38,39,40,41,
CHECKBIT IS 50   
           
665:  M←M AND NOT Q ! Z, M←E1, GSB
.BR = 14   83,82
.BL = 4   79
.MS = 27   65,64,63,61
.LMX = 1   72
.LMY = 1   73
.TE1Y = 1   88
CHECKBIT IS 87   
*

666:  LOADSR:    Z←SR←M, Q←6B3
.BL = 14   79,78
.SSP = 11   56,53
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.LSPX = 1   59
.C = 6000   30,31,
CHECKBIT IS 87   
           
667:  M←M AND Q, Q←2B3, PSB, DRETURN
.MCONT = 2   6
.BL = 10   78
.MS = 55   65,63,62,60
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
.C = 2000   31,
CHECKBIT IS 50   
           
670:  RES←M-Q
.BR = 5   85,83
.BL = 14   79,78
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.VCY = 1   86
CHECKBIT IS 87   
*
*

671:  ICP1:      RES←M EOR Q MRG 1, GOTO NX2
.BL = 6   80,79
.TCX = 1   43
.TXW = 1   47
.LRN = 5   71,69
.MC = 20   1
.B = 35    13,14,15,17,
.C = 1   41,
CHECKBIT IS 50   

672:  DIV11:     Z←NOT Z MRG 1, SETD, GOTO DIV2
.BR = 3   85,84
.MS = 36   64,63,62,61
.TCX = 1   43
.LZX = 1   76
.MC = 1   5
.B = 575    9,11,12,13,14,15,17,
.C = 1   41,

673:  ASAL4:     M AND Q, M←1, GOTO NX1 IF LB=0
.BL = 10   78
.TCY = 1   44
.LMY = 1   73
.MC = 22   4,1
.B = 34    13,14,15,
.C = 1   41,
           
674:  M, GOTO SPILL IF LB#0
.BL = 14   79,78
.MC = 23   5,4,1
.B = 476    9,12,13,14,15,16,

675:  ASAL5:     RES←AR←0, GOTO NX1 IF A, .VCY
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 33   5,4,2,1
.VCY = 1       86
.B = 34    13,14,15,
           
676:  M, GOTO SPILL IF LB#0
.BL = 14   79,78
.MC = 23   5,4,1
.B = 476    9,12,13,14,15,16,
            
677:  GOTO NX1
.MC = 20   1
.B = 34    13,14,15,
CHECKBIT IS 30   

