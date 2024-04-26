           ORG 2400B;
*
*
*  OPR TABLE:
*   THIS TABLE IS INDEXED BY A POSITIVE OPR OPERAND.
*   IF IT IS GREATER OR EQUAL THAN MAXOPR, THE OPR
*   IS PRIVILEGED.
*   Z=AR, Q=BR, M=XR IF NOT PRIVILEGED
*

2400:  BASEOPR:   BR←Z, Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.BR = 14   83,82
.SSP = 2   55
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 1   5
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
2401:  BR←Z
.BR = 14   83,82
.SSP = 2   55
.LSPX = 1   59
CHECKBIT IS 87   

2402:  CBA:       RES←AR←Q, SETBA, GOTO NX1
.BL = 12   80,78
.SSP = 1   56
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.B = 34    13,14,15,
CHECKBIT IS 30   

2403:  CBX:       XR←Q, Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.BL = 12   80,78
.SSP = 6   55,54
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,
           
2404:  XR←Q
.BL = 12   80,78
.SSP = 6   55,54
.LSPX = 1   59
           
2405:  BR←M, Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.BL = 14   79,78
.SSP = 2   55
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
2406:  XR←Z, Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.BR = 14   83,82
.SSP = 6   55,54
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 1   5
.B = 35    13,14,15,17,
           
2407:  XR←Z
.BR = 14   83,82
.SSP = 6   55,54
.LSPX = 1   59

2410:  CXA:       RES←AR←M, SETBA, GOTO NX1
.BL = 14   79,78
.SSP = 1   56
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.B = 34    13,14,15,
CHECKBIT IS 30   
           
2411:  RES←AR←NOT Z+1, SETBA, GOTO NX1
.BR = 3   85,84
.SSP = 1   56
.MS = 61   65,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.VCY = 1   86
.B = 34    13,14,15,
           
2412:  XR←NOT M+1, Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.BL = 3   81,80
.SSP = 6   55,54
.MS = 61   65,61,60
.LOC = 1   50
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.TAX = 1   49
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 1   5
.VCY = 1   86
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
2413:  RES←AR←0, SETBA, GOTO NX1
.SSP = 1   56
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
CHECKBIT IS 30   
           
2414:  AR←0, GOTO ZAB1
.SSP = 1   56
.LSPX = 1   59
.MC = 1   5
.B = 517    9,11,14,15,16,17,
           
2415:  BR←0, Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.SSP = 2   55
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
2416:  Q←GR, GOTO CBA
.SSP = 10   53
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.B = 402    9,16,
CHECKBIT IS 30   
           
2417:  Q←GR, GOTO XGA1
.SSP = 10   53
.TSPY = 1   45
.LQY = 1   75
.MC = 20   1
.B = 515    9,11,14,15,17,
           
2420:  Q←LR, GOTO CBA
.SSP = 7   56,55,54
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.B = 402    9,16,
CHECKBIT IS 30   
           
2421:  Q←LR, GOTO XLA1
.SSP = 7   56,55,54
.TSPY = 1   45
.LQY = 1   75
.MC = 20   1
.B = 516    9,11,14,15,16,
           
2422:  GOTO CSA1
.MC = 1   5
.B = 612    9,10,14,16,
           
2423:  GOTO XSA1
.MC = 20   1
.B = 614    9,10,14,15,
           
2424:  Z←ITR, ALERT, GOTO CTA1
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.MC = 1   5
.B = 617    9,10,14,15,16,17,
.C = 60   36,37,
           
2425:  Z←CTCR, ALERT, GOTO CTA1
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.MC = 20   1
.B = 617    9,10,14,15,16,17,
.C = 62   36,37,40,
CHECKBIT IS 50   

2426:  NOP:       Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
2427:  R0←Q-M, GOTO MVB1, SETA
.BR = 12   84,82
.BL = 3   81,80
.MS = 30   62,61
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 620    9,10,13,
           
2430:  R0←0, GOTO MVB1
.TXW = 1   47
.LR0 = 1   58
.MC = 1   5
.B = 620    9,10,13,
CHECKBIT IS 30   
;                                  * MVS
2431:                          *ILLEGAL INSTRUCTION
CHECKBIT IS 30

           
2432:  SETBA, GOTO TI
.MS = 61   65,61,60
.MC = 1   5
.B = 1400    8,9,
CHECKBIT IS 30   
           
2433:  M←NOT Q LCY 4 MRG 1774B, GOTO CLS1
.BL = 5   81,79
.MS = 4   63
.TCX = 1   43
.LMX = 1   72
.MC = 20   1
.B = 534    9,11,13,14,15,
.C = 1774   32,33,34,35,36,37,38,39,
           
2434:  Z←R6, S←M←Z, GOTO ASP1
.RRN = 6   67,66
.BR = 14   83,82
.THY = 1   46
.TXW = 1   47
.LMX = 1   72
.LZY = 1   77
.LRN = 2   70
.MC = 1   5
.B = 661    9,10,12,13,17,

2435:  LLT:       M←Z, GOTO LLT1
.BR = 14   83,82
.LMX = 1   72
.MC = 20   1
.B = 636    9,10,13,14,15,16,
CHECKBIT IS 30   
           
2436:  M←Q, R0←0, SETA, GOTO COB1
.BL = 12   80,78
.MS = 30   62,61
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
.MC = 1   5
.B = 664    9,10,12,13,15,
;                                  * [GRET]

2437:                          *ILLEGAL MICROINSTRUCTION
CHECKBIT IS 30

           
2440:  SETBA, GOTO TI
.MS = 61   65,61,60
.MC = 1   5
.B = 1400    8,9,
CHECKBIT IS 30   
           
2441:  IR←SR, GOTO LODS1
.SSP = 11   56,53
.TSPY = 1   45
.TYW = 1   48
.LRN = 4   69
.MC = 20   1
.B = 670    9,10,12,13,14,
           
2442:  GOTO STRS1
.MC = 1   5
.B = 701    9,10,11,17,
           
2443:  GOTO LSC1
.MC = 20   1
.B = 650    9,10,12,14,
           
2444:  S←Z, GOTO FIX1
.BR = 14   83,82
.TXW = 1   47
.LRN = 2   70
.MC = 1   5
.B = 1244    8,10,12,15,
           
2445:  M←BR←0, GOTO FLOAT1
.SSP = 2   55
.LMX = 1   72
.LSPX = 1   59
.MC = 20   1
.B = 1250    8,10,12,14,
CHECKBIT IS 30   
           
2446:  BR←NOT Q+1, HROV, GOTO FNA1
.BL = 5   81,79
.SSP = 2   55
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LSPX = 1   59
.MC = 1   5
.B = 1246    8,10,12,15,16,
           
2447:  R0←2406B+CPUNO, SETBA, GOTO BREAK
.MS = 61   65,61,60     (FOR CPU 0)    .MS = 61   65,61,60     (FOR CPU 1)
.TCX = 1   43                          .TCX = 1   43
.TXW = 1   47                          .TXW = 1   47
.LR0 = 1   58                          .LR0 = 1   58
.MC = 1   5                            .MC = 1   5
.B = 1700    8,9,10,11,                .B = 1700   8,9,10,11,
.C = 2406   31,33,39,40,               .C = 2407   31,33,39,40,41,
                                       CHECKBIT IS 50   

2450:  
CHECKBIT IS 56

2451:
CHECKBIT IS 30

2452:
CHECKBIT IS 87

2453:                          *ILLEGAL MICROINSTRUCTION
CHECKBIT IS 30

2454:  SETBA, GOTO TI
.MS = 61   65,61,60
.MC = 1   5
.B = 1400    8,9,
CHECKBIT IS 30   
*                                                   MAXOPR
           
2455:  X23LCK, Q←1, GOTO SLOK1
.MS = 71   65,62,61,60
.LOC = 1   50
.TAX = 1   49
.LQX = 1   74
.MC = 20   1
.B = 710    9,10,11,14,
CHECKBIT IS 30   
           
2456:  X23LCK, Z←4, GOTO UNPRO
.MS = 71   65,62,61,60
.TCX = 1   43
.LZX = 1   76
.MC = 1   5
.B = 464    9,12,13,15,
.C = 4   39,
CHECKBIT IS 50   
           
2457:  R0←XR, FETCH, GOTO CXA
.SSP = 6   55,54
.MS = 44   63,60
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.B = 410    9,14,
CHECKBIT IS 30   
           
2460:  R0←XR, M←Z, STORE, GOTO NOP
.BR = 14   83,82
.SSP = 6   55,54
.MS = 42   64,60
.TSPY = 1   45
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
.MC = 1   5
.B = 426    9,13,15,16,
           
2461:  R0←XR, GOTO AAX1
.SSP = 6   55,54
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.B = 707    9,10,11,15,16,17,
CHECKBIT IS 30   
           
2462:  Z, RES←0, PROTECT, DGOTO NOP
.BR = 14   83,82
.MS = 25   65,63,61
.TYW = 1   48
.LRN = 5   71,69
.MC = 20  1
.DGO = 1   87
.B = 426    9,13,15,16,
.VCY = 1       86
           
2463:  Z, RES←4B7, GOTO 1705B IF NPROTECT, SETBA
.BR = 14   83,82
.MS = 61   65,61,60
.TCY = 1   44
.TYW = 1   48
.LRN = 5   71,69
.MC = 30   2,1
.VCY = 1   86
.B = 1705    8,9,10,11,15,17,
.C = -40000000   18,

2464:  UNPRO:     Z, UNPROTECT, GOTO NOP
.BR = 14   83,82
.MS = 20   61
.MC = 1   5
.B = 426    9,13,15,16,
CHECKBIT IS 30   
           
2465:  Z, STROBE, GOTO NOP
.BR = 14   83,82
.MS = 17   65,64,63,62
.MC = 20   1
.B = 426    9,13,15,16,
           
2466:  S←Z, SETA, GOTO USCL1
.BR = 14   83,82
.MS = 30   62,61
.TXW = 1   47
.LRN = 2   70
.MC = 1   5
.B = 715    9,10,11,14,15,17,
           
2467:  CLEARMAP, GOTO NOP
.MS = 74   63,62,61,60
.MC = 20   1
.B = 426    9,13,15,16,
           
2470:  CMAPS, GOTO NOP
.MS = 62   64,61,60
.MC = 1   5
.B = 426    9,13,15,16,
CHECKBIT IS 30   
           
2471:  Z← ITL, ALERT, GOTO CAT1
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.MC = 20   1
.B = 712    9,10,11,14,16,
.C = 61   36,37,41,
           
2472:  Z← CTCL, ALERT, GOTO CAT1
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.MC = 1   5
.B = 712    9,10,11,14,16,
.C = 63   36,37,40,41,
CHECKBIT IS 50   
           
2473:  Z← UNLS, ALERT, GOTO RUN1
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.MC = 20   1
.B = 713    9,10,11,14,16,17,
.C = 64   36,37,39,
CHECKBIT IS 50   
           
2474:  Z, STOP, Z←P←P+1, GOTO NOP
.RRN = 1   68
.BR = 14   83,82
.MS = 76   64,63,62,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 426    9,13,15,16,
CHECKBIT IS 30   
           
2475:    SETBA, GOTO TI        *ILLEGAL MICROINSTRUCTION
.MS = 61       60,61,65,
.MC = 20       1
.B = 1400      8,9,
CHECKBIT IS 30

2476:  R0←XR, GOTO LDM1,SETBA
.MS = 61       60,61,65
.SSP = 6   55,54
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 1        5
.B = 1703    8,9,10,11,16,17,
CHECKBIT IS 30   
           
2477:  SETBA, GOTO TI
.MS = 61   65,61,60
.MC = 20   1
.B = 1400    8,9,
CHECKBIT IS 30   
*
*

2500:  OPR1:      Q←BR, ABCD0, GOTO OPR2 IF R0>=0
.SSP = 2   55
.MS = 70   62,61,60
.TSPY = 1   45
.LQY = 1   75
.MC = 12   4,2
.B = 703    9,10,11,16,17,
           
2501:  M←R6←XR, DGOTO M MRG BASEOPR
.MCONT = 3   7,6
.BL = 14   79,78
.SSP = 6   55,54
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LMY = 1   73
.LRN = 6   70,69
.MC = 20   1
.DGO = 1   87
.C = 2400   31,33,
CHECKBIT IS 50   
           
2502:  Z←AR
.SSP = 1   56
.TSPY = 1   45
.LZY = 1   77

2503:  SYSCLL:    M←R0←Q LCY 12, Z←377B
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.TXW = 1   47
.LMX = 1   72
.LZY = 1   77
.LR0 = 1   58
.C = 377   34,35,36,37,38,39,40,41,
           
2504:  IR←Q AND Z, Z←600000B, SETC
.BR = 10   82
.MS = 35   65,63,62,61
.TCY = 1   44
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
.C = 600000   24,25,
           
2505:  NEWG←Z, Z←R0←S←604000B, DGOTO SYSC1 IF R0[2]
.BR = 14   83,82
.SSP = 25   56,54,52
.TCY = 1   44
.TYW = 1   48
.LZY = 1   77
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 24   3,1
.DGO = 1   87
.B = 511    9,11,14,17,
.C = 604000   24,25,30,
           
2506:  K←M LCY 3 MRG 30000240B
.BL = 14   79,78
.MS = 3   65,64
.TCX = 1   43
.TXW = 1   47
.LRN = 3   71,70
.C = 30000240   19,20,34,36,
CHECKBIT IS 87   
           
2507:  Z←R0←S←403016B
.TCX = 1   43
.TXW = 1   47
.LZX = 1   76
.LRN = 2   70
.LR0 = 1   58
.C = 403016   24,31,32,38,39,40,

2510:  UCAL1:     NEWG←403000B
.SSP = 25   56,54,52
.TCX = 1   43
.LSPX = 1   59
.C = 403000   24,31,32,
CHECKBIT IS 87   

2511:  SYSC1:     SSOURCE
.MS = 52   64,62,60
           
2512:  ILIM←0, SETBA, CALL IA
.MCONT = 1   7
.SSP = 20   52
.MS = 61   65,61,60
.LSPX = 1   59
.MC = 1   5
.B = 101    11,17,
           
2513:  R6←R0, DGOTO BLL1
.THY = 1   46
.TYW = 1   48
.LRN = 6   70,69
.MC = 20   1
.DGO = 1   87
.B = 12    14,16,
CHECKBIT IS 30   
           
2514:  Z←K, Q←77B6, CALL BLODX
.MCONT = 1   7
.RRN = 3   68,67
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.B = 366    10,11,12,13,15,16,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
*

2515:  XGA1:      GR←Z, GOTO CBA
.BR = 14   83,82
.SSP = 10   53
.LSPX = 1   59
.MC = 20   1
.B = 402    9,16,

2516:  XLA1:      LR←Z, GOTO CBA
.BR = 14   83,82
.SSP = 7   56,55,54
.LSPX = 1   59
.MC = 1   5
.B = 402    9,16,

2517:  ZAB1:      BR←0, DGOTO NX2, SETBA
.SSP = 2   55
.MS = 61   65,61,60
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 35    13,14,15,17,
           
2520:  ER←0, Z←P←P+1
.RRN = 1   68
.SSP = 5   56,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
*
*

2521:  CLSMULT:   M←M+Z
.BR = 14   83,82
.BL = 14   79,78
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
           
2522:  M←M+Z
.BR = 14   83,82
.BL = 14   79,78
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
           
2523:  M←M+Z
.BR = 14   83,82
.BL = 14   79,78
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
           
2524:  Q←Q LCY 4
.BL = 12   80,78
.MS = 4   63
.LQX = 1   74
CHECKBIT IS 87   
           
2525:  R6←Q LCY 2, Q←AR
.BL = 12   80,78
.SSP = 1   56
.MS = 2   64
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
           
2526:  Q←Q LCY 4
.BL = 12   80,78
.MS = 4   63
.LQX = 1   74
CHECKBIT IS 87   
           
2527:  Q←Q LCY 2, Z←3B
.BL = 12   80,78
.MS = 2   64
.TCY = 1   44
.LQX = 1   74
.LZY = 1   77
.C = 3   40,41,
CHECKBIT IS 87   
           
2530:  R6←Q AND Z, Q←R6
.RRN = 6   67,66
.BR = 10   82
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
CHECKBIT IS 87   
           
2531:  Q←Q AND Z, Z←R6
.RRN = 6   67,66
.BR = 10   82
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
CHECKBIT IS 87   
           
2532:  Q←Q-Z, Z←P←P+1, SETBA, DGOTO NX2
.RRN = 1   68
.BR = 3   85,84
.BL = 12   80,78
.MS = 61   65,61,60
.LOC = 1   50
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.TAX = 1   49
.LQX = 1   74
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 35    13,14,15,17,
           
2533:  RES←AR←M+Q
.BR = 12   84,82
.BL = 14   79,78
.SSP = 1   56
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.VCY = 1   86
*

2534:  CLS1:      Q←NOT Q MRG 77B6
.BL = 5   81,79
.TCX = 1   43
.LQX = 1   74
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   
           
2535:  Z←NOT Z MRG 77B6
.BR = 3   85,84
.TCX = 1   43
.LZX = 1   76
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   
           
2536:  DGOTO NOT M+1 MRG 2520B
.MCONT = 3   7,6
.BL = 3   81,80
.LOC = 1   50
.TCX = 1   43
.TAX = 1   49
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.C = 2520   31,33,35,37,
CHECKBIT IS 56   
           
2537:  M←Z←NOT Q-NOT Z, Q←BR
.BR = 14   83,82
.BL = 5   81,79
.SSP = 2   55
.LOC = 1   50
.TSPY = 1   45
.TAX = 1   49
.LMX = 1   72
.LQY = 1   75
.LZX = 1   76
.VCY = 1   86
CHECKBIT IS 87   

2540:  BMACCQ: R0←Q, SETBA, GOTO BMACC1
.MS = 61   60,61,65
.MC = 20   1
.BL = 2   80
.LR0 = 1   58
.TXW = 1   47
.B = 1716   8,9,10,11,14,15,16,
CHECKBIT IS 30

       
*
*
*  ASP, M=AR, Z=XR, IR=M
*

2541:  ASPDIV:    M←Z←3, DGOTO ASP4
.TCX = 1   43
.LMX = 1   72
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.B = 550    9,11,12,14,
.C = 3   40,41,
           
2542:  M←Q AND NOT M LCY 2
.BL = 2   80
.MS = 2   64
.LMX = 1   72
           
2543:  Z←Q LCY 2, DGOTO ASP9, M←74B6
.BL = 12   80,78
.MS = 2   64
.TCY = 1   44
.LMY = 1   73
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.B = 576    9,11,12,13,14,15,16,
.C = -4000000   18,19,20,21,
           
2544:  M←Q AND NOT M + Z
.BR = 14   83,82
.BL = 2   80
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
CHECKBIT IS 30   
           
2545:  M←Z←1, DGOTO ASP4
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.B = 550    9,11,12,14,
CHECKBIT IS 30   
           
2546:  M←Q AND NOT M LCY 3
.BL = 2   80
.MS = 3   65,64
.LMX = 1   72
CHECKBIT IS 87   
           
2547:  M←Q LCY 4, Z←0
.BL = 12   80,78
.MS = 4   63
.LMX = 1   72
.LZY = 1   77

2550:  ASP4:      M←M LCY 20
.BL = 14   79,78
.MS = 10   62
.LMX = 1   72
CHECKBIT IS 87   

2551:  ASP5:      Q←Z AND Q, Z←S, DGOTO ASP8 IF A
.RRN = 2   67
.BR = 10   82
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 566    9,11,12,13,15,16,
CHECKBIT IS 30   
*
*  ASPX CAN BE CALLED AS A SUBROUTINE, TO ASP Q<CSIZE
*   INT0 Z.  M=0, C SET.

2552:  ASPX:      Q←Q LCY 16
.BL = 12   80,78
.MS = 7   65,64,63
.LQX = 1   74
CHECKBIT IS 87   
           
2553:  Z←Z+M
.BR = 14   83,82
.BL = 14   79,78
.TAX = 1   49
.LZX = 1   76
.VCY = 1   86

2554:  ASPX1:     M←Q LCY 2, Q←3B6
.BL = 12   80,78
.MS = 2   64
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 3000000   22,23,
CHECKBIT IS 87   
           
2555:  R6←M+Q AND Z
.BR = 10   82
.BL = 14   79,78
.TXW = 1   47
.TAX = 1   49
.LRN = 6   70,69
.VCY = 1   86
CHECKBIT IS 87   
           
2556:  Q←Z
.BR = 14   83,82
.LQX = 1   74
           
2557:  Q←Q LCY 20, M←6B5
.BL = 12   80,78
.MS = 10   62
.TCY = 1   44
.LMY = 1   73
.LQX = 1   74
.C = 600000   24,25,
CHECKBIT IS 87   
           
2560:  Q←M AND Q LCY 2, M←4B6
.BL = 10   78
.MS = 2   64
.TCY = 1   44
.LMY = 1   73
.LQX = 1   74
.C = 4000000   21,
CHECKBIT IS 87   
           
2561:  Q←Q-M, M←R6, DGOTO ASP6
.RRN = 6   67,66
.BR = 12   84,82
.BL = 3   81,80
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LQX = 1   74
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 564    9,11,12,13,15,
           
2562:  R6←M+Q, Q←3B6, GOTO *+1 IF X>=0
.BR = 12   84,82
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 6   70,69
.MC = 16   4,3,2
.VCY = 1   86
.B = 563    9,11,12,13,16,17,
.C = 3000000   22,23,
CHECKBIT IS 56   
           
2563:  Z←Z+1, M←R6, GOTO ASP6
.RRN = 6   67,66
.BR = 14   83,82
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.MC = 20   1
.VCY = 1   86
.B = 564    9,11,12,13,15,
CHECKBIT IS 30   

2564:  ASP6:      Z←M AND Q ! Z AND NOT Q, RETURN IF C
.MCONT = 2   6
.BR = 4   83
.BL = 10   78
.LZX = 1   76
.MC = 35   5,3,2,1
CHECKBIT IS 30   
           
2565:  XR←Z, SETBA, GOTO NX1
.BR = 14   83,82
.SSP = 6   55,54
.MS = 61   65,61,60
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
CHECKBIT IS 30   

2566:  ASP8:      Z←Z-M
.BR = 14   83,82
.BL = 3   81,80
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.VCY = 1   86
CHECKBIT IS 87   
           
2567:  M←Q LCY 2, Q←3B6
.BL = 12   80,78
.MS = 2   64
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 3000000   22,23,
CHECKBIT IS 87   
           
2570:  M←Q AND Z-M, GOTO ASP6 IF X>=0
.BR = 10   82
.BL = 3   81,80
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 16   4,3,2
.VCY = 1   86
.B = 564    9,11,12,13,15,
           
2571:  R6←Q←Z-1, Z←4B6
.BR = 14   83,82
.BL = 17   81,80,79,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQX = 1   74
.LZY = 1   77
.LRN = 6   70,69
.VCY = 1   86
.C = 4000000   21,
           
2572:  Q←Q LCY 20
.BL = 12   80,78
.MS = 10   62
.LQX = 1   74
CHECKBIT IS 87   
           
2573:  M←M+Z
.BR = 14   83,82
.BL = 14   79,78
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
           
2574:  Z←Q LCY 2, Q←3B6, DGOTO ASP6
.BL = 12   80,78
.MS = 2   64
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 564    9,11,12,13,15,
.C = 3000000   22,23,
           
2575:  M←M-Z AND Q, Z←R6
.RRN = 6   67,66
.BR = 7   85,84,83
.BL = 14   79,78
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.VCY = 1   86
*  DIVIDE Q BY 3 (MULTIPLY BY .25252...B)

2576:  ASP9:      Z←Q LCY 4, Q←3B
.BL = 12   80,78
.MS = 4   63
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = 3   40,41,
CHECKBIT IS 87   
           
2577:  IR←Z←M+Z, M←R6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 14   79,78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LRN = 4   69
.VCY = 1   86

2600:  ASP10:     M←M AND NOT Q LCY 20, Q←IR, GOTO ASP11 IF LB=0
.RRN = 4   66
.BL = 4   79
.MS = 10   62
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
.MC = 22   4,1
.B = 603    9,10,16,17,
CHECKBIT IS 30   
           
2601:  M←M LCY 2, Q←3, DGOTO ASP10
.BL = 14   79,78
.MS = 2   64
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
.B = 600    9,10,
.C = 3   40,41,
CHECKBIT IS 50   
           
2602:  IR←Z←M+Z
.BR = 14   83,82
.BL = 14   79,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LRN = 4   69
.VCY = 1   86

2603:  ASP11:     Q←NOT Q LCY 16 MRG 176B5
.BL = 5   81,79
.MS = 7   65,64,63
.TCX = 1   43
.LQX = 1   74
.C = 17600000   20,21,22,23,24,25,
           
2604:  IR←Q←NOT Q LCY 2
.BL = 5   81,79
.MS = 2   64
.TXW = 1   47
.LQX = 1   74
.LRN = 4   69
CHECKBIT IS 87   
           
2605:  Z←Q LCY 1
.BL = 12   80,78
.MS = 1   65
.LZX = 1   76
CHECKBIT IS 87   
           
2606:  Z←Q+Z, M←R6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 12   80,78
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.VCY = 1   86
           
2607:  Q←M-Z, Z←3
.BR = 3   85,84
.BL = 14   79,78
.LOC = 1   50
.TCY = 1   44
.TAX = 1   49
.LQX = 1   74
.LZY = 1   77
.VCY = 1   86
.C = 3   40,41,
CHECKBIT IS 87   

2610:  ASP12:     R0←Q-Z, M←IR, GOTO ASP5 IF X<0
.RRN = 4   66
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 551    9,11,12,14,17,
           
2611:  M←IR←M+1, Q←R0, GOTO ASP5
.BL = 14   79,78
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMX = 1   72
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.VCY = 1   86
.B = 551    9,11,12,14,17,
CHECKBIT IS 30   
*

2612:  CSA1:      SETBA, CALL GETSR
.MCONT = 1   7
.MS = 61   65,61,60
.MC = 1   5
.B = 662    9,10,12,13,16,
CHECKBIT IS 30   
           
2613:  GOTO CXA
.MC = 20   1
.B = 410    9,14,

2614:  XSA1:      R6←Z, SETBA, CALL GETSR
.MCONT = 1   7
.BR = 14   83,82
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 6   70,69
.MC = 1   5
.B = 662    9,10,12,13,16,
           
2615:  AR←M, M←R6, SETBA, CALL LOADSR
.MCONT = 1   7
.RRN = 6   67,66
.BL = 14   79,78
.SSP = 1   56
.MS = 61   65,61,60
.THY = 1   46
.LMY = 1   73
.LSPX = 1   59
.MC = 20   1
.B = 666    9,10,12,13,15,16,
           
2616:  Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 35    13,14,15,17,
CHECKBIT IS 30   
*

2617:  CTA1:      PIN, Q←E2, GOTO CBA
.MS = 16   64,63,62
.LQY = 1   75
.MC = 20   1
.TE2Y = 1   89
.B = 402    9,16,
.VCY = 1       86
*
*  MOVE BLOCK OR CONSTANT (IF A IS NOT SET)
*

2620:  MVB1:      GOTO MVB2 IF R0<0
.MC = 11   5,2
.B = 630    9,10,13,14,
CHECKBIT IS 30   
           
2621:  Q←Q+Z
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
*  MOVE TOP CELL FIRST
           
2622:  M←M+Z, SETBA, GOTO TI IF Z<0
.BR = 14   83,82
.BL = 14   79,78
.MS = 61   65,61,60
.TAX = 1   49
.LMX = 1   72
.MC = 4   3
.VCY = 1   86
.B = 1400    8,9,

2623:  MVB3:      K←Q←Q-1, SETBA, GOTO NX1 IF Z=0
.BR = 17   85,84,83,82
.BL = 12   80,78
.MS = 61   65,61,60
.TXW = 1   47
.TAX = 1   49
.LQX = 1   74
.LRN = 3   71,70
.MC = 2   4
.VCY = 1   86
.B = 34    13,14,15,
           
2624:  S←R0←M-1, M←XR, CALL BLODX IF A
.MCONT = 1   7
.BR = 17   85,84,83,82
.BL = 14   79,78
.SSP = 6   55,54
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LRN = 2   70
.LR0 = 1   58
.MC = 33   5,4,2,1
.VCY = 1   86
.B = 366    10,11,12,13,15,16,
CHECKBIT IS 30   
           
2625:  R0←K, CALL BSTR1
.MCONT = 1   7
.RRN = 3   68,67
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.B = 371    10,11,12,13,14,17,
           
2626:  AR←Z←Z-1, M←S, DGOTO MVB3
.RRN = 2   67
.BR = 14   83,82
.BL = 17   81,80,79,78
.SSP = 1   56
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 623    9,10,13,16,17,
           
2627:  SETBA, GOTO TOSP IF INTRPT1
.MS = 61   65,61,60
.MC = 64   3,1,0
.B = 1057    8,12,14,15,16,17,
CHECKBIT IS 30   
*
*  MOVE BOTTOM CELL FIRST

2630:  MVB2:      S←R0←M, SETBA, GOTO NX1 IF Z=0
.BL = 14   79,78
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 2   70
.LR0 = 1   58
.MC = 2   4
.B = 34    13,14,15,
CHECKBIT IS 30   
           
2631:  K←Q, CALL BLODX IF Z>=0
.MCONT = 1   7
.BL = 12   80,78
.TXW = 1   47
.LRN = 3   71,70
.MC = 5   5,3
.B = 366    10,11,12,13,15,16,
CHECKBIT IS 30   
           
2632:  R0←K, CALL BSTR1 IF Z>=0
.MCONT = 1   7
.RRN = 3   68,67
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 5   5,3
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
           
2633:  BR←Q←Q+1, M←S, SETBA, GOTO TI IF Z<0
.RRN = 2   67
.BL = 12   80,78
.SSP = 2   55
.MS = 61   65,61,60
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LQX = 1   74
.LSPX = 1   59
.MC = 4   3
.B = 1400    8,9,
CHECKBIT IS 30   
           
2634:  AR←Z←Z-1, DGOTO MVB2
.BR = 14   83,82
.BL = 17   81,80,79,78
.SSP = 1   56
.TAX = 1   49
.LZX = 1   76
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 630    9,10,13,14,
           
2635:  XR←M←M+1, SETBA, GOTO TOSP IF INTRPT1
.BL = 14   79,78
.SSP = 6   55,54
.MS = 61   65,61,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.LSPX = 1   59
.MC = 64   3,1,0
.VCY = 1   86
.B = 1057    8,12,14,15,16,17,
*
*  LOCATE LEADING TRANSITION IN AB
*   (RETURN IF C IS SET)

2636:  LLT1:      M, R0←0, GOTO LLT3 IF LB=0
.BL = 14   79,78
.TYW = 1   48
.LR0 = 1   58
.MC = 22   4,1
.B = 643    9,10,12,16,17,
           
2637:  NOT M, GOTO LLT3 IF LB=0
.BL = 3   81,80
.MC = 22   4,1
.B = 643    9,10,12,16,17,

2640:  LLT2:      DGOTO *+1
.MC = 1   5
.DGO = 1   87
.B = 641    9,10,12,17,
CHECKBIT IS 30   
           
2641:  M←M LCY 1, Z←R0←R0+1, DGOTO * IF M[0]=M[1]
.BL = 14   79,78
.MS = 1   65
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LZY = 1   77
.LR0 = 1   58
.MC = 73   5,4,2,1,0
.DGO = 1   87
.B = 641    9,10,12,17,
           
2642:  Z←Z-1, GOTO LLT5
.BR = 14   83,82
.BL = 17   81,80,79,78
.TAX = 1   49
.LZX = 1   76
.MC = 1   5
.VCY = 1   86
.B = 646    9,10,12,15,16,

2643:  LLT3:      M←M EOR Q, Z←0, GOTO LLT5 IF LB=0
.BL = 6   80,79
.LMX = 1   72
.LZY = 1   77
.MC = 22   4,1
.B = 646    9,10,12,15,16,
           
2644:  M, Z←24, GOTO LLT5 IF X<0
.BL = 14   79,78
.TCY = 1   44
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 646    9,10,12,15,16,
.C = 30   37,38,
CHECKBIT IS 50   
           
2645:  R0←Z, GOTO LLT2
.BR = 14   83,82
.TXW = 1   47
.LR0 = 1   58
.MC = 20   1
.B = 640    9,10,12,
CHECKBIT IS 30   

2646:  LLT5:      Q←K, RETURN IF C
.MCONT = 2   6
.RRN = 3   68,67
.THY = 1   46
.LQY = 1   75
.MC = 35   5,3,2,1
           
2647:  RES←XR←Z, SETBA, GOTO NX1
.BR = 14   83,82
.SSP = 6   55,54
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
*
*  LOAD STRING CONSTANT

2650:  LSC1:      Q←S←R0←M, CALL BLODX
.MCONT = 1   7
.BL = 14   79,78
.TXW = 1   47
.LQX = 1   74
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 366    10,11,12,13,15,16,
CHECKBIT IS 30   
           
2651:  Z←CR←M+Q
.BR = 12   84,82
.BL = 14   79,78
.SSP = 3   56,55
.TAX = 1   49
.LZX = 1   76
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
2652:  DR←Z, Q←14B6
.BR = 14   83,82
.SSP = 4   54
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.C = 14000000   20,21,
CHECKBIT IS 87   
           
2653:  Q←NOT M AND Q LCY 20
.BL = 2   80
.MS = 10   62
.LQX = 1   74
           
2654:  Z←Q LCY 2, Q←74B6
.BL = 12   80,78
.MS = 2   64
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = -4000000   18,19,20,21,
CHECKBIT IS 87   
           
2655:  M←M AND Q ! Z, Q←S
.RRN = 2   67
.BR = 14   83,82
.BL = 10   78
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
           
2656:  AR←M OR Q, SETBA, DGOTO NX2
.BL = 16   80,79,78
.SSP = 1   56
.MS = 61   65,61,60
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
2657:  BR←M OR Q, Z←P←P+1
.RRN = 1   68
.BL = 16   80,79,78
.SSP = 2   55
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
*
*  START OF ASP IS ALMOST HERE (AT ASP1)
*

2660:  ASP2:      Z←NOT Q+1, Q←3, SETA, GOTO ASP3
.BL = 5   81,79
.MS = 30   62,61
.LOC = 1   50
.TCY = 1   44
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.B = 662    9,10,12,13,16,
.C = 3   40,41,

2661:  ASP1:      M←M LCY 4, Q←3
.BL = 14   79,78
.MS = 4   63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 3   40,41,
CHECKBIT IS 87   

2662:  ASP3:      DGOTO M AND Q LCY 1 MRG ASPDIV
.MCONT = 3   7,6
.BL = 10   78
.MS = 1   65
.TCX = 1   43
.MC = 1   5
.DGO = 1   87
.C = 2541   31,33,35,36,41,
CHECKBIT IS 50   
           
2663:  R6←Q←Z, GOTO ASP2 IF Z<0
.BR = 14   83,82
.TXW = 1   47
.LQX = 1   74
.LRN = 6   70,69
.MC = 4   3
.B = 660    9,10,12,13,
*
*
*  COUNT ONE BITS IN AB
*

2664:  COB1:      Q←Z-1, GOTO COB2 IF Z=0
.BR = 14   83,82
.BL = 17   81,80,79,78
.TAX = 1   49
.LQX = 1   74
.MC = 2   4
.VCY = 1   86
.B = 666    9,10,12,13,15,16,
CHECKBIT IS 30   
           
2665:  Z←Q AND Z, R0←R0+1, GOTO COB1
.BR = 10   82
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 20   1
.B = 664    9,10,12,13,15,
CHECKBIT IS 30   

2666:  COB2:      Q←R0, Z←M, CLEARA, GOTO COB1 IF A
.BL = 14   79,78
.MS = 50   62,60
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
.MC = 33   5,4,2,1
.B = 664    9,10,12,13,15,
CHECKBIT IS 30   
           
2667:  RES←XR←Q, SETBA, GOTO NX1
.BL = 12   80,78
.SSP = 6   55,54
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
*
*  LOAD STATE, IR CONTAINS XMONT,XUTILT BITS
*

2670:  LODS1:     S←M, SETBA, CALL LSTATE
.MCONT = 1   7
.BL = 14   79,78
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 2   70
.MC = 1   5
.B = 1014    8,14,15,
CHECKBIT IS 30   
           
2671:  Z←R0←Q, Q←XX, SETA, DGOTO BMACC
.BL = 12   80,78
.SSP = 24   54,52
.MS = 30   62,61
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LZX = 1   76
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.B = 722   9,10,11,13,16,
CHECKBIT IS 30
           
2672:  MAP, R0←E1, .VCY, DGOTO *+1
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.VCY = 1   86
.TE1Y = 1   88
.MC = 1   5
.DGO = 1   87
.B = 673   9,10,12,13,14,16,17,
CHECKBIT IS 30
           
2673:  SSOURCE, R0←P
.RRN = 1   68
.MS = 52   64,62,60
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
           
2674:  MAP, R0←E1, .VCY, XR←Q
.BL = 12   80,78
.SSP = 6   55,54
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
.VCY = 1   86
.TE1Y = 1   88
CHECKBIT IS 87   
           
2675:  R0←M←Z, CALL MEXIT IF R0[1]
.MCONT = 1   7
.BR = 14   83,82
.TXW = 1   47
.LMX = 1   72
.LR0 = 1   58
.MC = 66   4,3,1,0
.B = 733    9,10,11,13,14,16,17,
           
2676:  Z←R0←P←M, SETBA, DGOTO NX5 IF A
.BL = 14   79,78
.MS = 61   65,61,60
.TXW = 1   47
.LZX = 1   76
.LRN = 1   71
.LR0 = 1   58
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 350    10,11,12,14,
CHECKBIT IS 30   
           
2677:  ABCD0
.MS = 70   62,61,60
           
2700:  Q←0, SETBA, GOTO FIXTRP
.MS = 61   65,61,60
.LQX = 1   74
.MC = 1   5
.B = 1046    8,12,15,16,

2701:  STRS1:     K←M-1, SETBA, CALL SSTATE
.MCONT = 1   7
.BR = 17   85,84,83,82
.BL = 14   79,78
.MS = 61   65,61,60
.TXW = 1   47
.TAX = 1   49
.LRN = 3   71,70
.MC = 20   1
.VCY = 1   86
.B = 775    9,10,11,12,13,14,15,17,
CHECKBIT IS 30   
           
2702:  Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 1   5
.B = 35    13,14,15,17,
CHECKBIT IS 30   
*
*  TEST FOR PRIVILEGED OPR-S

2703:  OPR2:      Z←MAXPRIV
.TCX = 1   43
.LZX = 1   76
.C = 100   35,
           
2704:  M-Z, Q←6B5, SETBA, GOTO TI IF X>=0
.BR = 3   85,84
.BL = 14   79,78
.MS = 61   65,61,60
.LOC = 1   50
.TCY = 1   44
.TAX = 1   49
.LQY = 1   75
.MC = 16   4,3,2
.VCY = 1   86
.B = 1400    8,9,
.C = 600000   24,25,
           
2705:  M←P, DGOTO M MRG BASEOPR
.MCONT = 3   7,6
.RRN = 1   68
.BL = 14   79,78
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.MC = 20   1
.DGO = 1   87
.C = 2400   31,33,
CHECKBIT IS 50   
           
2706:  NOT M AND Q, Z←AR, SETBA, GOTO TI IF LB#0
.BL = 2   80
.SSP = 1   56
.MS = 61   65,61,60
.TSPY = 1   45
.LZY = 1   77
.MC = 23   5,4,1
.B = 1400    8,9,
CHECKBIT IS 30   
*

2707:  AAX1:      MAP, .VCY, Q←E1, GOTO CBX
.MS = 37   65,64,63,62,61
.LQY = 1   75
.MC = 20   1
.VCY = 1   86
.TE1Y = 1   88
.B = 403    9,16,17,
CHECKBIT IS 30   
*

2710:  SLOK1:     4, PROTECT, GOTO * IF NPROTECT
.MS = 25   65,63,61
.TCX = 1   43
.MC = 30   2,1
.VCY = 1   86
.B = 710    9,10,11,14,
.C = 4   39,
CHECKBIT IS 50   
           
2711:  Z←P←P+1, SETBA, GOTO NX2
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
CHECKBIT IS 30   
*

2712:  CAT1:      Z←AR, POT, GOTO NOP
.SSP = 1   56
.MS = 15   65,63,62
.TSPY = 1   45
.LZY = 1   77
.MC = 1   5
.B = 426    9,13,15,16,

2713:  RUN1:      Q←E2, .VCY, PIN, DGOTO CTA1
.MS = 16   64,63,62
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.TE2Y = 1   89
.B = 617    9,10,14,15,16,17,
CHECKBIT IS 30   
           
2714:  BR←Q, Z←UNMS, ALERT
.BL = 12   80,78
.SSP = 2   55
.MS = 14   63,62
.TCY = 1   44
.LZY = 1   77
.LSPX = 1   59
.C = 65   36,37,39,41,
CHECKBIT IS 87   
*

2715:  USCL1:     P←P+1, SETBA, GOTO TOSP2
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 1   71
.MC = 20   1
.B = 1060    8,12,13,
CHECKBIT IS 30   
*
*
*

2716:  EAC1:      Z←M AND Q LCY 4 MRG BASEAD, CALL BLL48
.MCONT = 1   7
.BL = 10   78
.MS = 4   63
.TCX = 1   43
.LZX = 1   76
.MC = 1   5
.B = 205    10,15,17,
.C = 40   36,
CHECKBIT IS 50   
           
2717:  Z←RES←0, M←R0, GOTO EAC4 IF D
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LZX = 1   76
.LRN = 5   71,69
.MC = 47   5,4,3,0
.B = 724    9,10,11,13,15,

2720:  EAC2:      Q←77B6, IR←SHIFT, GOTO EAC5 IF A
.SSP = 13   56,55,53
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LQX = 1   74
.LRN = 4   69
.MC = 33   5,4,2,1
.B = 725    9,10,11,13,15,17,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
           
2721:  MAP, .VCY, R0←E1, GOTO EAC6 IF B
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 34   3,2,1
.VCY = 1   86
.TE1Y = 1   88
.B = 726    9,10,11,13,15,16,
CHECKBIT IS 30   

2722:  BMACC:     SETBA, GOTO BMACC1 IF R0[1]
.MS = 61   65,61,60
.MC = 66   4,3,1,0
.B = 1716   8,9,10,11,14,15,16,
CHECKBIT IS 30   

2723:  EAC3:      XR←M AND NOT Q ! Z AND Q, SETBA, Z←P←P+1, GOTO NX2
.RRN = 1   68
.BR = 10   82
.BL = 4   79
.SSP = 6   55,54
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,

2724:  EAC4:      Z←RES←2B6, M←0, GOTO EAC2
.TCX = 1   43
.TXW = 1   47
.LMY = 1   73
.LZX = 1   76
.LRN = 5   71,69
.MC = 1   5
.B = 720    9,10,11,13,
.C = 2000000   22,
CHECKBIT IS 50   

2725:  EAC5:      AR←M, Z←RES←1B6, GOTO EAC3
.BL = 14   79,78
.SSP = 1   56
.TCY = 1   44
.TYW = 1   48
.LZY = 1   77
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 723    9,10,11,13,16,17,
.C = 1000000   23,

2726:  EAC6:      Z←MASK
.SSP = 15   56,54,53
.TSPY = 1   45
.LZY = 1   77
           
2727:  AR←Z, Z←IR, DGOTO BMACC
.RRN = 4   66
.BR = 14   83,82
.SSP = 1   56
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 722    9,10,11,13,16,
CHECKBIT IS 30   
           
2730:  BR←Z, Z←RES←3B6
.BR = 14   83,82
.SSP = 2   55
.TCY = 1   44
.TYW = 1   48
.LZY = 1   77
.LRN = 5   71,69
.LSPX = 1   59
.C = 3000000   22,23,
*
*  Q = Z = R0 = BITS,

2731:  MON:       Z←IR←SR, DGOTO CLL IF R0[2]
.SSP = 11   56,53
.TSPY = 1   45
.TYW = 1   48
.LZY = 1   77
.LRN = 4   69
.MC = 24   3,1
.DGO = 1   87
.B = 742    9,10,11,12,16,
CHECKBIT IS 30   
           
2732:  R0←M, M←1
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.C = 1   41,

2733:  MEXIT:     Q←P, 4, UNPROTECT
.RRN = 1   68
.MS = 20   61
.TCX = 1   43
.THY = 1   46
.LQY = 1   75
.C = 4   39,
CHECKBIT IS 87   
           
2734:  Q←Q LCY 12, M←R0
.BL = 12   80,78
.MS = 6   64,63
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
           
2735:  Q←Q LCY 2, Z←IR
.RRN = 4   66
.BL = 12   80,78
.MS = 2   64
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
           
2736:  Z←Q AND Z, Q←100B
.BR = 10   82
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = 100   35,
           
2737:  Z←Q AND Z, X23LCK, DRETURN IF NZ[16]
.MCONT = 2   6
.BR = 10   82
.MS = 71   65,62,61,60
.LZX = 1   76
.MC = 10   2
.DGO = 1   87
           
2740:  IR←8, GOTO XTRP IF Z#0
.TCX = 1   43
.TXW = 1   47
.LRN = 4   69
.MC = 3   5,4
.B = 746    9,10,11,12,15,16,
.C = 10   38,
CHECKBIT IS 50   
           
2741:  IR←9, GOTO XTRP
.TCX = 1   43
.TXW = 1   47
.LRN = 4   69
.MC = 20   1
.B = 746    9,10,11,12,15,16,
.C = 11   38,41,
CHECKBIT IS 50   
*

2742:  CLL:       Q AND M, Q←60B, DRETURN IF LB=0
.MCONT = 2   6
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.MC = 22   4,1
.DGO = 1   87
.C = 60   36,37,
           
2743:  M←R0, SR←NOT Q AND Z
.BR = 4   83
.SSP = 11   56,53
.THY = 1   46
.LMY = 1   73
.LSPX = 1   59
CHECKBIT IS 87   
           
2744:  4, PROTECT, GOTO * IF NPROTECT
.MS = 25   65,63,61
.TCX = 1   43
.MC = 30   2,1
.VCY = 1   86
.B = 744    9,10,11,12,15,
.C = 4   39,
           
2745:  1, X23LCK, RETURN
.MCONT = 2   6
.MS = 71   65,62,61,60
.LOC = 1   50
.TAX = 1   49
.MC = 20   1
CHECKBIT IS 30   
*

2746:  XTRP:      CLEARA, RETURN
.MCONT = 2   6
.MS = 50   62,60
.MC = 1   5
CHECKBIT IS 30   
*
*
2747:    SETBA, CALL 1776B     *ILLEGAL MICROINSTRUCTION
.MS = 61       60,61,65,
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16,
CHECKBIT IS 30


