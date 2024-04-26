*
           ORG 700B;
*
*
*
*  THE FOLLOWING CODE IS ENTERED IF AN EMPTY
*    PAGE IS REFERENCED, OR THE RING PROTECTION
*    IS VIOLATED.
*

700:  PAGEF:     M←Z←R0 , Q←0, SETA, GOTO PAGE1
.MS = 30   62,61
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.B = 702    9,10,11,16,
*
*  OR IF A STORE IS ATTEMPTED INTO A PAGE WHICH
*  IS READ-ONLY OR NOT DIRTY.

701:  CSTORE:    Z←Q←M←R0, CLEARA, GOTO PAGEF IF R0<0
.MS = 50   62,60
.THY = 1   46
.LMY = 1   73
.LQY = 1   75
.LZY = 1   77
.MC = 11   5,2
.B = 700    9,10,11,
*  PREPARE MACC TRAP (#1)

702:  PAGE1:     R0←Q LCY 3, Q←3777B, GOTO PAGE5 IF M[4]
.BL = 12   80,78
.MS = 3   65,64
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.MC = 74   3,2,1,0
.B = 751    9,10,11,12,14,17,
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
           
703:  MAPAD, M←E1, IR←1, DGOTO MACC IF M[1]
.MS = 54   63,62,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LRN = 4   69
.MC = 21   5,1
.DGO = 1   87
.TE1Y = 1   88
.B = 747    9,10,11,12,15,16,17,
           
704:  PARAM←Z AND Q ! M AND NOT Q, Q←77B, GOTO PRO IF R0<0
.BR = 10   82
.BL = 4   79
.SSP = 26   55,54,52
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 11   5,2
.B = 746    9,10,11,12,15,16,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 50   
           
705:  Z←M LCY 12, M←MBA
.BL = 14   79,78
.MS = 6   64,63
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
           
706:  R0←Z AND Q+M, .VCY, Q←7777B, DGOTO PAGE2 IF Z<0
.BR = 10   82
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.MC = 4   3
.DGO = 1   87
.VCY = 1   86
.B = 711    9,10,11,14,17,
.C = 7777   30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 56   
           
707:  FETCH
.MS = 44   63,60
CHECKBIT IS 87   
           
710:  M←M LCY 12
.BL = 14   79,78
.MS = 6   64,63
.LMX = 1   72

711:  PAGE2:     MAPRO←M←M AND Q, Q←377B
.BL = 10   78
.SSP = 30   53,52
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.LSPX = 1   59
.C = 377   34,35,36,37,38,39,40,41,
           
712:  Z←M AND Q LCY 2, Q←MBA
.BL = 10   78
.MS = 2   64
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
*  60 IS 300B-200B-4
           
713:  Z←Z+Q, Q←60, GOTO PNIM IF Z=0
.BR = 14   83,82
.BL = 12   80,78
.TCY = 1   44
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.MC = 2   4
.VCY = 1   86
.B = 745    9,10,11,12,15,17,
.C = 74   36,37,38,39,
CHECKBIT IS 56   
           
714:  R0←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
715:  FETCH, Z←P
.RRN = 1   68
.MS = 44   63,60
.THY = 1   46
.LZY = 1   77
           
716:  UN0←Z←M, R0←R0+1, SSOURCE
.BL = 14   79,78
.SSP = 24   54,52
.MS = 52   64,62,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.LSPX = 1   59
           
717:  FETCH, IR←Q
.BL = 12   80,78
.MS = 44   63,60
.TXW = 1   47
.LRN = 4   69
CHECKBIT IS 87   
           
720:  UN1←M, R0←R0+1
.BL = 14   79,78
.SSP = 25   56,54,52
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
CHECKBIT IS 87   
           
721:  R0←R0+1, Q←2B7, FETCH
.MS = 44   63,60
.IHR = 1   42
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.C = 20000000   19,
           
722:  M LCY 12, K←0, GOTO PNIC IF X>=0
.BL = 14   79,78
.MS = 6   64,63
.TYW = 1   48
.LRN = 3   71,70
.MC = 16   4,3,2
.VCY = 1   86
.B = 740    9,10,11,12,
           
723:  PMTRO←M←M OR Q, STORE, K←0
.BL = 16   80,79,78
.SSP = 27   56,55,54,52
.MS = 42   64,60
.TYW = 1   48
.LMX = 1   72
.LRN = 3   71,70
.LSPX = 1   59
CHECKBIT IS 87   
           
724:  Q←Z, M←UN1, CALL CHTSRCH
.MCONT = 1   7
.BR = 14   83,82
.SSP = 25   56,54,52
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.MC = 1   5
.B = 756    9,10,11,12,14,15,16,
           
725:  Q←4B7, DGOTO PAGE3 IF A
.TCX = 1   43
.LQX = 1   74
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 730    9,10,11,13,14,
.C = -40000000   18,
           
726:  IR←M AND Q LCY 8
.BL = 10   78
.MS = 5   65,63
.TXW = 1   47
.LRN = 4   69
           
727:  M←M OR Q, IR←200B, STORE
.BL = 16   80,79,78
.MS = 42   64,60
.TCY = 1   44
.TYW = 1   48
.LMX = 1   72
.LRN = 4   69
.C = 200   34,
CHECKBIT IS 87   

730:  PAGE3:     M←NOT M LCY 12 MRG 3777700B, Q←MAPRO
.BL = 3   81,80
.SSP = 30   53,52
.MS = 6   64,63
.TCX = 1   43
.TSPY = 1   45
.LMX = 1   72
.LQY = 1   75
.C = 3777700   22,23,24,25,26,27,28,29,30,31,32,33,34,35,
CHECKBIT IS 87   
           
731:  Z←NOT M LCY 1, M←PMTRO
.BL = 3   81,80
.SSP = 27   56,55,54,52
.MS = 1   65
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
CHECKBIT IS 87   
           
732:  Q←Q LCY 12
.BL = 12   80,78
.MS = 6   64,63
.LQX = 1   74
           
733:  M←M OR Q LCY 8, Q←200B
.BL = 16   80,79,78
.MS = 5   65,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 200   34,
           
734:  M←M AND Q LCY 1, Q←IR
.RRN = 4   66
.BL = 10   78
.MS = 1   65
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
CHECKBIT IS 87   
           
735:  Q←Q LCY 2, R0←PARAM
.BL = 12   80,78
.SSP = 26   55,54,52
.MS = 2   64
.TSPY = 1   45
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
CHECKBIT IS 87   
           
736:  Z←M OR Q ! Z
.BR = 14   83,82
.BL = 16   80,79,78
.LZX = 1   76
CHECKBIT IS 87   
           
737:  ZTOMAP, .VCY, DGOTO IDLE3
.MS = 53   65,64,62,60
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 1144    8,11,12,15,
*  BEWARE OF DIRTY TRICK

740:  PNIC:      Q←K, M←2, UNPROTECT
.RRN = 3   68,67
.MS = 20   61
.TCX = 1   43
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
.C = 2   40,
CHECKBIT IS 87   
*  NOTE THAT 2 AND 4 = 0 USED AT PAGE4
           
741:  Q, Q←IR←4, GOTO PAGE4 IF X>=0
.BL = 12   80,78
.TCY = 1   44
.TYW = 1   48
.LQY = 1   75
.LRN = 4   69
.MC = 16   4,3,2
.VCY = 1   86
.B = 750    9,10,11,12,14,
.C = 4   39,
CHECKBIT IS 50   
*  SEND STROBE2 IF UN NOT FOUND AT WAKEUP
           
742:  M←K, 2, STOP
.RRN = 3   68,67
.MS = 76   64,63,62,61,60
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.C = 2   40,
           
743:  R0←Y←CPUNO*4+2454B, STORE, GOTO 1721B
.MS = 42   64,60      (FOR CPU 0)      .MS = 42   64,60      (FOR CPU 1)
.TCY = 1   44                          .TCY = 1   44
.TYW = 1   48                          .TYW = 1   48
.LR0 = 1   58                          .LR0 = 1   58
.MC = 1   5                            .MC = 1   5
.B = 1721   8,9,10,11,13,17            .B = 1721   8,9,10,11,13,17
.C = 2454   31,33,36,38,39             .C = 2460   31,33,36,37
                                       CHECKBIT IS 50

744:  S←3B6, CLEARA, GOTO TOSP2
.MS = 50   62,60
.TCX = 1   43
.TXW = 1   47
.LRN = 2   70
.MC = 1   5
.B = 1060    8,12,13,
.C = 3000000   22,23,
*

745:  PNIM:      IR←3, Q←0, GOTO PAGE4
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 750    9,10,11,12,14,
.C = 3   40,41,
CHECKBIT IS 50   

746:  PRO:       IR←2, Q←0, GOTO PAGE4
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 750    9,10,11,12,14,
.C = 2   40,

747:  MACC:      M←4B5, Q←E1, READS
.MS = 56   64,63,62,60
.TCX = 1   43
.LMX = 1   72
.LQY = 1   75
.TE1Y = 1   88
.C = 400000   24,

750:  PAGE4:     Q←M AND Q LCY 1, Q←PARAM, GOTO FIXTRP
.BL = 10   78
.SSP = 26   55,54,52
.MS = 1   65
.TSPY = 1   45
.LQX = 1   74
.LQY = 1   75
.MC = 1   5
.B = 1046    8,12,15,16,
*
*  SCAN PHYSICAL MAP FOR PAGE

751:  PAGE5:     R0←Y←CPUNO*4+2455B, FETCH
.MS = 44   63,60     (FOR CPU 0)       .MS = 44   63,60     (FOR CPU 1)
.TCY = 1   44                          .TCY = 1   44
.TYW = 1   48                          .TYW = 1   48
.LR0 = 1   58                          .LR0 = 1   58
.C = 2455   31,33,36,38,39,41          .C = 2461   31,33,36,37,41
                                       CHECKBIT IS 87   
           
752:  Z←M
.BL = 14   79,78
.LZX = 1   76
           
753:  SCAN, E1, M←4B7, GOTO PAGE6 IF Y<0
.MS = 63   65,64,61,60
.TCX = 1   43
.LMX = 1   72
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 755    9,10,11,12,14,15,17,
.C = -40000000   18,
CHECKBIT IS 50   
           
754:  STORE, IR←12, Q←0, GOTO FIXTRP
.MS = 42   64,60
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1046    8,12,15,16,
.C = 14   38,39,

755:  PAGE6:     STORE, GOTO IDLE3
.MS = 42   64,60
.MC = 20   1
.B = 1144    8,11,12,15,
*
*
*  Q=UN0, M=UN1, ROUTINE PUTS CHT INDEX+1 INTO R0
*

756:  CHTSRCH:   Q←IR←M EOR Q, M←132B3
.BL = 6   80,79
.TCY = 1   44
.TXW = 1   47
.LMY = 1   73
.LQX = 1   74
.LRN = 4   69
.C = 132000   26,28,29,31,
           
757:  2, PROTECT, GOTO * IF NPROTECT
.MS = 25   65,63,61
.TCX = 1   43
.MC = 30   2,1
.VCY = 1   86
.B = 757    9,10,11,12,14,15,16,17,
.C = 2   40,
CHECKBIT IS 50   
           
760:  M←M EOR Q LCY 8, Z←400B
.BL = 6   80,79
.MS = 5   65,63
.TCY = 1   44
.LMX = 1   72
.LZY = 1   77
.C = 400   33,
CHECKBIT IS 87   
           
761:  M←M EOR Q LCY 8 MRG 777774B2, Q←IR, .TCY←1
.RRN = 4   66
.BL = 6   80,79
.MS = 5   65,63
.TCX = 1   43
.TCY = 1   44
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
.C = -400   18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,
CHECKBIT IS 87   
           
762:  R0←M EOR Q+Z
.BR = 14   83,82
.BL = 6   80,79
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   

763:  CHT1:      FETCH, Q←77B6
.MS = 44   63,60
.TCX = 1   43
.LQX = 1   74
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   
           
764:  IR←R0←M AND NOT Q
.BL = 4   79
.TXW = 1   47
.LRN = 4   69
.LR0 = 1   58
CHECKBIT IS 87   
           
765:  FETCH, NOT M AND NOT Q, Q←UN0, GOTO PNIC IF LB=0
.BL = 1   81
.SSP = 24   54,52
.MS = 44   63,60
.TSPY = 1   45
.LQY = 1   75
.MC = 22   4,1
.B = 740    9,10,11,12,
           
766:  R0←R0+1, Q EOR M, GOTO CHT2 IF LB#0
.BL = 6   80,79
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 23   5,4,1
.B = 771    9,10,11,12,13,14,17,
CHECKBIT IS 30   
           
767:  FETCH, Q←UN1
.SSP = 25   56,54,52
.MS = 44   63,60
.TSPY = 1   45
.LQY = 1   75
           
770:  Q EOR M, R0←R0+1, GOTO CHT3 IF LB=0
.BL = 6   80,79
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 22   4,1
.B = 773    9,10,11,12,13,14,16,17,
CHECKBIT IS 30   

771:  CHT2:      Z←IR, Q←5, DGOTO CHT1
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
.B = 763    9,10,11,12,13,16,17,
.C = 5   39,41,
CHECKBIT IS 50
           
772:  R0←Z+Q, Q←77B6
.BR = 14   83,82
.BL = 12   80,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.VCY = 1   86
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   

773:  CHT3:      R0←R0+1, FETCH, Q←36B6, DRETURN
.MCONT = 2   6
.MS = 44   63,60
.IHR = 1   42
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.C = 36000000   19,20,21,22,
           
774:  M AND Q, Q←774B3, GOTO PNIC IF LB#0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.MC = 23   5,4,1
.B = 740    9,10,11,12,
.C = 774000   24,25,26,27,28,29,30,
*
*  K=BASE ADDRESS-1, DO NOT CHANGE HOLDING REGISTERS
*

775:  SSTATE:    R6←OS
.TYW = 1   48
.LRN = 6   70,69
.TOSY = 1   57
CHECKBIT IS 87   
           
776:  R0←K←K+1
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70
.LR0 = 1   58
CHECKBIT IS 87   
           
777:  M←P, CALL SST2 IF B
.MCONT = 1   7
.RRN = 1   68
.THY = 1   46
.LMY = 1   73
.MC = 34   3,2,1
.B = 1033    8,13,14,16,17,
CHECKBIT IS 30   
           
1000:  M←AR, CALL SST1
.MCONT = 1   7
.SSP = 1   56
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 1032    8,13,14,16,
           
1001:  M←BR, CALL SST1
.MCONT = 1   7
.SSP = 2   55
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.B = 1032    8,13,14,16,
           
1002:  M←CR, CALL SST1
.MCONT = 1   7
.SSP = 3   56,55
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 1032    8,13,14,16,
CHECKBIT IS 30   
           
1003:  M←DR, CALL SST1
.MCONT = 1   7
.SSP = 4   54
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.B = 1032    8,13,14,16,
           
1004:  M←ER, CALL SST1
.MCONT = 1   7
.SSP = 5   56,54
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 1032    8,13,14,16,
CHECKBIT IS 30   
           
1005:  R0←K←K+1
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70
.LR0 = 1   58
CHECKBIT IS 87   
           
1006:  M←XR, CALL SST2 IF B
.MCONT = 1   7
.SSP = 6   55,54
.TSPY = 1   45
.LMY = 1   73
.MC = 34   3,2,1
.B = 1033    8,13,14,16,17,
           
1007:  M←LR, CALL SST1
.MCONT = 1   7
.SSP = 7   56,55,54
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.B = 1032    8,13,14,16,
           
1010:  M←GR, CALL SST1
.MCONT = 1   7
.SSP = 10   53
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 1032    8,13,14,16,
           
1011:  CALL GETSR
.MCONT = 1   7
.MC = 20   1
.B = 662    9,10,12,13,16,
           
1012:  Z←R6, CALL SST1
.MCONT = 1   7
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.B = 1032    8,13,14,16,
CHECKBIT IS 30   
           
1013:  GOTO Z
.MCONT = 3   7,6
.BR = 14   83,82
.MC = 20   1
*
*  S=BASE ADDRESS
*

1014:  LSTATE:    R6←OS
.TYW = 1   48
.LRN = 6   70,69
.TOSY = 1   57
CHECKBIT IS 87   
           
1015:  R0←S, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.B = 1036    8,13,14,15,16,
           
1016:  R0←S←S+1, Q←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 1036    8,13,14,15,16,
CHECKBIT IS 30   
           
1017:  R0←S←S+1, AR←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 1   56
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 20   1
.B = 1036    8,13,14,15,16,
           
1020:  R0←S←S+1, BR←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 2   55
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 1   5
.B = 1036    8,13,14,15,16,
           
1021:  R0←S←S+1, CR←M, CALL LST1
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
.B = 1036    8,13,14,15,16,
CHECKBIT IS 30   
           
1022:  R0←S←S+1, DR←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 4   54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 1   5
.B = 1036    8,13,14,15,16,
           
1023:  R0←S←S+1, ER←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 5   56,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 20   1
.B = 1036    8,13,14,15,16,
CHECKBIT IS 30   
           
1024:  R0←S←S+1, XX←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 24   54,52
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 1   5
.B = 1036    8,13,14,15,16,
CHECKBIT IS 30   
           
1025:  R0←S←S+1, LR←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 7   56,55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 20   1
.B = 1036    8,13,14,15,16,
           
1026:  R0←S←S+1, GR←M, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.SSP = 10   53
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.LSPX = 1   59
.MC = 1   5
.B = 1036    8,13,14,15,16,
           
1027:  K←Q, CALL LOADSR
.MCONT = 1   7
.BL = 12   80,78
.TXW = 1   47
.LRN = 3   71,70
.MC = 20   1
.B = 666    9,10,12,13,15,16,
           
1030:  Z←R6
.RRN = 6   67,66
.THY = 1   46
.LZY = 1   77
CHECKBIT IS 87   
           
1031:  Q←K, GOTO Z
.MCONT = 3   7,6
.RRN = 3   68,67
.BR = 14   83,82
.THY = 1   46
.LQY = 1   75
.MC = 20   1
*
*  LOAD OR STORE ABSOLUTE DEPENDING ON B

1032:  SST1:      R0←K←K+1
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70
.LR0 = 1   58
CHECKBIT IS 87   

1033:  SST2:      MAP, GOTO SST3 IF B
.MS = 37   65,64,63,62,61
.MC = 34   3,2,1
.B = 1035    8,13,14,15,17,
           
1034:  R0←E1, GOTO CSTORE IF STERR, .VCY
.TYW = 1   48
.LR0 = 1   58
.MC = 32   4,2,1
.VCY = 1   86
.TE1Y = 1   88
.B = 701    9,10,11,17,

1035:  SST3:      E1, STORE, RETURN
.MCONT = 2   6
.MS = 42   64,60
.MC = 20   1
.TE1Y = 1   88
*

1036:  LST1:      MAP, GOTO LST2 IF B
.MS = 37   65,64,63,62,61
.MC = 34   3,2,1
.B = 1040    8,12,
CHECKBIT IS 30   
           
1037:  R0←E1, GOTO PAGEF IF Y<0
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,

1040:  LST2:      E1, FETCH, RETURN
.MCONT = 2   6
.MS = 44   63,60
.MC = 1   5
.TE1Y = 1   88
*
*  TRAPNO=TRAP, Q←PARAM

1041:  RDTRAP:    Z←GR, M←4, GOTO TOSP IF INTRPT1
.SSP = 10   53
.TCX = 1   43
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 64   3,1,0
.B = 1057    8,12,14,15,16,17,
.C = 4   39,
           
1042:  R0←K←Z+M, Z←M←P, ABCD0
.RRN = 1   68
.BR = 14   83,82
.BL = 14   79,78
.MS = 70   62,61,60
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LZY = 1   77
.LRN = 3   71,70
.LR0 = 1   58
.VCY = 1   86
           
1043:  R6←15000006B, SSOURCE, CALL STOR1
.MCONT = 1   7
.MS = 52   64,62,60
.TCX = 1   43
.TXW = 1   47
.LRN = 6   70,69
.MC = 20   1
.B = 403    9,16,17,
.C = 15000006   20,21,23,39,40,
           
1044:  M←Q, CALL STORX
.MCONT = 1   7
.BL = 12   80,78
.LMX = 1   72
.MC = 1   5
.B = 402    9,16,
           
1045:  R0←S←K+1, GOTO IA
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.MC = 20   1
.B = 101    11,17,
CHECKBIT IS 30   
*

1046:  FIXTRP:    S←Q, Z←6B5, SETB
.BL = 12   80,78
.MS = 26   64,63,61
.TCY = 1   44
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
.C = 600000   24,25,
           
1047:  SSOURCE, K←MBA MRG 2751B, CALL SSTATE
.MCONT = 1   7
.MS = 52   64,62,60
.TCY = 1   44
.TSPY = 1   45
.TYW = 1   48
.LRN = 3   71,70
.MC = 20   1
.B = 775    9,10,11,12,13,14,15,17,
.C = 2751   31,33,34,35,36,38,41,
           
1050:  M←S, Q←61B, X23LCK
.RRN = 2   67
.MS = 71   65,62,61,60
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.C = 61   36,37,41,
CHECKBIT IS 87   
           
1051:  4, Z←SR, PROTECT, GOTO * IF NPROTECT
.SSP = 11   56,53
.MS = 25   65,63,61
.TCX = 1   43
.TSPY = 1   45
.LZY = 1   77
.MC = 30   2,1
.VCY = 1   86
.B = 1051    8,12,14,17,
.C = 4   39,
CHECKBIT IS 50   
           
1052:  SR←Z AND NOT Q, Z←IR, CLM940
.RRN = 4   66
.BR = 4   83
.SSP = 11   56,53
.MS = 72   64,62,61,60
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
           
1053:  XR←Z
.BR = 14   83,82
.SSP = 6   55,54
.LSPX = 1   59
           
1054:  AR←M, Q←6B5, ABCD0, DGOTO NX5
.BL = 14   79,78
.SSP = 1   56
.MS = 70   62,61,60
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 350    10,11,12,14,
.C = 600000   24,25,
           
1055:  GR←Q, R0←P←Z←604002B
.BL = 12   80,78
.SSP = 10   53
.TCY = 1   44
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LR0 = 1   58
.LSPX = 1   59
.C = 604002   24,25,30,40,
*

1056:  T940:      P, GOTO NY8 IF M940
.RRN = 1   68
.THY = 1   46
.MC = 43   5,4,0
.B = 1177    8,11,12,13,14,15,16,17,

1057:  TOSP:      S←3B6, CLEARA, GOTO TOSP1 IF TO
.MS = 50   62,60
.TCX = 1   43
.TXW = 1   47
.LRN = 2   70
.MC = 72   4,2,1,0
.B = 1151    8,11,12,14,17,
.C = 3000000   22,23,
CHECKBIT IS 50   

1060:  TOSP2:     K←MBA MRG 2763B, SETB, CALL SSTATE
.MCONT = 1   7
.MS = 26   64,63,61
.TCY = 1   44
.TSPY = 1   45
.TYW = 1   48
.LRN = 3   71,70
.MC = 1   5
.B = 775    9,10,11,12,13,14,15,17,
.C = 2763   31,33,34,35,36,37,40,41,
CHECKBIT IS 50   
           
1061:  Z←CTCR, ALERT
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.C = 62   36,37,40,
           
1062:  PIN, M←E2, CALL SST1
.MCONT = 1   7
.MS = 16   64,63,62
.LMY = 1   73
.MC = 1   5
.TE2Y = 1   89
.B = 1032    8,13,14,16,
.VCY = 1       86
CHECKBIT IS 30
           
1063:  Z←ITR, ALERT
.MS = 14   63,62
.TCX = 1   43
.LZX = 1   76
.C = 60   36,37,
CHECKBIT IS 87   
           
1064:  Z←8, PIN, M←E2, CALL SST1
.MCONT = 1   7
.MS = 16   64,63,62
.TCX = 1   43
.LMY = 1   73
.LZX = 1   76
.MC = 1   5
.TE2Y = 1   89
.B = 1032    8,13,14,16,
.C = 10   38,
.VCY = 1       86
           
1065:  Q←M LCY 8, M←PRTINDEX, DGOTO TOSP3 IF SPANL
.BL = 14   79,78
.SSP = 21   56,52
.MS = 5   65,63
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.MC = 70   2,1,0
.DGO = 1   87
.B = 1072    8,12,13,14,16,
CHECKBIT IS 30   
           
1066:  R0←Z+M,  GOTO TOSP3 IF A
.BR = 14   83,82
.BL = 14   79,78
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.MC = 33   5,4,2,1
.VCY = 1   86
.B = 1072    8,12,13,14,16,
           
1067:  Q←12343210B, M←0
.TCX = 1   43
.LMY = 1   73
.LQX = 1   74
.C = 12343210   20,22,25,26,27,31,32,34,38,
CHECKBIT IS 87   
           
1070:  R0←Y←23B+CPUNO, STORE
.MS = 42   64,60     (FOR CPU 0)       .MS = 42   64,60     (FOR CPU 1)
.TCY = 1   44                          .TCY = 1   44
.TYW = 1   48                          .TYW = 1   48
.LR0 = 1   58                          .LR0 = 1   58
.C = 23   37,40,41                     .C = 24   37,39
CHECKBIT IS 87

1071:  BRK:       Q EOR M, Z←406B5, GOTO IDLE2 IF LB=0
.BL = 6   80,79
.TCY = 1   44
.LZY = 1   77
.MC = 22   4,1
.B = 1135    8,11,13,14,15,17,
.C = -37200000   18,24,25,

1072:  TOSP3:     Z←6B5, FETCH, GOTO BRK IF Z<0
.MS = 44   63,60
.TCX = 1   43
.LZX = 1   76
.MC = 4   3
.B = 1071    8,12,13,14,17,
.C = 600000   24,25,
CHECKBIT IS 50   
           
1073:  Z←Q LCY 1, Q←776B5
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = -200000   18,19,20,21,22,23,24,25,
CHECKBIT IS 87   
           
1074:  M←M AND NOT Q ! Z AND Q, Z←2, STORE
.BR = 10   82
.BL = 4   79
.MS = 42   64,60
.TCY = 1   44
.LMX = 1   72
.LZY = 1   77
.C = 2   40,
CHECKBIT IS 87   

1075:  TOSP4:     10B, PROTECT, GOTO * IF NPROTECT
.MS = 25   65,63,61
.TCX = 1   43
.MC = 30   2,1
.VCY = 1   86
.B = 1075    8,12,13,14,15,17,
.C = 10   38,
CHECKBIT IS 50   
           
1076:  R0←Y←USIBTOP, FETCH
.MS = 44   63,60
.TCY = 1   44
.TYW = 1   48
.LR0 = 1   58
.C = 15   38,39,41,
CHECKBIT IS 87   
           
1077:  M←M+Z, Q←USIBMSK
.BR = 14   83,82
.BL = 14   79,78
.TCY = 1   44
.TAX = 1   49
.LMX = 1   72
.LQY = 1   75
.VCY = 1   86
.C = 37   37,38,39,40,41,
CHECKBIT IS 87   
           
1100:  M AND Q, GOTO TOSP5 IF LB=0
.BL = 10   78
.MC = 22   4,1
.B = 1150    8,11,12,14,
           
1101:  STORE
.MS = 42   64,60
CHECKBIT IS 87   
           
1102:  R0←M, M←S, .VCY, .TSPY, .SSP←@PRTINDEX, CALL SST3,START/STOP
.MCONT = 1   7
.RRN = 2   67
.MS = 77       60,61,62,63,64,65
.BL = 14   79,78
.SSP = 21   56,52
.TSPY = 1   45
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.MC = 1   5
.VCY = 1   86
.B = 1035    8,13,14,15,17,
CHECKBIT IS 30   
           
1103:  M←CPUNO , R0←R0+1, STORE
.MS = 42   64,60     (FOR CPU 0)       .MS = 42   64,60     (FOR CPU 1)
        (THIS BIT NOT USED IN 0)       .LOC = 1   50
.IHR = 1   42                          .IHR = 1   42
.THY = 1   46                          .THY = 1   46
.TYW = 1   48                          .TYW = 1   48
        (THIS BIT NOT USED IN 0)       .TAX = 1   49
.LMX = 1   72                          .LMX = 1   72
.LR0 = 1   58                          .LR0 = 1   58
           
1104:  10B, M←0, UNPROTECT, DGOTO IDLE
.MS = 20   61
.TCX = 1   43
.LMY = 1   73
.MC = 1   5
.DGO = 1   87
.B = 1114    8,11,14,15,
.C = 10   38,
CHECKBIT IS 50   
           
1105:  2, STROBE
.MS = 17   65,64,63,62
.TCX = 1   43
.C = 2   40,
CHECKBIT IS 87   
*
*
*  SYSTEM INITIALIZING
*

1106:  RSET1:     17B, UNPROTECT
.MS = 20   61
.TCX = 1   43
.C = 17   38,39,40,41,
CHECKBIT IS 87   
           
1107:  2, LPF
.MS = 22   64,61
.TCX = 1   43
.C = 2   40,
CHECKBIT IS 87   
           
1110:  CLEARSP
.MS = 23   65,64,61
           
1111:  X23LCK
.MS = 71   65,62,61,60
CHECKBIT IS 87   
           
1112:  CRSL2
.MS = 32   64,62,61
       
1113:   
CHECKBIT IS 50
* SPARE INSTRUCTION
*
*

1114:  IDLE:      R0←6+CPUNO, GOTO IDLE1 IF NSP
.TCX = 1   43     (FOR CPU 0)          .TCX = 1   43     (FOR CPU 1)
.TXW = 1   47                          .TXW = 1   47
.LR0 = 1   58                          .LR0 = 1   58
.MC = 27   5,4,3,1                     .MC = 27   5,4,3,1
.B = 1116    8,11,14,15,16,            .B = 1116    8,11,14,15,16,
.C = 6   39,40,                        .C = 7   39,40,41,
CHECKBIT IS 50

1115:  CLEARSP, GOTO IDLEA
.MS = 23   65,64,61
.MC = 20   1
.B = 1122    8,11,13,16,
CHECKBIT IS 30   

1116:  IDLE1:     R0←Y←CPUNO*4+2455B
.TCY = 1   44     (FOR CPU 0)          .TCY = 1   44     (FOR CPU 1)
.TYW = 1   48                          .TYW = 1   48
.LR0 = 1   58                          .LR0 = 1   58
.C = 2455   31,33,36,38,39,41,         .C = 2461   31,33,36,37,41,
                                       CHECKBIT IS 87   
           
1117:  M, M←4B7, GOTO IDLE IF X<0
.BL = 14   79,78
.TCY = 1   44
.LMY = 1   73
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1114    8,11,14,15,
.C = -40000000   18,
CHECKBIT IS 50   
           
1120:  SCAN
.MS = 63   65,64,61,60
CHECKBIT IS 87   
           
1121:   GOTO IDLE
.MC = 20   1
.B = 1114    8,11,14,15,

1122:  IDLEA:     FETCH
.MS = 44   63,60
CHECKBIT IS 87   
           
1123:  Q←M, M←0, X23LCK, GOTO IDLEA IF LB=0
.BL = 14   79,78
.MS = 71   65,62,61,60
.LMY = 1   73
.LQX = 1   74
.MC = 22   4,1
.B = 1122    8,11,13,16,
CHECKBIT IS 30   
           
1124:  STORE, PRTINDEX←IR←Q
.BL = 12   80,78
.SSP = 21   56,52
.MS = 42   64,60
.TXW = 1   47
.LRN = 4   69
.LSPX = 1   59
           
1125:  R0←IR, FETCH
.RRN = 4   66
.MS = 44   63,60
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
CHECKBIT IS 87   
           
1126:  4, UNPROTECT
.MS = 20   61
.TCX = 1   43
.C = 4   39,
           
1127:  UN0←Q←M, R0←R0+1, FETCH
.BL = 14   79,78
.SSP = 24   54,52
.MS = 44   63,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.LSPX = 1   59
CHECKBIT IS 87   
*  NOTE THAT K IS PREPARED TO CRASH IF PNIC
           
1130:  UN1←M, K←4B7, SETB, CALL CHTSRCH
.MCONT = 1   7
.BL = 14   79,78
.SSP = 25   56,54,52
.MS = 26   64,63,61
.TCY = 1   44
.TYW = 1   48
.LRN = 3   71,70
.LSPX = 1   59
.MC = 1   5
.B = 756    9,10,11,12,14,15,16,
.C = -40000000   18,
           
1131:  MBA←M←M AND Q MRG 200B, CLEARMAP
.BL = 10   78
.MS = 74   63,62,61,60
.TCX = 1   43
.LMX = 1   72
.LSPX = 1   59
.C = 200   34,
           
1132:  Q←M LCY 12, R0←602764B
.BL = 14   79,78
.MS = 6   64,63
.TCY = 1   44
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.C = 602764   24,25,31,33,34,35,36,37,39,
*  SET DIRTY BIT IN CONTEXT BLOCK ENTRY
           
1133:  Z←Q LCY 1 MRG 1000B, DGOTO PATCH
.BL = 12   80,78
.MS = 1   65
.TCX = 1   43
.LZX = 1   76
.MC = 1        5
.DGO = 1   87
.B = 1715    8,9,10,11,14,15,17
.C = 1000   32,
           
1134:  ZTOMAP, .VCY, Z←R0
.MS = 53   65,64,62,60
.THY = 1   46
.LZY = 1   77
.VCY = 1   86

1135:  IDLE2:     S←MBA MRG 2764B, SSOURCE, CALL LSTATE
.MCONT = 1   7
.MS = 52   64,62,60
.TCY = 1   44
.TSPY = 1   45
.TYW = 1   48
.LRN = 2   70
.MC = 20   1
.B = 1014    8,14,15,
.C = 2764   31,33,34,35,36,37,39,
           
1136:  R0←S←S+1, Z←CTCL, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.IHR = 1   42
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 1036    8,13,14,15,16,
.C = 63   36,37,40,41,
           
1137:  Q←XX, P←Q, ALERT
.BL = 12   80,78
.SSP = 24   54,52
.MS = 14   63,62
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 1   71
CHECKBIT IS 87   
           
1140:  POT, Z←M, R0←S+1, CALL LST1
.MCONT = 1   7
.RRN = 2   67
.BL = 14   79,78
.MS = 15   65,63,62
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.B = 1036    8,13,14,15,16,
CHECKBIT IS 30   
           
1141:  XR←Q, Z←ITL, ALERT
.BL = 12   80,78
.SSP = 6   55,54
.MS = 14   63,62
.TCY = 1   44
.LZY = 1   77
.LSPX = 1   59
.C = 61   36,37,41,
CHECKBIT IS 87   
           
1142:  Z←M, POT
.BL = 14   79,78
.MS = 15   65,63,62
.LZX = 1   76
CHECKBIT IS 87   
           
1143:  2       *UNPROTECT REMOVED
.TCX = 1   43
.C = 2   40,
CHECKBIT IS 87   

1144:  IDLE3:     R0←S←Z←P, ABCD0
.RRN = 1   68
.MS = 70   62,61,60
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 2   70
.LR0 = 1   58
           
1145:  MFETCH, R0←E1, .VCY, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
           
1146:  R0+1, TAKE, DGOTO NX3
.MS = 75   65,63,62,61,60
.IHR = 1   42
.THY = 1   46
.MC = 1   5
.DGO = 1   87
.B = 36    13,14,15,16,
           
1147:  R6←M, Q←7B7, SSOURCE, GOTO 1712B IF INTRPT3
.BL = 14   79,78
.MS = 52   64,62,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.MC = 56   4,3,2,0
.B = 1712    8,9,10,11,14,16,
.C = -10000000   18,19,20,
*

1150:  TOSP5:     10B, UNPROTECT, GOTO TOSP4
.MS = 20   61
.TCX = 1   43
.MC = 1   5
.B = 1075    8,12,13,14,15,17,
.C = 10   38,
CHECKBIT IS 50   
*

1151:  TOSP1:     IR←5, Q←0, GOTO FIXTRP
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 1046    8,12,15,16,
.C = 5   39,41,
*
*
*
*
*
*

1152:  NY1:       Z←P←P+1
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
CHECKBIT IS 87   

1153:  NY2:       M←NI, SABCDE0, GOTO NY6 IF INTRPT, .C←1, .TSPY←0
.SSP = 50   53,51
.MS = 33   65,64,62,61
.LMY = 1   73
.MC = 41   5,0
.B = 1176    8,11,12,13,14,15,16,
.C = 1   41,

1154:  NY3:       R0←H2←M LCY 1, Z←XR, DGOTO INDEX IF M[1]
.BL = 14   79,78
.SSP = 6   55,54
.MS = 1   65
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 3   71,70
.LR0 = 1   58
.MC = 21   5,1
.DGO = 1   87
.B = 1203    8,10,16,17,
CHECKBIT IS 30   
           
1155:  H3←X←M, Q←7774B4, SETA, GOTO POP940 IF R0[1]
.BL = 14   79,78
.MS = 30   62,61
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 66   4,3,1,0
.B = 1367    8,10,11,12,13,15,16,17,
.C = -40000   18,19,20,21,22,23,24,25,26,27,
CHECKBIT IS 50   

1156:  NY4:       Z←R0←H1←M AND NOT Q, GOTO INDIRECT IF M[9]
.BL = 4   79
.TXW = 1   47
.LZX = 1   76
.LRN = 2   70
.LR0 = 1   58
.MC = 71   5,2,1,0
.B = 1204    8,10,15,
CHECKBIT IS 30   

1157:  NY5:       M←H2, Q←176B5, GOTO NY9
.RRN = 3   68,67
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 1        5
.B = 1161    8,11,12,13,17,
.C = 17600000   20,21,22,23,24,25,
CHECKBIT IS 50   
           
1160:  GOTO TOSP
.MC = 1   5
.B = 1057    8,12,14,15,16,17,

1161:  NY9:       Q←AR, DGOTO M AND Q LCY 8 MRG BASE940 IF A
.MCONT = 3   7,6
.BL = 10   78
.SSP = 1   56
.MS = 5   65,63
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.MC = 33   5,4,2,1
.DGO = 1   87
.C = 1600   32,33,34,
           
1162:  MFETCH, .VCY, R0←E1, DGOTO ASGNA IF A
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 33   5,4,2,1
.DGO = 1   87
.VCY = 1   86
.TE1Y = 1   88
.B = 1216    8,10,14,15,16,
*  A HAS BEEN CLEARED IF THE INSTRUCTION IS A SYSPOP
           
1163:  M←H2, Q←176B5, RESETCM
.RRN = 3   68,67
.MS = 24   63,61
.TCX = 1   43
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.C = 17600000   20,21,22,23,24,25,
CHECKBIT IS 87   
           
1164:  IR←M AND Q LCY 8, Q←LR
.BL = 10   78
.SSP = 7   56,55,54
.MS = 5   65,63
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
CHECKBIT IS 87   
           
1165:  Z←3, ABCD0
.MS = 70   62,61,60
.TCX = 1   43
.LZX = 1   76
.C = 3   40,41,
           
1166:  Z←R0←Z+Q, M←P, CALL STOR1
.MCONT = 1   7
.RRN = 1   68
.BR = 14   83,82
.BL = 12   80,78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.VCY = 1   86
.B = 403    9,16,17,
           
1167:  K←Z-1, CALL SSTATE
.MCONT = 1   7
.BR = 14   83,82
.BL = 17   81,80,79,78
.TXW = 1   47
.TAX = 1   49
.LRN = 3   71,70
.MC = 20   1
.VCY = 1   86
.B = 775    9,10,11,12,13,14,15,17,
           
1170:  Q←LR, Z←9
.SSP = 7   56,55,54
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
.C = 11   38,41,
           
1171:  R0←Q+Z, M←XR, CALL STOR1
.MCONT = 1   7
.BR = 14   83,82
.BL = 12   80,78
.SSP = 6   55,54
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 403    9,16,17,
           
1172:  M←IR
.RRN = 4   66
.THY = 1   46
.LMY = 1   73
           
1173:  XR←M, M←H1
.RRN = 2   67
.BL = 14   79,78
.SSP = 6   55,54
.THY = 1   46
.LMY = 1   73
.LSPX = 1   59
CHECKBIT IS 87   
           
1174:  AR←M, Z←2, DGOTO NX5
.BL = 14   79,78
.SSP = 1   56
.TCY = 1   44
.LZY = 1   77
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 350    10,11,12,14,
.C = 2   40,
           
1175:  R0←Z←P←Z+Q, CLM940
.BR = 14   83,82
.BL = 12   80,78
.MS = 72   64,62,61,60
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LRN = 1   71
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
*
*  PAGE CROSSING IN THE INSTUCTION SEQUENCE

1176:  NY6:       R0←P+1, .C←1, GOTO TOSP IF INTRPT2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 65   5,3,1,0
.B = 1057    8,12,14,15,16,17,
.C = 1   41,
CHECKBIT IS 30   

1177:  NY8:       MAP, DGOTO NY3
.MS = 37   65,64,63,62,61
.MC = 20   1
.DGO = 1   87
.B = 1154    8,11,12,14,15,
CHECKBIT IS 30   
           
1200:  R0←E1, TAKE, GOTO PAGEF IF Y<0
.MS = 75   65,63,62,61,60
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
*
*  ENTRY TO THE MAIN LOOP AT INSTRUCTION R0=Z

1201:  NY7:       MFETCH, R0←E1, .VCY, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
           
1202:  GOTO BRU1, ABCD0
.MS = 70   62,61,60
.MC = 1   5
.B = 1224    8,10,13,15,
CHECKBIT IS 30   
*
*

1203:  INDEX:     Z←M+Z, GOTO INDA
.BR = 14   83,82
.BL = 14   79,78
.TAX = 1   49
.LZX = 1   76
.MC = 20   1
.VCY = 1   86
.B = 1702    8,9,10,11,16,
* 

1204:  INDIRECT:  MFETCH, .VCY, R0←E1, Z←114B5, GOTO TOSP IF INTRPT1
.MS = 34   63,62,61
.TCX = 1   43
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 64   3,1,0
.VCY = 1   86
.TE1Y = 1   88
.B = 1057    8,12,14,15,16,17,
.C = 11400000   20,23,24,
           
1205:  Q←H2
.RRN = 3   68,67
.THY = 1   46
.LQY = 1   75
CHECKBIT IS 87   
           
1206:  Q←Q EOR Z, Z←176B5
.BR = 6   84,83
.TCY = 1   44
.LQX = 1   74
.LZY = 1   77
.C = 17600000   20,21,22,23,24,25,
           
1207:  Z←Q AND Z, Q←H2
.RRN = 3   68,67
.BR = 10   82
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
CHECKBIT IS 87   
           
1210:  Z←132B5, GOTO RCH IF Z=0
.TCX = 1   43
.LZX = 1   76
.MC = 2   4
.B = 1646    8,9,10,12,15,16,
.C = 13200000   20,22,23,25,
           
1211:  Z←Q AND Z
.BR = 10   82
.LZX = 1   76
CHECKBIT IS 87   
           
1212:  Q←7774B4, GOTO NY5 IF Z=0
.TCX = 1   43
.LQX = 1   74
.MC = 2   4
.B = 1157    8,11,12,14,15,16,17,
.C = -40000   18,19,20,21,22,23,24,25,26,27,
CHECKBIT IS 50   
           
1213:  H3←M, Z←XR, DGOTO INDEX IF M[1]
.BL = 14   79,78
.SSP = 6   55,54
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
.MC = 21   5,1
.DGO = 1   87
.B = 1203    8,10,16,17,
           
1214:  GOTO PAGEF IF R0<0
.MC = 11   5,2
.B = 700    9,10,11,
           
1215:  GOTO NY4
.MC = 20   1
.B = 1156    8,11,12,14,15,16,
* 

1216:  ASGNA:     GOTO PAGEF IF R0<0
.MC = 11   5,2
.B = 700    9,10,11,
           
1217:  AR←M, DGOTO NY2
.BL = 14   79,78
.SSP = 1   56
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1153    8,11,12,14,16,17,
CHECKBIT IS 30   
           
1220:  Z←P←P+1
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
CHECKBIT IS 87   
* 

1221:  BRX1:      XR←Z←Z+1, Q←4B4
.BR = 14   83,82
.SSP = 6   55,54
.LOC = 1   50
.TCY = 1   44
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.LSPX = 1   59
.VCY = 1   86
.C = 40000   27,
CHECKBIT IS 87   
           
1222:  Z←Z AND Q, DGOTO 1710B
.BR = 10   82
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 1710    8,9,10,11,14,
           
1223:  Z←H1, GOTO NY1 IF Z=0, ABCD0
.RRN = 2   67
.MS = 70   62,61,60
.THY = 1   46
.LZY = 1   77
.MC = 2   4
.B = 1152    8,11,12,14,16,
CHECKBIT IS 30   
*  BRU, Z=EFF. ADDRESS, M=NEW INSTR. , R0=Z MAPPED

1224:  BRU1:      P←Z, R0+1, TAKE, DGOTO NY3
.BR = 14   83,82
.MS = 75   65,63,62,61,60
.IHR = 1   42
.THY = 1   46
.TXW = 1   47
.LRN = 1   71
.MC = 1   5
.DGO = 1   87
.B = 1154    8,11,12,14,15,
CHECKBIT IS 30   
           
1225:  R0←P+1, SABCDE0, GOTO 1706B IF INTRPT, .C←1
.RRN = 1   68
.MS = 33   65,64,62,61
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 41   5,0
.B = 1706    8,9,10,11,15,16,
.C = 1   41,
CHECKBIT IS 50
* 

1226:  XMA1:      R0, GOTO CSTORE IF STERR, .VCY
.THY = 1   46
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
           
1227:  AR←M, M←H1
.RRN = 2   67
.BL = 14   79,78
.SSP = 1   56
.THY = 1   46
.LMY = 1   73
.LSPX = 1   59

1230:  ST940:     R0, GOTO CSTORE IF STERR, .VCY
.THY = 1   46
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
           
1231:  STORE, Z←P←P+1, GOTO NY2
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 1153    8,11,12,14,16,17,
*

1232:  BRM1:      Z←H1+1, DGOTO BRM2 IF OV
.RRN = 2   67
.IHR = 1   42
.THY = 1   46
.LZY = 1   77
.MC = 37   5,4,3,2,1
.DGO = 1   87
.B = 1235    8,10,13,14,15,17,
CHECKBIT IS 30   

1233:  BRM3:      M←M OR Q, Q←4B7, GOTO CSTORE IF R0[2]
.BL = 16   80,79,78
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 24   3,1
.B = 701    9,10,11,17,
.C = -40000000   18,
           
1234:  M←M EOR Q
.BL = 6   80,79
.LMX = 1   72

1235:  BRM2:      STORE, DGOTO NY7
.MS = 42   64,60
.MC = 20   1
.DGO = 1   87
.B = 1201    8,10,17,
           
1236:  R0←Z
.BR = 14   83,82
.TXW = 1   47
.LR0 = 1   58
CHECKBIT IS 87   
* 

1237:  SKB1:      Z←M AND Q
.BL = 10   78
.LZX = 1   76
CHECKBIT IS 87   

1240:  SKE1:      Z←P, Q←2, GOTO NY1 IF Z#0
.RRN = 1   68
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 3   5,4
.B = 1152    8,11,12,14,16,
.C = 2   40,
           
1241:  R0←Z←Z+Q, GOTO NY7
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 1201    8,10,17,
* 

1242:  BRR1:      Z←R0←M AND NOT Q, Q←4B7, GOTO NY7 IF Z>=0
.BL = 4   79
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LZX = 1   76
.LR0 = 1   58
.MC = 5   5,3
.B = 1201    8,10,17,
.C = -40000000   18,
CHECKBIT IS 50   
           
1243:  Q+M, ROV, GOTO NY7
.BR = 12   84,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.TAX = 1   49
.MC = 20   1
.VCY = 1   86
.B = 1201    8,10,17,
CHECKBIT IS 30   
* 
*

1244:  SUC1:      GOTO SUB IF Z<0
.MC = 4   3
.B = 1654    8,9,10,12,14,15,
           
1245:  M←Q+NOT M, ROV, GOTO ADD1
.BR = 12   84,82
.BL = 3   81,80
.MS = 66   64,63,61,60
.TAX = 1   49
.LMX = 1   72
.MC = 20   1
.VCY = 1   86
.B = 1250    8,10,12,14,
CHECKBIT IS 30   

1246:  ADC1:      GOTO ADD IF Z>=0
.MC = 5   5,3
.B = 1655    8,9,10,12,14,15,17,
           
1247:  M←Q+M, .LOC, ROV
.BR = 12   84,82
.BL = 14   79,78
.MS = 66   64,63,61,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
CHECKBIT IS 87   

1250:  ADD1:      AR←M, Q←4B7, GOTO ADD2 IF CAR
.BL = 14   79,78
.SSP = 1   56
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 46   4,3,0
.B = 1252    8,10,12,14,16,
.C = -40000000   18,
           
1251:  XR←Z AND NOT Q, Z←P←P+1, GOTO NY2
.RRN = 1   68
.BR = 4   83
.SSP = 6   55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 1153    8,11,12,14,16,17,

1252:  ADD2:      XR←Z OR Q, Z←P←P+1, GOTO NY2
.RRN = 1   68
.BR = 16   84,83,82
.SSP = 6   55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 1   5
.B = 1153    8,11,12,14,16,17,
* 

1253:  SKR1:      Z←M, Q←2, GOTO CSTORE IF R0[2]
.BL = 14   79,78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 24   3,1
.B = 701    9,10,11,17,
.C = 2   40,
CHECKBIT IS 50   
           
1254:  STORE, Z←P, GOTO NY1 IF Z>=0
.RRN = 1   68
.MS = 42   64,60
.THY = 1   46
.LZY = 1   77
.MC = 5   5,3
.B = 1152    8,11,12,14,16,
CHECKBIT IS 30   
           
1255:  R0←Z←Z+Q, GOTO NY7
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 1201    8,10,17,
* 

1256:  SKM1:      Z←M AND Q, GOTO SKE1
.BL = 10   78
.LZX = 1   76
.MC = 1   5
.B = 1240    8,10,12,
CHECKBIT IS 30   
* 

1257:  EAX1:      Z←H1
.RRN = 2   67
.THY = 1   46
.LZY = 1   77

1260:  LDX1:      XR←M AND NOT Q ! Q AND Z, Z←P←P+1, GOTO NY2
.RRN = 1   68
.BR = 10   82
.BL = 4   79
.SSP = 6   55,54
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 1   5
.B = 1153    8,11,12,14,16,17,
CHECKBIT IS 30   
* 

1261:  LDB1:      BR←M, Z←P←P+1, GOTO NY2
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
.B = 1153    8,11,12,14,16,17,
*

1262:  SKG1:      GOTO SKG2 IF OVFLW
.MC = 57   5,4,3,2,0
.B = 1265    8,10,12,13,15,17,

1263:  SKN1:      Z←P, Q←2, GOTO NY1 IF Z>=0
.RRN = 1   68
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 5   5,3
.B = 1152    8,11,12,14,16,
.C = 2   40,
           
1264:  Z←R0←Z+Q, GOTO NY7
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.VCY = 1   86
.B = 1201    8,10,17,

1265:  SKG2:      Z←P, Q←2, GOTO NY1 IF Z<0
.RRN = 1   68
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 4   3
.B = 1152    8,11,12,14,16,
.C = 2   40,
CHECKBIT IS 50   
           
1266:  R0←Z←Z+Q, GOTO NY7
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.VCY = 1   86
.B = 1201    8,10,17,
*

1267:  ROV1:      Q AND M, M←2, GOTO REO IF LB#0
.BL = 10   78
.TCY = 1   44
.LMY = 1   73
.MC = 23   5,4,1
.B = 1274    8,10,12,13,14,15,
.C = 2   40,
           
1270:  Q AND M, Q←E1, GSB, GOTO ROV2 IF LB=0
.BL = 10   78
.MS = 27   65,64,63,61
.LQY = 1   75
.MC = 22   4,1
.TE1Y = 1   88
.B = 1272    8,10,12,13,14,16,
           
1271:  CLEAROV
.MS = 73   65,64,62,61,60

1272:  ROV2:      GOTO NY1 IF NZ[16]
.MC = 10   2
.B = 1152    8,11,12,14,16,
CHECKBIT IS 30   
           
1273:  Z←Q AND M, GOTO SKE1
.BL = 10   78
.LZX = 1   76
.MC = 20   1
.B = 1240    8,10,12,
CHECKBIT IS 30   

1274:  REO:       Q←M←XR, Z←37777B3
.SSP = 6   55,54
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQY = 1   75
.LZX = 1   76
.C = 37777000   19,20,21,22,23,24,25,26,27,28,29,30,31,32,
           
1275:  M←M LCY 1
.BL = 14   79,78
.MS = 1   65
.LMX = 1   72
CHECKBIT IS 87   
           
1276:  M←M EOR Q, Q←1B3, DGOTO NY2
.BL = 6   80,79
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 1153    8,11,12,14,16,17,
.C = 1000   32,
CHECKBIT IS 50   
           
1277:  M AND Q+Z, ROV, Z←P←P+1
.RRN = 1   68
.BR = 14   83,82
.BL = 10   78
.MS = 66   64,63,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.TAX = 1   49
.LZY = 1   77
.LRN = 1   71
.VCY = 1   86
*

1300:  RCH1:      M←M LCY 12, H2←0, GOTO RCH2 IF NZ[16]
.BL = 14   79,78
.MS = 6   64,63
.TYW = 1   48
.LMX = 1   72
.LRN = 3   71,70
.MC = 10   2
.B = 1302    8,10,11,16,
           
1301:  Q←77777B3
.TCX = 1   43
.LQX = 1   74
.C = -1000   18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,

1302:  RCH2:      M←M LCY 1, Z←AR, CALL CNA IF X<0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 1   56
.MS = 1   65
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1326    8,10,11,13,15,16,
           
1303:  M←M LCY 1, Z←AR, CALL CX IF X<0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 1   56
.MS = 1   65
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1331    8,10,11,13,14,17,
           
1304:  M←M LCY 1, Z←XR, CALL CA IF X<0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 6   55,54
.MS = 1   65
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1327    8,10,11,13,15,16,17,
           
1305:  M←M LCY 2, CALL CB IF X<0
.MCONT = 1   7
.BL = 14   79,78
.MS = 2   64
.LMX = 1   72
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1330    8,10,11,13,14,
           
1306:  M←M LCY 1, Z←BR, CALL CX IF X<0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 2   55
.MS = 1   65
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1331    8,10,11,13,14,17,
           
1307:  M←M LCY 1, Z←BR, CALL CA IF X<0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 2   55
.MS = 1   65
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1327    8,10,11,13,15,16,17,
CHECKBIT IS 30   
           
1310:  M←M LCY 1, Z←AR, CALL CB IF X<0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 1   56
.MS = 1   65
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1330    8,10,11,13,14,
CHECKBIT IS 30   
           
1311:  M←M LCY 1, Z←0, CALL CB IF X<0
.MCONT = 1   7
.BL = 14   79,78
.MS = 1   65
.LMX = 1   72
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1330    8,10,11,13,14,
CHECKBIT IS 30   
           
1312:  M←M LCY 1, CALL CA IF X<0
.MCONT = 1   7
.BL = 14   79,78
.MS = 1   65
.LMX = 1   72
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1327    8,10,11,13,15,16,17,
           
1313:  M←M LCY 2, CALL CX IF X<0
.MCONT = 1   7
.BL = 14   79,78
.MS = 2   64
.LMX = 1   72
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1331    8,10,11,13,14,17,
CHECKBIT IS 30   
           
1314:  Z←R0, CALL CCA IF A
.MCONT = 1   7
.THY = 1   46
.LZY = 1   77
.MC = 33   5,4,2,1
.B = 1320    8,10,11,13,
           
1315:  Z←H1, CALL CCB IF B
.MCONT = 1   7
.RRN = 2   67
.THY = 1   46
.LZY = 1   77
.MC = 34   3,2,1
.B = 1322    8,10,11,13,16,
CHECKBIT IS 30   
           
1316:  Z←H2, CALL CCX IF D
.MCONT = 1   7
.RRN = 3   68,67
.THY = 1   46
.LZY = 1   77
.MC = 47   5,4,3,0
.B = 1324    8,10,11,13,15,
CHECKBIT IS 30   
           
1317:  Z←P←P+1, GOTO NY2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 20   1
.B = 1153    8,11,12,14,16,17,
*

1320:  CCA:       M←AR, DRETURN
.MCONT = 2   6
.SSP = 1   56
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.DGO = 1   87
CHECKBIT IS 30   
           
1321:  AR←M AND Q ! Z AND NOT Q
.BR = 4   83
.BL = 10   78
.SSP = 1   56
.LSPX = 1   59
CHECKBIT IS 87   

1322:  CCB:       M←BR, DRETURN
.MCONT = 2   6
.SSP = 2   55
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.DGO = 1   87
CHECKBIT IS 30   
           
1323:  BR←M AND Q ! Z AND NOT Q
.BR = 4   83
.BL = 10   78
.SSP = 2   55
.LSPX = 1   59
CHECKBIT IS 87   

1324:  CCX:       XR←Z AND NOT Q, RETURN IF NZ[15]
.MCONT = 2   6
.BR = 4   83
.SSP = 6   55,54
.LSPX = 1   59
.MC = 7   5,4,3
CHECKBIT IS 30   
           
1325:  XR←Z OR Q, RETURN
.MCONT = 2   6
.BR = 16   84,83,82
.SSP = 6   55,54
.LSPX = 1   59
.MC = 20   1
CHECKBIT IS 30   
*

1326:  CNA:       Z←NOT Z+1
.BR = 3   85,84
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.VCY = 1   86
CHECKBIT IS 87   

1327:  CA:        R0←R0, .TXW, Z, SETA, RETURN
.MCONT = 2   6
.BR = 14   83,82
.MS = 30   62,61
.THY = 1   46
.TXW = 1   47
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
CHECKBIT IS 30   

1330:  CB:        H1←H1, .TXW, Z, SETB, RETURN
.MCONT = 2   6
.RRN = 2   67
.BR = 14   83,82
.MS = 26   64,63,61
.THY = 1   46
.TXW = 1   47
.TYW = 1   48
.LRN = 2   70
.MC = 1   5
CHECKBIT IS 30   

1331:  CX:        H2←H2, .TXW, Z, SETD, RETURN
.MCONT = 2   6
.RRN = 3   68,67
.BR = 14   83,82
.MS = 36   64,63,62,61
.THY = 1   46
.TXW = 1   47
.TYW = 1   48
.LRN = 3   71,70
.MC = 20   1
*

1332:  SKD1:      M←Z AND Q, Z←BR, GOTO SKD2 IF NZ[15]
.BR = 10   82
.SSP = 2   55
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 7   5,4,3
.B = 1334    8,10,11,13,14,15,
CHECKBIT IS 30   
           
1333:  M←M OR NOT Q
.BL = 15   81,79,78
.LMX = 1   72
CHECKBIT IS 87   

1334:  SKD2:      Q←Z AND Q, Z←777B, GOTO SKD3 IF NZ[15]
.BR = 10   82
.TCY = 1   44
.LQX = 1   74
.LZY = 1   77
.MC = 7   5,4,3
.B = 1336    8,10,11,13,14,15,16,
.C = 777   33,34,35,36,37,38,39,40,41,
           
1335:  Q←Q OR NOT Z
.BR = 13   85,84,82
.LQX = 1   74
CHECKBIT IS 87   

1336:  SKD3:      R0←Z←Q-M
.BR = 12   84,82
.BL = 3   81,80
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
1337:  M←Q EOR M, Q←777B, GOTO SKD4 IF R0>=0
.BL = 6   80,79
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 12   4,2
.B = 1341    8,10,11,12,17,
.C = 777   33,34,35,36,37,38,39,40,41,
           
1340:  Z←NOT Z+1
.BR = 3   85,84
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.VCY = 1   86
CHECKBIT IS 87   

1341:  SKD4:      XR←M AND NOT Q ! Q AND Z, Z←R0, GOTO SKN1
.BR = 10   82
.BL = 4   79
.SSP = 6   55,54
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
.MC = 20   1
.B = 1263    8,10,12,13,16,17,
CHECKBIT IS 30   
*

1342:  RS1:       R0←M LCY 8, M←H1
.RRN = 2   67
.BL = 14   79,78
.MS = 5   65,63
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
           
1343:  CLEARA, M←M AND Q, Q←48, DGOTO RCY IF R0[2]
.BL = 10   78
.MS = 50   62,60
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 24   3,1
.DGO = 1   87
.B = 1346    8,10,11,12,15,16,
.C = 60   36,37,
           
1344:  Z←NOT M+1, M←H3, DGOTO ASDR IF NR0[2]
.RRN = 4   66
.BL = 3   81,80
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.MC = 25   5,3,1
.DGO = 1   87
.VCY = 1   86
.B = 534    9,11,13,14,15,
CHECKBIT IS 30   
           
1345:  M←BR, Q←77B, GOTO NY1 IF Z=0
.SSP = 2   55
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.MC = 2   4
.B = 1152    8,11,12,14,16,
.C = 77   36,37,38,39,40,41,

1346:  RCY:       M LCY 12, Q←48, GOTO LRSH IF X<0
.BL = 14   79,78
.MS = 6   64,63
.TCY = 1   44
.LQY = 1   75
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1351    8,10,11,12,14,17,
.C = 60   36,37,
           
1347:  R0←Z←Q+Z, DGOTO CYDL
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 452    9,12,14,16,
           
1350:  Q←77B, GOTO NX1 IF R0<0
.TCX = 1   43
.LQX = 1   74
.MC = 11   5,2
.B = 34    13,14,15,
.C = 77   36,37,38,39,40,41,

1351:  LRSH:      M←BR, Q←77B, SETA, GOTO ASDR
.SSP = 2   55
.MS = 30   62,61
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.MC = 20   1
.B = 534    9,11,13,14,15,
.C = 77   36,37,38,39,40,41,

1352:  LS1:       M←M LCY 2, Z←H1
.RRN = 2   67
.BL = 14   79,78
.MS = 2   64
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
           
1353:  CLEARA, Z←Z AND Q, R0←Q←48, GOTO NORM IF M[9]
.BR = 10   82
.MS = 50   62,60
.TCY = 1   44
.TYW = 1   48
.LQY = 1   75
.LZX = 1   76
.LR0 = 1   58
.MC = 71   5,2,1,0
.B = 1356    8,10,11,12,14,15,16,
.C = 60   36,37,
           
1354:  M LCY 8, M←BR, GOTO ASDL IF X>=0
.BL = 14   79,78
.SSP = 2   55
.MS = 5   65,63
.TSPY = 1   45
.LMY = 1   73
.MC = 16   4,3,2
.VCY = 1   86
.B = 511    9,11,14,17,
           
1355:  R0←Q-Z, DGOTO CYDL
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 452    9,12,14,16,

1356:  NORM:      S←M←AR, SETC, GOTO NY1 IF R0<0
.SSP = 1   56
.MS = 35   65,63,62,61
.TSPY = 1   45
.TYW = 1   48
.LMY = 1   73
.LRN = 2   70
.MC = 11   5,2
.B = 1152    8,11,12,14,16,
CHECKBIT IS 30   
           
1357:  Q←BR, K←Z, SETBB, CALL LLT1
.MCONT = 1   7
.BR = 14   83,82
.SSP = 2   55
.MS = 60   61,60
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 3   71,70
.MC = 20   1
.B = 636    9,10,13,14,15,16,
CHECKBIT IS 30   
           
1360:  Z←Z-1, M←XR, GOTO NORM1 IF Z#0
.BR = 14   83,82
.BL = 17   81,80,79,78
.SSP = 6   55,54
.TSPY = 1   45
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.MC = 3   5,4
.VCY = 1   86
.B = 1363    8,10,11,12,13,16,17,
CHECKBIT IS 30   
           
1361:  Z←47, S, GOTO NORM1 IF Y<0
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.LZX = 1   76
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1363    8,10,11,12,13,16,17,
.C = 57   36,38,39,40,41,
           
1362:  Z←48
.TCX = 1   43
.LZX = 1   76
.C = 60   36,37,
CHECKBIT IS 87   

1363:  NORM1:     Q-Z, GOTO NORM2 IF X>=0
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.TAX = 1   49
.MC = 16   4,3,2
.VCY = 1   86
.B = 1365    8,10,11,12,13,15,17,
           
1364:  Z←Q
.BL = 12   80,78
.LZX = 1   76

1365:  NORM2:     XR←M-Z, DGOTO ASDL
.BR = 3   85,84
.BL = 14   79,78
.SSP = 6   55,54
.LOC = 1   50
.TAX = 1   49
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 511    9,11,14,17,
CHECKBIT IS 30   
           
1366:  M←BR, SETA
.SSP = 2   55
.MS = 30   62,61
.TSPY = 1   45
.LMY = 1   73
*
*

1367:  POP940:    H3, GOTO SYSPOP IF Y<0
.RRN = 4   66
.THY = 1   46
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1374    8,10,11,12,13,14,15,
CHECKBIT IS 30   
           
1370:  M←H2, R0←0
.RRN = 3   68,67
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
CHECKBIT IS 87   
           
1371:  MAP, .VCY, R0←E1, Q←376B5, GOTO CSTORE IF STERR
.MS = 37   65,64,63,62,61
.TCX = 1   43
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 32   4,2,1
.VCY = 1   86
.TE1Y = 1   88
.B = 701    9,10,11,17,
.C = 37600000   19,20,21,22,23,24,25,
           
1372:  Z←M AND Q LCY 8, Q←4324B4, DGOTO BRM3
.BL = 10   78
.MS = 5   65,63
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,
.C = -34540000   18,22,23,25,27,
           
1373:  M←P, CLEAROV, DGOTO BRM2 IF OV
.RRN = 1   68
.MS = 73   65,64,62,61,60
.THY = 1   46
.LMY = 1   73
.MC = 37   5,4,3,2,1
.DGO = 1   87
.B = 1235    8,10,13,14,15,17,
CHECKBIT IS 30   
*

1374:  SYSPOP:    CLEARA, DGOTO NY4
.MS = 50   62,60
.MC = 1   5
.DGO = 1   87
.B = 1156    8,11,12,14,15,16,
CHECKBIT IS 30   
           
1375:  GOTO INDEX IF M[1]
.MC = 21   5,1
.B = 1203    8,10,16,17,
CHECKBIT IS 30   
*
*
*
*
           
1376:  R0←P+1, GOTO NX6 IF XPAGE
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 42   4,0
.B = 32    13,14,16,
CHECKBIT IS 30
           
1377:  GOTO NY3
.MC = 20   1
.B = 1154    8,11,12,14,15,
CHECKBIT IS 30   
