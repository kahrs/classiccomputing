           ORG 2750B;

2750:  BASEFP:    R0←R0
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
           
2751:  Q←Q LCY 12, R0←1, GOTO FP2
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 20   1
.B = 765    9,10,11,12,13,15,17,
.C = 1   41,
           
2752:  Q←Q LCY 12, R0←3, GOTO FP2
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 1   5
.B = 765    9,10,11,12,13,15,17,
.C = 3   40,41,
CHECKBIT IS 50   
           
2753:  Q←Q LCY 12, R0←7, GOTO FP2
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 20   1
.B = 765    9,10,11,12,13,15,17,
.C = 7   39,40,41,
           
2754:  Z←Q←IR←RES, GOTO FP1
.RRN = 5   68,66
.THY = 1   46
.TYW = 1   48
.LQY = 1   75
.LZY = 1   77
.LRN = 4   69
.MC = 1   5
.B = 761    9,10,11,12,13,17,
CHECKBIT IS 30   
           
2755:  Z←P←P+1, ER←0, SETBA, GOTO NX2
.RRN = 1   68
.SSP = 5   56,54
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.B = 35    13,14,15,17,
           
2756:  AR←RES←6B7, M←K
.RRN = 3   68,67
.SSP = 1   56
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 5   71,69
.LSPX = 1   59
.C = -20000000   18,19,
CHECKBIT IS 87   
           
2757:  Q←1B4, DGOTO TFO
.TCX = 1   43
.LQX = 1   74
.MC = 20   1
.DGO = 1   87
.B = 1234    8,10,13,14,15,
.C = 10000   29,
CHECKBIT IS 50   
           
2760:  ER←M←M+Q
.BR = 12   84,82
.BL = 14   79,78
.SSP = 5   56,54
.TAX = 1   49
.LMX = 1   72
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
*

2761:  FP1:       CALL FRSH
.MCONT = 1   7
.MC = 20   1
.B = 1166    8,11,12,13,15,16,
CHECKBIT IS 30   
           
2762:  HROV, ER←Q
.BL = 12   80,78
.SSP = 5   56,54
.MS = 57   65,64,63,62,60
.LSPX = 1   59
CHECKBIT IS 87   
           
2763:  BR←M, DGOTO FPT
.BL = 14   79,78
.SSP = 2   55
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1013    8,14,16,17,
CHECKBIT IS 30   
           
2764:  RES←AR←Z, DGOTO TFOL
.BR = 14   83,82
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,
*

2765:  FP2:       Z←K, K←Z
.RRN = 3   68,67
.BR = 14   83,82
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 3   71,70
           
2766:  ER←Z-Q, Z←K
.RRN = 3   68,67
.BR = 14   83,82
.BL = 5   81,79
.SSP = 5   56,54
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LZY = 1   77
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
2767:  M←M LCL Z
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
           
2770:  M←M LCH Z, Q←RES
.RRN = 5   68,66
.BL = 14   79,78
.MS = 12   64,62
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
           
2771:  Q←Q LCL Z
.BL = 12   80,78
.MS = 11   65,62
.LQX = 1   74
           
2772:  Z←Q LCH Z, Q←R0
.BL = 12   80,78
.MS = 12   64,62
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
           
2773:  AR←RES←Z←M AND Q ! Z AND NOT Q, DGOTO FPT
.BR = 4   83
.BL = 10   78
.SSP = 1   56
.TXW = 1   47
.LZX = 1   76
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1013    8,14,16,17,
CHECKBIT IS 30   
           
2774:  BR←M←M AND NOT Q, DGOTO TFOL
.BL = 4   79
.SSP = 2   55
.LMX = 1   72
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,
CHECKBIT IS 30   


2775:  FAD1:      Q←Z-Q, R6←BR, GOTO XCH IF X<0
.BR = 14   83,82
.BL = 5   81,79
.SSP = 2   55
.LOC = 1   50
.TSPY = 1   45
.TYW = 1   48
.TAX = 1   49
.LQX = 1   74
.LRN = 6   70,69
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1015    8,14,15,17,
           
2776:  Z←Q LCY 12, Q←24
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = 30   37,38,

2777:  FAD2:      Z←Q-Z, GOTO FAD5 IF X<0
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1023    8,13,16,17,
CHECKBIT IS 30   
*

3000:  FAD3:      M←M LCH Z, GOTO FAD6 IF Z<0
.BL = 14   79,78
.MS = 12   64,62
.LMX = 1   72
.MC = 4   3
.B = 1026    8,13,15,16,
CHECKBIT IS 30   
           
3001:  R0←M←M LCL Z, Q←S
.RRN = 2   67
.BL = 14   79,78
.MS = 11   65,62
.THY = 1   46
.TXW = 1   47
.LMX = 1   72
.LQY = 1   75
.LR0 = 1   58
CHECKBIT IS 87   
           
3002:  M←Q LCH Z
.BL = 12   80,78
.MS = 12   64,62
.LMX = 1   72
           
3003:  DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.BR = 14   83,82
.TCX = 1   43
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
3004:  SETBB, DGOTO *+1
.MS = 60   61,60
.MC = 1   5
.DGO = 1   87
.B = 1005    8,15,17,
           
3005:  Z←M LCL Z,M←R0
.BL = 14   79,78
.MS = 11   65,62
.THY = 1   46
.LMY = 1   73
.LZX = 1   76
           
3006:  M←M AND Q ! Z AND NOT Q, S, GOTO FAD7 IF Y<0
.RRN = 2   67
.BR = 4   83
.BL = 10   78
.THY = 1   46
.LMX = 1   72
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1027    8,13,15,16,17,
           
3007:  Z←Z AND Q, Q←R6
.RRN = 6   67,66
.BR = 10   82
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
CHECKBIT IS 87   

3010:  FAD4:      BR←M+Q, HROV, Q←IR
.RRN = 4   66
.BR = 12   84,82
.BL = 14   79,78
.SSP = 2   55
.MS = 57   65,64,63,62,60
.THY = 1   46
.TAX = 1   49
.LQY = 1   75
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   

3011:  FAD9:      GOTO FAD8 IF CARRY
.MC = 61   5,1,0
.B = 1031    8,13,14,17,
           
3012:  AR←RES←Z←Z+Q, HROV, DGOTO TFOL
.BR = 14   83,82
.BL = 12   80,78
.SSP = 1   56
.MS = 57   65,64,63,62,60
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 1233    8,10,13,14,16,17,

3013:  FPT:       M, .VCY, Q←Z←E1, GOTO FNORM IF FPN
.BL = 14   79,78
.LQY = 1   75
.LZY = 1   77
.MC = 76   4,3,2,1,0
.VCY = 1   86
.TE1Y = 1   88
.B = 1014    8,14,15,
CHECKBIT IS 30   

3014:  FNORM:     K←ER, GOTO Q MRG BASEFP
.MCONT = 3   7,6
.BL = 12   80,78
.SSP = 5   56,54
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LRN = 3   71,70
.MC = 1   5
.C = 2750   31,33,34,35,36,38,
CHECKBIT IS 50   
*

3015:  XCH:       R6←M, M←R6
.RRN = 6   67,66
.BL = 14   79,78
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 6   70,69
           
3016:  Q←R0, Z←Q LCY 12 MRG 7777B4
.BL = 12   80,78
.MS = 6   64,63
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
.TCX = 1   43
.C = 77770000   18,19,20,21,22,23,24,25,26,27,28,29,
CHECKBIT IS 87
           
3017:  ER←Q, Q←IR
.RRN = 4   66
.BL = 12   80,78
.SSP = 5   56,54
.THY = 1   46
.LQY = 1   75
.LSPX = 1   59
CHECKBIT IS 87   
           
3020:  IR←S
.RRN = 2   67
.THY = 1   46
.TYW = 1   48
.LRN = 4   69
CHECKBIT IS 87   
           
3021:  S←Q, Q←24, DGOTO FAD3
.BL = 12   80,78
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 2   70
.MC = 20   1
.DGO = 1   87
.B = 1000    8,
.C = 30   37,38,
           
3022:  Z←Q+Z, GOTO FAD5 IF X<0
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LZX = 1   76
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 1023    8,13,16,17,
*

3023:  FAD5:      Z←Z+Q, M←S, .VCY, DGOTO FAD3 IF Y<0
.BR = 14   82,83
.BL = 12   78,80
.TAX = 1   49
.LZX = 1   76
.RRN = 2   67
.THY = 1   46
.LMY = 1   73
.MC = 53   0,2,4,5,
.DGO = 1   87
.VCY = 1   86
.B = 1000    8,
CHECKBIT IS 30   
           
3024:  S←-1
.BL = 17   78,79,80,81
.TXW = 1   47
.LRN = 2   70
CHECKBIT IS 87   
           
3025:  S←0, GOTO FAD3
.TYW = 1   48
.LRN = 2   70
.MC = 20   1
.B = 1000    8,
CHECKBIT IS 30
*

3026:  FAD6:      M←Z←0, Q←R6, GOTO FAD4
.RRN = 6   67,66
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.B = 1010    8,14,
*

3027:  FAD7:      Z←Z OR NOT Q, Q←R6, DGOTO FAD9
.RRN = 6   67,66
.BR = 15   85,83,82
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.B = 1011    8,14,17,
           
3030:  BR←M+Q, HROV, Q←IR
.RRN = 4   66
.BR = 12   84,82
.BL = 14   79,78
.SSP = 2   55
.MS = 57   65,64,63,62,60
.THY = 1   46
.TAX = 1   49
.LQY = 1   75
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
*

3031:  FAD8:      AR←RES←Z←Z+Q, .LOC, HROV, DGOTO TFOL
.BR = 14   83,82
.BL = 12   80,78
.SSP = 1   56
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 1233    8,10,13,14,16,17,
CHECKBIT IS 30   
           
3032:  M, .VCY, Q←Z←E1, GOTO FNORM IF FPN
.BL = 14   79,78
.LQY = 1   75
.LZY = 1   77
.MC = 76   4,3,2,1,0
.VCY = 1   86
.TE1Y = 1   88
.B = 1014    8,14,15,
CHECKBIT IS 30   
*

3033:  FSB1:      M←NOT M+1, HROV
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.VCY = 1   86
           
3034:  S←NOT Z, Z←ER, DGOTO FAD1
.BR = 3   85,84
.SSP = 5   56,54
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
.MC = 1   5
.DGO = 1   87
.B = 775    9,10,11,12,13,14,15,17,
CHECKBIT IS 30   
           
3035:  R0←Q, GOTO FSB2 IF CARRY
.BL = 12   80,78
.TXW = 1   47
.LR0 = 1   58
.MC = 61   5,1,0
.B = 1036    8,13,14,15,16,
CHECKBIT IS 30   

3036:  FSB2:      S←S+1, GOTO FAD1
.RRN = 2   67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.MC = 1   5
.B = 775    9,10,11,12,13,14,15,17,
CHECKBIT IS 30   

3037:  FLD1:      BR←M, SETBA, P←P+1, DGOTO NX2
.RRN = 1   68
.BL = 14   79,78
.SSP = 2   55
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 1   71
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
3040:  RES←AR←Z, Z←P
.RRN = 1   68
.BR = 14   83,82
.SSP = 1   56
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 5   71,69
.LSPX = 1   59
CHECKBIT IS 87   

3041:  FMUL1:     M EOR Q, S←Q←3777B4, GOTO UFN IF LB=0
.BL = 6   80,79
.TCY = 1   44
.TYW = 1   48
.LQY = 1   75
.LRN = 2   70
.MC = 22   4,1
.B = 1241    8,10,12,17,
.C = 37770000   19,20,21,22,23,24,25,26,27,28,29,
           
3042:  R0←R6+1, Q←M AND Q, DGOTO FMUL5 IF M[1]
.RRN = 6   67,66
.BL = 10   78
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 21   5,1
.DGO = 1   87
.B = 1076    8,12,13,14,15,16,
           
3043:  MFETCH, Z←M, R0←E1, .VCY, GOTO BPAGEF IF Y<0
.BL = 14   79,78
.MS = 34   63,62,61
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,

3044:  FMUL6:     K←Q, GOTO FMUL2 IF Z>=0
.BL = 12   80,78
.TXW = 1   47
.LRN = 3   71,70
.MC = 5   5,3
.B = 1046    8,12,15,16,
           
3045:  M←NOT M+1, HROV, S←4B7, CALL FNS
.MCONT = 1   7
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TCY = 1   44
.TYW = 1   48
.TAX = 1   49
.LMX = 1   72
.LRN = 2   70
.MC = 20   1
.VCY = 1   86
.B = 1272    8,10,12,13,14,16,
.C = -40000000   18,

3046:  FMUL2:     Q←Z
.BR = 14   83,82
.LQX = 1   74
           
3047:  M←M LCY 12
.BL = 14   79,78
.MS = 6   64,63
.LMX = 1   72
           
3050:  Z←Q LCY 12, Q←7777B
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = 7777   30,31,32,33,34,35,36,37,38,39,40,41,
           
3051:  MH1←Z AND NOT Q ! M AND Q, .LSPX←0
.BR = 4   83
.BL = 10   78
.SSP = 40   51
           
3052:  ML1←M, Q←7777B, .LSPX←0
.BL = 14   79,78
.SSP = 41   56,51
.TCY = 1   44
.LQY = 1   75
.C = 7777   30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
3053:  Z←AR
.SSP = 1   56
.TSPY = 1   45
.LZY = 1   77
           
3054:  M←IR←BR, CALL FROUND
.MCONT = 1   7
.SSP = 2   55
.TSPY = 1   45
.TYW = 1   48
.LMY = 1   73
.LRN = 4   69
.MC = 1   5
.B = 1160    8,11,12,13,
           
3055:  R0←Q←S, GOTO FMUL3 IF Z>=0
.RRN = 2   67
.THY = 1   46
.TYW = 1   48
.LQY = 1   75
.LR0 = 1   58
.MC = 5   5,3
.B = 1060    8,12,13,
CHECKBIT IS 30   
           
3056:  R0←NOT Q
.BL = 5   81,79
.TXW = 1   47
.LR0 = 1   58
CHECKBIT IS 87   
           
3057:  M←NOT M+1, HROV, CALL FNS
.MCONT = 1   7
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 20   1
.VCY = 1   86
.B = 1272    8,10,12,13,14,16,

3060:  FMUL3:     ML2←M←M LCY 1, .LSPX←0
.BL = 14   79,78
.SSP = 42   55,51
.MS = 1   65
.LMX = 1   72
CHECKBIT IS 87   
           
3061:  Q←Z
.BR = 14   83,82
.LQX = 1   74
           
3062:  Z←Q LCY 1, Q←1
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = 1   41,
           
3063:  MH2←Z AND NOT Q ! M AND Q, Q←K, .LSPX←0
.RRN = 3   68,67
.BR = 4   83
.BL = 10   78
.SSP = 43   56,55,51
.THY = 1   46
.LQY = 1   75
           
3064:  Z←ER, M←1B4
.SSP = 5   56,54
.TCX = 1   43
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.C = 10000   29,
           
3065:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
3066:  ER←Q+M, HROV
.BR = 12   84,82
.BL = 14   79,78
.SSP = 5   56,54
.MS = 57   65,64,63,62,60
.TAX = 1   49
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
*  MULTIPLICATION IS COMPLETED, RESULT IS IN MHR,MMR AND MLR
           
3067:  M←MMR, DGOTO FMUL4 IF R0<0, .TSPY←0
.SSP = 45   56,54,51
.LMY = 1   73
.MC = 11   5,2
.DGO = 1   87
.B = 1073    8,12,13,14,16,17,
           
3070:  Z←MHR, .TSPY←0
.SSP = 44   54,51
.LZY = 1   77
           
3071:  BR←M, DGOTO FPT
.BL = 14   79,78
.SSP = 2   55
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1013    8,14,16,17,
CHECKBIT IS 30   
           
3072:  RES←AR←Z, DGOTO TFOL
.BR = 14   83,82
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,
*

3073:  FMUL4:     M←NOT M+1, HROV, CALL FNS
.MCONT = 1   7
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 20   1
.VCY = 1   86
.B = 1272    8,10,12,13,14,16,
           
3074:  BR←M+0, HROV, DGOTO FPT
.BL = 14   79,78
.SSP = 2   55
.MS = 57   65,64,63,62,60
.TAX = 1   49
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 1013    8,14,16,17,
           
3075:  RES←AR←Z, DGOTO TFOL
.BR = 14   83,82
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,

3076:  FMUL5:     Q←Q MRG 4B7, GOTO FMUL6
.BL = 12   80,78
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.B = 1044    8,12,15,
.C = -40000000   18,
*

3077:  FDIV1:     K←NOT Z, GOTO FDIV2 IF Z>=0
.BR = 3   85,84
.TXW = 1   47
.LRN = 3   71,70
.MC = 5   5,3
.B = 1101    8,11,17,
CHECKBIT IS 30   
           
3100:  M←NOT M+1, HROV, CALL FNS
.MCONT = 1   7
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 1   5
.VCY = 1   86
.B = 1272    8,10,12,13,14,16,

3101:  FDIV2:     DV1←Z, IR←0, GOTO FDIVZ IF Z=0
.BR = 14   83,82
.SSP = 26   55,54,52
.TYW = 1   48
.LRN = 4   69
.LSPX = 1   59
.MC = 2   4
.B = 1237    8,10,13,14,15,16,17,
CHECKBIT IS 30   
           
3102:  DV2←M, R6←0
.BL = 14   79,78
.SSP = 27   56,55,54,52
.TYW = 1   48
.LRN = 6   70,69
.LSPX = 1   59
CHECKBIT IS 87   

3103:  FDIV3:     Z←ER, M←1B4
.SSP = 5   56,54
.TCX = 1   43
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.C = 10000   29,
           
3104:  Z←Z-M, RES←37777732B
.BR = 14   83,82
.BL = 3   81,80
.LOC = 1   50
.TCY = 1   44
.TYW = 1   48
.TAX = 1   49
.LZX = 1   76
.LRN = 5   71,69
.VCY = 1   86
.C = 37777732   19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,40,
CHECKBIT IS 87   
           
3105:  ER←Z-Q
.BR = 14   83,82
.BL = 5   81,79
.SSP = 5   56,54
.LOC = 1   50
.TAX = 1   49
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
3106:  Z←AR
.SSP = 1   56
.TSPY = 1   45
.LZY = 1   77
           
3107:  M←BR, GOTO FDIVD IF Z>=0
.SSP = 2   55
.TSPY = 1   45
.LMY = 1   73
.MC = 5   5,3
.B = 1111    8,11,14,17,
           
3110:  S←K, M←NOT M+1, HROV, CALL FNS
.MCONT = 1   7
.RRN = 3   68,67
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.THY = 1   46
.TYW = 1   48
.TAX = 1   49
.LMX = 1   72
.LRN = 2   70
.MC = 1   5
.VCY = 1   86
.B = 1272    8,10,12,13,14,16,
CHECKBIT IS 30   

3111:  FDIVD:     S←Z, Z←S
.RRN = 2   67
.BR = 14   83,82
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
           
3112:  DSIGN←Z
.BR = 14   83,82
.SSP = 31   56,53,52
.LSPX = 1   59
CHECKBIT IS 87   
           
3113:  Z←K←M, M←S, DGOTO FDIV7
.RRN = 2   67
.BL = 14   79,78
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LZX = 1   76
.LRN = 3   71,70
.MC = 20   1
.DGO = 1   87
.B = 1127    8,11,13,15,16,17,
           
3114:  Q←DV2
.SSP = 27   56,55,54,52
.TSPY = 1   45
.LQY = 1   75
CHECKBIT IS 87   
*

3115:  FDIV4:     M←M LCY 1, Q←R6
.RRN = 6   67,66
.BL = 14   79,78
.MS = 1   65
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
CHECKBIT IS 87   
           
3116:  Z←Q LCY 1, Q←1, DGOTO FDIV5 IF R0>=0
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 12   4,2
.DGO = 1   87
.B = 1122    8,11,13,16,
.C = 1   41,
CHECKBIT IS 50   
           
3117:  IR←M AND NOT Q ! Z AND Q
.BR = 10   82
.BL = 4   79
.TXW = 1   47
.LRN = 4   69
CHECKBIT IS 87   
           
3120:  R6←Z AND NOT Q, Q←K, DGOTO FDIV6
.RRN = 3   68,67
.BR = 4   83
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.MC = 1   5
.DGO = 1   87
.B = 1124    8,11,13,15,
CHECKBIT IS 30   
           
3121:  Z←Q LCY 1, M←S
.RRN = 2   67
.BL = 12   80,78
.MS = 1   65
.THY = 1   46
.LMY = 1   73
.LZX = 1   76

3122:  FDIV5:     R6←Z AND NOT Q MRG 2B3, Q←LO
.BR = 4   83
.SSP = 32   55,53,52
.TCX = 1   43
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.C = 2000   31,
           
3123:  Z←Q LCY 1, M←R0
.BL = 12   80,78
.MS = 1   65
.THY = 1   46
.LMY = 1   73
.LZX = 1   76
CHECKBIT IS 87   

3124:  FDIV6:     M←M LCY 1, Q←1
.BL = 14   79,78
.MS = 1   65
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 1   41,
           
3125:  S←M←M AND NOT Q ! Z AND Q, RES, GOTO FDIV9 IF Y<0
.RRN = 5   68,66
.BR = 10   82
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.LMX = 1   72
.LRN = 2   70
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1133    8,11,13,14,16,17,
           
3126:  K←Z←Z AND NOT Q, Q←DV2
.BR = 4   83
.SSP = 27   56,55,54,52
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LZX = 1   76
.LRN = 3   71,70

3127:  FDIV7:     Z←Z-Q, HROV, Q←DV1
.BR = 14   83,82
.BL = 5   81,79
.SSP = 26   55,54,52
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TSPY = 1   45
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.VCY = 1   86
CHECKBIT IS 87   
           
3130:  LO←Z, RES←RES+1, GOTO FDIV8 IF CARRY
.RRN = 5   68,66
.BR = 14   83,82
.SSP = 32   55,53,52
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 5   71,69
.LSPX = 1   59
.MC = 61   5,1,0
.B = 1132    8,11,13,14,16,
           
3131:  R0←M+NOT Q, M←IR, GOTO FDIV4
.RRN = 4   66
.BR = 5   85,83
.BL = 14   79,78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 1115    8,11,14,15,17,

3132:  FDIV8:     R0←M-Q, M←IR, GOTO FDIV4
.RRN = 4   66
.BR = 5   85,83
.BL = 14   79,78
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 1   5
.VCY = 1   86
.B = 1115    8,11,14,15,17,
CHECKBIT IS 30   
*

3133:  FDIV9:     Z←K, Z←M
.RRN = 3   68,67
.BL = 14   79,78
.THY = 1   46
.LZX = 1   76
.LZY = 1   77
           
3134:  Z←IR, S←1B3, DGOTO FDIVA IF Z#0
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
.MC = 3   5,4
.DGO = 1   87
.B = 1137    8,11,13,14,15,16,17,
.C = 1000   32,
           
3135:  Q←K←ER
.SSP = 5   56,54
.TSPY = 1   45
.TYW = 1   48
.LQY = 1   75
.LRN = 3   71,70
           
3136:  S←0
.TXW = 1   47
.LRN = 2   70
CHECKBIT IS 87   

3137:  FDIVA:     M←R6, CALL FRSH IF Z<0
.MCONT = 1   7
.RRN = 6   67,66
.THY = 1   46
.LMY = 1   73
.MC = 4   3
.B = 1166    8,11,12,13,15,16,
CHECKBIT IS 30   
           
3140:  ER←Q, Q←S
.RRN = 2   67
.BL = 12   80,78
.SSP = 5   56,54
.THY = 1   46
.LQY = 1   75
.LSPX = 1   59
CHECKBIT IS 87   
           
3141:  DSIGN, GOTO FDIVC IF Y<0
.SSP = 31   56,53,52
.TSPY = 1   45
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1144    8,11,12,15,

3142:  FDIVB:     BR←M OR Q, HROV, DGOTO FPT
.BL = 16   80,79,78
.SSP = 2   55
.MS = 57   65,64,63,62,60
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 1013    8,14,16,17,
CHECKBIT IS 30   
           
3143:  RES←AR←Z, DGOTO TFOL
.BR = 14   83,82
.SSP = 1   56
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,

3144:  FDIVC:     M←NOT M+1, HROV, CALL FNS
.MCONT = 1   7
.BL = 3   81,80
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LMX = 1   72
.MC = 1   5
.VCY = 1   86
.B = 1272    8,10,12,13,14,16,
           
3145:  GOTO FDIVB
.MC = 20   1
.B = 1142    8,11,12,16,


3146:  FCP1:      Z EOR Q, Z←ER, SETBA, GOTO NX1 IF X<0
.BR = 6   84,83
.SSP = 5   56,54
.MS = 61   65,61,60
.TSPY = 1   45
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 34    13,14,15,
           
3147:  Z←Z-M, M←BR
.BR = 14   83,82
.BL = 3   81,80
.SSP = 2   55
.LOC = 1   50
.TSPY = 1   45
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.VCY = 1   86
           
3150:  RES←Z EOR Q, Q←K, SETBA, GOTO NX1 IF Z#0
.RRN = 3   68,67
.BR = 6   84,83
.MS = 61   65,61,60
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 5   71,69
.MC = 3   5,4
.B = 34    13,14,15,
           
3151:  IR←M-Q, HROV, M←AR
.BR = 5   85,83
.BL = 14   79,78
.SSP = 1   56
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LRN = 4   69
.VCY = 1   86
           
3152:  Q←S, GOTO FCP2 IF CARRY
.RRN = 2   67
.THY = 1   46
.LQY = 1   75
.MC = 61   5,1,0
.B = 1154    8,11,12,14,15,
           
3153:  RES←Z←M+NOT Q, GOTO FCP3
.BR = 5   85,83
.BL = 14   79,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LRN = 5   71,69
.MC = 20   1
.VCY = 1   86
.B = 1155    8,11,12,14,15,17,

3154:  FCP2:      RES←Z←M-Q
.BR = 5   85,83
.BL = 14   79,78
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LRN = 5   71,69
.VCY = 1   86

3155:  FCP3:      Z←IR, SETBA, GOTO NX1 IF Z#0
.RRN = 4   66
.MS = 61   65,61,60
.THY = 1   46
.LZY = 1   77
.MC = 3   5,4
.B = 34    13,14,15,
           
3156:  Z←P←P+1, SETBA, GOTO NX2 IF Z=0
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 2   4
.B = 35    13,14,15,17,
CHECKBIT IS 30   
           
3157:  RES←1, SETBA, GOTO NX2
.MS = 61   65,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.MC = 20   1
.B = 35    13,14,15,17,

*  ROUND TO NEAREST NUMBER
*  IR = M = LOW ORD., Z = HIGH, Q = 7777B, K = EXP
*

3160:  FROUND:    M←Q AND M, Q←2B3
.BL = 10   78
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 2000   31,
           
3161:  Q EOR M, M←IR, RETURN IF LB=0
.MCONT = 2   6
.RRN = 4   66
.BL = 6   80,79
.THY = 1   46
.LMY = 1   73
.MC = 22   4,1
CHECKBIT IS 30   
           
3162:  M←Q+M, HROV, Q←3777B, DRETURN
.MCONT = 2   6
.BR = 12   84,82
.BL = 14   79,78
.MS = 57   65,64,63,62,60
.TCY = 1   44
.TAX = 1   49
.LMX = 1   72
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 56   
           
3163:  M←M AND NOT Q, GOTO FRO1 IF CARRY
.BL = 4   79
.LMX = 1   72
.MC = 61   5,1,0
.B = 1164    8,11,12,13,15,
CHECKBIT IS 30   

3164:  FRO1:      Z←Z+1, HROV, DRETURN
.MCONT = 2   6
.BR = 14   83,82
.MS = 57   65,64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
CHECKBIT IS 30   
           
3165:  GOTO FRSH IF OVFLW, IR←Z
.BR = 14   83,82
.TXW = 1   47
.LRN = 4   69
.MC = 57   5,4,3,2,0
.B = 1166    8,11,12,13,15,16,
*
*  SHIFT (Z,M) RIGHT 1.  IR = Z, K = EXP
*

3166:  FRSH:      M←M LCY 20
.BL = 14   79,78
.MS = 10   62
.LMX = 1   72
CHECKBIT IS 87   
           
3167:  M←M LCY 3, Q←IR
.RRN = 4   66
.BL = 14   79,78
.MS = 3   65,64
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
CHECKBIT IS 87   
           
3170:  Q←Q LCY 20
.BL = 12   80,78
.MS = 10   62
.LQX = 1   74
CHECKBIT IS 87   
           
3171:  Z←Q LCY 3, Q←4B7
.BL = 12   80,78
.MS = 3   65,64
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = -40000000   18,
CHECKBIT IS 87   
           
3172:  IR←M AND NOT Q ! Z AND Q, M←IR
.RRN = 4   66
.BR = 10   82
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 4   69
           
3173:  M←M AND Q
.BL = 10   78
.LMX = 1   72
CHECKBIT IS 87   
           
3174:  Z←Z AND NOT Q ! M EOR Q, M←1B4
.BR = 4   83
.BL = 6   80,79
.TCY = 1   44
.LMY = 1   73
.LZX = 1   76
.C = 10000   29,
           
3175:  Q←K, DRETURN
.MCONT = 2   6
.RRN = 3   68,67
.THY = 1   46
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
           
3176:  Q←K←M+Q, M←IR
.RRN = 4   66
.BR = 12   84,82
.BL = 14   79,78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LQX = 1   74
.LRN = 3   71,70
.VCY = 1   86
*


3177:  STF1:      Q←S←Z, SETBA, GOTO ROIA IF D
.MS = 61   60,61,65
.BR = 14   83,82
.TXW = 1   47
.LQX = 1   74
.LRN = 2   70
.MC = 47   0,3,4,5
.B = 372    10,11,12,13,14,16,
CHECKBIT IS 30
           
3200:  M←M LCY 12, R0, .VCY, GOTO BCSTORE IF STERR
.BL = 14   79,78
.MS = 6   64,63
.THY = 1   46
.VCY = 1   86
.LMX = 1   72
.MC = 32   1,2,4
.B = 373    10,11,12,13,14,16,17,
           
3201:  Z←M LCY 1
.BL = 14   79,78
.MS = 1   65
.LZX = 1   76
CHECKBIT IS 87   
           
3202:  Q←Q LCY 12
.BL = 12   80,78
.MS = 6   64,63
.LQX = 1   74
           
3203:  M←Q LCY 1, Q←17777B
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 17777   29,30,31,32,33,34,35,36,37,38,39,40,41,
           
3204:  IR←Z AND Q ! M AND NOT Q, Z←K
.RRN = 3   68,67
.BR = 10   82
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
CHECKBIT IS 87   
           
3205:  Q←7777B, S, DGOTO STF2 IF Y<0
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.MC = 53   5,4,2,0
.DGO = 1   87
.VCY = 1   86
.B = 1210    8,10,14,
.C = 7777   30,31,32,33,34,35,36,37,38,39,40,41,
           
3206:  M←M AND Q ! Z MRG 4B7, Q←0
.BR = 14   83,82
.BL = 10   78
.TCX = 1   43
.LMX = 1   72
.LQY = 1   75
.C = -40000000   18,
           
3207:  Q←4B7
.TCX = 1   43
.LQX = 1   74
.C = -40000000   18,

3210:  STF2:      Q←Q MRG 2B7
.BL = 12   80,78
.TCX = 1   43
.LQX = 1   74
.C = 20000000   19,
           
3211:  Z←IR, M←M EOR Q, STORE
.RRN = 4   66
.BL = 6   80,79
.MS = 42   64,60
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
CHECKBIT IS 87   
           
3212:  M←Z, R0←R6+1, CALL BSTR1
.MCONT = 1   7
.RRN = 6   67,66
.BR = 14   83,82
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
.MC = 1   5
.B = 371    10,11,12,13,14,17,
           
3213:  M←K, DGOTO TFO
.RRN = 3   68,67
.THY = 1   46
.LMY = 1   73
.MC = 20   1
.DGO = 1   87
.B = 1234    8,10,13,14,15,
           
3214:  GOTO BLL84 IF C
.MC = 35   5,3,2,1
.B = 260    10,12,13,
*

3215:  FIXS:      Q←Q LCY 12, IR←4B7, GOTO FIXS2 IF Z>=0
.BL = 12   80,78
.MS = 6   64,63
.TCY = 1   44
.TYW = 1   48
.LQX = 1   74
.LRN = 4   69
.MC = 5   5,3
.B = 1221    8,10,13,17,
.C = -40000000   18,
           
3216:  M, IR←0, GOTO FIXS3 IF LB#0
.BL = 14   79,78
.TYW = 1   48
.LRN = 4   69
.MC = 23   5,4,1
.B = 1220    8,10,13,
CHECKBIT IS 30   
           
3217:  S←NOT Z+1, GOTO FIXS2
.BR = 3   85,84
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 2   70
.MC = 20   1
.VCY = 1   86
.B = 1221    8,10,13,17,
CHECKBIT IS 30   

3220:  FIXS3:     S←NOT Z
.BR = 3   85,84
.TXW = 1   47
.LRN = 2   70
CHECKBIT IS 87   

3221:  FIXS2:     R6, M←22, GOTO FIXS1 IF Y<0
.RRN = 6   67,66
.TCX = 1   43
.THY = 1   46
.LMX = 1   72
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1232    8,10,13,14,16,
.C = 26   37,39,40,
CHECKBIT IS 50   
           
3222:  R0←Q←M-Q, M←24
.BR = 5   85,83
.BL = 14   79,78
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LQX = 1   74
.LR0 = 1   58
.VCY = 1   86
.C = 30   37,38,
CHECKBIT IS 87   
           
3223:  Z←M-Q, M←S, GOTO FXTRP IF R0<0
.RRN = 2   67
.BR = 5   85,83
.BL = 14   79,78
.LOC = 1   50
.THY = 1   46
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.MC = 11   5,2
.VCY = 1   86
.B = 1240    8,10,12,
CHECKBIT IS 30   
           
3224:  M←M LCL Z
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
           
3225:  DGOTO Z MRG BASEMSK
.MCONT = 3   7,6
.BR = 14   83,82
.TCX = 1   43
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
3226:  SETBB, DGOTO *+1
.MS = 60   61,60
.MC = 1   5
.DGO = 1   87
.B = 1227    8,10,13,15,16,17,
CHECKBIT IS 30   
           
3227:  M←M LCH Z
.BL = 14   79,78
.MS = 12   64,62
.LMX = 1   72
           
3230:  Z←M AND Q, IR, RETURN IF Y<0
.MCONT = 2   6
.RRN = 4   66
.BL = 10   78
.THY = 1   46
.LZX = 1   76
.MC = 53   5,4,2,0
.VCY = 1   86
CHECKBIT IS 30   
           
3231:  Z←NOT M OR NOT Q+1, RETURN
.MCONT = 2   6
.BL = 7   81,80,79
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 20   1
.VCY = 1   86

3232:  FIXS1:     Z←0, RETURN
.MCONT = 2   6
.LZX = 1   76
.MC = 1   5


3233:  TFOL:      M←ER
.SSP = 5   56,54
.TSPY = 1   45
.LMY = 1   73
CHECKBIT IS 87   

3234:  TFO:       Z←P←P+1, Q←0, SETBA, GOTO NX2 IF M[0]=M[1]
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LZY = 1   77
.LRN = 1   71
.MC = 73   5,4,2,1,0
.B = 35    13,14,15,17,

3235:  TFO1:      M, IR←2, .VCY, SETBA, GOTO RDTRAP IF X>=0
.BL = 14   79,78
.MS = 61   65,61,60
.TCY = 1   44
.TYW = 1   48
.LRN = 4   69
.MC = 16   4,3,2
.VCY = 1   86
.B = 1041    8,12,17
.C = 2    40
CHECKBIT IS 50
           
3236:  IR←3, SETBA, GOTO RDTRAP
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LRN = 4   69
.MC = 1   5
.B = 1041    8,12,17
.C = 3    40,41,
CHECKBIT IS 50
*

3237:  FDIVZ:     IR←8, Q←0, SETBA, GOTO RDTRAP
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 1041    8,12,17,
.C = 10   38,
CHECKBIT IS 50   

3240:  FXTRP:     IR←7, Q←0, SETBA, GOTO RDTRAP
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1041    8,12,17,
.C = 7   39,40,41,
CHECKBIT IS 50   

3241:  UFN:       IR←6, Q←0, SETBA, GOTO RDTRAP
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 1041    8,12,17,
.C = 6   39,40,


3242:  FLX1:      CALL FIXS
.MCONT = 1   7
.MC = 1   5
.B = 1215    8,10,14,15,17,
           
3243:  XR←Z, Z←P←P+1, SETBA, GOTO NX2
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
.MC = 20   1
.B = 35    13,14,15,17,

3244:  FIX1:      M←Q, R6←Q←ER, CALL FIXS
.MCONT = 1   7
.BL = 12   80,78
.SSP = 5   56,54
.TSPY = 1   45
.TYW = 1   48
.LMX = 1   72
.LQY = 1   75
.LRN = 6   70,69
.MC = 1   5
.B = 1215    8,10,14,15,17,
           
3245:  AR←RES←Z, SETBA, GOTO NX1
.BR = 14   83,82
.SSP = 1   56
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
CHECKBIT IS 30   

3246:  FNA1:      AR←RES←NOT Z+1, SETBA, GOTO NX1 IF CARRY
.BR = 3   85,84
.SSP = 1   56
.MS = 61   65,61,60
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.LSPX = 1   59
.MC = 61   5,1,0
.VCY = 1   86
.B = 34    13,14,15,
           
3247:  AR←RES←NOT Z, SETBA, GOTO NX1
.BR = 3   85,84
.SSP = 1   56
.MS = 61   65,61,60
.TXW = 1   47
.LRN = 5   71,69
.LSPX = 1   59
.MC = 20   1
.B = 34    13,14,15,
CHECKBIT IS 30   

3250:  FLOAT1:    RES←Z+0, HROV, DGOTO FPT
.BR = 14   83,82
.MS = 57   65,64,63,62,60
.TXW = 1   47
.TAX = 1   49
.LRN = 5   71,69
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 1013    8,14,16,17,
CHECKBIT IS 30   
           
3251:  ER←26B4, DGOTO TFOL
.SSP = 5   56,54
.TCX = 1   43
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 1233    8,10,13,14,16,17,
.C = 260000   25,27,28,
*

3252:  FOP1:      FETCH
.MS = 44   63,60
CHECKBIT IS 87   
           
3253:  SETBA, GOTO TI IF A
.MS = 61   65,61,60
.MC = 33   5,4,2,1
.B = 1400    8,9,
           
3254:  CLEARA
.MS = 50   62,60
CHECKBIT IS 87   

3255:  FOP:       S←M←M EOR Q, Q←2001B4, GOTO FOP1 IF AORB
.BL = 6   80,79
.TCY = 1   44
.TXW = 1   47
.LMX = 1   72
.LQY = 1   75
.LRN = 2   70
.MC = 51   5,2,0
.B = 1252    8,10,12,14,16,
.C = 20010000   19,29,
           
3256:  M EOR Q, Q←3777B4, GOTO UFN IF LB=0
.BL = 6   80,79
.TCY = 1   44
.LQY = 1   75
.MC = 22   4,1
.B = 1241    8,10,12,17,
.C = 37770000   19,20,21,22,23,24,25,26,27,28,29,
           
3257:  R0←R6+1, Z←M AND Q
.RRN = 6   67,66
.BL = 10   78
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
CHECKBIT IS 87   
           
3260:  Q←NOT M LCY 3 MRG 1B5, DGOTO FOP3 IF M[1]
.BL = 3   81,80
.MS = 3   65,64
.TCX = 1   43
.LQX = 1   74
.MC = 21   5,1
.DGO = 1   87
.B = 1270    8,10,12,13,14,
.C = 100000   26,
           
3261:  MFETCH, R0←E1, .VCY, GOTO BPAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,
CHECKBIT IS 30   
           
3262:  R6←Z
.BR = 14   83,82
.TXW = 1   47
.LRN = 6   70,69

3263:  FOP2:      Z←NOT Q LCY 8, R0←S, DGOTO FOP4 IF Y<0
.RRN = 2   67
.BL = 5   81,79
.MS = 5   65,63
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 53   5,4,2,0
.DGO = 1   87
.VCY = 1   86
.B = 1271    8,10,12,13,14,17,
           
3264:  M←M LCY 3, DGOTO *+1 IF R0<0
.BL = 14   79,78
.MS = 3   65,64
.LMX = 1   72
.MC = 11   5,2
.DGO = 1   87
.B = 1265    8,10,12,13,15,17,
CHECKBIT IS 30   
           
3265:  M←M LCY 8, Q←3777B
.BL = 14   79,78
.MS = 5   65,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
3266:  S←Z←Z AND NOT Q ! M AND Q, DRETURN
.MCONT = 2   6
.BR = 4   83
.BL = 10   78
.TXW = 1   47
.LZX = 1   76
.LRN = 2   70
.MC = 1   5
.DGO = 1   87
CHECKBIT IS 30   
           
3267:  K←M←M AND NOT Q, Q←R6
.RRN = 6   67,66
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.LMX = 1   72
.LQY = 1   75
.LRN = 3   71,70
*

3270:  FOP3:      R6←Z MRG 4B7, GOTO FOP2
.BR = 14   83,82
.TCX = 1   43
.TXW = 1   47
.LRN = 6   70,69
.MC = 1   5
.B = 1263    8,10,12,13,16,17,
.C = -40000000   18,
CHECKBIT IS 50   
*

3271:  FOP4:      Z←Z MRG 4B7
.BR = 14   83,82
.TCX = 1   43
.LZX = 1   76
.C = -40000000   18,
*
*
*  FLOATING NEGATE, CALL:  M←NOT M+1, HROV, CALL FNS
*

3272:  FNS:       DRETURN
.MCONT = 2   6
.MC = 1   5
.DGO = 1   87
           
3273:  Z←NOT Z, GOTO FNS1 IF CARRY
.BR = 3   85,84
.LZX = 1   76
.MC = 61   5,1,0
.B = 1274    8,10,12,13,14,15,
CHECKBIT IS 30   

3274:  FNS1:      Z←Z+1, RETURN
.MCONT = 2   6
.BR = 14   83,82
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 1   5
.VCY = 1   86
CHECKBIT IS 30   

3275:  
CHECKBIT IS 30

3276:  
CHECKBIT IS 87

3277:    SETBA, CALL 1776B     *ILLEGAL MICROINSTRUCTION
.MS = 61       60,61,65
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16,
CHECKBIT IS 30
*
