            ORG 2000B;
*

2000:  BLLBASE:   R0←0
.TXW = 1   47
.LR0 = 1   58
CHECKBIT IS 87   
           
2001:  Z←0
.LZX = 1   76
           
2002:  Z←1
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
           
2003:  Z←1B4, CALL BLL86
.MCONT = 1   7
.TCX = 1   43
.LZX = 1   76
.MC = 20   1
.B = 262    10,12,13,16,
.C = 10000   29,
           
2004:  Z←3
.TCX = 1   43
.LZX = 1   76
.C = 3   40,41,
CHECKBIT IS 87   
           
2005:  Z←3
.TCX = 1   43
.LZX = 1   76
.C = 3   40,41,
CHECKBIT IS 87   
           
2006:  Z←3
.TCX = 1   43
.LZX = 1   76
.C = 3   40,41,
CHECKBIT IS 87   
           
2007:  Z←3, R0←BITS, GOTO BLL42
.SSP = 31   56,53,52
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 20   1
.B = 264    10,12,13,15,
.C = 3   40,41,
           
2010:  IR←NOT M LCY 4, GOTO BLL44
.BL = 3   81,80
.MS = 4   63
.TXW = 1   47
.LRN = 4   69
.MC = 1   5
.B = 303    10,11,16,17,
CHECKBIT IS 30   
           
2011:  Z←1
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
*
*
*  Z CONTAINS THE BITS WHICH DISTINGUISH
*  BETWEEN THE FOLLOWING VERSIONS:
*
*  OPERATION,   BITS IN Z,      NEWG,      Q
     
*    BLL        16               G         Q
*    BLLN       0,16             G         Q
*    UCAL       1,2,16,18     403000B   IA(403010B)
*    UCALN      0,1,2,16,18   403000B   IA(403010B)
*    MCAL       1,2,16,18,23  600000B   IA(604000B)
*    MCALN      0,1,2,16,18,23600000B   IA(604000B)
*    POP        -                G       IA(G)
*  BIT 3 IN BITS CONTROLS THE SKIP IF FTN=1
*  BIT 18 IS THE RETURN BIT
*
*
*  START OF COMMON BLL CODE.  NEWG HAS BEEN SET.
*    THE FIRST WORD OF THE BRANCH DESCRIPTOR IS IN M.
*    R6=Q, Q=77B6

2012:  BLL1:      BITS←Z, R0←Q←16B6, GOTO BLL1TRP IF M[5]
.BR = 14   83,82
.SSP = 31   56,53,52
.TCY = 1   44
.TYW = 1   48
.LQY = 1   75
.LR0 = 1   58
.LSPX = 1   59
.MC = 50   2,0
.B = 357    10,11,12,14,15,16,17,
.C = 16000000   20,21,22,
CHECKBIT IS 50   
           
2013:  NEWP←Z←M AND NOT Q, R0←R6+1, CALL BLL85 IF M[4]
.MCONT = 1   7
.RRN = 6   67,66
.BL = 4   79
.SSP = 26   55,54,52
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.LSPX = 1   59
.MC = 74   3,2,1,0
.B = 202    10,16,
           
2014:  MFETCH, R0←E1, .VCY, Q←77B6, GOTO BPAGEF IF Y<0
.MS = 34   63,62,61
.TCX = 1   43
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
           
2015:  SSOURCE, BRD←M, R0←P
.RRN = 1   68
.BL = 14   79,78
.SSP = 33   56,55,53,52
.MS = 52   64,62,60
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
CHECKBIT IS 87   
           
2016:  MAP, R0←E1, .VCY, DGOTO BLL3
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.TE1Y = 1   88
.B = 24    13,15,
           
2017:  IR←R0←M, Z←R6, GOTO *+1 IF R0[1]
.RRN = 6   67,66
.BL = 14   79,78
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
.LR0 = 1   58
.MC = 66   4,3,1,0
.B = 20    13,
CHECKBIT IS 30   
           
2020:  M←GR, Q←14B, SSOURCE
.SSP = 10   53
.MS = 52   64,62,60
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.C = 14   38,39,
CHECKBIT IS 87   
           
2021:  R0←M+Q, Q←BITS MRG 40B, CALL BLODX
.MCONT = 1   7
.BR = 12   84,82
.BL = 14   79,78
.SSP = 31   56,53,52
.TCY = 1   44
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 366    10,11,12,13,15,16,
.C = 40   36,
           
2022:  BITS←Q, Q←77B6
.BL = 12       78,80
.SSP = 31   56,53,52
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   
           
2023:  NEWG←M, M←IR
.RRN = 4   66
.BL = 14   79,78
.SSP = 25   56,54,52
.THY = 1   46
.LMY = 1   73
.LSPX = 1   59

2024:  BLL3:      SSOURCE, Q←S←M AND NOT Q, GOTO BLL4 IF M[1]
.BL = 4   79
.MS = 52   64,62,60
.TXW = 1   47
.LQX = 1   74
.LRN = 2   70
.MC = 21   5,1
.B = 135    11,13,14,15,17,
CHECKBIT IS 30   
           
2025:  Q←NEWP, Q, GOTO BLL5 IF LB=0
.BL = 12   80,78
.SSP = 26   55,54,52
.TSPY = 1   45
.LQY = 1   75
.MC = 22   4,1
.B = 155    11,12,14,15,17,
CHECKBIT IS 30   
*  Q=NEWP, M=BRD, S=NEWL, SP IS SET IF STK=1

2026:  BLL6:      Z←S
.RRN = 2   67
.THY = 1   46
.LZY = 1   77
*  M=BRD, Z=NEWL, R6=Q, SOURCE=Q(+1)

2027:  BLL10:     R0←NEWP
.SSP = 26   55,54,52
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
CHECKBIT IS 87   

2030:  BLL12:     MAPSS, .VCY, R0←E1, NEWL←Z
.BR = 14   83,82
.SSP = 30   53,52
.MS = 51   65,62,60
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
.VCY = 1   86
.TE1Y = 1   88
CHECKBIT IS 87   
           
2031:  Z←NEWP, Q←M LCY 2, DGOTO BLL13 IF X>=0
.BL = 14   79,78
.SSP = 26   55,54,52
.MS = 2   64
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 16   4,3,2
.DGO = 1   87
.VCY = 1   86
.B = 213    10,14,16,17,
CHECKBIT IS 30   
           
2032:  M←P+1, SETBA, GOTO BMACC1 IF R0[1]
.RRN = 1   68
.MS = 61   65,61,60
.IHR = 1   42
.THY = 1   46
.LMY = 1   73
.MC = 66   4,3,1,0
.B = 1716   8,9,10,11,14,15,16,
CHECKBIT IS 30   
*  CPA=1, Z=NFW, M=NAW, SOURCE=NEWP

2033:  BLL14:     R0←NFW←Z, Q←17B5
.BR = 14   83,82
.SSP = 34   54,53,52
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.LSPX = 1   59
.C = 1700000   23,24,25,26,
CHECKBIT IS 87   
           
2034:  NAW←S←M, Z←P, CALL BLODX
.MCONT = 1   7
.RRN = 1   68
.BL = 14   79,78
.SSP = 35   56,54,53,52
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
.LSPX = 1   59
.MC = 1   5
.B = 366    10,11,12,13,15,16,
CHECKBIT IS 30   
           
2035:  R6←Q AND M, Z←BITS, SSOURCE
.BL = 10   78
.SSP = 31   56,53,52
.MS = 52   64,62,60
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 6   70,69
CHECKBIT IS 87   
*  M=FORMAL PARAM, R6=FTYPE, SOURCE=P, Z=BITS
           
2036:  M←NI, IR←M, CALL BLL72 IF XPAGE, .TSPY←0
.MCONT = 1   7
.BL = 14   79,78
.SSP = 50   53,51
.TXW = 1   47
.LMY = 1   73
.LRN = 4   69
.MC = 42   4,0
.B = 156    11,12,14,15,16,

2037:  BLL71:     Q AND M, Q←IR, DGOTO BLL73 IF LB=0
.RRN = 4   66
.BL = 10   78
.THY = 1   46
.LQY = 1   75
.MC = 22   4,1
.DGO = 1   87
.B = 161    11,12,13,17,
           
2040:  FP←Q, R0←S, GOTO BLL39 IF NZ[16]
.RRN = 2   67
.BL = 12   80,78
.SSP = 37   56,55,54,53,52
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
.MC = 10   2
.B = 206    10,15,16,
CHECKBIT IS 30   
           
2041:  Q←Q EOR M, GOTO BLL2TRP IF Z<0
.BL = 6   80,79
.LQX = 1   74
.MC = 4   3
.B = 360    10,11,12,13,
CHECKBIT IS 30   

2042:  BLL33:     R0←Q LCY 8, Q←17B5
.BL = 12   80,78
.MS = 5   65,63
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.C = 1700000   23,24,25,26,
CHECKBIT IS 87   
           
2043:  Q←M AND Q, Z←R6, GOTO BLL2TRP IF R0[1]
.RRN = 6   67,66
.BL = 10   78
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 66   4,3,1,0
.B = 360    10,11,12,13,
CHECKBIT IS 30   
*  M=ACTUAL PARAM, Q=ATYPE, IR=FP, Z=R6=FTYPE
*  THE END FLAGS DO MATCH.

2044:  BLL76:     Z←Q EOR Z, Q←IR, CLEARA, DGOTO BLL16 IF M[4]
.RRN = 4   66
.BR = 6   84,83
.MS = 50   62,60
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
.MC = 74   3,2,1,0
.DGO = 1   87
.B = 55    12,14,15,17,
           
2045:  Z←Q LCY 4, Q←16B5, GOTO BLL74 IF Z#0
.BL = 12   80,78
.MS = 4   63
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 3   5,4
.B = 170    11,12,13,14,
.C = 1600000   23,24,25,
*  STR=0 OR STR=2, Z[0]=FSTR, M=AP
           
2046:  M LCY 3, Q←IR←NAW, GOTO BLL17 IF X>=0
.BL = 14   79,78
.SSP = 35   56,54,53,52
.MS = 3   65,64
.TSPY = 1   45
.TYW = 1   48
.LQY = 1   75
.LRN = 4   69
.MC = 16   4,3,2
.VCY = 1   86
.B = 54    12,14,15,
*  STR=2, IR=NAW
           
2047:  Q←IR+1, IR←M, DGOTO BLL16 IF Z<0
.RRN = 4   66
.BL = 14   79,78
.IHR = 1   42
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 4   3
.DGO = 1   87
.B = 55    12,14,15,17,
CHECKBIT IS 30   

2050:  BLL70:     R0←NAW←Q, CALL 376B
.MCONT = 1   7
.BL = 12   80,78
.SSP = 35   56,54,53,52
.TXW = 1   47
.LR0 = 1   58
.LSPX = 1   59
.MC = 1   5
.B = 376    10,11,12,13,14,15,16,
CHECKBIT IS 30   
           
2051:  Q←BRD, Z←1B6
.SSP = 33   56,55,53,52
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
.C = 1000000   23,
           
2052:  Z←Q AND Z, Q←BITS, CLEARA, DGOTO 374B
.BR = 10   82
.SSP = 31   56,53,52
.MS = 50   62,60
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 374    10,11,12,13,14,15,
           
2053:  BITS←Q MRG 4B6, Q←IR, GOTO BLL3TRP IF Z=0
.RRN = 4   66
.BL = 12   80,78
.SSP = 31   56,53,52
.TCX = 1   43
.THY = 1   46
.LQY = 1   75
.LSPX = 1   59
.MC = 2   4
.B = 361    10,11,12,13,17,
.C = 4000000   21,

2054:  BLL17:     Z←4B7, SETA, GOTO BLL70 IF Z<0
.MS = 30   62,61
.TCX = 1   43
.LZX = 1   76
.MC = 4   3
.B = 50    12,14,
.C = -40000000   18,
CHECKBIT IS 50   
*  M=AP, (Z[0]=FSTR, STR=1 OR 3)

2055:  BLL16:     AP←M, Q←07740000B, GOTO BLL3TRP IF Z>=0
.BL = 14   79,78
.SSP = 36   55,54,53,52
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 5   5,3
.B = 361    10,11,12,13,17,
.C = 7740000   21,22,23,24,25,26,27,
           
2056:  M AND NOT Q, Q←7B7, GOTO BLL19 IF LB=0
.BL = 4   79
.TCY = 1   44
.LQY = 1   75
.MC = 22   4,1
.B = 233    10,13,14,16,17,
.C = -10000000   18,19,20,
*  M=AP, SOURCE=NAW, R6=FTYPE, S=NAW
           
2057:  Z←M AND Q LCY 4 MRG BASEAD, IR←XR, CALL BLL48
.MCONT = 1   7
.BL = 10   78
.SSP = 6   55,54
.MS = 4   63
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LZX = 1   76
.LRN = 4   69
.MC = 20   1
.B = 205    10,15,17,
.C = 40   36,
*  R0=ARGADR, R6=FTYPE, M=FIELD IF B
           
2060:  M←FP, Q←2B6
.SSP = 37   56,55,54,53,52
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.C = 2000000   22,
CHECKBIT IS 87   
           
2061:  M LCY 3, GOTO BLL30 IF X<0
.BL = 14   79,78
.MS = 3   65,64
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 223    10,13,16,17,
           
2062:  Q←R6, IR←NOT M, DGOTO BLL36 IF B
.RRN = 6   67,66
.BL = 3   81,80
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 34   3,2,1
.DGO = 1   87
.B = 220    10,13,
CHECKBIT IS 30   
           
2063:  Q←Q LCY 8, S←R0, GOTO BLL29 IF A
.BL = 12   80,78
.MS = 5   65,63
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 2   70
.MC = 33   5,4,2,1
.B = 215    10,14,15,17,
CHECKBIT IS 30   
*  Q=FTYPE/2, S=ARGADR, IR=NOT FP

2064:  BLL27:     MFETCH, .VCY, R0←E1, GOTO BPAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,
CHECKBIT IS 30   

2065:  BLL28:     R6←Q←Q LCY 1, Z←10
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.TXW = 1   47
.LQX = 1   74
.LZY = 1   77
.LRN = 6   70,69
.C = 12   38,40,
           
2066:  Z←Q-Z, Q←E1, READS
.BR = 3   85,84
.BL = 12   80,78
.MS = 56   64,63,62,60
.LOC = 1   50
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.VCY = 1   86
.TE1Y = 1   88
CHECKBIT IS 87   

2067:  BLL50:     XSRC←Q, Q←IR MRG 77774000B, GOTO BLL4TRP IF Z>=0
.RRN = 4   66
.BL = 12   80,78
.SSP = 32   55,53,52
.TCY = 1   44
.THY = 1   46
.LQY = 1   75
.LSPX = 1   59
.MC = 5   5,3
.B = 362    10,11,12,13,16,
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
CHECKBIT IS 50   
           
2070:  Z←NEWL
.SSP = 30   53,52
.TSPY = 1   45
.LZY = 1   77
CHECKBIT IS 87   
           
2071:  Q←IR, K←NOT Q+Z, CALL ITSG IF Y<0
.MCONT = 1   7
.RRN = 4   66
.BR = 14   83,82
.BL = 5   81,79
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 3   71,70
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 354    10,11,12,14,15,
CHECKBIT IS 30   
           
2072:  Q←R6, Q LCY 4, GOTO BLL40 IF X<0
.RRN = 6   67,66
.BL = 12   80,78
.MS = 4   63
.THY = 1   46
.LQY = 1   75
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 331    10,11,13,14,17,
           
2073:  DGOTO Q MRG BLLBASE, Z←NEWP
.MCONT = 3   7,6
.BL = 12   80,78
.SSP = 26   55,54,52
.TCX = 1   43
.TSPY = 1   45
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
.C = 2000   31,
           
2074:  Q←FP, SSOURCE, DGOTO BLL31
.SSP = 37   56,55,54,53,52
.MS = 52   64,62,60
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 75    12,13,14,15,17,
*  K=CPYADDR, S=ARGADDR, Q=FP, SOURCE=NEWP, Z=COUNT

2075:  BLL31:     R0←K, SETA, CALL BSTR1
.MCONT = 1   7
.RRN = 3   68,67
.MS = 30   62,61
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.B = 371    10,11,12,13,14,17,
           
2076:  Z←XSRC, R0←IR←Z-1
.BR = 14   83,82
.BL = 17   81,80,79,78
.SSP = 32   55,53,52
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LRN = 4   69
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
2077:  K←K+1, GOTO BLL32 IF R0<0
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70
.MC = 11   5,2
.B = 103    11,16,17,
CHECKBIT IS 30   
           
2100:  R0←S←S+1, SSOURCE, CALL BLODX
.MCONT = 1   7
.RRN = 2   67
.MS = 52   64,62,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 366    10,11,12,13,15,16,
           
2101:  Z←NEWP, DGOTO BLL31 IF A
.SSP = 26   55,54,52
.TSPY = 1   45
.LZY = 1   77
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 75    12,13,14,15,17,
           
2102:  Z←IR, SSOURCE
.RRN = 4   66
.MS = 52   64,62,60
.THY = 1   46
.LZY = 1   77
CHECKBIT IS 87   

2103:  BLL32:     Z←NAW, M←4B4
.SSP = 35   56,54,53,52
.TCX = 1   43
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.C = 40000   27,
           
2104:  Q AND M, S←NFW, DGOTO BLL14 IF LB=0
.BL = 10   78
.SSP = 34   54,53,52
.TSPY = 1   45
.TYW = 1   48
.LRN = 2   70
.MC = 22   4,1
.DGO = 1   87
.B = 33    13,14,16,17,
CHECKBIT IS 30   
           
2105:  M←Z+1, Z←S+1, .LRN←6, SETBA, GOTO TOSP IF INTRPT1
.RRN = 2   67
.BR = 14   83,82
.MS = 61   65,61,60
.LOC = 1   50
.IHR = 1   42
.THY = 1   46
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.LRN = 6   70,69
.MC = 64   3,1,0
.VCY = 1   86
.B = 1057    8,12,14,15,16,17,
CHECKBIT IS 30   
*  Z=NFW+1, M=RETURN, SOURCE=NEWP, R6=0
*

2106:  BLL34:     Q←Z←BRD, S←Z, SETA, DGOTO BLL22 IF Y<0
.BR = 14   83,82
.SSP = 33   56,55,53,52
.MS = 30   62,61
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LZY = 1   77
.LRN = 2   70
.MC = 53   5,4,2,0
.DGO = 1   87
.VCY = 1   86
.B = 111    11,14,17,
CHECKBIT IS 30   
           
2107:  Q LCY 1, GOTO BLL23 IF X<0
.BL = 12   80,78
.MS = 1   65
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 131    11,13,14,17,
CHECKBIT IS 30   
*  CLL=0, STK=0
           
2110:  Z←Q←BITS, GOTO BLL21
.SSP = 31   56,53,52
.TSPY = 1   45
.LQY = 1   75
.LZY = 1   77
.MC = 1   5
.B = 121    11,13,17,
CHECKBIT IS 30   
*  CLL=1, R6=FSTK(STK)

2111:  BLL22:     Z←Q LCY 3, R0←K←NEWL, CALL BSTR1
.MCONT = 1   7
.BL = 12   80,78
.SSP = 30   53,52
.MS = 3   65,64
.TSPY = 1   45
.TYW = 1   48
.LZX = 1   76
.LRN = 3   71,70
.LR0 = 1   58
.MC = 20   1
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
           
2112:  Q←LR, DGOTO BLL25 IF Z>=0
.SSP = 7   56,55,54
.TSPY = 1   45
.LQY = 1   75
.MC = 5   5,3
.DGO = 1   87
.B = 115    11,14,15,17,
CHECKBIT IS 30   
           
2113:  Q←Q, Q←R6
.RRN = 6   67,66
.BL = 12   80,78
.THY = 1   46
.LQX = 1   74
.LQY = 1   75
           
2114:  Q←Q MRG 1B7
.BL = 12   80,78
.TCX = 1   43
.LQX = 1   74
.C = 10000000   20,

2115:  BLL25:     Q←BITS, M←Q, CALL BSTRX
.MCONT = 1   7
.BL = 12   80,78
.SSP = 31   56,53,52
.TSPY = 1   45
.LMX = 1   72
.LQY = 1   75
.MC = 20   1
.B = 370    10,11,12,13,14,
           
2116:  Z←P, Q LCY 2, CALL BLL63 IF X<0
.MCONT = 1   7
.RRN = 1   68
.BL = 12   80,78
.MS = 2   64
.THY = 1   46
.LZY = 1   77
.MC = 17   5,4,3,2
.VCY = 1   86
.B = 176    11,12,13,14,15,16,
CHECKBIT IS 30   
           
2117:  M←SP, Z←Q, GOTO BLL21 IF A
.BL = 12   80,78
.SSP = 22   55,52
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
.MC = 33   5,4,2,1
.B = 121    11,13,17,
CHECKBIT IS 30   
           
2120:  R0←SPAD, CALL BSTR1
.MCONT = 1   7
.SSP = 23   56,55,52
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   

2121:  BLL21:     R0←Z, M←BRD, SETA, CALL MON IF Z[18]
.MCONT = 1   7
.BR = 14   83,82
.SSP = 33   56,55,53,52
.MS = 30   62,61
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.MC = 14   3,2
.B = 731    9,10,11,13,14,17,
           
2122:  Z←NEWG
.SSP = 25   56,54,52
.TSPY = 1   45
.LZY = 1   77
           
2123:  GR←Z, R0←P←Z←S, CALL BLL18 IF M[5]
.MCONT = 1   7
.RRN = 2   67
.BR = 14   83,82
.SSP = 10   53
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LR0 = 1   58
.LSPX = 1   59
.MC = 50   2,0
.B = 127    11,13,15,16,17,
CHECKBIT IS 30   

2124:  BLL20:     Q←NEWL, SETBA, DGOTO NX5 IF A
.SSP = 30   53,52
.MS = 61   65,61,60
.TSPY = 1   45
.LQY = 1   75
.MC = 33   5,4,2,1
.DGO = 1   87
.B = 350    10,11,12,14,
CHECKBIT IS 30   
           
2125:  LR←Q, SABCDE0
.BL = 12   80,78
.SSP = 7   56,55,54
.MS = 33   65,64,62,61
.LSPX = 1   59
CHECKBIT IS 87   
*  XMON OR XUTIL TRAP
           
2126:  SETBA, Q←0, GOTO FIXTRP
.MS = 61   65,61,60
.LQX = 1   74
.MC = 1   5
.B = 1046    8,12,15,16,
*
*  FTN=1

2127:  BLL18:     Q LCY 3, RETURN IF X<0
.MCONT = 2   6
.BL = 12   80,78
.MS = 3   65,64
.MC = 17   5,4,3,2
.VCY = 1   86
CHECKBIT IS 30   
           
2130:  R0←P←Z←S+1, RETURN
.MCONT = 2   6
.RRN = 2   67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.LR0 = 1   58
.MC = 1   5
*
*  STK=1

2131:  BLL23:     Z←P, R6←2B7, CLEARA, GOTO BLL22 IF Z<0
.RRN = 1   68
.MS = 50   62,60
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 6   70,69
.MC = 4   3
.B = 111    11,14,17,
.C = 20000000   19,
CHECKBIT IS 50   
*  STK=1, CLL=0
           
2132:  Q←GR, Z←2, SSOURCE
.SSP = 10   53
.MS = 52   64,62,60
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
.C = 2   40,
           
2133:  R0←Q+Z, M←SP, CALL BSTR1
.MCONT = 1   7
.BR = 14   83,82
.BL = 12   80,78
.SSP = 22   55,52
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
           
2134:  Z←Q←BITS, GOTO BLL21
.SSP = 31   56,53,52
.TSPY = 1   45
.LQY = 1   75
.LZY = 1   77
.MC = 1   5
.B = 121    11,13,17,
CHECKBIT IS 30   
*  S=E, M=BRD, STK=1

2135:  BLL4:      R0←M LCY 3, Q←LR, GOTO BLL7 IF R0<0
.BL = 14   79,78
.SSP = 7   56,55,54
.MS = 3   65,64
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.MC = 11   5,2
.B = 145    11,12,15,17,
*  CLL=0
           
2136:  SP←Q, Z←S, DGOTO BLL6
.RRN = 2   67
.BL = 12   80,78
.SSP = 22   55,52
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
.MC = 1   5
.DGO = 1   87
.B = 26    13,15,16,
           
2137:  Q←NEWP, R0←M, GOTO BLL8 IF R0<0
.BL = 14   79,78
.SSP = 26   55,54,52
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.MC = 11   5,2
.B = 140    11,12,
*  CLL=0, UWSTK=1, SOURCE=Q(+1)

2140:  BLL8:      SP←Z, R0←S+1
.RRN = 2   67
.BR = 14   83,82
.SSP = 22   55,52
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
CHECKBIT IS 87   
           
2141:  MAP, .VCY, R0←E1, Q←77B6, GOTO BPAGEF IF Y<0
.MS = 37   65,64,63,62,61
.TCX = 1   43
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
           
2142:  FETCH, IR←M
.BL = 14   79,78
.MS = 44   63,60
.TXW = 1   47
.LRN = 4   69
CHECKBIT IS 87   
           
2143:  S←M AND NOT Q, M←IR, DGOTO BLL6
.RRN = 4   66
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 2   70
.MC = 20   1
.DGO = 1   87
.B = 26    13,15,16,
           
2144:  R0←M, Q←NEWP
.BL = 14   79,78
.SSP = 26   55,54,52
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
*
*  STK=1, CLL=1, SOURCE=Q(+1)

2145:  BLL7:      M←NEWG, Z←2
.SSP = 25   56,54,52
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
.C = 2   40,
CHECKBIT IS 87   
           
2146:  R0←SPAD←IR←M+Z, CALL BLODX
.MCONT = 1   7
.BR = 14   83,82
.BL = 14   79,78
.SSP = 23   56,55,52
.TXW = 1   47
.TAX = 1   49
.LRN = 4   69
.LR0 = 1   58
.LSPX = 1   59
.MC = 1   5
.VCY = 1   86
.B = 366    10,11,12,13,15,16,
           
2147:  R0←IR+1, Q←M, CALL BLODX
.MCONT = 1   7
.RRN = 4   66
.BL = 14   79,78
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 20   1
.B = 366    10,11,12,13,15,16,
CHECKBIT IS 30   
           
2150:  Z←S, S←Q
.RRN = 2   67
.BL = 12   80,78
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 2   70
           
2151:  Z←SP←Q+Z
.BR = 14   83,82
.BL = 12   80,78
.SSP = 22   55,52
.TAX = 1   49
.LZX = 1   76
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
2152:  R0←M←BRD, Z←Z-M, DGOTO BLL6
.BR = 14   83,82
.BL = 3   81,80
.SSP = 33   56,55,53,52
.LOC = 1   50
.TSPY = 1   45
.TYW = 1   48
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 26    13,15,16,
           
2153:  Q←NEWP, GOTO STAKOV IF Z>0
.SSP = 26   55,54,52
.TSPY = 1   45
.LQY = 1   75
.MC = 6   4,3
.B = 154    11,12,14,15,

2154:  STAKOV:    IR←9, Q←0, SETBA, GOTO RDTRAP
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1041    8,12,17,
.C = 11   38,41,
*
*  STK=0, NEWL=0

2155:  BLL5:      S←LR, GOTO BLL6
.SSP = 7   56,55,54
.TSPY = 1   45
.TYW = 1   48
.LRN = 2   70
.MC = 20   1
.B = 26    13,15,16,
CHECKBIT IS 30   
*
*  PAGE CROSSING IN AAW SEQUENCE, R6=NEW

2156:  BLL72:     R0←S+1
.RRN = 2   67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
           
2157:  MAP,DRETURN
.MCONT = 2   6
.MS = 37   65,64,63,62,61
.MC = 20   1
.DGO = 1       87
CHECKBIT IS 30
           
2160:  R0←E1, TAKE, GOTO BPAGEF IF Y<0
.MS = 75   65,63,62,61,60
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,
CHECKBIT IS 30   
*
*  JUMP TYPE AAW, M=ACTUAL PARAM

2161:  BLL73:     Q←7B7, GOTO BLL2TRP IF Z<0
.TCX = 1   43
.LQX = 1   74
.MC = 4   3
.B = 360    10,11,12,13,
.C = -10000000   18,19,20,
CHECKBIT IS 50   
           
2162:  Z←M AND Q LCY 4 MRG BASEAD, IR←XR, CALL BLL48
.MCONT = 1   7
.BL = 10   78
.SSP = 6   55,54
.MS = 4   63
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LZX = 1   76
.LRN = 4   69
.MC = 1   5
.B = 205    10,15,17,
.C = 40   36,
           
2163:  R6←Q←R0, GOTO BLL5TRP IF A
.THY = 1   46
.TYW = 1   48
.LQY = 1   75
.LRN = 6   70,69
.MC = 33   5,4,2,1
.B = 363    10,11,12,13,16,17,
           
2164:  MFETCH, NAW←Q, R0←E1, .VCY, GOTO BPAGEF IF Y<0
.BL = 12   80,78
.SSP = 35   56,54,53,52
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.LSPX = 1   59
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 367    10,11,12,13,15,16,17,
           
2165:  R0+1, TAKE
.MS = 75   65,63,62,61,60
.IHR = 1   42
.THY = 1   46
           
2166:  Z←IR←FP, Q←17B5, DGOTO BLL71
.SSP = 37   56,55,54,53,52
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LQX = 1   74
.LZY = 1   77
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 37    13,14,15,16,17,
.C = 1700000   23,24,25,26,
CHECKBIT IS 50   
           
2167:  Z←BITS, R6←Q AND Z, GOTO BLL72 IF XPAGE
.BR = 10   82
.SSP = 31   56,53,52
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 6   70,69
.MC = 42   4,0
.B = 156    11,12,14,15,16,
CHECKBIT IS 30
*
*  Q=16B5, M=AP, R6=FTYPE, TYPE MISMATCH

2170:  BLL74:     AP←M
.BL = 14   79,78
.SSP = 36   55,54,53,52
.LSPX = 1   59
           
2171:  M←Q EOR M, Q←17B5
.BL = 6   80,79
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = 1700000   23,24,25,26,
           
2172:  M AND Q, Q←16B5, GOTO BLL75 IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.MC = 22   4,1
.B = 175    11,12,13,14,15,17,
.C = 1600000   23,24,25,
CHECKBIT IS 50   
           
2173:  R6←Q, M←R6
.RRN = 6   67,66
.BL = 12   80,78
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 6   70,69
           
2174:  M EOR Q, GOTO BLL3TRP IF LB#0
.BL = 6   80,79
.MC = 23   5,4,1
.B = 361    10,11,12,13,17,
CHECKBIT IS 30   

2175:  BLL75:     M←AP, Q←Z←0, GOTO BLL76
.SSP = 36   55,54,53,52
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.LZX = 1   76
.MC = 20   1
.B = 44    12,15,
*
*  TEST TRANSITION TO HIGHER RING

2176:  BLL63:     Z←R0←NEWP, SSOURCE
.SSP = 26   55,54,52
.MS = 52   64,62,60
.TSPY = 1   45
.TYW = 1   48
.LZY = 1   77
.LR0 = 1   58
CHECKBIT IS 87   
           
2177:  MAPSS, M←NEWG
.SSP = 25   56,54,52
.MS = 51   65,62,60
.TSPY = 1   45
.LMY = 1   73
CHECKBIT IS 87   
           
2200:  R0←E1, Z←14B, SSOURCE, DRETURN
.MCONT = 2   6
.MS = 52   64,62,60
.TCX = 1   43
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.TE1Y = 1   88
.C = 14   38,39,
           
2201:  R0←M+Z, M←GR, GOTO BSTR1 IF R0<0
.BR = 14   83,82
.BL = 14   79,78
.SSP = 10   53
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 11   5,2
.VCY = 1   86
.B = 371    10,11,12,13,14,17,
*
*  REL BIT IS SET IN BRD

2202:  BLL85:     Z←R6, Q←777B5, DRETURN IF M[9]
.MCONT = 2   6
.RRN = 6   67,66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 71   5,2,1,0
.DGO = 1   87
.C = -100000   18,19,20,21,22,23,24,25,26,
           
2203:  NEWP←Z←Z+M OR Q
.BR = 14   83,82
.BL = 16   80,79,78
.SSP = 26   55,54,52
.TAX = 1   49
.LZX = 1   76
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
2204:  NEWP←Z←Z-Q, RETURN
.MCONT = 2   6
.BR = 14   83,82
.BL = 5   81,79
.SSP = 26   55,54,52
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.LSPX = 1   59
.MC = 1   5
.VCY = 1   86
CHECKBIT IS 30   
*
*  DISPATCH ON ADDRESSING MODE

2205:  BLL48:     SETC, GOTO ILIM←Z, Q←37777B
.MCONT = 3   7,6
.BR = 14   83,82
.SSP = 20   52
.MS = 35   65,63,62,61
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.MC = 20   1
.C = 37777   28,29,30,31,32,33,34,35,36,37,38,39,40,41,
*
*  POP, Q=FP, Z=BITS, R0=NAW

2206:  BLL39:     MAP, M←POPW, Q←200B
.SSP = 24   54,52
.MS = 37   65,64,63,62,61
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.LQX = 1   74
.C = 200   34,
CHECKBIT IS 87   
           
2207:  BITS←Z OR Q, E1, TAKE
.BR = 16   84,83,82
.SSP = 31   56,53,52
.MS = 75   65,63,62,61,60
.LSPX = 1   59
.TE1Y = 1   88
           
2210:  Z←IR, Q←374B4
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.C = 3740000   22,23,24,25,26,27,
           
2211:  M←M AND NOT Q ! Z AND Q, Z←NAW, DGOTO BLL33
.BR = 10   82
.BL = 4   79
.SSP = 35   56,54,53,52
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
.B = 42    12,16,
           
2212:  S←NAW←Z-1, Q←0
.BR = 14   83,82
.BL = 17   81,80,79,78
.SSP = 35   56,54,53,52
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 2   70
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
*
*  CPA=0, Z=NEWP, M=PR+1, SOURCE =NEWP

2213:  BLL13:     R0←BITS, DGOTO BLL34
.SSP = 31   56,53,52
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.B = 106    11,15,16,
           
2214:  R6←0, GOTO BLL2TRP IF R0>=0
.TXW = 1   47
.LRN = 6   70,69
.MC = 12   4,2
.B = 360    10,11,12,13,
*
*  IMMEDIATE, CVAL=0, Q=FTYPE, M=FP, R0=ARGADR

2215:  BLL29:     M LCY 4, M←4B7, GOTO BLL5TRP IF X>=0
.BL = 14   79,78
.MS = 4   63
.TCY = 1   44
.LMY = 1   73
.MC = 16   4,3,2
.VCY = 1   86
.B = 363    10,11,12,13,16,17,
.C = -40000000   18,
CHECKBIT IS 50   
           
2216:  Z←Q EOR M, M←R0, DGOTO BLL28
.BL = 6   80,79
.THY = 1   46
.LMY = 1   73
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.B = 65    12,13,15,17,
           
2217:  GOTO BLL5TRP IF Z#0
.MC = 3   5,4
.B = 363    10,11,12,13,16,17,
CHECKBIT IS 30   
*
*  FIELD, M=FP, IR=THE FIELD

2220:  BLL36:     M←4B7
.TCX = 1   43
.LMX = 1   72
.C = -40000000   18,
           
2221:  Q EOR M, M←IR, GOTO BLL28 IF LB=0
.RRN = 4   66
.BL = 6   80,79
.THY = 1   46
.LMY = 1   73
.MC = 22   4,1
.B = 65    12,13,15,17,
           
2222:  GOTO BLL27
.MC = 1   5
.B = 64    12,13,15,
CHECKBIT IS 30   
*
*  CVAL=1, R0=ARGADR, NOTE THAT Z IS LEFT NEGATIVE

2223:  BLL30:     M←R0, IR←NOT M AND NOT Q, GOTO BLL87 IF A
.BL = 1   81
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 4   69
.MC = 33   5,4,2,1
.B = 231    10,13,14,17,
           
2224:  Z←AP, Q←6B6
.SSP = 36   55,54,53,52
.TCX = 1   43
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.C = 6000000   21,22,
CHECKBIT IS 87   
           
2225:  Q←NOT Q ! Q AND Z, MAP, GOTO BLL38 IF D
.BR = 10   82
.BL = 5   81,79
.MS = 37   65,64,63,62,61
.LQX = 1   74
.MC = 47   5,4,3,0
.B = 227    10,13,15,16,17,
CHECKBIT IS 30   
           
2226:  NOT Q, R0←E1, DGOTO BLL50 IF LB#0
.BL = 5   81,79
.TYW = 1   48
.LR0 = 1   58
.MC = 23   5,4,1
.DGO = 1   87
.TE1Y = 1   88
.B = 67    12,13,15,16,17,
CHECKBIT IS 30   

2227:  BLL38:     R6←Z←40000001B, SETBA, GOTO BMACC1 IF R0[1]
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LZX = 1   76
.LRN = 6   70,69
.MC = 66   4,3,1,0
.B = 1716   8,9,10,11,14,15,16,
.C = -37777777   18,41,
           
2230:  M←M, M←1B7, GOTO BLL50
.BL = 14   79,78
.TCY = 1   44
.LMX = 1   72
.LMY = 1   73
.MC = 1   5
.B = 67    12,13,15,16,17,
.C = 10000000   20,
CHECKBIT IS 50   
*  CVAL=1, IMMEDIATE

2231:  BLL87:     Q←3777B, DGOTO BLL38
.TCX = 1   43
.LQX = 1   74
.MC = 20   1
.DGO = 1   87
.B = 227    10,13,15,16,17,
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
2232:  M←M AND Q MRG 1634B4, R0←0, DGOTO BLL50
.BL = 10   78
.TCX = 1   43
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 67    12,13,15,16,17,
.C = 16340000   20,21,22,25,26,27,
CHECKBIT IS 50   
*
*  ARGUMENT IN CENTRAL REGISTER, R6=FTYPE, A=(STR=0)
*

2233:  BLL19:     Q←R6, Z←7B5, GOTO BLL4TRP IF A
.RRN = 6   67,66
.TCX = 1   43
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
.MC = 33   5,4,2,1
.B = 362    10,11,12,13,16,
.C = 700000   24,25,26,
CHECKBIT IS 50   
           
2234:  R0←Z←Z-Q, M←FP
.BR = 14   83,82
.BL = 5   81,79
.SSP = 37   56,55,54,53,52
.LOC = 1   50
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.LR0 = 1   58
.VCY = 1   86
           
2235:  Z←M LCY 3, R0←R0+1, GOTO BLL82 IF Z<=0
.BL = 14   79,78
.MS = 3   65,64
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LR0 = 1   58
.MC = 13   5,4,2
.B = 254    10,12,14,15,
CHECKBIT IS 30   

2236:  BLL81:     IR←NOT M, Z←NEWP, SETC, GOTO BLL5TRP IF Z<0
.BL = 3   81,80
.SSP = 26   55,54,52
.MS = 35   65,63,62,61
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
.MC = 4   3
.B = 363    10,11,12,13,16,17,
CHECKBIT IS 30   
           
2237:  M←Q LCY 8, Q←IR MRG 77774000B
.RRN = 4   66
.BL = 12   80,78
.MS = 5   65,63
.TCY = 1   44
.THY = 1   46
.LMX = 1   72
.LQY = 1   75
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
CHECKBIT IS 87   
           
2240:  M←AR, DGOTO M LCY1 MRG BLLBASE
.MCONT = 3   7,6
.BL = 14   79,78
.SSP = 1   56
.MS = 1   65
.TCX = 1   43
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.DGO = 1   87
.C = 2000   31,
CHECKBIT IS 50   
           
2241:  SSOURCE, DGOTO BLL51
.MS = 52   64,62,60
.MC = 20   1
.DGO = 1   87
.B = 242    10,12,16,
CHECKBIT IS 30   

2242:  BLL51:     Z←NEWL, R6←NOT Z, DGOTO BLL83 IF R0<0
.BR = 3   85,84
.SSP = 30   53,52
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 6   70,69
.MC = 11   5,2
.DGO = 1   87
.B = 256    10,12,14,15,16,
           
2243:  IR, K←NOT Q+Z, CALL ITSG IF Y<0
.MCONT = 1   7
.RRN = 4   66
.BR = 14   83,82
.BL = 5   81,79
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LRN = 3   71,70
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 354    10,11,12,14,15,
           
2244:  R0←K, CALL BSTR1
.MCONT = 1   7
.RRN = 3   68,67
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.B = 371    10,11,12,13,14,17,
           
2245:  Z←R6+1
.RRN = 6   67,66
.IHR = 1   42
.THY = 1   46
.LZY = 1   77
           
2246:  Q←FP, GOTO BLL32 IF Z=0
.SSP = 37   56,55,54,53,52
.TSPY = 1   45
.LQY = 1   75
.MC = 2   4
.B = 103    11,16,17,
           
2247:  Z←Z+1, M←BR, CALL BSTRX
.MCONT = 1   7
.BR = 14   83,82
.SSP = 2   55
.LOC = 1   50
.TSPY = 1   45
.TAX = 1   49
.LMY = 1   73
.LZX = 1   76
.MC = 20   1
.VCY = 1   86
.B = 370    10,11,12,13,14,
CHECKBIT IS 30   
           
2250:  GOTO BLL32 IF Z=0
.MC = 2   4
.B = 103    11,16,17,
CHECKBIT IS 30   
           
2251:  M←CR, CALL BSTRX
.MCONT = 1   7
.SSP = 3   56,55
.TSPY = 1   45
.LMY = 1   73
.MC = 20   1
.B = 370    10,11,12,13,14,
           
2252:  M←DR, CALL BSTRX
.MCONT = 1   7
.SSP = 4   54
.TSPY = 1   45
.LMY = 1   73
.MC = 1   5
.B = 370    10,11,12,13,14,
CHECKBIT IS 30   
           
2253:  GOTO BLL32
.MC = 20   1
.B = 103    11,16,17,
CHECKBIT IS 30   
*  CHECK IF POINTER TYPE

2254:  BLL82:     R0←Z←R0+1, DGOTO BLL81
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 236    10,13,14,15,16,
CHECKBIT IS 30   
           
2255:  Z←M LCY 3, GOTO BLL4TRP IF Z#0
.BL = 14   79,78
.MS = 3   65,64
.LZX = 1   76
.MC = 3   5,4
.B = 362    10,11,12,13,16,
CHECKBIT IS 30   
*
*  STORE FLOATING ACCUMULATOR

2256:  BLL83:     R6←R0←K, Q←M, SETBA, DGOTO STFX
.RRN = 3   68,67
.BL = 14   79,78
.MS = 61   65,61,60
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 6   70,69
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 1430    8,9,13,14,
           
2257:  MAP, R0←E1, .VCY, GOTO PAGEF IF Y<0
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
*
*  TEST FOR FLOATING OVERFLOW, M=EXPONENT

2260:  BLL84:     Q←FP, DGOTO TFO1
.SSP = 37   56,55,54,53,52
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 1235    8,10,13,14,15,17,
           
2261:  GOTO BLL32 IF M[0]=M[1]
.MC = 73   5,4,2,1,0
.B = 103    11,16,17,
CHECKBIT IS 30   
*
*  TEST FOR UNDEFINED FLOATING NUMBER

2262:  BLL86:     Z←M-Z, R0←4B7, DRETURN
.MCONT = 2   6
.BR = 3   85,84
.BL = 14   79,78
.LOC = 1   50
.TCY = 1   44
.TYW = 1   48
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.C = -40000000   18,
           
2263:  Z←1, GOTO UFN IF Z=0
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 2   4
.B = 1241    8,10,12,17,
CHECKBIT IS 30   
*
*  FTYPE=STRING, R0=BITS, M=FIRST STRING WORD
*  K=CPYADR, S=ARGADR
*

2264:  BLL42:     IR←0, GOTO BLL31 IF NR0[2]
.TXW = 1   47
.LRN = 4   69
.MC = 25   5,3,1
.B = 75    12,13,14,15,17,
CHECKBIT IS 30   
           
2265:  Q←14B6, DGOTO BLL53
.TCX = 1   43
.LQX = 1   74
.MC = 20   1
.DGO = 1   87
.B = 271    10,12,13,14,17,
.C = 14000000   20,21,
           
2266:  R6←M AND Q MRG 40000003B
.BL = 10   78
.TCX = 1   43
.TXW = 1   47
.LRN = 6   70,69
.C = -37777775   18,40,41,
CHECKBIT IS 87   

2267:  BLL55:     SSOURCE, R0←S←S+1, CALL BLODX
.MCONT = 1   7
.RRN = 2   67
.MS = 52   64,62,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.MC = 20   1
.B = 366    10,11,12,13,15,16,
           
2270:  K←K+1
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70

2271:  BLL53:     Z←S, Q←77B6
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.C = -1000000   18,19,20,21,22,23,
           
2272:  R0←M AND NOT Q, Z←IR, SSOURCE, DGOTO BMACC
.RRN = 4   66
.BL = 4   79
.MS = 52   64,62,60
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 722   9,10,11,13,16
CHECKBIT IS 30
           
2273:  MAP, .VCY, R0←E1, DGOTO *+1
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.VCY = 1   86
.TE1Y = 1   88
.MC = 20   1
.DGO = 1   87
.B = 274   10,12,13,14,15
CHECKBIT IS 30

2274:  Z←R0←M AND NOT Q-Z AND NOT Q
.BR = 13   85,84,82
.BL = 4   79
.LOC = 1   50
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 30   
           
2275:  Q←3B6, Z←IR, DGOTO BLL54 IF Z=0
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 2   4
.DGO = 1   87
.B = 276    10,12,13,14,15,16,
.C = 3000000   22,23,

2276:  BLL54:     R0←M AND Q-Z AND Q, Z←NEWP, GOTO BLL6TRP IF R0<0
.BR = 7   85,84,83
.BL = 10   78
.SSP = 26   55,54,52
.LOC = 1   50
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LR0 = 1   58
.MC = 11   5,2
.VCY = 1   86
.B = 364    10,11,12,13,15,
           
2277:  Z←R6, Q←74B6, SSOURCE
.RRN = 6   67,66
.MS = 52   64,62,60
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.C = -4000000   18,19,20,21,
           
2300:  M←M AND NOT Q ! Z AND Q, R0←K, CALL BSTR1
.MCONT = 1   7
.RRN = 3   68,67
.BR = 10   82
.BL = 4   79
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
.MC = 1   5
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
           
2301:  R6←Z←Z-1, Q←FP, DGOTO BLL32
.BR = 14   83,82
.BL = 17   81,80,79,78
.SSP = 37   56,55,54,53,52
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.LRN = 6   70,69
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 103    11,16,17,
CHECKBIT IS 30   
           
2302:  IR←M, Z←XSRC, GOTO BLL55 IF NZ[16]
.BL = 14   79,78
.SSP = 32   55,53,52
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
.MC = 10   2
.B = 267    10,12,13,15,16,17,
CHECKBIT IS 30   
*  FTYPE=LABEL, IR[0]=NOT REL
*

2303:  BLL44:     Z←S, Q←777B5, DGOTO BLL46 IF M[9]
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 71   5,2,1,0
.DGO = 1   87
.B = 310    10,11,14,
.C = -100000   18,19,20,21,22,23,24,25,26,
CHECKBIT IS 50   
           
2304:  Z←M OR Q+Z, IR, SSOURCE, GOTO BLL45 IF Y<0
.RRN = 4   66
.BR = 14   83,82
.BL = 16   80,79,78
.MS = 52   64,62,60
.THY = 1   46
.TAX = 1   49
.LZX = 1   76
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 306    10,11,15,16,
           
2305:  Z←Z-Q, GOTO BLL46
.BR = 14   83,82
.BL = 5   81,79
.LOC = 1   50
.TAX = 1   49
.LZX = 1   76
.MC = 20   1
.VCY = 1   86
.B = 310    10,11,14,
CHECKBIT IS 30   

2306:  BLL45:     Q←77B6
.TCX = 1   43
.LQX = 1   74
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   
           
2307:  Z←M AND NOT Q
.BL = 4   79
.LZX = 1   76
CHECKBIT IS 87   

2310:  BLL46:     R0←BITS, Q←75B6
.SSP = 31   56,53,52
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.C = -3000000   18,19,20,21,23,
           
2311:  R0←M←M AND Q ! Z AND NOT Q, Z←NEWP, GOTO BLL47 IF NR0[2]
.BR = 4   83
.BL = 10   78
.SSP = 26   55,54,52
.TSPY = 1   45
.TXW = 1   47
.LMX = 1   72
.LZY = 1   77
.LR0 = 1   58
.MC = 25   5,3,1
.B = 314    10,11,14,15,
           
2312:  MAP, R0←E1, .VCY
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.VCY = 1   86
.TE1Y = 1   88
           
2313:  SETBA, R6←4B7, GOTO BMACC1 IF R0[1]
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LRN = 6   70,69
.MC = 66   4,3,1,0
.B = 1716   8,9,10,11,14,15,16,
.C = -40000000   18,

2314:  BLL47:     SSOURCE, R0←K, CALL BSTR1
.MCONT = 1   7
.RRN = 3   68,67
.MS = 52   64,62,60
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
           
2315:  Z←XSRC, Q←57B6
.SSP = 32   55,53,52
.TCX = 1   43
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.C = 57000000      18,20,21,22,23
CHECKBIT IS 87   
           
2316:  R0←S←S+1, SSOURCE, CALL BLODX
.MCONT = 1   7
.RRN = 2   67
.MS = 52   64,62,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 366    10,11,12,13,15,16,
           
2317:  Z←NEWP, M AND NOT Q, DGOTO BLL57 IF LB#0
.BL = 4   79
.SSP = 26   55,54,52
.TSPY = 1   45
.LZY = 1   77
.MC = 23   5,4,1
.DGO = 1   87
.B = 326    10,11,13,15,16,
           
2320:  SSOURCE, Z←M, Q←BRD
.BL = 14   79,78
.SSP = 33   56,55,53,52
.MS = 52   64,62,60
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
CHECKBIT IS 87   
           
2321:  Q←NEWL MRG 24B6, Q LCY 1, DGOTO *+1 IF X>=0
.BL = 12   80,78
.SSP = 30   53,52
.MS = 1   65
.TCY = 1   44
.TSPY = 1   45
.LQY = 1   75
.MC = 16   4,3,2
.DGO = 1   87
.VCY = 1   86
.B = 322    10,11,13,16,
.C = 24000000   19,21,
           
2322:  Z←M OR Q, Q←LR
.BL = 16   80,79,78
.SSP = 7   56,55,54
.TSPY = 1   45
.LQY = 1   75
.LZX = 1   76
           
2323:  R6, Q←4B7, GOTO BLL58 IF Y<0
.RRN = 6   67,66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 330    10,11,13,14,
.C = -40000000   18,

2324:  BLL49:     Q←FP, M←Z, CALL BSTRX
.MCONT = 1   7
.BR = 14   83,82
.SSP = 37   56,55,54,53,52
.TSPY = 1   45
.LMX = 1   72
.LQY = 1   75
.MC = 1   5
.B = 370    10,11,12,13,14,
           
2325:  GOTO BLL32
.MC = 20   1
.B = 103    11,16,17,
CHECKBIT IS 30   
*

2326:  BLL57:     DGOTO BLL49
.MC = 1   5
.DGO = 1   87
.B = 324    10,11,13,15,
CHECKBIT IS 30   
           
2327:  R6, GOTO BLL6TRP IF Y<0
.RRN = 6   67,66
.THY = 1   46
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 364    10,11,12,13,15,

2330:  BLL58:     Z←Z AND NOT Q, GOTO BLL49
.BR = 4   83
.LZX = 1   76
.MC = 1   5
.B = 324    10,11,13,15,
*
*  FSTR=0, CVAL=1

2331:  BLL40:     Z←NEWP, Q←6B7
.SSP = 26   55,54,52
.TCX = 1   43
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.C = -20000000   18,19,
           
2332:  R0←K, SSOURCE, CALL BSTR1
.MCONT = 1   7
.RRN = 3   68,67
.MS = 52   64,62,60
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
           
2333:  R0←BITS, SETC, NOT M AND Q, GOTO BLL6TRP IF LB#0
.BL = 2   80
.SSP = 31   56,53,52
.MS = 35   65,63,62,61
.TSPY = 1   45
.TYW = 1   48
.LR0 = 1   58
.MC = 23   5,4,1
.B = 364    10,11,12,13,15,
           
2334:  ILIM←0, Z←R6←S, GOTO BLL59 IF NR0[2]
.RRN = 2   67
.SSP = 20   52
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 6   70,69
.LSPX = 1   59
.MC = 25   5,3,1
.B = 342    10,11,12,16,
           
2335:  M LCY 4, Q←774B5, DGOTO BLL60 IF X<0
.BL = 14   79,78
.MS = 4   63
.TCY = 1   44
.LQY = 1   75
.MC = 17   5,4,3,2
.DGO = 1   87
.VCY = 1   86
.B = 336    10,11,13,14,15,16,
.C = -400000   18,19,20,21,22,23,24,
CHECKBIT IS 50   

2336:  BLL60:     IR←M AND NOT Q, Q←7776B4, SSOURCE
.BL = 4   79
.MS = 52   64,62,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.C = -20000   18,19,20,21,22,23,24,25,26,27,28,
           
2337:  SETBA, CALL IASUB
.MCONT = 1   7
.MS = 61   65,61,60
.MC = 20   1
.B = 103    11,16,17,
CHECKBIT IS 30   
           
2340:  MAP, .VCY, R0←E1
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.VCY = 1   86
.TE1Y = 1   88
           
2341:  ILIM←0, Z←R6, CLEARA, GOTO BMACC IF R0[1]
.RRN = 6   67,66
.SSP = 20   52
.MS = 50   62,60
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
.MC = 66   4,3,1,0
.B = 722    9,10,11,13,16,

2342:  BLL59:     S←R0←R6+1, Q←16B6, SSOURCE, CALL BLODX
.MCONT = 1   7
.RRN = 6   67,66
.MS = 52   64,62,60
.IHR = 1   42
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.B = 366    10,11,12,13,15,16,
.C = 16000000   20,21,22,
           
2343:  Q←NOT M OR NOT Q LCY 2, IR←0
.BL = 7   81,80,79
.MS = 2   64
.TYW = 1   48
.LQX = 1   74
.LRN = 4   69
           
2344:  SETBA, CALL BASEIA
.MCONT = 1   7
.MS = 61   65,61,60
.MC = 1   5
.B = 110    11,14,
           
2345:  MAP, .VCY, Q←E1, M←12B6, DGOTO BLL61 IF D
.MS = 37   65,64,63,62,61
.TCX = 1   43
.LMX = 1   72
.LQY = 1   75
.MC = 47   5,4,3,0
.DGO = 1   87
.VCY = 1   86
.TE1Y = 1   88
.B = 350    10,11,12,14,
.C = 12000000   20,22,
           
2346:  Z←BITS, GOTO BLL6TRP IF A
.SSP = 31   56,53,52
.TSPY = 1   45
.LZY = 1   77
.MC = 33   5,4,2,1
.B = 364    10,11,12,13,15,
CHECKBIT IS 30   
           
2347:  M←4B6
.TCX = 1   43
.LMX = 1   72
.C = 4000000   21,

2350:  BLL61:     R0←Z AND Q, Z←R0
.BR = 10   82
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
           
2351:  Z←NEWP, R0←Z, GOTO BMACCQ IF R0[1]
.BR = 14   83,82
.SSP = 26   55,54,52
.TSPY = 1   45
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
.MC = 66   4,3,1,0
.B = 540    9,11,12,
CHECKBIT IS 30

2352:  M←M, M←R0, SSOURCE, CALL BSTRX
.MCONT = 1   7
.BL = 14   79,78
.MS = 52   64,62,60
.THY = 1   46
.LMX = 1   72
.LMY = 1   73
.MC = 1   5
.B = 370    10,11,12,13,14,
           
2353:  Q←FP, GOTO BLL32
.SSP = 37   56,55,54,53,52
.TSPY = 1   45
.LQY = 1   75
.MC = 20   1
.B = 103    11,16,17,
*
*  IR= NOT FP

2354:  ITSG:      Z←NEWG
.SSP = 25   56,54,52
.TSPY = 1   45
.LZY = 1   77
           
2355:  Q←IR MRG 7774B4, DRETURN
.MCONT = 2   6
.RRN = 4   66
.TCY = 1   44
.THY = 1   46
.LQY = 1   75
.MC = 20   1
.DGO = 1   87
.C = -40000   18,19,20,21,22,23,24,25,26,27,
           
2356:  K←NOT Q+Z, Q←IR
.RRN = 4   66
.BR = 14   83,82
.BL = 5   81,79
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 3   71,70
.VCY = 1   86
CHECKBIT IS 87   
*

2357:  BLL1TRP:   Q←1B6, GOTO BLLTRAP
.TCX = 1   43
.LQX = 1   74
.MC = 20   1
.B = 365    10,11,12,13,15,17,
.C = 1000000   23,
CHECKBIT IS 50   

2360:  BLL2TRP:   Q←NAW MRG 2B6, GOTO BLLTRAP
.SSP = 35   56,54,53,52
.TCY = 1   44
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.B = 365    10,11,12,13,15,17,
.C = 2000000   22,

2361:  BLL3TRP:   Q←NAW MRG 3B6, GOTO BLLTRAP
.SSP = 35   56,54,53,52
.TCY = 1   44
.TSPY = 1   45
.LQY = 1   75
.MC = 20   1
.B = 365    10,11,12,13,15,17,
.C = 3000000   22,23,
CHECKBIT IS 50   

2362:  BLL4TRP:   Q←NAW MRG 4B6, GOTO BLLTRAP
.SSP = 35   56,54,53,52
.TCY = 1   44
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.B = 365    10,11,12,13,15,17,
.C = 4000000   21,

2363:  BLL5TRP:   Q←NAW MRG 5B6, GOTO BLLTRAP
.SSP = 35   56,54,53,52
.TCY = 1   44
.TSPY = 1   45
.LQY = 1   75
.MC = 20   1
.B = 365    10,11,12,13,15,17,
.C = 5000000   21,23,
CHECKBIT IS 50   

2364:  BLL6TRP:   Q←NAW MRG 6B6, GOTO BLLTRAP
.SSP = 35   56,54,53,52
.TCY = 1   44
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.B = 365    10,11,12,13,15,17,
.C = 6000000   21,22,
CHECKBIT IS 50   

2365:  BLLTRAP:   IR←10, SETBA, GOTO RDTRAP
.MS = 61   65,61,60
.TCX = 1   43
.TXW = 1   47
.LRN = 4   69
.MC = 20   1
.B = 1041    8,12,17,
.C = 12   38,40,
CHECKBIT IS 50   
*
*  SUBROUTINE M←CONTENTS(R0)

2366:  BLODX:     MFETCH, R0←E1, .VCY, DRETURN
.MCONT = 2   6
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.TE1Y = 1   88
CHECKBIT IS 30   

2367:  BPAGEF:    SETBA, GOTO PAGEF IF R0<0
.MS = 61   65,61,60
.MC = 11   5,2
.B = 700    9,10,11,
CHECKBIT IS 30   
*
*  SUBROUTINE CONTENTS(K←K+1)←M

2370:  BSTRX:     R0←K←K+1
.RRN = 3   68,67
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LRN = 3   71,70
.LR0 = 1   58
CHECKBIT IS 87   

2371:  BSTR1:     MAP, .VCY, R0←E1, GOTO BCSTORE IF STERR
.MS = 37   65,64,63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 32   4,2,1
.VCY = 1   86
.TE1Y = 1   88
.B = 373    10,11,12,13,14,16,17,
           
2372:  STORE, RETURN
.MCONT = 2   6
.MS = 42   64,60
.MC = 1   5
CHECKBIT IS 30   

2373:  BCSTORE:   SETBA, GOTO CSTORE
.MS = 61   65,61,60
.MC = 20   1
.B = 701    9,10,11,17,
CHECKBIT IS 30   
*
*
*  STORE NAW MRG 1B6 IF FTNAT

2374:  BLL64:     M←FP, Q←R0←2B6, DGOTO BLL38
.SSP = 37   56,55,54,53,52
.TCX = 1   43
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LQX = 1   74
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 227    10,13,15,16,17,
.C = 2000000   22,
           
2375:  IR←NOT M AND NOT Q, M←NAW MRG 1B6, DGOTO BLL50
.BL = 1   81
.SSP = 35   56,54,53,52
.TCY = 1   44
.TSPY = 1   45
.TXW = 1   47
.LMY = 1   73
.LRN = 4   69
.MC = 20   1
.DGO = 1   87
.B = 67    12,13,15,16,17,
.C = 1000000   23,
CHECKBIT IS 50   
            
2376:  R0←R0+1, GOTO 157B
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.B = 157    11,12,14,15,16,17,

2377:          SETBA, CALL 1776B       * ILLEGAL MICROINSTRUCTION
.MS = 61       60,61,65,
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16
CHECKBIT IS 30

