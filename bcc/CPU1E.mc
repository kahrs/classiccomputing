*
*
           ORG  0;
*
*
*  START OF THE CPU MICROCODE..
           
0:  GOTO RSET1, E1
.MC = 1   5
.TE1Y = 1   88
.B = 1106    8,11,15,16,
CHECKBIT IS 30   
           
1:  P←Z←0, GOTO NX2
.TXW = 1   47
.LZX = 1   76
.LRN = 1   71
.MC = 20   1
.B = 35    13,14,15,17,
CHECKBIT IS 30   
*
*
*
*  THE FOLLOWING ACTIONS IMPLEMENT REL (TAG=7)
*    ADDRESSING MODE.  TABLE BASEREL IS INDEXED
*    EITHER WITH W[10,11] OR IAW[6,8].  TABLE
*    BASEREL1 IS INDEXED WITH W[10,12] OR IAW[7,9].
*    NOTE THAT BBASEREL = ABASEREL LCY1
*    Z CONTAINS A MASK FOR THE CONSTANT DISPLACEMENT

2:  XIANREL:   Z←R0, R0←Z
.BR = 14   83,82
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
CHECKBIT IS 87   
           
3:  Z←IR, DGOTO Z
.MCONT = 3   7,6
.RRN = 4   66
.BR = 14   83,82
.THY = 1   46
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
           
4:  Z←Q+Z, Q←R0, DGOTO ENDA1
.BR = 14   83,82
.BL = 12   80,78
.THY = 1   46
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.B = 337    10,11,13,14,15,16,17,
CHECKBIT IS 30   

5:  XIIANREL:  Z←R0, R0←Z
.BR = 14   83,82
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
CHECKBIT IS 87   
           
6:  Z←IR, DGOTO Z
.MCONT = 3   7,6
.RRN = 4   66
.BR = 14   83,82
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
           
7:  Z←Q+Z, Q←R0, DGOTO IA
.BR = 14   83,82
.BL = 12   80,78
.THY = 1   46
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.VCY = 1   86
.B = 101    11,17,

10:  ABASEREL:  Z←LR, Q←Z, DGOTO ENDA1
.BR = 14   83,82
.SSP = 7   56,55,54
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
           
11:  Z←S, Q←Z, DGOTO ENDA1
.RRN = 2   67
.BR = 14   83,82
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
           
12:  Z←S, Q←Z, DGOTO IA
.RRN = 2   67
.BR = 14   83,82
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 101    11,17,
CHECKBIT IS 30   
           
13:  Q←NOT M MRG 77774000B, Z←IR, SETA, DGOTO ENDA IF NM[13]
.RRN = 4   66
.BL = 3   81,80
.MS = 30   62,61
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 40   0
.DGO = 1   87
.B = 336    10,11,13,14,15,16,
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
CHECKBIT IS 50   
           
14:  Q←LR, GOTO XIANREL
.SSP = 7   56,55,54
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.B = 2    16,
           
15:  Q←S, GOTO XIANREL
.RRN = 2   67
.THY = 1   46
.LQY = 1   75
.MC = 20   1
.B = 2    16,
           
16:  Q←S, GOTO XIIANREL
.RRN = 2   67
.THY = 1   46
.LQY = 1   75
.MC = 1   5
.B = 5    15,17,
CHECKBIT IS 30   
           
17:  Q←NOT M MRG 77774000B, Z←IR, SETA, DGOTO ENDA IF NM[13]
.RRN = 4   66
.BL = 3   81,80
.MS = 30   62,61
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 40   0
.DGO = 1   87
.B = 336    10,11,13,14,15,16,
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
CHECKBIT IS 50   

20:  BBASEREL:  R0←M AND Q+Z, M←R6, RETURN IF C
.MCONT = 2   6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 10   78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
21:  S←R0←M AND Q+Z, GOTO IA
.BR = 14   83,82
.BL = 10   78
.TXW = 1   47
.TAX = 1   49
.LRN = 2   70
.LR0 = 1   58
.MC = 20   1
.VCY = 1   86
.B = 101    11,17,
           
22:  R0←M AND Q+Z, M←R6, RETURN IF C
.MCONT = 2   6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 10   78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
23:  R0←M OR NOT Q+Z,M←R6,RETURN IF C
.MCONT = 2   6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 15   81,79,78
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
24:  S←R0←M AND Q+Z
.BR = 14   83,82
.BL = 10   78
.TXW = 1   47
.TAX = 1   49
.LRN = 2   70
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
25:  S←R0←M OR NOT Q+Z
.BR = 14   83,82
.BL = 15   81,79,78
.TXW = 1   47
.TAX = 1   49
.LRN = 2   70
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
26:  Q←R0← NOT Q+Z, Z←77774B3, GOTO IMX1 IF M[13]
.BR = 14   83,82
.BL = 5   81,79
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQX = 1   74
.LZY = 1   77
.LR0 = 1   58
.MC = 75   5,3,2,1,0
.VCY = 1   86
.B = 30    13,14,
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
           
27:  Q←R0← NOT Q, Z←77774000B, DGOTO ENDA IF M[13]
.BL = 5   81,79
.TCY = 1   44
.TXW = 1   47
.LQX = 1   74
.LZY = 1   77
.LR0 = 1   58
.MC = 75   5,3,2,1,0
.DGO = 1   87
.B = 336    10,11,13,14,15,16,
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
CHECKBIT IS 50   

30:  IMX1:      R0←Q+Z, GOTO ENDA
.BR = 14   83,82
.BL = 12   80,78
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.MC = 1   5
.VCY = 1   86
.B = 336    10,11,13,14,15,16,
*
*  START OF THE MAIN INSTRUCTION LOOP:
*    SOME INSTRUCTIONS INCLUDE NX1 AND RETURN TO NX2. JUMPS
*    RETURN TO NX3, LOADS RETURNS TO NX3.

31:  NX4:       R0←P+1, GOTO T940 IF INTRPT2
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58
.MC = 65   5,3,1,0
.B = 1056    8,12,14,15,16,
CHECKBIT IS 30   

32:  NX6:       Q←7B7, MAP, DGOTO NX3
.MS = 37   65,64,63,62,61
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 36    13,14,15,16,
.C = -10000000   18,19,20,
CHECKBIT IS 50   
           
33:  R0←E1, TAKE, GOTO PAGEF IF Y<0
.MS = 75   65,63,62,61,60
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   

34:  NX1:       Z←P←P+1, GOTO NY2 IF M940
.RRN = 1   68
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
.MC = 43   5,4,0
.B = 1153    8,11,12,14,16,17,

35:  NX2:   R6←M←NI, Q←7B7, SABCDE0, GOTO NX4 IF INTRPT, .TSPY←0
.SSP = 50   53,51
.MS = 33   65,64,62,61
.TCX = 1   43
.TYW = 1   48
.LMY = 1   73
.LQX = 1   74
.LRN = 6   70,69
.MC = 41   5,0
.B = 31    13,14,17,
.C = -10000000   18,19,20,
CHECKBIT IS 50

36:  NX3:       IR←XR, DGOTO M AND Q LCY 4 MRG BASEAD
.MCONT = 3   7,6
.BL = 10   78
.SSP = 6   55,54
.MS = 4   63
.TCX = 1   43
.TSPY = 1   45
.TYW = 1   48
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.C = 40   36,
           
37:  Q←ILIM←37777B, S←P, GOTO POP IF M[9]
.RRN = 1   68
.SSP = 20   52
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 2   70
.LSPX = 1   59
.MC = 71   5,2,1,0
.B = 353    10,11,12,14,16,17,
.C = 37777   28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
*  DISPATCH ON TAG
*    M CONTAINS W, S CONTAINS THE SOURCE OF W. Q=37777B, ILIM=0
*

40:  BASEAD:    Z←GR, DGOTO ENDA1
.SSP = 10   53
.TSPY = 1   45
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
CHECKBIT IS 30   
           
41:  R0←M AND Q+Z, RETURN IF C
.MCONT = 2   6
.BR = 14   83,82
.BL = 10   78
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
42:  Z←GR, DGOTO IA
.SSP = 10   53
.TSPY = 1   45
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 101    11,17,
           
43:  S←R0←M AND Q+Z
.BR = 14   83,82
.BL = 10   78
.TXW = 1   47
.TAX = 1   49
.LRN = 2   70
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
44:  Z←IR, DGOTO ENDA1
.RRN = 4   66
.THY = 1   46
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
CHECKBIT IS 30   
           
45:  R0←M AND Q+Z, RETURN IF C
.MCONT = 2   6
.BR = 14   83,82
.BL = 10   78
.TXW = 1   47
.TAX = 1   49
.LR0 = 1   58
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
46:  M←M LCY 16, Q←77B, DGOTO BX1
.BL = 14   79,78
.MS = 7   65,64,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 321    10,11,13,17,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 50   
           
47:  Z←M AND Q, Q←37B, GOTO BXIR IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 22   4,1
.B = 334    10,11,13,14,15,
.C = 37   37,38,39,40,41,
CHECKBIT IS 50   
           
50:  Q←377B, DGOTO PD1
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 310    10,11,14,
.C = 377   34,35,36,37,38,39,40,41,
           
51:  Z←M AND Q, Q←177B, GOTO PDIR IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 22   4,1
.B = 237    10,13,14,15,16,17,
.C = 177   35,36,37,38,39,40,41,
           
52:  Q←377B, SETA, DGOTO PD1
.MS = 30   62,61
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 310    10,11,14,
.C = 377   34,35,36,37,38,39,40,41,
           
53:  Z←M AND Q, Q←177B, GOTO PDIR IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 22   4,1
.B = 237    10,13,14,15,16,17,
.C = 177   35,36,37,38,39,40,41,
           
54:  Q←377B, DGOTO BXD1
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 126    11,13,15,16,
.C = 377   34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
55:  Z←M AND Q, Q←177B, GOTO BXDIR IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 22   4,1
.B = 177    11,12,13,14,15,16,17,
.C = 177   35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
56:  DGOTO Q←M AND Q LCY 12 MRG ABASEREL
.MCONT = 3   7,6
.BL = 10   78
.MS = 6   64,63
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.C = 10   38,
CHECKBIT IS 50   
           
57:  DGOTO Q LCY 1, Z←3777B
.MCONT = 3   7,6
.BL = 12   80,78
.MS = 1   65
.TCY = 1   44
.LZY = 1   77
.MC = 20   1
.DGO = 1   87
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
*
*  THE FOLLOWING TABLE IS INDEXED WITH THE TAG FIELD OF
*  AN IAW.  (IAW[2,4])
*  R0 AND Z ARE INITIALIZED FOR THE REL (7) ACTION.
*  ROIAD IS USED BY TRAP ROIA

60:  IAN:       Q←77B6, DGOTO ENDA1
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
.C = -1000000   18,19,20,21,22,23,
           
61:  R0←M AND NOT Q, RETURN IF C, M←R6
.MCONT = 2   6
.RRN = 6   67,66
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.MC = 35   5,3,2,1
CHECKBIT IS 30   
           
62:  Q←77B6, DGOTO IA
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 101    11,17,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
           
63:  S←R0←M AND NOT Q
.BL = 4   79
.TXW = 1   47
.LRN = 2   70
.LR0 = 1   58
CHECKBIT IS 87   
           
64:  Z←IR, Q←77B6, DGOTO ENDA1
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
           
65:  R0←M AND NOT Q+Z, RETURN IF C, M←R6
.MCONT = 2   6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 4   79
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
66:  M←M LCY 16, Q←77B, DGOTO BX1
.BL = 14   79,78
.MS = 7   65,64,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 321    10,11,13,17,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 50   
           
67:  Z←M AND Q, Q←37B, GOTO BXIR IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 22   4,1
.B = 334    10,11,13,14,15,
.C = 37   37,38,39,40,41,
CHECKBIT IS 50   
           
70:  Q←77B6, SETD, DGOTO ENDA1
.MS = 36   64,63,62,61
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
.C = -1000000   18,19,20,21,22,23,
           
71:  ROIAD←R0←M AND NOT Q, M←R6, RETURN IF C
.MCONT = 2   6
.RRN = 6   67,66
.BL = 4   79
.SSP = 16   55,54,53
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.LSPX = 1   59
.MC = 35   5,3,2,1
CHECKBIT IS 30   
           
72:  Z←IR, Q←77B6, SETD, DGOTO ENDA1
.RRN = 4   66
.MS = 36   64,63,62,61
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 337    10,11,13,14,15,16,17,
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 50   
           
73:  ROIAD←R0←M AND NOT Q+Z, M←R6, RETURN IF C
.MCONT = 2   6
.RRN = 6   67,66
.BR = 14   83,82
.BL = 4   79
.SSP = 16   55,54,53
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LMY = 1   73
.LR0 = 1   58
.LSPX = 1   59
.MC = 35   5,3,2,1
.VCY = 1   86
CHECKBIT IS 30   
           
74:  Q←377B, DGOTO BXD1
.TCX = 1   43
.LQX = 1   74
.MC = 1   5
.DGO = 1   87
.B = 126    11,13,15,16,
.C = 377   34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
75:  Z←M AND Q, Q←177B, GOTO BXDIR IF LB=0
.BL = 10   78
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.MC = 22   4,1
.B = 177    11,12,13,14,15,16,17,
.C = 177   35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
76:  M←M LCY 8, Q←60000003B
.BL = 14   79,78
.MS = 5   65,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.C = -17777775   18,19,40,41,
           
77:  DGOTO M←M AND Q LCY 1 MRG ABASEREL
.MCONT = 3   7,6
.BL = 10   78
.MS = 1   65
.TCX = 1   43
.LMX = 1   72
.MC = 20   1
.DGO = 1   87
.C = 10   38,
           
100:  M←R0, DGOTO R0←M AND Q LCY 1 MRG BBASEREL
.MCONT = 3   7,6
.BL = 10   78
.MS = 1   65
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.C = 20   37,
CHECKBIT IS 50   
*
*  START OF THE PROCEDURE IA().
*    THE PROCEDURE CAN BE CALLED BY THE MICRO INSTRUCTION:
*      SETC, CALL IASUB, ILIM←0
*    M HAS TO CONTAIN THE IAW. S=SOURCE OF IAW
*

101:  IA:        MAPSS,.VCY, R0←E1, GOTO PAGEF IF Y<0
.MS = 51   65,62,60
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   

102:  IA1:       FETCH
.MS = 44   63,60
CHECKBIT IS 87   

103:  IASUB:     Z←ILIM, Q←2B6
.SSP = 20   52
.TCX = 1   43
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.C = 2000000   22,
CHECKBIT IS 87   
           
104:  Z←ILIM←X←Z+Q, Q←6B7
.BR = 14   83,82
.BL = 12   80,78
.SSP = 20   52
.TCY = 1   44
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.LSPX = 1   59
.VCY = 1   86
.C = -20000000   18,19,
           
105:  DGOTO M AND Q LCY 3 MRG *+3 IF Z>=0
.MCONT = 3   7,6
.BL = 10   78
.MS = 3   65,64
.TCX = 1   43
.MC = 5   5,3
.DGO = 1   87
.C = 110   35,38,
           
106:  Q←NOT M LCY 2 MRG 7777777B
.BL = 3   81,80
.MS = 2   64
.TCX = 1   43
.LQX = 1   74
.C = 7777777   21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
107:  IR←X←11, Q←S, GOTO FIXTRP
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 1046    8,12,15,16,
.C = 13   38,40,41,
CHECKBIT IS 50   
*
*  TABLE FOR THE 4 INDIRECT ADDRESSING TYPES FOLLOWS:
*

110:  BASEIA:    DGOTO NOT Q LCY4 MRG IAN
.MCONT = 3   7,6
.BL = 5   81,79
.MS = 4   63
.TCX = 1   43
.MC = 1   5
.DGO = 1   87
.C = 60   36,37,
CHECKBIT IS 50   
           
111:  R0←M, Z←37777B, GOTO IATRAP IF M[5]
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LZY = 1   77
.LR0 = 1   58
.MC = 50   2,0
.B = 370    10,11,12,13,14,
.C = 37777   28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
112:  Z←IR, Q←3777B, DGOTO IAF1
.RRN = 4   66
.TCX = 1   43
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 1   5
.DGO = 1   87
.B = 262    10,12,13,16,
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
113:  Q←M AND Q, DGOTO IAF1 IF M[13]
.BL = 10   78
.LQX = 1   74
.MC = 75   5,3,2,1,0
.DGO = 1   87
.B = 262    10,12,13,16,
CHECKBIT IS 30   
           
114:  S←R0←M, Q←74B6, DGOTO IAS1
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 2   70
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 260    10,12,13,
.C = -4000000   18,19,20,21,
CHECKBIT IS 50   
           
115:  M←M LCY 2
.BL = 14   79,78
.MS = 2   64
.LMX = 1   72
CHECKBIT IS 87   
           
116:  R0←M, Q←774B5, DGOTO IAA3 IF M[4]
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.MC = 74   3,2,1,0
.DGO = 1   87
.B = 346    10,11,12,15,16,
.C = -400000   18,19,20,21,22,23,24,
           
117:  Q←M AND NOT Q, Z←IR, GOTO IAA0 IF Y<0, .VCY
.RRN = 4   66
.BL = 4   79
.THY = 1   46
.LQX = 1   74
.LZY = 1   77
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 122    11,13,16,
*  THE LARGE ELEMENT BIT (LEB) IS 0.
           
120:  IR←R0, M←M LCY 4, DGOTO IAA1 IF NR0[2]
.BL = 14   79,78
.MS = 4   63
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LRN = 4   69
.MC = 25   5,3,1
.DGO = 1   87
.B = 123    11,13,16,17,
           
121:  R0←Q-Z, Q←3B7
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.VCY = 1   86
.C = 30000000   19,20,

122:  IAA0:      R0←Z←Z-1, DGOTO ABE IF R0<0
.BR = 14   83,82
.BL = 17   81,80,79,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 11   5,2
.DGO = 1   87
.VCY = 1   86
.B = 374    10,11,12,13,14,15,
CHECKBIT IS 30   

123:  IAA1:      S←R0←S+1, Q←NOT M AND Q LCY 3 MRG *+65, GOTO ABE IF R0<0
.RRN = 2   67
.BL = 2   80
.MS = 3   65,64
.IHR = 1   42
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 2   70
.LR0 = 1   58
.MC = 11   5,2
.B = 374    10,11,12,13,14,15,
.C = 224   34,37,39,
CHECKBIT IS 50   
           
124:  MAPSS, .VCY, R0←E1, DGOTO Q
.MCONT = 3   7,6
.BL = 12   80,78
.MS = 51   65,62,60
.TYW = 1   48
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.VCY = 1   86
.TE1Y = 1   88
           
125:  Q←Z, FETCH, GOTO PAGEF IF R0<0
.BR = 14   83,82
.MS = 44   63,60
.LQX = 1   74
.MC = 11   5,2
.B = 700    9,10,11,
CHECKBIT IS 30   
*  BXD ADDRESSING.  THE LAST INSTRUCTION IS A DUMMY

126:  BXD1:      Q←Q AND Z, Z←LR, DGOTO BXD2 IF NZ[16]
.BR = 10   82
.SSP = 7   56,55,54
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 10   2
.DGO = 1   87
.B = 127    11,13,15,16,17,
CHECKBIT IS 30   

127:  BXD2:      R0←Z+Q, Z←GR
.BR = 14   83,82
.BL = 12   80,78
.SSP = 10   53
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LR0 = 1   58
.VCY = 1   86
           
130:  MFETCH, Q←M, .VCY, R0←E1, GOTO PAGEF IF Y<0
.BL = 14   79,78
.MS = 34   63,62,61
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,

131:  BXD3:      Z←37400B
.TCX = 1   43
.LZX = 1   76
.C = 37400   28,29,30,31,32,33,
CHECKBIT IS 87   
           
132:  Q←Q AND Z
.BR = 10   82
.LQX = 1   74
CHECKBIT IS 87   
           
133:  Z←Q LCY 16
.BL = 12   80,78
.MS = 7   65,64,63
.LZX = 1   76
CHECKBIT IS 87   
           
134:  S←R0←IR, Z← NOT Z, DGOTO BXD4 IF Z[18]
.RRN = 4   66
.BR = 3   85,84
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LRN = 2   70
.LR0 = 1   58
.MC = 14   3,2
.DGO = 1   87
.B = 135    11,13,14,15,17,
CHECKBIT IS 30   

135:  BXD4:      M←IR←M+ NOT Z, Z←77B, DGOTO IA IF Z[18]
.BR = 3   85,84
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.LRN = 4   69
.MC = 14   3,2
.DGO = 1   87
.VCY = 1   86
.B = 101    11,17,
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 56   
           
136:  R0←R0
.THY = 1   46
.TYW = 1   48
.LR0 = 1   58

137:   CALL 1776B
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16,
*  MASK TABLE:
*    EXECUTING THE N-TH INSTRUCTION IN THIS TABLE
*    WILL PUT 2↑N-1 INTO Q
*
*  THE MICROSTORE ADDRESS IS 140B NOW
*                                              WIDTH IN BITS
           
140:  Q←0
.LQX = 1   74
           
141:  Q←1B
.LOC = 1   50
.TAX = 1   49
.LQX = 1   74
           
142:  Q←3B
.TCX = 1   43
.LQX = 1   74
.C = 3   40,41,
CHECKBIT IS 87   
           
143:  Q←7B
.TCX = 1   43
.LQX = 1   74
.C = 7   39,40,41,
           
144:  Q←17B
.TCX = 1   43
.LQX = 1   74
.C = 17   38,39,40,41,
CHECKBIT IS 87   
           
145:  Q←37B
.TCX = 1   43
.LQX = 1   74
.C = 37   37,38,39,40,41,
           
146:  Q←77B
.TCX = 1   43
.LQX = 1   74
.C = 77   36,37,38,39,40,41,
CHECKBIT IS 87   
           
147:  Q←177B
.TCX = 1   43
.LQX = 1   74
.C = 177   35,36,37,38,39,40,41,
           
150:  Q←377B
.TCX = 1   43
.LQX = 1   74
.C = 377   34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
151:  Q←777B
.TCX = 1   43
.LQX = 1   74
.C = 777   33,34,35,36,37,38,39,40,41,
           
152:  Q←1777B
.TCX = 1   43
.LQX = 1   74
.C = 1777   32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
153:  Q←3777B
.TCX = 1   43
.LQX = 1   74
.C = 3777   31,32,33,34,35,36,37,38,39,40,41,
           
154:  Q←Y←7777B
.TCY = 1   44
.LQY = 1   75
.C = 7777   30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
155:  Q←Y←17777B
.TCY = 1   44
.LQY = 1   75
.C = 17777   29,30,31,32,33,34,35,36,37,38,39,40,41,
           
156:  Q←Y←37777B
.TCY = 1   44
.LQY = 1   75
.C = 37777   28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
157:  Q←Y←77777B
.TCY = 1   44
.LQY = 1   75
.C = 77777   27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
160:  Q←Y←177777B
.TCY = 1   44
.LQY = 1   75
.C = 177777   26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
161:  Q←Y←377777B
.TCY = 1   44
.LQY = 1   75
.C = 377777   25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
162:  Q←Y←777777B
.TCY = 1   44
.LQY = 1   75
.C = 777777   24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
163:  Q←Y←1777777B
.TCY = 1   44
.LQY = 1   75
.C = 1777777   23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
164:  Q←Y←3777777B
.TCY = 1   44
.LQY = 1   75
.C = 3777777   22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
165:  Q←Y←7777777B
.TCY = 1   44
.LQY = 1   75
.C = 7777777   21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
166:  Q←Y←17777777B
.TCY = 1   44
.LQY = 1   75
.C = 17777777   20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
167:  Q←Y←37777777B
.TCY = 1   44
.LQY = 1   75
.C = 37777777   19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
           
170:  Q←-1
.BL = 17   81,80,79,78
.LQX = 1   74
*
*
*  ARRAY INDIRECT ADDRESSING WITH LEB=1
*    ( SEE ALSO ENDA-2 )
*

171:  IAA5:      R0←Q-Z,Q←16B4
.BR = 3   85,84
.BL = 12   80,78
.LOC = 1   50
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LR0 = 1   58
.VCY = 1   86
.C = 160000   26,27,28,
CHECKBIT IS 87   
           
172:  R0←Z←Z-1,GOTO ABE IF R0<0
.BR = 14   83,82
.BL = 17   81,80,79,78
.TXW = 1   47
.TAX = 1   49
.LZX = 1   76
.LR0 = 1   58
.MC = 11   5,2
.VCY = 1   86
.B = 374    10,11,12,13,14,15,

173:  IAA4:      M←NOT M AND Q LCY 3, R0←S, GOTO ABE IF R0<0
.RRN = 2   67
.BL = 2   80
.MS = 3   65,64
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LR0 = 1   58
.MC = 11   5,2
.B = 374    10,11,12,13,14,15,
CHECKBIT IS 30   
           
174:  MAPSS
.MS = 51   65,62,60
           
175:  DGOTO M LCY 8 MRG *+3, R0←E1
.MCONT = 3   7,6
.BL = 14   79,78
.MS = 5   65,63
.TCX = 1   43
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.TE1Y = 1   88
.C = 200   34,
           
176:  U←Q←Z, FETCH, GOTO PAGEF IF R0<0
.BR = 14   83,82
.SSP = 24   54,52
.MS = 44   63,60
.LQX = 1   74
.LSPX = 1   59
.MC = 11   5,2
.B = 700    9,10,11,
*
*  PART OF BXD CODE

177:  BXDIR:     Q←M, M←0, GOTO BXD3
.BL = 14   79,78
.LMY = 1   73
.LQX = 1   74
.MC = 20   1
.B = 131    11,13,14,17,
*
*
*  ARRAY MULTIPLIER TABLE 1.  THE TABLE IS ENTERED
*    AT BASE + NOT IAW[8,10] IF LEB=1. Z IS IR-LB
*

200:  AM1BASE:   Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
201:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
202:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
203:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
204:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
205:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
206:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
207:  Q←IR MRG 76177777B, R0←Q
.RRN = 4   66
.BL = 12   80,78
.TCY = 1   44
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LR0 = 1   58
.C = -1600001   18,19,20,21,22,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
*  DISPATCH TO MULTIPLY BY IAW[5,7]
           
210:  DGOTO NOT Q LCY 8 MRG *+8, Q←U
.MCONT = 3   7,6
.BL = 5   81,79
.SSP = 24   54,52
.MS = 5   65,63
.TCX = 1   43
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.C = 220   34,37,
           
211:  Q←R0, Z←Q LCY 3
.BL = 12   80,78
.MS = 3   65,64
.THY = 1   46
.LQY = 1   75
.LZX = 1   76
*
*  PART OF FIELD INDIRECTION CODE
*

212:  IAF6:      M←IR, R0←Z, RETURN
.MCONT = 2   6
.RRN = 4   66
.BR = 14   83,82
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LR0 = 1   58
.MC = 1   5
*
*  STRING INDIRECT ADDRESSING.  NOTE HOW CHECKING
*    FOR MAPPING ERRORS OCCURS AT IASTR.  THE PROPER
*    FIELD DESCRIPTORS ARE UNPACKED FROM A TABLE
*    INDEXED BY IAW[2,5].  (CSIZE CONCAT CPOS)
*

213:  IAS4:      FETCH, DGOTO IAS2 IF R0>=0
.MS = 44   63,60
.MC = 12   4,2
.DGO = 1   87
.B = 214    10,14,15,
CHECKBIT IS 30   

214:  IAS2:      SHIFT←Q, Z←7617400B
.BL = 12   80,78
.SSP = 13   56,55,53
.TCY = 1   44
.LZY = 1   77
.LSPX = 1   59
.C = 7617400   21,22,23,24,25,29,30,31,32,33,
CHECKBIT IS 87   
           
215:  Q←Q AND Z
.BR = 10   82
.LQX = 1   74
CHECKBIT IS 87   
           
216:  DGOTO Q LCY 8 MRG BASEMSK
.MCONT = 3   7,6
.BL = 12   80,78
.MS = 5   65,63
.TCX = 1   43
.MC = 1   5
.DGO = 1   87
.C = 140   35,36,
           
217:  Z←Q LCY 16, DGOTO IAS3
.BL = 12   80,78
.MS = 7   65,64,63
.LZX = 1   76
.MC = 20   1
.DGO = 1   87
.B = 232    10,13,14,16,
CHECKBIT IS 30   
*
*  ARRAY MULTIPLIER TABLE 2.  THE TABLE IS ENTERED
*    AT BASE + NOT IAW[5,7] IF LEB=1 OTHERWISE
*    AT BASE + NOT IAW[5,6] +4. Z IS IR-LB
*

220:  AM2BASE:   Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
221:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
222:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
223:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86

224:  AM3BASE:   Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
225:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
226:  Q←Z+Q
.BR = 14   83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.VCY = 1   86
           
227:  Z←IR,IR←Q
.RRN = 4   66
.BL = 12   80,78
.THY = 1   46
.TXW = 1   47
.LZY = 1   77
.LRN = 4   69
           
230:  Q←NOT M LCY 2 MRG 4000177B, R0←R6, DGOTO BASEIA
.RRN = 6   67,66
.BL = 3   81,80
.MS = 2   64
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 1   5
.DGO = 1   87
.B = 110    11,14,
.C = 4000177   21,35,36,37,38,39,40,41,
CHECKBIT IS 50   
           
231:  GOTO IATRP IF Z[3]#R0=LAX
.MC = 55   5,3,2,0
.B = 371    10,11,12,13,14,17,
CHECKBIT IS 30   
*
*  CONCLUSION OF STRING INDIRECT ADDRESSING.
*

232:  IAS3:      M←M LCL Z
.BL = 14   79,78
.MS = 11   65,62
.LMX = 1   72
           
233:  M←WORD←M LCH Z, DGOTO IAF3
.BL = 14   79,78
.SSP = 14   54,53
.MS = 12   64,62
.LMX = 1   72
.LSPX = 1   59
.MC = 20   1
.DGO = 1   87
.B = 277    10,12,13,14,15,16,17,
           
234:  IR←M AND Q, M←R6
.RRN = 6   67,66
.BL = 10   78
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 4   69

235:  IASTR:     GOTO PAGEF IF R0<0
.MC = 11   5,2
.B = 700    9,10,11,
           
236:  GOTO TI
.MC = 1   5
.B = 1400    8,9,
*  PART OF PD CODE
*

237:  PDIR:      Q←M, M←IR, GOTO PD3
.RRN = 4   66
.BL = 14   79,78
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 20   1
.B = 313    10,11,14,16,17,
CHECKBIT IS 30   
*
*  TABLE TO DESCRIBE STRING CHARACTERS.  THE TABLE
*    CONSISTS OF 16 ENTRIES EACH WITH 3, 8 BITS WIDE FIELDS.
*                             CSIZE,CPOS    SIZE/POS/SHIFT

240:  BASECHAR:  Q←1403022B
.TCX = 1   43
.LQX = 1   74
.C = 1403022   23,24,31,32,37,40,
CHECKBIT IS 87   
           
241:  Q←1406014B
.TCX = 1   43
.LQX = 1   74
.C = 1406014   23,24,30,31,38,39,
CHECKBIT IS 87   
           
242:  Q←1411006B
.TCX = 1   43
.LQX = 1   74
.C = 1411006   23,24,29,32,39,40,
CHECKBIT IS 87   
           
243:  Q←1400000B
.TCX = 1   43
.LQX = 1   74
.C = 1400000   23,24,
CHECKBIT IS 87   
           
244:  Q←2004020B
.TCX = 1   43
.LQX = 1   74
.C = 2004020   22,30,37,
           
245:  Q←2010010B
.TCX = 1   43
.LQX = 1   74
.C = 2010010   22,29,38,
           
246:  Q←2000000B
.TCX = 1   43
.LQX = 1   74
.C = 2000000   22,
           
247:  GOTO IASTR
.MC = 20   1
.B = 235    10,13,14,15,17,
CHECKBIT IS 30   
           
250:  Q←3006014B
.TCX = 1   43
.LQX = 1   74
.C = 3006014   22,23,30,31,38,39,
CHECKBIT IS 87   
           
251:  Q←3000000B
.TCX = 1   43
.LQX = 1   74
.C = 3000000   22,23,
CHECKBIT IS 87   
           
252:  GOTO IASTR
.MC = 1   5
.B = 235    10,13,14,15,17,
CHECKBIT IS 30   
           
253:  GOTO IASTR
.MC = 20   1
.B = 235    10,13,14,15,17,
CHECKBIT IS 30   
           
254:  Q←6000000B
.TCX = 1   43
.LQX = 1   74
.C = 6000000   21,22,
CHECKBIT IS 87   
           
255:  GOTO IASTR
.MC = 20   1
.B = 235    10,13,14,15,17,
CHECKBIT IS 30   
           
256:  GOTO IASTR
.MC = 1   5
.B = 235    10,13,14,15,17,
CHECKBIT IS 30   
           
257:  GOTO IASTR
.MC = 20   1
.B = 235    10,13,14,15,17,
CHECKBIT IS 30   
*
*  STRING INDIRECT ADDRESSING (SEE ALSO IAS4)
*

260:  IAS1:      MAP, DGOTO IAS4
.MS = 37   65,64,63,62,61
.MC = 1   5
.DGO = 1   87
.B = 213    10,14,16,17,
           
261:  R0←E1, DGOTO M AND Q LCY 4 MRG BASECHAR
.MCONT = 3   7,6
.BL = 10   78
.MS = 4   63
.TCX = 1   43
.TYW = 1   48
.LR0 = 1   58
.MC = 20   1
.DGO = 1   87
.TE1Y = 1   88
.C = 240   34,36,
CHECKBIT IS 50   
*
*  FIELD INDIRECT ADDRESSING
*    Z=IR, M=IAW, Q=DISP
*

262:  IAF1:      Q←S←R0←Z+Q, Z←77774000B
.BR = 14   83,82
.BL = 12   80,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LQX = 1   74
.LZY = 1   77
.LRN = 2   70
.LR0 = 1   58
.VCY = 1   86
.C = -4000   18,19,20,21,22,23,24,25,26,27,28,29,30,
           
263:  MFETCH, Q←M, .VCY, R0←E1, GOTO PAGEF IF Y<0
.BL = 14   79,78
.MS = 34   63,62,61
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
           
264:  U←Q←Q LCY 8, Z←37B
.BL = 12   80,78
.SSP = 24   54,52
.MS = 5   65,63
.TCY = 1   44
.LQX = 1   74
.LZY = 1   77
.LSPX = 1   59
.C = 37   37,38,39,40,41,
           
265:  IR←Z←Q AND Z
.BR = 10   82
.TXW = 1   47
.LZX = 1   76
.LRN = 4   69
CHECKBIT IS 87   
           
266:  Q←Q LCY 4
.BL = 12   80,78
.MS = 4   63
.LQX = 1   74
CHECKBIT IS 87   
           
267:  Q←NOT Q LCY 1 MRG 77777740B
.BL = 5   81,79
.MS = 1   65
.TCX = 1   43
.LQX = 1   74
.C = -40   18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
CHECKBIT IS 87   
           
270:  Z←NOT Q+Z, Q←24
.BR = 14   83,82
.BL = 5   81,79
.TCY = 1   44
.TAX = 1   49
.LQY = 1   75
.LZX = 1   76
.VCY = 1   86
.C = 30   37,38,
           
271:  SHIFT←IR←Q-Z, Q←IR
.RRN = 4   66
.BR = 3   85,84
.BL = 12   80,78
.SSP = 13   56,55,53
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 4   69
.LSPX = 1   59
.VCY = 1   86
CHECKBIT IS 87   
           
272:  IR, GOTO TI IF Y<0
.RRN = 4   66
.THY = 1   46
.MC = 53   5,4,2,0
.VCY = 1   86
.B = 1400    8,9,
           
273:  DGOTO Q MRG BASEMSK
.MCONT = 3   7,6
.BL = 12   80,78
.TCX = 1   43
.MC = 20   1
.DGO = 1   87
.C = 140   35,36,
           
274:  M←M LCL Z, DGOTO IAF2, IR←U
.BL = 14   79,78
.SSP = 24   54,52
.MS = 11   65,62
.TSPY = 1   45
.TYW = 1   48
.LMX = 1   72
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 275    10,12,13,14,15,17,
CHECKBIT IS 30   

275:  IAF2:      WORD←M←M LCH Z, Z←IR
.RRN = 4   66
.BL = 14   79,78
.SSP = 14   54,53
.MS = 12   64,62
.THY = 1   46
.LMX = 1   72
.LZY = 1   77
.LSPX = 1   59
           
276:  IR←M AND Q, M←R6, DGOTO IAF5 IF Z[18]
.RRN = 6   67,66
.BL = 10   78
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 4   69
.MC = 14   3,2
.DGO = 1   87
.B = 303    10,11,16,17,
CHECKBIT IS 30   

277:  IAF3:      MASK←Q, Z←S, SETB
.RRN = 2   67
.BL = 12   80,78
.SSP = 15   56,54,53
.MS = 26   64,63,61
.THY = 1   46
.LZY = 1   77
.LSPX = 1   59
CHECKBIT IS 87   
*  THE FOLLOWING IS A COPY OF THE ENDA CODE

300:  IAF4:      M←M LCY 8, Q←40000037B, GOTO IAF6 IF C
.BL = 14   79,78
.MS = 5   65,63
.TCY = 1   44
.LMX = 1   72
.LQY = 1   75
.MC = 35   5,3,2,1
.B = 212    10,14,16,
.C = -37777741   18,37,38,39,40,41,
CHECKBIT IS 50   
           
301:  DGOTO K←Q AND M LCY 2 MRG BASEINST, M←IR
.MCONT = 3   7,6
.RRN = 4   66
.BL = 10   78
.MS = 2   64
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LMY = 1   73
.LRN = 3   71,70
.MC = 20   1
.DGO = 1   87
.C = 1400   32,33,
           
302:  Q←AR, R6←Z
.BR = 14   83,82
.SSP = 1   56
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
CHECKBIT IS 87   
*  SE=1

303:  IAF5:      M←IR
.RRN = 4   66
.THY = 1   46
.LMY = 1   73
           
304:  M←M LCY 1
.BL = 14   79,78
.MS = 1   65
.LMX = 1   72
CHECKBIT IS 87   
           
305:  M←NOT Q AND M, DGOTO IAF4 IF LB=0
.BL = 4   79
.LMX = 1   72
.MC = 22   4,1
.DGO = 1   87
.B = 300    10,11,
           
306:  M←R6, M, DGOTO IAF4 IF LB#0
.RRN = 6   67,66
.BL = 14   79,78
.THY = 1   46
.LMY = 1   73
.MC = 23   5,4,1
.DGO = 1   87
.B = 300    10,11,
CHECKBIT IS 30   
           
307:  IR←NOT Q, IR, .TYW
.RRN = 4   66
.BL = 5   81,79
.THY = 1   46
.TXW = 1   47
.TYW = 1   48
.LRN = 4   69
*
*  PD OR PDI ADDRESSING MODE (A IS SET IF PDI)
*    NOTE THE TRICK AT PD4.  BIT 18 IS 0 IN -64.

310:  PD1:       Q←Z AND Q, Z←LR, DGOTO PD2 IF NZ[16]
.BR = 10   82
.SSP = 7   56,55,54
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 10   2
.DGO = 1   87
.B = 311    10,11,14,17,

311:  PD2:       R0←Z+Q,Z←GR
.BR = 14   83,82
.BL = 12   80,78
.SSP = 10   53
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LR0 = 1   58
.VCY = 1   86
           
312:  MFETCH, Q←M, .VCY, R0←E1, GOTO PAGEF IF Y<0
.BL = 14   79,78
.MS = 34   63,62,61
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,

313:  PD3:       Z←37400B
.TCX = 1   43
.LZX = 1   76
.C = 37400   28,29,30,31,32,33,
CHECKBIT IS 87   
           
314:  Q←Q AND Z
.BR = 10   82
.LQX = 1   74
CHECKBIT IS 87   
           
315:  Z←Q LCY 16
.BL = 12   80,78
.MS = 7   65,64,63
.LZX = 1   76
CHECKBIT IS 87   
           
316:  Z← NOT Z, DGOTO PD4 IF Z[18]
.BR = 3   85,84
.LZX = 1   76
.MC = 14   3,2
.DGO = 1   87
.B = 317    10,11,14,15,16,17,
CHECKBIT IS 30   

317:  PD4:       M←S←R0←M+ NOT Z, Z←77B, DGOTO ENDA IF Z[18]
.BR = 3   85,84
.BL = 14   79,78
.TCY = 1   44
.TXW = 1   47
.TAX = 1   49
.LMX = 1   72
.LZY = 1   77
.LRN = 2   70
.LR0 = 1   58
.MC = 14   3,2
.DGO = 1   87
.VCY = 1   86
.B = 336    10,11,13,14,15,16,
.C = 77   36,37,38,39,40,41,
           
320:  CLEARA, GOTO IA IF A
.MS = 50   62,60
.MC = 33   5,4,2,1
.B = 101    11,17,
CHECKBIT IS 30   
*
*  BX ADDRESSING MODE (TAG=3)
*

321:  BX1:       Q←Q AND Z, Z←LR, DGOTO BX2 IF NZ[18]
.BR = 10   82
.SSP = 7   56,55,54
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 15   5,3,2
.DGO = 1   87
.B = 322    10,11,13,16,

322:  BX2:       R0←Q+Z, Z←GR
.BR = 14   83,82
.BL = 12   80,78
.SSP = 10   53
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LR0 = 1   58
.VCY = 1   86
           
323:  Q←M LCY 8
.BL = 14   79,78
.MS = 5   65,63
.LQX = 1   74
           
324:  MFETCH, .VCY, R0←E1, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   

325:  BX3:       Z←377B
.TCX = 1   43
.LZX = 1   76
.C = 377   34,35,36,37,38,39,40,41,
CHECKBIT IS 87   
           
326:  Z←Q AND Z, Q←177B
.BR = 10   82
.TCY = 1   44
.LQY = 1   75
.LZX = 1   76
.C = 177   35,36,37,38,39,40,41,
           
327:  GOTO BXIR1 IF Z=0
.MC = 2   4
.B = 335    10,11,13,14,15,17,
           
330:  Q←Q AND Z,Z←LR, DGOTO BX4 IF NZ[16]
.BR = 10   82
.SSP = 7   56,55,54
.TSPY = 1   45
.LQX = 1   74
.LZY = 1   77
.MC = 10   2
.DGO = 1   87
.B = 331    10,11,13,14,17,
CHECKBIT IS 30   

331:  BX4:       S←R0←Q+Z, Z←GR
.BR = 14   83,82
.BL = 12   80,78
.SSP = 10   53
.TSPY = 1   45
.TXW = 1   47
.TAX = 1   49
.LZY = 1   77
.LRN = 2   70
.LR0 = 1   58
.VCY = 1   86
CHECKBIT IS 87   
           
332:  IR←M, DGOTO IA1
.BL = 14   79,78
.TXW = 1   47
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 102    11,16,
CHECKBIT IS 30   
           
333:  MAPSS, .VCY, R0←E1, GOTO PAGEF IF Y<0
.MS = 51   65,62,60
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   

334:  BXIR:      M←IR, Q←M LCY 8, GOTO BX3
.RRN = 4   66
.BL = 14   79,78
.MS = 5   65,63
.THY = 1   46
.LMY = 1   73
.LQX = 1   74
.MC = 1   5
.B = 325    10,11,13,15,17,
CHECKBIT IS 30   

335:  BXIR1:     S←R0←IR, GOTO BX4+1
.RRN = 4   66
.THY = 1   46
.TYW = 1   48
.LRN = 2   70
.LR0 = 1   58
.MC = 20   1
.B = 332    10,11,13,14,16,
*
*
*  DISPATCH ON THE INSTRUCTION CODE
*    INSTRUCTIONS RECEIVE THE FOLLOWING INFORMATION
*    IN THE REGISTERS:
*      M=OPERAND, UNLESS THE INSTRUCTION IS A STORE
*      R6=Q=Z, UNLESS THE ADDRESS IS IMMEDIATE
*      Q=A REGISTER, R0[2]=PMRO OR NOT DB
*    BIT A IS SET IF THE ADDRESS IS IMMEDIATE, BIT B IF
*    IT CONTAINS A FIELD AND BIT D IF THE ADDRESS
*    IS READ ONLY. (SEE ALSO IAF4)
*    THE SETTINGS OF A AND B ARE ALSO REFLECTED IN
*    K[0] AND K[1]. P IS SAVED IN S IF A IS SET. (SEE BRU1)
*

336:  ENDA:      M←R6, RETURN IF C
.MCONT = 2   6
.RRN = 6   67,66
.THY = 1   46
.LMY = 1   73
.MC = 35   5,3,2,1

337:  ENDA1:     Z←R6←R0, Q←77B5, MAP, GOTO ENDA3 IF STINST
.MS = 37   65,64,63,62,61
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LZY = 1   77
.LRN = 6   70,69
.MC = 54   3,2,0
.B = 343    10,11,12,16,17,
.C = 7700000   21,22,23,24,25,26,
CHECKBIT IS 50   
           
340:  R0←E1, Q←M AND Q, FETCH, GOTO PAGEF IF Y<0
.BL = 10   78
.MS = 44   63,60
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
           
341:  Q←Q LCY 8
.BL = 12   80,78
.MS = 5   65,63
.LQX = 1   74
           
342:  GOTO K←Q LCY 2 MRG BASEINST+4B7, Q←AR
.MCONT = 3   7,6
.BL = 12   80,78
.SSP = 1   56
.MS = 2   64
.TCX = 1   43
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 3   71,70
.MC = 1   5
.C = -37776400   18,32,33,
CHECKBIT IS 50   

343:  ENDA3:     Q←M AND Q LCY 8, R0←E1
.BL = 10   78
.MS = 5   65,63
.TYW = 1   48
.LQX = 1   74
.LR0 = 1   58
.TE1Y = 1   88
           
344:  DGOTO K←Q LCY 2 MRG BASEINST+2B7, Q←AR
.MCONT = 3   7,6
.BL = 12   80,78
.SSP = 1   56
.MS = 2   64
.TCX = 1   43
.TSPY = 1   45
.TXW = 1   47
.LQY = 1   75
.LRN = 3   71,70
.MC = 1   5
.DGO = 1   87
.C = 20001400   19,32,33,
           
345:  M←Z, S←P
.RRN = 1   68
.BR = 14   83,82
.THY = 1   46
.TYW = 1   48
.LMX = 1   72
.LRN = 2   70
*
*  ARRAY INDIRECT ADDRESSING WITH LEB=1
*

346:  IAA3:      Q←7776B4, IR←R0, DGOTO IAA5
.TCX = 1   43
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 171    11,12,13,14,17,
.C = -20000   18,19,20,21,22,23,24,25,26,27,28,
           
347:  Q←NOT Q AND M, S←R0←S+1, DGOTO IAA4 IF NR0[2]
.RRN = 2   67
.BL = 4   79
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LQX = 1   74
.LRN = 2   70
.LR0 = 1   58
.MC = 25   5,3,1
.DGO = 1   87
.B = 173    11,12,13,14,16,17,
CHECKBIT IS 30   
*
*
*  ENTRY TO THE MAIN INSTRUCTION LOOP AT AN ARBITRARY ADDRESS
*  ADDRESS IS IN R0,P AND Z; TO, SP AND 940 MODE ARE CHECKED
*  NOTE THAT THE FLAGS ARE PRESUMED TO BE CLEARED
*

350:  NX5:       MFETCH, R0←E1, .VCY, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
           
351:  R0+1, TAKE, DGOTO NX3
.MS = 75   65,63,62,61,60
.IHR = 1   42
.THY = 1   46
.MC = 20   1
.DGO = 1   87
.B = 36    13,14,15,16,
           
352:  R6←M, Q←7B7, SSOURCE, GOTO NX4 IF INTRPT
.BL = 14   79,78
.MS = 52   64,62,60
.TCY = 1   44
.TXW = 1   47
.LQY = 1   75
.LRN = 6   70,69
.MC = 41   5,0
.B = 31    13,14,17,
.C = -10000000   18,19,20,
CHECKBIT IS 50
*
*  POP-S EXECUTE AS A BLL $:G[0] WITH THE OPCODE IN IR.
*  THE EFFECTIVE ADDRESS WILL BE CALCULATED IN THEBLL CODE.
*  (SPEC=1, MCAL=0)
*

353:  POP:       POPW←M, Q←77B5, SETC
.BL = 14   79,78
.SSP = 24   54,52
.MS = 35   65,63,62,61
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.C = 7700000   21,22,23,24,25,26,
           
354:  M←M AND Q LCY 8, S←R0←GR
.BL = 10   78
.SSP = 10   53
.MS = 5   65,63
.TSPY = 1   45
.TYW = 1   48
.LMX = 1   72
.LRN = 2   70
.LR0 = 1   58
           
355:  IR←M LCY 1, CALL IA
.MCONT = 1   7
.BL = 14   79,78
.MS = 1   65
.TXW = 1   47
.LRN = 4   69
.MC = 20   1
.B = 101    11,17,
           
356:  R6←R0, Z←0, GOTO TI IF A
.THY = 1   46
.TYW = 1   48
.LZX = 1   76
.LRN = 6   70,69
.MC = 33   5,4,2,1
.B = 1400    8,9,
           
357:  MFETCH, .VCY, R0←E1, GOTO PAGEF IF Y<0
.MS = 34   63,62,61
.TYW = 1   48
.LR0 = 1   58
.MC = 53   5,4,2,0
.VCY = 1   86
.TE1Y = 1   88
.B = 700    9,10,11,
CHECKBIT IS 30   
           
360:  SETBB, Q←GR, DGOTO BLL1
.SSP = 10   53
.MS = 60   61,60
.TSPY = 1   45
.LQY = 1   75
.MC = 1   5
.DGO = 1   87
.B = 12    14,16,
           
361:  NEWG←Q, Q←77B6
.BL = 12   80,78
.SSP = 25   56,54,52
.TCY = 1   44
.LQY = 1   75
.LSPX = 1   59
.C = -1000000   18,19,20,21,22,23,
CHECKBIT IS 87   
*
*  IF THERE IS ANYTHING FISHY ABOUT A STORE (PRECISELY
*    IF R0[2]=1 OR A=1 OR B=1 OR D=1), THE FOLLOWING
*    CODE IS ENTERED:
*

362:  STRFLD:    Q←MASK, GOTO TI IF A
.SSP = 15   56,54,53
.TSPY = 1   45
.LQY = 1   75
.MC = 33   5,4,2,1
.B = 1400    8,9,
           
363:  M←WORD, Z←M AND Q
.BL = 10   78
.SSP = 14   54,53
.TSPY = 1   45
.LMY = 1   73
.LZX = 1   76
CHECKBIT IS 87   
           
364:  Z←SHIFT, M←M AND NOT Q ! Z, GOTO ROIA IF D
.BR = 14   83,82
.BL = 4   79
.SSP = 13   56,55,53
.TSPY = 1   45
.LMX = 1   72
.LZY = 1   77
.MC = 47   5,4,3,0
.B = 372    10,11,12,13,14,16,
           
365:  M←M LCL Z, R0, GOTO CSTORE IF STERR, .VCY
.BL = 14   79,78
.MS = 11   65,62
.THY = 1   46
.LMX = 1   72
.MC = 32   4,2,1
.VCY = 1   86
.B = 701    9,10,11,17,
CHECKBIT IS 30

366:  M←M LCH Z, DGOTO NX2
.BL = 14   79,78
.MS = 12   64,62
.LMX = 1   72
.MC = 1   5
.DGO = 1   87
.B = 35    13,14,15,17,
           
367:  STORE, Z←P←P+1
.RRN = 1   68
.MS = 42   64,60
.IHR = 1   42
.THY = 1   46
.TYW = 1   48
.LZY = 1   77
.LRN = 1   71
CHECKBIT IS 87   
*
*  VARIOUS TRAPS IN THE ADDRESSING MECHANISM.
*    IR←TRAP NUMBER, Q←PARAMETER FOR THE TRAP.
*

370:  IATRAP:    IR←X←5, Q←S, GOTO RDTRAP
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 1   5
.B = 1041    8,12,17,
.C = 5   39,41,
CHECKBIT IS 50   

371:  IATRP:     IR←X←5, Q←S, GOTO ABE1
.RRN = 2   67
.TCX = 1   43
.THY = 1   46
.TXW = 1   47
.LQY = 1   75
.LRN = 4   69
.MC = 20   1
.B = 375    10,11,12,13,14,15,17,
.C = 5   39,41,
CHECKBIT IS 50   

372:  ROIA:      IR←X←4, DGOTO RDTRAP
.TCX = 1   43
.TXW = 1   47
.LRN = 4   69
.MC = 1   5
.DGO = 1   87
.B = 1041    8,12,17,
.C = 4   39,
           
373:  Q←ROIAD
.SSP = 16   55,54,53
.TSPY = 1   45
.LQY = 1   75

374:  ABE:       IR←X←1, Q←S
.RRN = 2   67
.LOC = 1   50
.THY = 1   46
.TXW = 1   47
.TAX = 1   49
.LQY = 1   75
.LRN = 4   69

375:  ABE1:      Q←Q-1, GOTO RDTRAP
.BR = 17   85,84,83,82
.BL = 12   80,78
.TAX = 1   49
.LQX = 1   74
.MC = 20   1
.VCY = 1   86
.B = 1041    8,12,17,

376:
CHECKBIT IS 87

377:   CALL 1776B
.MCONT = 1     7
.MC = 20       1
.B = 1776      8,9,10,11,12,13,14,15,16,
       
