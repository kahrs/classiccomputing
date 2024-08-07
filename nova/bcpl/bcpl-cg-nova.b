// Header file for the NOVA code-generator.
// September 1977

GET "LIBHDR"

// nova operation code mnemonics
MANIFEST
$(1 // move data instructions
    F.LDA=#020000
    F.STA=#040000

    // modify memory instructions
    F.ISZ=#010000
    F.DSZ=#014000

    // jump instructions
    F.JMP=#000000
    F.JSR=#004000

    // arithmetic and logical instructions
    F.COM=#100000
    F.NEG=#100400
    F.MOV=#101000
    F.INC=#101400
    F.ADC=#102000
    F.SUB=#102400
    F.ADD=#103000
    F.AND=#103400

    // shift operations
    F.L=#000100
    F.R=#000200
    F.S=#000300

    // base values for carry
    F.Z=#000020
    F.O=#000040
    F.C=#000060

    // bit indicating no load
    F.NL=#000010

    // skip functions
    F.SKP=#000001
    F.SZC=#000002
    F.SNC=#000003
    F.SZR=#000004
    F.SNR=#000005
    F.SEZ=#000006
    F.SBN=#000007

    // indirect bit
    F.I=#002000

    // base register field
    BASE.R0=#000000
    BASE.R1=#000400
    BASE.R2=#001000
    BASE.R3=#001400

    // convenience instructions
    F.NOP=F.JMP+BASE.R1+1  // no operation. is JMP .+1
$)1

MANIFEST
// OCODE keywords
$(  S.TRUE=4; S.FALSE=5
    S.RV=8; S.FNAP=10
    S.MULT=11; S.DIV=12; S.REM=13
    S.PLUS=14; S.MINUS=15; S.QUERY=16; S.NEG=17; S.ABS=19
    S.EQ=20; S.NE=21; S.LS=22; S.GR=23; S.LE=24; S.GE=25
    S.NOT=30; S.LSHIFT=31; S.RSHIFT=32
    S.LOGAND=33; S.LOGOR=34
    S.EQV=35; S.NEQV=36; S.COND=37
    S.LP=40; S.LG=41; S.LN=42; S.LSTR=43; S.LL=44
    S.LLP=45; S.LLG=46; S.LLL=47
    S.NEEDS=48; S.SECTION=49
    S.RTAP=51; S.GOTO=52; S.FOR=56
    S.RETURN=67; S.FINISH=68
    S.SWITCHON=70
    S.GLOBAL=76; S.LOCAL=77;
    S.SP=80; S.SG=81; S.SL=82; S.STIND=83
    S.JUMP=85; S.JT=86; S.JF=87
    S.ENDFOR=88; S.BLAB=89;
    S.LAB=90; S.STACK=91; S.STORE=92
    S.RSTACK=93; S.ENTRY=94
    S.SAVE=95; S.FNRN=96; S.RTRN=97; S.RES=98
    S.DATALAB=100; S.ITEML=101; S.ITEMN=102
    S.ENDPROC=103
    S.DEBUG=109; S.NONE=111
    S.GETBYTE=120; S.PUTBYTE=121
    S.PLS=152; S.PGR=153; S.PLE=154; S.PGE=155
$)

MANIFEST
// selectors
$(  H1=0; H2=1; H3=2
$)

MANIFEST
// register mnemonics
$(  R0=0; R1=1; R2=2; R3=3
$)

MANIFEST
$(  K.NONE=0
    K.NUMB=1; K.LOC=2; K.GLOB=3; K.LAB=4
    K.ABS=5; K.REG=6; K.R3=7
    K.LVLOC=8; K.LVGLOB=9; K.LVLAB=10
    K.DLAB=11; K.LABI=12
$)

MANIFEST
$(  T.HUNK=1000; T.RELOC=1001; T.END=1002
$)

MANIFEST
// page 0 locations
$(  A.WORK=14
    PAGE0PTRS=240
    A.GSAVE=PAGE0PTRS+0
    A.FINISH=PAGE0PTRS+1+F.I
    A.BGET=PAGE0PTRS+2+F.I
    A.BPUT=PAGE0PTRS+3+F.I
    A.MULT=PAGE0PTRS+4+F.I
    A.DIVREM=PAGE0PTRS+5+F.I
    A.LSHIFT=PAGE0PTRS+6+F.I
    A.RSHIFT=PAGE0PTRS+7+F.I
    PAGE0CODE=88
    A.ENT=PAGE0CODE+1
    A.ENTC=PAGE0CODE+5
    A.PRFC=PAGE0CODE+7
    A.RTN=PAGE0CODE+19
    A.STKCHK=PAGE0CODE+23
    A.CONSTANTS=128
$)

MANIFEST $(
    SECTIONWORD=12345
    CGWORKMAX=10000
$)

GLOBAL
$(  // GENERAL GLOBALS
    RC          : 150
    VERSTREAM   : 152
    OCODESTREAM : 153
    CODESTREAM  : 154
    DATVEC      : 155
    OCODEFILE   : 158

    // CG OPTIONS
    CGWORKSIZE  : 160
    NAMING      : 162
    CALLCOUNTING: 163
    PROFCOUNTING: 164
    STKCHKING   : 165
    CG.A        : 168   // TYPE OF OUTPUT:
                        // =0 => TRIPOS BINARY
                        // =1 => RDOS ASSEMBLER
                        // =2 => RDOS RELOCATABLE BINARY
    CG.Y        : 175   // DEBUGGING SWITCH
    DEBUGGING   : 175

    // CG VARIABLES
    ERR.P       : 180
    ERR.L       : 181
    WORKSPACE   : 182
    SWITCHSPACE : 183
    NLABREFS    : 184
    NAMESECTION : 185
    TEMPV       : 186
    TEMPT       : 187
    ARG1        : 188
    ARG2        : 189
    SSP         : 190
    MAXSSP      : 191
    MAXGN       : 192
    PENDINGOP   : 193
    OP          : 194
    KREFV       : 195
    KREFP       : 196
    KREFT       : 197
    KCMPV       : 198
    KCMPP       : 199
    KCMPT       : 200
    DLIST       : 201
    DLISTE      : 202
    REFLIST     : 203
    REFLISTE    : 204
    FREELIST    : 205
    STV         : 206
    STVP        : 207
    DP          : 208
    PROGSIZE    : 209
    REG.K       : 210
    REG.N       : 211
    PROCSTK     : 212
    PROCSTKP    : 213
    PROCSTKT    : 214
    LABV        : 215
    LABT        : 216
    CASEK       : 217
    CASEL       : 218
    SWREG       : 219
    SKIPLAB     : 220
    PARAMNUMBER : 221
    INCODE      : 222
    SECTIONCOUNT: 223
    COUNTFLAG   : 224

    // CG PROCEDURES
    CGERROR     : 230
    INITSTACK   : 231
    INITDATALISTS:232
    FORGETALL   : 233
    CODE        : 234
    RDN         : 235
    SCAN        : 236
    DBOUTPUT    : 237
    OUTPUTSECTION:238
    LOADT       : 239
    STORET      : 240
    RDGN        : 241
    RDL         : 242
    CGSTRING    : 243
    STOREIN     : 244
    STOREI      : 245
    CGRV        : 246
    CGPENDINGOP : 247
    STORE       : 248
    CHKREFS     : 249
    GENAX       : 250
    LOSE1       : 251
    STACK       : 252
    GENJMP      : 253
    CGBRANCH    : 254
    ADDRJMP     : 255
    CGLAB       : 256
    CGENTRY     : 257
    CGSAVE      : 258
    CGAPPLY     : 259
    CGRETURN    : 260
    CGSTATICS   : 261
    MOVETOR     : 262
    EXIT        : 263
    CGSWITCH    : 264
    CGGLOBAL    : 265
    CGDATA      : 266
    GENSKIP     : 267
    ANYBUT      : 268
    GENSD       : 269
    FORGETREG   : 270
    MOVETOANYRS : 271
    FREEREG     : 272
    MOVETOANYR  : 273
    ADD         : 274
    CGMULT      : 275
    CGDIV       : 276
    CGSHIFT     : 277
    ISFREE      : 278
    REGUSEDBY   : 279
    MOVEINFO    : 280
    GENRAX      : 281
    SETINFO     : 282
    ADDRLDA     : 283
    SETKTOR     : 284
    MOVETOANYBUTR3:285
    ADDRLOC     : 286
    FORGETALLVARS:287
    FORGETVAR   : 288
    REFCONST    : 289
    ADDRGLOB    : 290
    REMEM       : 291
    REFINRANGE  : 292
    DEALWITHKREF: 293
    SETLAB      : 294
    NEXTPARAM   : 295
    CHECKSPACE  : 296
    GENSKIPRK   : 297
    GENSKIPRR   : 298
    GETBLK      : 299
    REMOVEREFSTO: 300
    GETKREF     : 301
    GETKCMP     : 302
    ADDKREF     : 303
    ADDKCMP     : 304
    INSERTCOUNT : 305
    FILLINRELREF: 306
$)


LET START(PARMS) BE
$(1 WRITES("NOVA CG (July 1979)*N")

    ERR.P, ERR.L := LEVEL(), EXIT.LABEL

    WORKSPACE, SWITCHSPACE := 0, 0
    DEBUGGING := 0
    CG.A := 1 // RDOS

//    OCODESTREAM := FINDINPUT(OCODEFILE)
//    IF OCODESTREAM=0 DO
//    $(  CGERROR("Can't open %S", OCODEFILE)
//        EXIT(20)
//    $)
    OCODESTREAM := INPUT()
    SELECTINPUT(OCODESTREAM)
    VERSTREAM := OUTPUT()

    CODESTREAM := FINDOUTPUT("c.out")

    CGWORKSIZE := CGWORKMAX
    WORKSPACE := GETVEC(CGWORKSIZE)
    IF WORKSPACE=0
    THEN
    $(  CGERROR("Can't get CG workspace")
        EXIT(20)
    $)

    SECTIONCOUNT, PROGSIZE := 0, 0

    IF DEBUGGING
    THEN CG.A := -1

    CGSECTS(WORKSPACE, CGWORKSIZE)

    WRITEF("Program size = %N words*N", PROGSIZE)

    EXIT(0)
EXIT.LABEL:
    RETURN
$)1

AND EXIT(N) BE
$(  UNLESS OCODESTREAM=0
    THEN
    $(  ENDREAD()
        OCODESTREAM := 0
    $)

//    UNLESS WORKSPACE=0
//    THEN FREEVEC(WORKSPACE)
//
//    UNLESS SWITCHSPACE=0
//    THEN FREEVEC(SWITCHSPACE)
//
//    RC := N
//    LONGJUMP(ERR.P, ERR.L)
$)

AND CGSECTS(WORKVEC, VECSIZE) BE
$(1 LET P = WORKVEC

    NAMESECTION := P; P := P+4 // Section name string

    TEMPV := P; P := P+3*100
    TEMPT := P

    MAXSSP, MAXGN := 0, 0
    INITSTACK(2)

    KREFV, KREFP := P, P; P := P+3*128
    KREFT := P

    KCMPV, KCMPP := P, P; P := P+3*32
    KCMPT := P

    INITDATALISTS()

    REG.K := P; P := P+4
    REG.N := P; P := P+4
    FORGETALL()

    PROCSTKP, PROCSTKT := 0, 2*10
    PROCSTK := P; P := P+PROCSTKT

    DP := WORKVEC+VECSIZE
    FREELIST := 0

    LABV := P; P := P+(DP-P)/10+10
    LABT := P
    PARAMNUMBER := LABT-LABV
    FOR LP = LABV TO LABT-1 DO !LP := -1
    SKIPLAB := 0
    COUNTFLAG := FALSE
    INCODE := FALSE

    STV := P
    STVP := 0

    IF (STV-DP)>0 DO
    $(  CGERROR("Insufficient workspace")
        EXIT(20)
    $)

    CODE(0, 0)
    CODE(SECTIONWORD, 0)
    OP := RDN()
    IF OP=0 DO
    $(  IF CG.A=1 DO
        $(  UNLESS CODESTREAM=0 DO SELECTOUTPUT(CODESTREAM)
            WRITES(".END*N")
            UNLESS VERSTREAM=0 DO SELECTOUTPUT(VERSTREAM)
        $)
        RETURN
    $)
    TEST OP=S.SECTION
    THEN
    $(2 LET N = RDN() // actual string length
        LET V = VEC 17 // holds up to first 7 characters
                       // of section name followed by
                       // today's date in form
                       // ' DD-MMM-YY'
        FOR I=1 TO N DO
        $(3 LET K = RDN()
            IF I<=7 DO V!I := K
        $)3
        PUTBYTE(NAMESECTION, 0, (N>7 -> 7, N))
        // section name length
        FOR I = 1 TO GETBYTE(NAMESECTION, 0) DO
        PUTBYTE(NAMESECTION, I,
            (V!I&#177)
/*/(Remove for non-ASCII machines)
            !TABLE
            '?', '?', '?', '?', '?', '?', '?', '?',
            '?', '?', '?', '?', '?', '?', '?', '?',
            '?', '?', '?', '?', '?', '?', '?', '?',
            '?', '?', '?', '?', '?', '?', '?', '?',
           '*S', '!','*"', '#', '$', '%', '&','*'',
            '(', ')','**', '+', ',', '-', '.', '/',
            '0', '1', '2', '3', '4', '5', '6', '7',
            '8', '9', ':', ';', '<', '=', '>', '?',
            '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G',
            'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
            'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
            'X', 'Y', 'Z', '[', '\', ']', '^', '_',
            '?', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
            'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
            'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
            'x', 'y', 'z', '{', '|', '}', '~', '?'
/*/
            )
        FOR I=N+1 TO 8 DO V!I := #40 // ASCII space
//        FOR I = 9 TO 17 DO V!I := GETBYTE(DATVEC, I-8)
        V!0 := 17 // string length
        FOR I=0 TO 16 BY 2 DO CODE((V!I<<8)+V!(I+1), 0)
        OP := RDN()
    $)2 // section name
    ELSE NAMESECTION := 0
    SECTIONCOUNT := SECTIONCOUNT+1
    SCAN()
    IF DEBUGGING DO DBOUTPUT()
    STV!0 := STVP
    OUTPUTSECTION()
    PROGSIZE := PROGSIZE+STVP
$)1 REPEAT

LET RDN() = VALOF
// read in OCODE operator or argument
// argument may be of form Ln
$(1 LET A, SIGN = 0, '+'
    LET CH = 0
    CH := RDCH() REPEATWHILE CH='*S' \/ CH='*N' \/ CH='L'

    IF CH=ENDSTREAMCH RESULTIS 0

    IF CH='-' DO
    $(  SIGN := '-'
        CH := RDCH()
    $)

    WHILE '0'<=CH<='9' DO
    $(  A := 10*A+CH-'0'
        CH := RDCH()
    $)

    IF SIGN='-' DO A := -A
    RESULTIS A
$)1

AND RDL() = RDN()
// read in OCODE label

AND RDGN() = VALOF
// read in global number
$(1 LET G = RDN()
    IF MAXGN<G DO MAXGN := G
    // MAXGN is highest referenced global
    RESULTIS G
$)1

AND NEXTPARAM() = VALOF
// yields next available compiler generated label
$(1 PARAMNUMBER := PARAMNUMBER-1
    RESULTIS PARAMNUMBER
$)1

AND INITSTACK(N) BE
// initialise simulated stack
$(1 ARG2, ARG1 := TEMPV, TEMPV+3
    SSP := N
    PENDINGOP := S.NONE
    H1!ARG2, H2!ARG2, H3!ARG2 := K.LOC, SSP-2, SSP-2
    H1!ARG1, H2!ARG1, H3!ARG1 := K.LOC, SSP-1, SSP-1
    IF MAXSSP<SSP DO MAXSSP := SSP
$)1

AND CGERROR(N,A) BE
$(1 WRITES("*N******ERROR: ")
    WRITEF(N,A)
    NEWLINE()
$)1

AND STACK(N) BE
// move simulated stack pointer (SSP) to N
$(1 IF MAXSSP<N DO MAXSSP := N
    IF N>=SSP+4 DO
    $(  STORE(0, SSP-1)
        INITSTACK(N)
        RETURN
    $)

    WHILE N>SSP DO LOADT(K.LOC, SSP)

    UNTIL N=SSP DO
    $(  IF ARG2=TEMPV DO
        $(  TEST N=SSP-1
            THEN
            $(  SSP := N
                H1!ARG1 := H1!ARG2
                H2!ARG1 := H2!ARG2
                H3!ARG1 := SSP-1
                H1!ARG2 := K.LOC
                H2!ARG2 := SSP-2
                H3!ARG2 := SSP-2
            $)
            ELSE INITSTACK(N)
            RETURN
        $)

        ARG1, ARG2 := ARG1-3, ARG2-3
        SSP := SSP-1
    $)
$)1

AND STORE(A, B) BE
// store simulated stack items involving registers first
// then store remaining items on second scan
$(1 FOR P = TEMPV TO ARG1 BY 3 DO
    $(  LET S=H3!P
        IF S>B BREAK
        IF S>=A & (H1!P=K.REG \/ H1!P=K.R3) DO STORET(P)
    $)
    FOR P = TEMPV TO ARG1 BY 3 DO
    $(  LET S=H3!P
        IF S>B BREAK
        IF S>=A DO STORET(P)
    $)
$)1

LET SCAN() BE
// switch on all possible OCODE operators
$(1 IF DEBUGGING DO DBOUTPUT()
    SWITCHON OP INTO
    $(2
    ERR:
    DEFAULT:    CGERROR("IN SCAN %N", OP); ENDCASE

    CASE 0:     RETURN // end of file reached

    CASE S.DEBUG:
                DEBUGGING := NOT DEBUGGING; ENDCASE

    CASE S.NEEDS: // ignore NEEDS directive
                FOR I = 1 TO RDN() DO RDN(); ENDCASE

    CASE S.LP:  LOADT(K.LOC, RDN()); ENDCASE
    CASE S.LG:  LOADT(K.GLOB, RDGN()); ENDCASE
    CASE S.LL:  LOADT(K.LAB, RDL()); ENDCASE

    CASE S.LN:  LOADT(K.NUMB, RDN()); ENDCASE

    CASE S.LSTR:CGSTRING(RDN()); ENDCASE

    CASE S.TRUE:LOADT(K.NUMB, -1); ENDCASE
    CASE S.FALSE:
                LOADT(K.NUMB, 0); ENDCASE

    CASE S.LLP: LOADT(K.LVLOC, RDN()); ENDCASE
    CASE S.LLG: LOADT(K.LVGLOB, RDGN()); ENDCASE
    CASE S.LLL: LOADT(K.LVLAB, RDL()); ENDCASE

    CASE S.SP:  STOREIN(K.LOC, RDN()); ENDCASE
    CASE S.SG:  STOREIN(K.GLOB, RDGN()); ENDCASE
    CASE S.SL:  STOREIN(K.LAB, RDL()); ENDCASE
    CASE S.STIND:
                STOREI(); ENDCASE

    CASE S.RV:  CGRV(); ENDCASE

    CASE S.GETBYTE:
                CGPENDINGOP()
                STORE(0, SSP-1)
                CHKREFS(2)
                GENAX(F.JSR, A.BGET, R0)
                CODE(SSP-2, 0)
                FORGETALL()
                LOSE1(R0)
                ENDCASE

    CASE S.PUTBYTE:
                CGPENDINGOP()
                STORE(0, SSP-1)
                CHKREFS(2)
                GENAX(F.JSR, A.BPUT, R0)
                CODE(SSP-3, 0)
                FORGETALL()
                IF MAXSSP>SSP+2 DO MAXSSP := SSP+2
                STACK(SSP-3)
                ENDCASE

    CASE S.MULT:  CASE S.DIV:  CASE S.REM:
    CASE S.PLUS:  CASE S.MINUS:  CASE S.NEG:  CASE S.ABS:
    CASE S.EQ:  CASE S.NE:  CASE S.LS:
    CASE S.GR:  CASE S.LE:  CASE S.GE:
    CASE S.PLS: CASE S.PGE: CASE S.PGR: CASE S.PLE:
    CASE S.LSHIFT:  CASE S.RSHIFT:
    CASE S.LOGAND:  CASE S.LOGOR:
    CASE S.EQV:  CASE S.NEQV:  CASE S.NOT:
                CGPENDINGOP()
                PENDINGOP := OP
                ENDCASE

    CASE S.JUMP:CGPENDINGOP()
                STORE(0, SSP-1)
                GENJMP(RDL())
                ENDCASE

    CASE S.ENDFOR:
                CGPENDINGOP()
                PENDINGOP := S.LE
                OP := S.JT
                // simulate 'LE JT Ln'
    CASE S.JT:  CASE S.JF:
                CGBRANCH(OP, RDL())
                COUNTFLAG := PROFCOUNTING
                ENDCASE

    CASE S.GOTO:CGPENDINGOP()
                STORE(0, SSP-2)
                GENAX(F.JMP, ADDRJMP(ARG1), R0)
                INCODE := FALSE
                FORGETALL()
                STACK(SSP-1)
                ENDCASE

    CASE S.QUERY:
                CGPENDINGOP()
                STACK(SSP+1)
                ENDCASE

    CASE S.LAB:
                CGPENDINGOP()
                STORE(0, SSP-1)
                FORGETALL()
                CGLAB(RDL(), 20)
                INCODE := TRUE
                COUNTFLAG := PROFCOUNTING
                ENDCASE

    CASE S.BLAB:
                CGPENDINGOP()
                STORE(0, SSP-1)
                FORGETALL()
                CGLAB(RDL(), 20)
                INCODE := TRUE
                COUNTFLAG := PROFCOUNTING
                ENDCASE

    CASE S.STACK:
                CGPENDINGOP()
                STACK(RDN())
                ENDCASE

    CASE S.STORE:
                CGPENDINGOP()
                STORE(0, SSP-1)
                ENDCASE

    CASE S.ENTRY:
                CGENTRY()
                ENDCASE

    CASE S.SAVE:CGSAVE(RDN())
                PROCSTK!PROCSTKP := MAXSSP
                IF STKCHKING DO
                $(  CHKREFS(2)
                    GENAX(F.JSR, A.STKCHK, R0)
                    PROCSTK!(PROCSTKP+1) := STVP
                    CODE(0, 0)
                $)
                PROCSTKP := PROCSTKP+2
                IF PROCSTKP>=PROCSTKT GOTO ERR
                MAXSSP := SSP
                ENDCASE

    CASE S.FNAP:  CASE S.RTAP:
                CGAPPLY(OP, RDN())
                ENDCASE

    CASE S.RTRN:  CASE S.FNRN:
                CGRETURN(OP)
                ENDCASE

    CASE S.ENDPROC:
                $(3 LET N = RDN()

                    PROCSTKP := PROCSTKP-2
                    IF STKCHKING DO
                        PROCSTK!(PROCSTKP+1)!STV := MAXSSP
                    MAXSSP := PROCSTK!PROCSTKP
                    CGSTATICS()
                    ENDCASE
                $)3

    CASE S.RES: CGPENDINGOP()
                STORE(0, SSP-2)
                MOVETOR(ARG1, R0)
                GENJMP(RDL())
                FORGETALL()
                STACK(SSP-1)
                ENDCASE

    CASE S.RSTACK:
                INITSTACK(RDN())
                LOADT(K.REG, R0)
                ENDCASE

    CASE S.FINISH:
                CGPENDINGOP()
                GENAX(F.JMP, A.FINISH, R0)
                INCODE := FALSE
                FORGETALL()
                ENDCASE

    CASE S.SWITCHON:
                $(  LET N = RDN()*2 - 1
                    SWITCHSPACE := GETVEC(N)
                    IF SWITCHSPACE=0
                    THEN
                    $(  CGERROR("Can't get workspace for SWITCHON")
                        EXIT(20)
                    $)
                    CGSWITCH(SWITCHSPACE, N)
                    FREEVEC(SWITCHSPACE)
                    SWITCHSPACE := 0
                    ENDCASE
                $)

    CASE S.GLOBAL:
                CGGLOBAL(RDN())
                RETURN

    CASE S.DATALAB:  CASE S.ITEML:
                CGDATA(OP, RDL()); ENDCASE
    CASE S.ITEMN:
                CGDATA(OP, RDN())
    $)2
    OP := RDN()
$)1 REPEAT

LET CGPENDINGOP() BE
// compile code to deal with any pending operator
// setting PENDINGOP to S.NONE
$(1 LET R, F, RAND1, RAND2 = -1, 0, ARG1, ARG2
    LET SW = FALSE
    SWITCHON PENDINGOP INTO
    $(2
    DEFAULT:    CGERROR("IN CGPENDINGOP %N", PENDINGOP)
    CASE S.NONE:RETURN

    // comparision is ARG2 <PENDINGOP> ARG1
    CASE S.EQ:  CASE S.NE:  CASE S.LS:
    CASE S.GR:  CASE S.LE:  CASE S.GE:
    CASE S.PLS: CASE S.PGE: CASE S.PGR: CASE S.PLE:
                GENSKIP(PENDINGOP)
                R := ANYBUT(-1)
                GENSD(F.ADC+F.SKP, R, R)
                GENSD(F.SUB, R, R)
                FORGETREG(R)
                PENDINGOP := S.NONE
                LOADT(K.REG, R)
                RETURN

    CASE S.EQV: SW := TRUE
    CASE S.NEQV:MOVETOANYRS()
                $(3 LET RS = H2!ARG1
                    LET S = 0
                    R := H2!ARG2
                    S := 4-RS-R
                    FREEREG(S)
                    MOVETOR(ARG2, R)
                    GENSD(F.MOV, R,  S)  // S := R
                    GENSD(F.AND+F.Z+F.L, RS, S)
                    // if R=0 source is -2(R & RS)!
                    GENSD(F.ADD, RS, R)
                    GENSD(F.SUB, S, R)
                    // RS XOR R = R+RS-2(R&RS)
                    IF SW DO GENSD(F.COM, R, R)
                    FORGETREG(S)
                    ENDCASE
                $)3

    CASE S.PLUS:IF H1!ARG2=K.NUMB DO
                    RAND1, RAND2 := ARG2, ARG1
                IF H1!RAND2=K.NUMB DO
                // special case of <constant>+<constant>
                $(3 LET N = H2!RAND2+H2!RAND1
                    STACK(SSP-1)
                    H1!ARG1, H2!ARG1 := K.NUMB, N
                    PENDINGOP := S.NONE
                    RETURN
                $)3
                R := MOVETOANYR(RAND2)
                IF H1!RAND1=K.NUMB DO
                $(  ADD(H2!RAND1, R, R)
                    LOSE1(R)
                    RETURN
                $)
                MOVETOANYRS()
                R := H2!ARG2
                GENSD(F.ADD, H2!ARG1, R)
                ENDCASE

    CASE S.MINUS:
                IF H1!ARG1=K.NUMB & H1!ARG2=K.NUMB DO
                // special case of <constant>-<constant>
                $(3 LET N = H2!ARG2-H2!ARG1
                    STACK(SSP-1)
                    H1!ARG1, H2!ARG1 := K.NUMB, N
                    PENDINGOP := S.NONE
                    RETURN
                $)3
                R := MOVETOANYR(ARG2)
                IF H1!ARG1=K.NUMB DO
                $(  ADD(-H2!ARG1, R, R)
                    LOSE1(R)
                    RETURN
                $)
                MOVETOANYRS()
                R := H2!ARG2
                GENSD(F.SUB, H2!ARG1, R)
                ENDCASE

    CASE S.MULT:R := CGMULT(); ENDCASE

    CASE S.REM: SW := TRUE
    CASE S.DIV: R := CGDIV(SW)
                ENDCASE

    CASE S.LOGOR:
                SW := TRUE
    CASE S.LOGAND:
                MOVETOANYRS()
                $(3 LET RS = H2!ARG1
                    R := H2!ARG2

                    IF SW DO
                    $(  GENSD(F.COM, RS, RS)
                        TEST REG.K!RS=K.NUMB
                        THEN REG.N!RS := NOT REG.N!RS
                        ELSE FORGETREG(RS)
                    $)
                    // A!B = A&~B + B
                    GENSD(F.AND, RS, R)
                    IF SW DO GENSD(F.ADC, RS, R)
                $)3
                ENDCASE

    CASE S.NEG: SW := TRUE
    CASE S.NOT: R := MOVETOANYR(ARG1)
                GENSD((SW -> F.NEG, F.COM), R, R)
                TEST REG.K!R=K.NUMB
                THEN REG.N!R := SW -> -REG.N!R,
                    NOT REG.N!R
                ELSE FORGETREG(R)
                PENDINGOP := S.NONE
                RETURN

    CASE S.ABS: R := MOVETOANYR(ARG1)
                CHKREFS(2)
                GENSD(F.NL+F.MOV+F.L+F.SZC, R, R)
                GENSD(F.NEG, R, R)
                TEST REG.K!R=K.NUMB
                THEN REG.N!R := REG.N!R<0 ->
                    -REG.N!R, REG.N!R
                ELSE FORGETREG(R)
                PENDINGOP := S.NONE
                RETURN

    CASE S.LSHIFT:
                SW := TRUE
    CASE S.RSHIFT:
                R := CGSHIFT(SW)
    $)2
    FORGETREG(R)
    LOSE1(R)
$)1

LET MOVETOANYR(A) = VALOF
// move simulated stack item A to some suitable register
$(1 LET K, N = H1!A, H2!A
    IF K=K.REG RESULTIS N
    FOR I = R0 TO R3 DO
        IF K=REG.K!I & N=REG.N!I & ISFREE(I) DO
        $(  H1!A, H2!A := K.REG, I
            RESULTIS I
        $)
    IF K=K.R3 UNLESS ISFREE(R0) \/ ISFREE(R1) DO
    $(  MOVETOR(A, R3)
        RESULTIS R3
    $)
    RESULTIS MOVETOR(A, ANYBUT(-1))
$)1

AND MOVETOANYRS() BE
// moves ARG1 and ARG2 to any registers
$(1 LET A1, A2 = ARG1, ARG2
    IF H1!A1=K.REG & H2!A1=R3 DO A1, A2 := ARG2, ARG1
    MOVETOANYBUTR3(A1)
    MOVETOANYR(A2)
$)1

AND MOVETOANYBUTR3(A) = VALOF
$(1 LET K, N = H1!A, H2!A
    LET R0FREE, R1FREE = TRUE, TRUE
    LET ITEMUSINGR0R1 = -1
    LET S = -1 // will hold the chosen register
    IF K=K.REG & N\=R3 RESULTIS N // no work to do
    FOR T = TEMPV TO ARG1 BY 3 IF H1!T=K.REG DO
    $(2 LET R = H2!T
        UNLESS R0<=R<=R1 LOOP
        IF ITEMUSINGR0R1=-1 DO ITEMUSINGR0R1 := T
        TEST R=R0
        THEN R0FREE := FALSE
        ELSE R1FREE := FALSE
    $)2
    // attempt to choose a suitable register
    IF R1FREE & REG.K!R1=K.NONE DO S := R1
    IF R0FREE & REG.K!R0=K.NONE DO S := R0
    IF N=REG.N!R1 & K=REG.K!R1 & R1FREE DO S := R1
    IF N=REG.N!R0 & K=REG.K!R0 & R0FREE DO S := R0
    IF S=-1 DO
    $(  IF R1FREE DO S := R1
        IF R0FREE DO S := R0
    $)
    UNLESS S=-1 RESULTIS MOVETOR(A, S)
    STORET(ITEMUSINGR0R1)
$)1 REPEAT

AND MOVETOR(A, R) = VALOF
// move simulated stack item A to register R
$(1 LET K, N, S = 0, 0, -1
    UNLESS REGUSEDBY(A)=R DO FREEREG(R)
    K, N := H1!A, H2!A
    IF K=K.REG
        TEST N=R
        THEN RESULTIS R
        ELSE
        $(  GENSD(F.MOV, N, R)
            MOVEINFO(N, R)
            GOTO RET
        $)

    FOR I = R3 TO R0 BY -1 DO
        IF K=REG.K!I & N=REG.N!I DO S := I

    IF K=REG.K!R & N=REG.N!R DO S := R // prefer R

    IF S>=0 DO  // found in slave
    $(  UNLESS S=R DO
        $(  GENSD(F.MOV, S, R)
            MOVEINFO(S, R)
        $)
        GOTO RET
    $)
    IF K=K.NUMB DO
    $(  SETKTOR(N, R)
        GOTO RET
    $)
    IF K=K.LVLOC DO
    $(  ADD(N-128, R2, R)
        GOTO RET
    $)
    IF K=K.LVGLOB DO
    $(2 LET D = 0
        FOR I = R0 TO R3 DO
            IF K.LVGLOB=REG.K!I DO S, D := I, REG.N!I
        IF S=-1 DO
        $(  GENRAX(F.LDA, R, A.GSAVE, R0)
            SETINFO(R, K.LVGLOB, 128)
            S, D := R, 128
        $)
        ADD(H2!A-D, S, R)
        GOTO RET
    $)2

    GENRAX(F.LDA, R, ADDRLDA(K, N), R0)
    SETINFO(R, K, N)

RET:H1!A, H2!A := K.REG, R
    RESULTIS R
$)1


LET STORET(A) BE
// stores simulated stack item A in true stack location
$(1 LET S = H3!A  // stack addr of item to be stored
    IF H1!A=K.LOC & H2!A=S RETURN
    // item already there

    IF S<=255 DO
    // item is addressable using R2
    $(2 LET R = MOVETOANYR(A)
        GENRAX(F.STA, R, (S-128)&255, R2)
        FORGETVAR(K.LOC, S)
        REMEM(R, K.LOC, S)
        H1!A, H2!A := K.LOC, S
        RETURN
    $)2

    // it is necessary to use R3
    $(3 LET T = ITEMUSING(R3)
        LET SLK, SLN = REG.K!R3, REG.N!R3
        IF T=-1 DO  // this means that R3 is free
        $(4 LET R = MOVETOANYBUTR3(A)
            // will not disturb most recent of R0, R1
            GENRAX(F.STA, R, ADDRLOC(S), R0)
            // does not change either R0 or R1
            FORGETVAR(K.LOC, S)
            REMEM(R, K.LOC, S)
            H1!A, H2!A := K.LOC, S
            RETURN
        $)4

        // T is using R3 so move it to WORK
        UNLESS H1!T=K.REG DO // T is of type K.R3
        $(  MOVETOR(T, R3) // to force H1!T = K.REG
            SLK, SLN := K.NONE, 0 // note that R3 no
                                  // longer contains
                                  // useful information
        $)
        GENRAX(F.STA, R3, A.WORK, R0)
        H1!T, H2!T := K.ABS, A.WORK

        // at this point R3 is free
        MOVETOANYBUTR3(A)
        // may call FREEREG(R0 or R1) but ok
        STORET(A)  // compiles an STA using R3
        IF H1!T=K.ABS DO
        $(  MOVETOR(T, R3)
            FORGETREG(R3)
            UNLESS (SLK=K.LOC & SLN=S) \/ SLK=K.NONE DO
                SETINFO(R3, SLK, SLN)
        $)
    $)3
$)1

AND ANYBUT(R) = VALOF
// return any free register except R
$(1 FOR I = R0 TO R1 DO
        IF R\=I & REG.K!I=K.NONE & ISFREE(I) RESULTIS I

    FOR I = R0 TO R1 IF R\=I & ISFREE(I) RESULTIS I

    IF R\=R3 & REG.K!R3=K.NONE & ISFREE(R3) RESULTIS R3
    FOR T = TEMPV TO ARG1 BY 3 DO
    $(2 LET S = REGUSEDBY(T)
        IF S=-1 \/ S=R LOOP
        TEST R=R3 & H3!T>255 & ISFREE(R3)
        THEN
        $(3 LET SLK, SLN = REG.K!R3, REG.N!R3
            GENRAX(F.STA, R3, A.WORK, R0)
            STORET(T) // will change R3 but not WORK
            GENRAX(F.LDA, R3, A.WORK, R0)
            FORGETREG(R3)
            UNLESS SLK=K.LOC & SLN=H3!T DO
                SETINFO(R3, SLK, SLN)
        $)3
        ELSE STORET(T) // will not corrupt R
        // this will free S and so
        RESULTIS S
    $)2
$)1

AND FREEREG(R) BE
$(1 LET T = ITEMUSING(R)
    IF T=-1 RETURN
    IF H1!T=K.R3 DO MOVETOANYBUTR3(T)
    STORET(T)
$)1

AND ITEMUSING(R) = VALOF
$(1 FOR P = TEMPV TO ARG1 BY 3
        IF (R=H2!P & H1!P=K.REG) \/
            (R=R3 & H1!P=K.R3) RESULTIS P
    RESULTIS -1
$)1

AND FORGETALL() BE
$(1 FOR R = R0 TO R3 DO REG.K!R, REG.N!R := K.NONE, 0
    REG.K!2, REG.N!2 := K.LVLOC, 128
$)1

AND FORGETREG(R) BE REG.K!R, REG.N!R := K.NONE, 0

AND FORGETVAR(K, N) BE  // K is K.LOC, K.GLOB or K.LAB
    FOR R = R0 TO R3
        IF REG.N!R=N & REG.K!R=K DO FORGETREG(R)

AND FORGETALLVARS() BE
// called after BCPL indirect assignment
    FOR R = R0 TO R3 DO
    $(1 LET K = REG.K!R
        IF K=K.LOC \/ K=K.GLOB \/ K=K.LAB DO
            FORGETREG(R)
    $)1

AND REMEM(R, K, N) BE IF REG.K!R=K.NONE DO
    REG.K!R, REG.N!R := K, N

AND SETINFO(R, K, N) BE
    TEST K=K.REG \/ K=K.R3 \/ K=K.ABS
    THEN FORGETREG(R)
    ELSE REG.K!R, REG.N!R := K, N

AND MOVEINFO(S, D) BE REG.K!D, REG.N!D := REG.K!S, REG.N!S


LET LOADT(K, N) BE
// load item (K, N) onto the simulated stack
$(1 CGPENDINGOP()
    ARG2 := ARG1
    ARG1 := ARG1+3
    IF ARG1=TEMPT DO
    $(  CGERROR("IN LOADT")
        EXIT(20)
    $)
    H1!ARG1, H2!ARG1, H3!ARG1 := K, N, SSP
    SSP := SSP+1
    IF MAXSSP<SSP DO MAXSSP := SSP
$)1

AND LOSE1(R) BE
// replace top two items of simulated stack
// by contents of register R
$(1 PENDINGOP := S.NONE
    STACK(SSP-1)
    H1!ARG1, H2!ARG1 := K.REG, R
$)1

AND REGUSEDBY(T) = H1!T=K.REG -> H2!T,
                   H1!T=K.R3 -> R3, -1

AND ISFREE(R) = VALOF
$(1 IF R=R2 RESULTIS FALSE
    FOR T = TEMPV TO ARG1 BY 3
        IF REGUSEDBY(T)=R RESULTIS FALSE
    RESULTIS TRUE
$)1

AND STOREI() BE
// compile indirect assignment
$(1 LET K, R = FINDOFFSET(), 0
    STORE(0, SSP-3)
    // at this point
    // EITHER R0 or R1 is free
    // OR ARG2 is in R0 or R1
    R := MOVETOANYBUTR3(ARG2)
    MOVETOR(ARG1, R3) // this will not disturb R
    GENRAX(F.STA, R, K&255, R3)
    FORGETALLVARS()
    // an indirect assignment may alter any word of store
    // - in particular the word corresponding to item
    // ARG1. reluctantly, therefore, we must not
    // remember that R3 corresponds to the value of
    // item ARG1
    STACK(SSP-2)
$)1

AND FINDOFFSET() = VALOF  // used by STOREI and CGRV
$(1 IF PENDINGOP=S.MINUS & H1!ARG1=K.NUMB DO
        PENDINGOP, H2!ARG1 := S.PLUS, -H2!ARG1
    IF PENDINGOP=S.PLUS DO
    $(2 LET K, N, A = K.NONE, 0, 0
        IF ISOFFSET(ARG2) DO
            K, N, A := H1!ARG1, H2!ARG1, H2!ARG2
        IF ISOFFSET(ARG1) DO
            K, N, A := H1!ARG2, H2!ARG2, H2!ARG1
        UNLESS K=K.NONE DO
        $(  STACK(SSP-1)
            H1!ARG1, H2!ARG1 := K, N
            PENDINGOP := S.NONE
            RESULTIS A
        $)
    $)2
    CGPENDINGOP()
    RESULTIS 0
$)1

AND STOREIN(K, N) BE
// compile assignment to a simple variable (K, N)
// the only operations that can be optimised
// are S+:=1, S-:=1
$(1 LET B = (H1!ARG1=K & H2!ARG1=N) -> 1,
        (H1!ARG2=K & H2!ARG2=N) -> 2, 0
    LET R, ADDR = 0, 0
    LET RAND1, RAND2 = ARG1, ARG2
    IF B=1 DO RAND1, RAND2 := ARG2, ARG1

    UNLESS B=0 SWITCHON PENDINGOP INTO
    $(2
    CASE S.NONE:IF B=1 DO
                $(  STACK(SSP-1)
                    RETURN
                $)  // case X := X
                ENDCASE

    CASE S.MINUS:
                IF B=1 ENDCASE  // reverse subtract!
                UNLESS H1!RAND1=K.NUMB ENDCASE
                // case X := X-k
                PENDINGOP := S.PLUS
                H2!RAND1 := -H2!RAND1

    CASE S.PLUS:IF H1!RAND1=K.NUMB THEN
                $(3 LET M = H2!RAND1
                    UNLESS -1<=M<=1 ENDCASE
                    UNLESS M=0 DO
                    $(  GENAX((M>0 -> F.ISZ, F.DSZ),
                            ADDRLDA(K, N), R0)
                        CODE(F.NOP, 0)
                        FORGETVAR(K, N)
                    $)
                    PENDINGOP := S.NONE
                    STACK(SSP-2)
                    RETURN
                $)3
    $)2

    CGPENDINGOP()
    SWITCHON K INTO // K=K.LAB, K.GLOB or K.LOC
    $(4
    CASE K.LAB: R := MOVETOANYR(ARG1)
                ADDR := REFCONST(K.LAB, N)
                ENDCASE
    CASE K.GLOB:R := MOVETOANYBUTR3(ARG1)
                ADDR := ADDRGLOB(N)
                ENDCASE
    CASE K.LOC: TEST N<=255
                THEN R := MOVETOANYR(ARG1)
                ELSE R := MOVETOANYBUTR3(ARG1)
                ADDR := ADDRLOC(N)
    $)4
    GENRAX(F.STA, R, ADDR, R0)
    FORGETVAR(K, N)
    REMEM(R, K, N)
    STACK(SSP-1)
$)1

AND ISOFFSET(A) = H1!A=K.NUMB & (-128 <= H2!A <= 127) ->
    TRUE, FALSE

AND CGLAB(N, LEN) BE
// used for LAB and DATALAB
// optimises case where there are outstanding
// references to Ln
$(1 UNLESS INCODE DO
    $(2 LET P = KREFV
        WHILE P\=KREFP & N=H3!P &
            (H2!P=K.LAB \/ H2!P=K.LABI) &
            STVP-H1!P<126 DO P := P+3
        // P does not point to a ref that will
        // be resolved by SETLAB(N)
        IF REFINRANGE(P, LEN) BREAK
        DEALWITHKREF(P)
        // this won't set SKIPLAB
    $)2 REPEAT
    SETLAB(N)
$)1

AND CGRV() BE
// make top stack item addressable by R3
$(1 LET N = FINDOFFSET()
    MOVETOR(ARG1, R3)
    H1!ARG1, H2!ARG1 := K.R3, N
$)1

AND CGMULT() = VALOF
$(1 LET R = MOVETOANYBUTR3(ARG1)
    FREEREG(R3)
    MOVETOR(ARG2, R1-R)
    GENAX(F.JSR, A.MULT, R0)
    FORGETALL()
    RESULTIS R1
$)1

AND CGDIV(ISREM) = VALOF
$(1 MOVETOR(ARG1, R0)
    MOVETOR(ARG2, R1)
    FREEREG(R3)
    MOVETOR(ARG2, R1) // incase FREEREG dumped it
    GENAX(F.JSR, A.DIVREM, R0)
    FORGETALL()
    RESULTIS ISREM -> R0, R1
$)1

AND CGSHIFT(ISLEFT) = VALOF
$(1 IF H1!ARG1=K.NUMB & H2!ARG1=1 DO
    $(2 LET R = MOVETOANYR(ARG2)
        GENSD(F.MOV+F.Z+(ISLEFT -> F.L, F.R), R, R)
        FORGETREG(R)
        RESULTIS R
    $)2
    MOVETOR(ARG1, R1)
    MOVETOR(ARG2, R0)
    FREEREG(R3)
    MOVETOR(ARG2, R0) // incase FREEREG dumped it
    GENAX(F.JSR, (ISLEFT -> A.LSHIFT, A.RSHIFT), R0)
    FORGETALL()
    RESULTIS R0
$)1


LET CGSTATICS() BE
$(1 LET D = DLIST
    UNTIL D=0 DO
    $(2 LET LEN = 0
        LET L = H1!D
        UNLESS H2!D=S.DATALAB DO CGERROR("IN CGSTATICS")
        UNTIL L=0 \/ H2!L=S.DATALAB DO
        $(  LEN := LEN+1
            L := H1!L
        $)
        CGLAB(H3!D, LEN)
        D := H1!D
        UNTIL D=0 \/ H2!D=S.DATALAB DO
        $(3 LET NEXT, K, N = H1!D, H2!D, H3!D
            TEST K=S.ITEMN
            THEN CODE(N, 0)
            ELSE CODE(0, N)
            RTNBLK(D)
            D := NEXT
        $)3
    $)2
    DLIST := 0
    DLISTE := @DLIST
$)1

AND CGGLOBAL(N) BE
$(1 CGSTATICS()
    CHKREFS(128)
    // force resolving of ALL outstanding references
    CODE(0, 0)
    FOR I = 1 TO N DO
    $(  CODE(RDGN(), 0)
        CODE(LABV!RDL(), 0)
    $)
    CODE(MAXGN, 0)
$)1

AND CGDATA(K, N) BE
$(1 LET P = GETBLK()
    H2!P, H3!P := K, N
    !DLISTE := P
    DLISTE := P
$)1

AND CGSTRING(N) BE
$(1 LET L, W = NEXTPARAM(), N<<8
    LOADT(K.LVLAB, L)
    CGDATA(S.DATALAB, L)
    $(  UNLESS N=0 DO W := W \/ RDN()
        CGDATA(S.ITEMN, W)
        IF N<=1 RETURN
        N, W := N-2, RDN()<<8
    $) REPEAT
$)1

AND ADD(N, RS, RD) BE
$(1 LET SLK, SLN = REG.K!RS, REG.N!RS
    TEST REG.K!RD=K.NUMB & REG.N!RD=N
    THEN GENSD(F.ADD, RS, RD)
    ELSE TEST N=0
    THEN
    $(  UNLESS RS=RD DO
        $(  GENSD(F.MOV, RS, RD)
            MOVEINFO(RS, RD)
        $)
    $)
    ELSE TEST N=1 \/ N=2 THEN
    $(  GENSD(F.INC, RS, RD)
        IF N=2 DO GENSD(F.INC, RD, RD)
    $)
    ELSE TEST N=-1 DO
    $(  GENSD(F.NEG, RS, RD)
        GENSD(F.COM, RD, RD)
    $)
    ELSE
    $(  TEST RS=RD
        THEN
        $(  RS := ANYBUT(RD)
            SETKTOR(N, RS)
        $)
        ELSE SETKTOR(N, RD)
        GENSD(F.ADD, RS, RD)
    $)
    FORGETREG(RD)
    IF SLK=K.NUMB \/ SLK=K.LVLOC \/ SLK=K.LVGLOB DO
        REMEM(RD, SLK, SLN+N)
$)1

AND SETKTOR(K, R) = VALOF
// load register R with constant K
$(1 IF REG.N!R=K & REG.K!R=K.NUMB RESULTIS R
    SWITCHON K INTO
    $(2
    DEFAULT:    GENRAX(F.LDA, R, REFCONST(K.NUMB, K), R0)
                ENDCASE
    CASE -2:    GENSD(F.ADC+F.Z+F.L, R, R); ENDCASE
    CASE -1:    GENSD(F.ADC, R, R); ENDCASE
    CASE 0:     GENSD(F.SUB, R, R); ENDCASE
    CASE 1:     GENSD(F.SUB+F.Z+F.L, R, R); ENDCASE
    CASE #077777:
                GENSD(F.ADC+F.Z+F.R, R, R); ENDCASE
    CASE #100000:
                GENSD(F.SUB+F.Z+F.R, R, R); ENDCASE
    $)2
    SETINFO(R, K.NUMB, K)
    RESULTIS R
$)1

AND GETBLK() = VALOF
$(1 LET P = FREELIST
    TEST P=0
    THEN
    $(  DP := DP-3
        CHECKSPACE()
        P := DP
    $)
    ELSE FREELIST := H1!FREELIST
    !P := 0
    RESULTIS P
$)1

AND RTNBLK(P) BE
$(1 !P := FREELIST
    FREELIST := P
$)1


LET CGENTRY() BE
$(1 LET N = RDN()
    LET L = RDL()
    LET V = VEC 7
    CHKREFS(20)
    V!0 := 7  // string length
    FOR I = 1 TO N DO
    $(2 LET K=RDN()
        IF I <= 7 DO V!I := K
    $)2
    FOR I = N+1 TO 7 DO V!I := #40  // ASCII space
    IF NAMING FOR I = 0 TO 6 BY 2 DO
        CODE((V!I<<8)+V!(I+1), 0)

    SETLAB(L)
    INCODE := TRUE

// entry sequence

    GENRAX(F.LDA, R1, 0, R3)
    GENSD(F.ADD, R1, R2)
    GENRAX(F.STA, R3, ADDRLOC(0), R0)
    GENAX(F.JSR, (CALLCOUNTING -> A.ENTC, A.ENT), R0)
    IF CALLCOUNTING DO
    $(  CODE(0, 0)
        CODE(0, 0)
    $)
$)1

AND CGSAVE(N) BE
$(1 FORGETALL()
    IF N>2 DO SETINFO(R0, K.LOC, 2)
    INITSTACK(N)
$)1

AND CGAPPLY(OP, K) BE
$(1 CGPENDINGOP()
    $(2 LET BASE = H3!TEMPV
        STORE(K+3, SSP-2)  // store args 2,3,...

        // now deal with non-args
        FOR T = TEMPV TO ARG2 BY 3 DO
        $(  IF H3!T>K BREAK
            IF REGUSEDBY(T)>=0 DO STORET(T)
        $)

        // deal with loading of R0
        IF K+2<=SSP-2 DO  // if argument
            TEST K+2>=BASE
            THEN MOVETOR(TEMPV+(K+2-BASE)*3, R0)
            ELSE GENRAX(F.LDA, R0, ADDRLOC(K+2), R0)
    $)2
    GENAX(F.JSR, ADDRJMP(ARG1), R0)
    CODE(K, 0)
    FORGETALL()
    STACK(K)
    IF OP=S.FNAP DO LOADT(K.REG, R0)
$)1

AND CGRETURN(OP) BE
$(1 CGPENDINGOP()
    IF OP=S.FNRN DO
    $(  MOVETOR(ARG1, R0)
        STACK(SSP-1)
    $)
    GENAX(F.JMP, A.RTN, R0)
    INCODE := FALSE
    INITSTACK(SSP)
$)1


LET CGSWITCH(V, M) BE
// compile code for SWITCHON
// N=number of cases
// D=default label
$(1 LET N = (M+1)/2
    LET D = RDL()
    LET NUMNEG = 0 // number of negative case constants
    CASEK, CASEL := V-1, V+N-1
    // vectors CASEK and CASEL will be accessed using
    // offsets 1 to N

    // sort case constants into arithmetic order
    $(2 LET NEXTNEG, NEXTPOS = 1, N
        FOR I = 1 TO N DO
        $(3 LET K = RDN()
            LET L = RDL()
            TEST K>=0
            THEN
            $(4 LET J = NEXTPOS
                UNTIL J=N DO
                $(  IF K<CASEK!(J+1) BREAK
                    CASEK!J := CASEK!(J+1)
                    CASEL!J := CASEL!(J+1)
                    J := J+1
                $)
                CASEK!J, CASEL!J := K, L
                NEXTPOS := NEXTPOS-1
            $)4
            ELSE
            $(5 LET J = NEXTNEG
                UNTIL J=1 DO
                $(  IF K>CASEK!(J-1) BREAK
                    CASEK!J := CASEK!(J-1)
                    CASEL!J := CASEL!(J-1)
                    J := J-1
                $)
                CASEK!J, CASEL!J := K, L
                NEXTNEG := NEXTNEG+1
                NUMNEG := NUMNEG+1
            $)5
        $)3
    $)2

    CGPENDINGOP()
    STORE(0, SSP-2)
    SWREG := MOVETOANYR(ARG1)
    STACK(SSP-1)

    TEST 4*N-6>CASEK!N/2-CASEK!1/2 // care with overflow
    // want to try 3 ?
    THEN LSWITCH(1, N, D)
    ELSE
    $(  UNLESS NUMNEG=0 \/ NUMNEG=N DO
        // unless all case constants of same sign
        // arrange numbers in logical order
        $(6 LET MIN = N-NUMNEG<NUMNEG -> N-NUMNEG, NUMNEG
            FOR I = 1 TO MIN IF CASEK!I<0 DO
            $(7 LET P = I-NUMNEG
                LET SAVEK, SAVEL = CASEK!I, CASEL!I
                CASEK!I := 0 // positive end marker
                $(  P := P+N
                    $(8 LET TK, TL = CASEK!P, CASEL!P
                        CASEK!P, CASEL!P := SAVEK, SAVEL
                        SAVEK, SAVEL := TK, TL
                        P := P-NUMNEG
                    $)8 REPEATWHILE P>0
                $) REPEATWHILE SAVEK<0
            $)7
        $)6
        // now produce binary chop code
        // on these logical values
        BSWITCH(1, N, D)
        GENJMP(D)
    $)
$)1

AND BSWITCH(P, Q, D) BE
// binary chop instance
    TEST Q-P>6
    THEN
    $(1 LET M = NEXTPARAM()
        LET T = (P+Q)/2
        GENSKIPRK(S.PLE, SWREG, CASEK!T)
        GENJMP(M)
        INCODE := TRUE
        BSWITCH(P, T-1, D)
        GENJMP(D)
        CGLAB(M, 0)
        INCODE := TRUE
        GENSKIPRK(S.EQ, SWREG, CASEK!T)
        GENJMP(CASEL!T)
        INCODE := TRUE
        BSWITCH(T+1, Q, D)
    $)1
    ELSE FOR I = P TO Q DO
    $(  GENSKIPRK(S.EQ, SWREG, CASEK!I)
        GENJMP(CASEL!I)
        INCODE := TRUE
    $)

AND LSWITCH(P, Q, D) BE
// label vector instance
$(1 LET L = NEXTPARAM()
    GENSKIPRK(S.GR, SWREG, CASEK!P)
    GENJMP(D)
    INCODE := TRUE
    GENSKIPRK(S.LS, SWREG, CASEK!Q)
    GENJMP(D)
    INCODE := TRUE
    $(2 LET R = SWREG=R3 -> R0, R3
        CHKREFS(4)
        GENRAX(F.LDA, R, 3, R1)
        GENSD(F.ADD, (SWREG=R3 -> R, SWREG), R3)
        // redesign ?
        GENAX(F.JMP+F.I, 0, R3)
        INCODE := FALSE
        CODE(-CASEK!P, L)
        FORGETALL()
        INCODE := FALSE
    $)2

    CGLAB(L, CASEK!Q-CASEK!P+1)

    FOR K=CASEK!P TO CASEK!Q
        TEST CASEK!P=K
        THEN
        $(  CODE(0, CASEL!P)
            P := P+1
        $)
        ELSE CODE(0, D)
$)1


LET SWAPOP(OP) = VALOF
    SWITCHON OP INTO
    $(1
    DEFAULT:    RESULTIS OP
    CASE S.GE:  RESULTIS S.LE
    CASE S.LS:  RESULTIS S.GR
    CASE S.GR:  RESULTIS S.LS
    CASE S.LE:  RESULTIS S.GE
    CASE S.PGE: RESULTIS S.PLE
    CASE S.PLS: RESULTIS S.PGR
    CASE S.PGR: RESULTIS S.PLS
    CASE S.PLE: RESULTIS S.PGE
    $)1


AND CGBRANCH(OP, L) BE
// entry after JT or JF!
$(1 LET F = 0
    LET R = 0
    LET B = OP=S.JT
    SWITCHON PENDINGOP INTO
    $(2
    DEFAULT:    CGPENDINGOP()
    CASE S.NONE:LOADT(K.NUMB, 0)
    CASE S.NE:  B := NOT B
    CASE S.EQ:  F := B -> S.EQ, S.NE
                ENDCASE
    CASE S.LS:  B := NOT B
    CASE S.GE:  F := B -> S.GE, S.LS
                ENDCASE
    CASE S.LE:  B := NOT B
    CASE S.GR:  F := B -> S.GR, S.LE
                ENDCASE
    CASE S.PLS: B := NOT B
    CASE S.PGE: F := B -> S.PGE, S.PLS
                ENDCASE
    CASE S.PLE: B := NOT B
    CASE S.PGR: F := B -> S.PGR, S.PLE
    $)2
    PENDINGOP := S.NONE

    GENSKIP(F)
    GENJMP(L)
    INCODE := TRUE
$)1

AND GENSKIP(OP) BE
// used in CGPENDINGOP and CGBRANCH
$(1 LET A1, A2 = ARG1, ARG2
// compiles code to skip instruction UNLESS A2 <OP> A1
// if either A1 or A2 is a constant it moves the other
// to a register and calls GENSKIPRK(F, R, K)
    STORE(0,SSP-3)
    IF H1!A1=K.NUMB DO
    $(2 LET T = A2
        A2 := A1
        A1 := T
        OP := SWAPOP(OP)
    $)2
    TEST H1!A2=K.NUMB
    THEN GENSKIPRK(OP, MOVETOANYR(A1), H2!A2)
    ELSE
    $(3 LET S = MOVETOANYBUTR3(A1)
        GENSKIPRR(OP, S, MOVETOANYR(A2))
    $)3
    STACK(SSP-2)
$)1

AND GENSKIPRK(OP, R, K) BE
$(1 LET F = 0
    IF K=-1 DO F := F.NL+F.Z+
        VALOF SWITCHON OP INTO
        $(2
        CASE S.EQ:  RESULTIS F.INC+F.SNR
        CASE S.NE:  RESULTIS F.INC+F.SZR
        CASE S.LS:  RESULTIS F.MOV+F.SNC+F.L
        CASE S.GE:  RESULTIS F.MOV+F.SZC+F.L
        CASE S.GR:  RESULTIS F.INC+F.SZC+F.L
        CASE S.LE:  RESULTIS F.INC+F.SNC+F.L
        CASE S.PLS: // always false
                    RESULTIS F.MOV+F.SKP
        CASE S.PGE: // always true
                    CHKREFS(2); RETURN
        CASE S.PGR: RESULTIS F.COM+F.SZR
        CASE S.PLE: RESULTIS F.COM+F.SNR
        $)2
    IF K=0 DO F := F.NL+F.Z+
        VALOF SWITCHON OP INTO
        $(3
        CASE S.EQ:  RESULTIS F.MOV+F.SNR
        CASE S.NE:  RESULTIS F.MOV+F.SZR
        CASE S.LS:  RESULTIS F.NEG+F.SZC+F.L
        CASE S.GE:  RESULTIS F.NEG+F.SNC+F.L
        CASE S.GR:  RESULTIS F.MOV+F.SZC+F.L
        CASE S.LE:  RESULTIS F.MOV+F.SNC+F.L
        CASE S.PLS: RESULTIS F.MOV+F.SZR
        CASE S.PGE: RESULTIS F.MOV+F.SNR
        CASE S.PGR: // always false
                    RESULTIS F.MOV+F.SKP
        CASE S.PLE: // always true
                    CHKREFS(2); RETURN
        $)3
    TEST F=0
    THEN
    $(4 LET S = -1
        FOR I = R0 TO R3
            IF REG.K!I=K.NUMB & REG.N!I=K DO
            $(  S := I; BREAK
            $)
        GENSKIPRR(OP, R,
            [S=-1 -> SETKTOR(K, ANYBUT[R]), S])
    $)4
    ELSE
    $(  CHKREFS(3)
        GENSD(F, R, R)
    $)
$)1


LET GENSKIPRR(OP, S, D) BE
$(1 LET F = F.NL+F.Z+
        VALOF SWITCHON OP INTO
        $(2
        CASE S.EQ:  RESULTIS F.SUB+F.SNR
        CASE S.NE:  RESULTIS F.SUB+F.SZR
        CASE S.GR:  $(3 LET T = S
                        S := D
                        D := T
                    $)3
        CASE S.LS:  RESULTIS F.SUB+F.SZC+F.L
        CASE S.LE:  $(4 LET T = S
                        S := D
                        D := T
                    $)4
        CASE S.GE:  RESULTIS F.SUB+F.SNC+F.L
        CASE S.PLS: RESULTIS F.SUB+F.SNC
        CASE S.PGE: RESULTIS F.SUB+F.SZC
        CASE S.PGR: RESULTIS F.ADC+F.SZC
        CASE S.PLE: RESULTIS F.ADC+F.SNC
        $)2

    CHKREFS(3)
    GENSD(F, S, D)
$)1

AND CODE(A, L) BE
// compile a word
$(1 UNLESS L=0 DO LABREF(L, STVP)
    STV!STVP := A
    IF DEBUGGING DO
    $(  WRITEF("CODE:%O6", A)
        UNLESS L=0 DO WRITEF(" (L%N)", L)
        NEWLINE()
    $)
    INCRSTVP()
$)1

AND GENRAX(OP, R, A, X) BE
// compile a memory reference instruction
    IF INCODE DO
    $(  IF COUNTFLAG DO INSERTCOUNT()
        CHKREFS(1)
        CODE(OP+A+(R<<11)+(X<<8), 0)
    $)

AND GENAX(OP, A, X) BE
// compile a memory modify or jump instruction
    IF INCODE DO
    $(  IF COUNTFLAG DO INSERTCOUNT()
        CHKREFS(1)
        CODE(OP+A+(X<<8), 0)
    $)

AND GENSD(OP, RS, RD) BE
// compile an arithmetic or logical insruction
    IF INCODE DO
    $(  IF COUNTFLAG DO INSERTCOUNT()
        CHKREFS(1)
        CODE(OP+(RS<<13)+(RD<<11), 0)
    $)

AND GENJMP(L) BE
$(1 GENAX(F.JMP, REFCONST(K.LAB, L), R0)
    INCODE := FALSE
$)1

AND INSERTCOUNT() BE
// produce code for PROFCOUNTING option
$(1 COUNTFLAG := FALSE
    FREEREG(R3)
    CHKREFS(3)
    GENAX(F.JSR, A.PRFC, R0)
    FORGETREG(R3)
    CODE(0, 0)
    CODE(0, 0)
$)1

AND LABREF(L, A) BE
$(1 LET P = GETBLK()
    H2!P, H3!P := L, A
    !REFLISTE := P
    REFLISTE := P
    NLABREFS := NLABREFS+1
$)1

AND INITDATALISTS()BE
$(1 REFLIST := 0
    REFLISTE := @REFLIST
    NLABREFS := 0
    DLIST := 0
    DLISTE := @DLIST
$)1

AND CHECKSPACE() BE
IF (STV+STVP-DP)>=0 DO
$(1 CGERROR("Program too large - %N words compiled", STVP)
    EXIT(20)
$)1

AND INCRSTVP() BE
$(1 STVP := STVP+1
    CHECKSPACE()
    UNLESS KCMPP=KCMPV IF H1!KCMPV+128<STVP DO
    $(  REMOVEREFSTO(H1!KCMPV, H2!KCMPV, H3!KCMPV)
        KCMPP := KCMPP-3
        FOR P = KCMPV TO KCMPP-1 DO P!0 := P!3
    $)
$)1


LET CHKREFS(N) BE
$(1 UNTIL REFINRANGE(KREFV, N) DO DEALWITHKREF(KREFV)
    IF SKIPLAB=0 RETURN
    SETLAB(SKIPLAB)
    SKIPLAB := 0
    INCODE := TRUE
$)1

AND SETLAB(N) BE
$(1 LABV!N := STVP
    $(2 LET P = GETKREF(K.LAB, N)
        IF P=-1 BREAK
        FILLINRELREF(P, STVP, 0)
    $)2 REPEAT
    $(3 LET P = GETKREF(K.LABI, N)
        IF P=-1 BREAK
        FILLINRELREF(P, STVP, 0)
    $)3 REPEAT
$)1

AND DEALWITHKREF(T) BE
$(1 LET P, K, N, IND = H1!T, H2!T, H3!T, 0
    KREFP := KREFP-3  // remove item from KREFV
    FOR Q = T TO KREFP-1 DO Q!0 := Q!3
    SWITCHON K INTO
    $(2
    CASE K.LAB: K, IND := K.DLAB, F.I
    CASE K.LABI:$(3 LET A = GETKCMP(K, N)
                    IF A>=0 DO
                    $(  FILLINRELREF(P, A, IND)
                        RETURN
                    $)
                $)3
    $)2
    // we must compile a word
    IF INCODE DO  // we must compile a jump round it
    $(  SKIPLAB := NEXTPARAM()
        ADDKREF(K.LAB, SKIPLAB)
        CODE(F.JMP+BASE.R1, 0)
        INCODE := FALSE
    $)
    FILLINRELREF(P, STVP, IND)
    ADDKCMP(K, N)
    TEST K=K.NUMB
    THEN CODE(N, 0)
    ELSE CODE((K=K.LABI -> #100000, 0), N)
$)1

AND FILLINRELREF(P, A, BITS) BE
    STV!P := STV!P+BITS+(A-P & 255)

AND REFINRANGE(P, N) = P=KREFP -> TRUE,
    (H1!P+126)>=STVP+N

AND REMOVEREFSTO(A, K, N) BE
$(1 LET P = 0  // used to hold address of
               // referencing instruction
    SWITCHON K INTO
    $(2
    CASE K.DLAB:// can be used to satisfy (K.LAB, N) refs
                $(  P := GETKREF(K.LAB, N)
                    IF P=-1 BREAK
                    FILLINRELREF(P, A, F.I)
                $) REPEAT
    CASE K.LABI:
    CASE K.NUMB:$(  P := GETKREF(K, N)
                    IF P=-1 RETURN
                    FILLINRELREF(P, A, 0)
                $) REPEAT
    $)2
$)1

AND ADDRJMP(A) = VALOF
$(1 LET K, N = H1!A, H2!A
    UNLESS INCODE RESULTIS 0
    IF COUNTFLAG DO INSERTCOUNT()
    CHKREFS(2)
    SWITCHON K INTO
    $(2
    CASE K.NUMB: CASE K.REG:
    CASE K.LVLOC: CASE K.LVGLOB: CASE K.LVLAB:
                MOVETOR(A, R3)
                CHKREFS(2)
                RESULTIS (0)+BASE.R3
    CASE K.R3:  RESULTIS F.I+(N&255)+BASE.R3
    CASE K.LOC: RESULTIS F.I+ADDRLOC(N)
    CASE K.GLOB:RESULTIS F.I+ADDRGLOB(N)
    CASE K.LAB: RESULTIS F.I+REFCONST(K.LABI, N)
    $)2
$)1

AND ADDRLDA(K, N) = VALOF
$(1 UNLESS INCODE RESULTIS 0
    IF COUNTFLAG DO INSERTCOUNT()
    CHKREFS(2)
    SWITCHON K INTO
    $(2
    CASE K.NUMB:RESULTIS REFCONST(K.NUMB, N)
    CASE K.LOC: RESULTIS ADDRLOC(N)
    CASE K.GLOB:RESULTIS ADDRGLOB(N)
    CASE K.LAB: RESULTIS REFCONST(K.LAB, N)
    CASE K.ABS: RESULTIS (N)+BASE.R0
    CASE K.R3:  RESULTIS (N&255)+BASE.R3
    CASE K.LVLAB:
                RESULTIS REFCONST(K.DLAB, N)
    DEFAULT:    CGERROR("IN ADDRLDA %N", K)
                RESULTIS 0
    $)2
$)1


LET ADDRLOC(N) = VALOF
$(1 UNLESS INCODE RESULTIS 0
    IF COUNTFLAG DO INSERTCOUNT()
    CHKREFS(2)
    FOR R = R2 TO R3  // try to use R2 or R3
        IF REG.K!R=K.LVLOC & IS8BITINT(N-REG.N!R)
            RESULTIS (N-REG.N!R & 255)+(R<<8)
    // we need to use R3
    FREEREG(R3)

    IF REG.K!R3=K.NUMB &
        IS8BITINT(N-128+REG.N!R3) DO
    $(  ADD(REG.N!R3, R2, R3)
        LOOP
    $)
    ADD((N-128) & -64, R2, R3)
$)1 REPEAT

AND ADDRGLOB(N) = VALOF
$(1 UNLESS INCODE RESULTIS 0
    IF COUNTFLAG DO INSERTCOUNT()
    CHKREFS(2)
    IF REG.K!R3=K.LVGLOB & IS8BITINT(N-REG.N!R3)
        RESULTIS (N-REG.N!R3 & 255)+BASE.R3
    // we must change R3
    FREEREG(R3)
    UNLESS REG.K!R3=K.LVGLOB DO
    $(  GENRAX(F.LDA, R3, A.GSAVE, R0)
        SETINFO(R3, K.LVGLOB, 128)
        LOOP
    $)
    ADD((N-REG.N!R3) & -64, R3, R3)
$)1 REPEAT

AND REFCONST(K, N) = VALOF
$(1 LET A = 0
    UNLESS INCODE RESULTIS 0
    // ensure that the instruction using
    // (K, N) will be compiled at STVP
    IF COUNTFLAG DO INSERTCOUNT()
    CHKREFS(2)
    SWITCHON K INTO
    $(2
    CASE K.NUMB:// ref to integer constant N
                A := PAGE0ADDR(N)
                IF A>=0 RESULTIS A
    CASE K.DLAB:// ref to word containing constant
                // (K, N) e.g. LDA r,=Ln
                A := GETKCMP(K, N)
                IF A>=0 RESULTIS RELADDR(A)
                ENDCASE
    CASE K.LABI:// indirect ref to word labelled
                // Ln, e.g JMP @Ln
    CASE K.LAB: // ref to word labelled Ln
                // e.g. JMP Ln or LDA r,Ln
                A := LABV!N
                IF A>=0 & IS8BITINT(A-STVP)
                    RESULTIS RELADDR(A)
                ENDCASE
    $)2
    ADDKREF(K, N)
    RESULTIS BASE.R1
$)1

AND PAGE0ADDR(N) = VALOF
    FOR I = 0 TO 1000 DO
    $(1 LET C = I!TABLE 2,3,4,5,6,7,8,9,10,11,12,13,14,15,
            128,256,384,512,127,255,-116,-128,
            #40,#60,#71,#101,#132
            //              *S,'0','9', 'A', 'Z'
        IF C=N RESULTIS A.CONSTANTS+I
        IF C=#132 RESULTIS -1  // no page0 constant
    $)1

AND RELADDR(A) = (A-STVP & 255)+BASE.R1

AND ADDKCMP(K, N) BE
$(1 IF KCMPP=KCMPT DO
    $(  REMOVEREFSTO(H1!KCMPV, H2!KCMPV, H3!KCMPV)
        KCMPP := KCMPP-3
        FOR P = KCMPV TO KCMPP-1 DO P!0 := P!3
    $)
    H1!KCMPP, H2!KCMPP, H3!KCMPP := STVP, K, N
    KCMPP := KCMPP+3
    IF K=K.NUMB \/ K=K.DLAB DO
    $(2 LET P = GETKREF(K, N)
        IF P=-1 BREAK
        FILLINRELREF(P, STVP, 0)
    $)2 REPEAT
$)1

AND GETKCMP(K, N) = VALOF
// returns address of recently compiled constant (K, N)
$(1 FOR P = KCMPP-3 TO KCMPV BY -3
       IF H3!P=N & H2!P=K RESULTIS H1!P
    RESULTIS -1
$)1

AND ADDKREF(K, N) BE
$(1 IF KREFP=KREFT DO
    $(  CGERROR("IN ADDKREF")
        EXIT(20)
    $)
    H1!KREFP, H2!KREFP, H3!KREFP := STVP, K, N
    KREFP := KREFP+3
$)1

AND GETKREF(K, N) = VALOF
// returns address of instruction making reference (K, N)
// and removes item from KREFV
$(1 FOR P = KREFV TO KREFP-3 BY 3
        IF H3!P=N & H2!P=K DO
    $(  LET A = H1!P
        KREFP := KREFP-3
        FOR Q = P TO KREFP-1 DO Q!0 := Q!3
        RESULTIS A
    $)
    RESULTIS -1
$)1

AND IS8BITINT(A) = -128<=A<=127 -> TRUE, FALSE



LET OUTPUTSECTION() BE
// output octal assembly text
$(1 LET RL = REFLIST
    UNTIL RL=0 DO
    $(3 LET L = H2!RL
        LET LABVAL = LABV!L
        LET A = H3!RL
        IF LABVAL=-1 DO CGERROR("LABEL L%N UNSET", L)
        STV!A := STV!A+LABVAL
        RL := !RL
    $)3
    SWITCHON CG.A INTO
    $(
    CASE 1:     // RDOS ASSEMBLER
        $(4 SELECTOUTPUT(CODESTREAM)
            UNLESS NAMESECTION=0 DO
            $(  IF PROGSIZE=0 DO
                    WRITEF(".TITLE %S*N", NAMESECTION)
                WRITEF(".ENT %S*N", NAMESECTION)
            $)
            WRITES(".NREL*N")
            UNLESS NAMESECTION=0 DO
                WRITEF("%S:*N", NAMESECTION)
            WRITEF("B%N:*N;*N", SECTIONCOUNT)
            RL := REFLIST
            FOR P = 0 TO STVP-1 DO
            $(5 
                LET W = STV!P & #177777
                // WRITEF("%O6 ", P)
                WRITEF("%O6", W)
                IF RL\=0 & H3!RL=P
                THEN
                $(  WRITEF("+B%N", SECTIONCOUNT)
                    RL := !RL
                $)
                NEWLINE()
            $)5
            WRITEF(";END %S*N", NAMESECTION=0 ->
                "", NAMESECTION)
            SELECTOUTPUT(VERSTREAM)
            ENDCASE
        $)4

    CASE 0:     // TRIPOS BINARY
        $(
            LET WRITEWORDS(V, N) BE
                FOR I = 0 TO N-1 DO
                $(  WRCH( [(V!I) >> 8] & 255) // *** BINWRCH changed to WRCH
                    WRCH(  (V!I)       & 255)
                $)
            SELECTOUTPUT(CODESTREAM)
//            SETBINARY(CODESTREAM) // *** SETBINARY missing (not needed?)

            WRITEWORDS( [TABLE T.HUNK], 1)
            WRITEWORDS( @STVP, 1)
            WRITEWORDS( STV, STVP)

            WRITEWORDS( [TABLE T.RELOC], 1)
            WRITEWORDS( @NLABREFS, 1)
            RL := REFLIST
            UNTIL RL=0 DO
            $(  WRITEWORDS(H3+RL, 1)
                RL := !RL
            $)

            WRITEWORDS( [TABLE T.END], 1)

            SELECTOUTPUT(VERSTREAM)
            ENDCASE
        $)

    CASE 2:     // RDOS RELOCATABLE BINARY
        $(6 LET WRWRD(WORD) BE
            $(  WRCH(WORD & 255)
                WRCH(WORD>>8)
                // note we output low order byte first
            $)
            AND RADIX40(NAME, CODE) BE
            $(  LET T = VEC 5
                AND H, L, CRY = ?, ?, ?
                FOR I = 1 TO 5 DO T!I := 0
                FOR I = 1 TO GETBYTE(NAME, 0) IF I<=5 DO
                $(  LET CH = GETBYTE(NAMESECTION, I)
                    T!I := 38 // question mark character
                    FOR J = 1 TO 37
                        IF CH=GETBYTE("0123456789*
                        *ABCDEFGHIJKLMNOPQRSTUVWXYZ.", J) DO
                        $(  T!I := J; BREAK
                        $)
                $)
 //               H := MULDIV(T!0*40+T!1, 125, 64) // *** MULDIV missing
                RESULT2 := RESULT2<<9
                L := (T!2*40+T!3)*40+T!4
                CRY := (H REM 2)+(L<0 -> 1, 0)
                L := L & #077777
                L := L+RESULT2
                CRY := CRY+(L<0 -> 1, 0)
                H := (H>>1)+CRY/2
                L := L & #077777
                IF (CRY REM 2)=1 DO L := L \/ #100000
                CODE!0 := (H<<5)+(L>>11)
                CODE!1 := L<<5
            $)

            SELECTOUTPUT(CODESTREAM)

            // TITLE block
            $(  LET BLOCK = VEC 9
                LET CHECKSUM = 0
                BLOCK!1 := #7 // block type indicant
                              // TITLE symbol
                BLOCK!2 := -3 // word count
                              // (symbol+equivalence)
                BLOCK!3 := 0
                BLOCK!4 := 0
                BLOCK!5 := 0

                BLOCK!6 := 0 // future checksum

                RADIX40((NAMESECTION=0 -> ".MAIN",
                    NAMESECTION), BLOCK+7) // section name
                BLOCK!8 := BLOCK!8+4 // flag as TITLE symbol

                BLOCK!9 := 0 // equivalence
                             // (whatever that is)

                FOR I = 1 TO 9 DO CHECKSUM := CHECKSUM+BLOCK!I

                BLOCK!6 := -CHECKSUM // sum of all words in the
                                     // block should be zero
                FOR I = 1 TO 9 DO WRWRD(BLOCK!I)
            $)

            // RELOCATABLE DATA block:
            FOR P = 0 TO STVP-1 BY 14 DO
            $(7 LET BLOCK = VEC 21 // max. block size is 21
                LET CHECKSUM = 0
                LET WRDCNT = STVP-P>=14 -> 14, STVP-P
                LET RL = REFLIST
                LET PP = P

                // format the block
                BLOCK!1 := #2 // block type indicant
                              // (relocatable data)
                BLOCK!2 := -WRDCNT-1 // (-1 for address word)
                FOR I = 0 TO 2 DO
                $(8 LET TEMP = 0
                    FOR J = 12 TO 0 BY -3 DO
                    $(  TEST I=0 & J=12
                        THEN TEMP := #040000 // relocatable
                        ELSE TEMP := TEMP+VALOF
                            TEST RL\=0 & H3!RL=PP
                            THEN
                            $(  RL := !RL
                                RESULTIS 2<<J // relocatable
                            $)
                            ELSE RESULTIS 1<<J // absolute
                        PP := PP+1 // increment proper P
                    $)
                    BLOCK!(3+I) := TEMP
                $)8
                BLOCK!6 := 0 // will be set to the checksum
                             // for the block
                BLOCK!7 := P // address of data
                FOR I = 0 TO WRDCNT-1 DO
                    BLOCK!(8+I) := STV!(P+I)

                // set checksum.
                // checksum is such that the sum of all words
                // in the block is zero
                FOR I = 1 TO 7+WRDCNT DO
                    CHECKSUM := CHECKSUM+BLOCK!I
                    // **beware of overflow**
                BLOCK!6 := -CHECKSUM

                // output block
                FOR I = 1 TO 7+WRDCNT DO WRWRD(BLOCK!I)
            $)7

            SELECTOUTPUT(VERSTREAM)
            ENDCASE
        $)6
    $)
$)1

LET DBOUTPUT() BE
$(1 WRITES("SMSTK: ")
    FOR P = ARG1 TO TEMPV BY -3 DO WRKN(H1!P, H2!P)
    UNLESS KREFV=KREFP DO
    $(  WRITES("*NKREFV: ")
        FOR P = KREFV TO KREFP-3 BY 3 DO
        $(  WRITEF("%N:", H1!P)
            WRKN(H2!P, H3!P)
        $)
    $)
    UNLESS KCMPV=KCMPP DO
    $(  WRITES("*NKCMPV: ")
        FOR P = KCMPV TO KCMPP-3 BY 3 DO
        $(  WRITEF("%N:", H1!P)
            WRKN(H2!P, H3!P)
        $)
    $)
    WRITEF("*NSTVP=%N OP=%N PDOP=%N SSP=%N ", STVP,
        OP, PENDINGOP, SSP)
    FOR R = R0 TO R3 UNLESS R=R2 \/ REG.K!R=K.NONE DO
    $(  WRITEF("R%N=", R)
        WRKN(REG.K!R, REG.N!R)
    $)
    NEWLINE()
$)1

AND WRKN(K, N) BE
$(1 LET S = VALOF
        SWITCHON K INTO
        $(2
        DEFAULT:    RESULTIS "?"
        CASE K.NONE:RESULTIS "Z"
        CASE K.NUMB:RESULTIS "N"
        CASE K.LOC: RESULTIS "P"
        CASE K.GLOB:RESULTIS "G"
        CASE K.LAB: RESULTIS "L"
        CASE K.REG: RESULTIS "R"
        CASE K.R3:  RESULTIS "I"
        CASE K.LVLOC:
                    RESULTIS "@P"
        CASE K.LVGLOB:
                    RESULTIS "@G"
        CASE K.LVLAB:
                    RESULTIS "@L"
        CASE K.DLAB:RESULTIS "DL"
        CASE K.LABI:RESULTIS "LI"
        $)2
    WRITEF("%S%N ", S, N)
$)1


