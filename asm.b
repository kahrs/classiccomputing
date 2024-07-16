// (C) COPYRIGHT 1979 TRIPOS RESEARCH GROUP
//     UNIVERSITY OF CAMBRIDGE
//     COMPUTER LABORATORY

GET "LIBHDR"

GLOBAL $(

ABSLOC         :       150
ABSMIN         :       151
ABSMAX         :       152
ABSVEC         :       153
ABSRP          :       154
ABSRVEC        :       155
AVEC           :       156

RELLOC         :       160
RELMIN         :       161
RELMAX         :       162
RELVEC         :       163
RELRP          :       164
RELRVEC        :       165

LOCATION       :       170
MINLOC         :       171
MAXLOC         :       172
CODEVEC        :       173
RELP           :       174
RELOCVEC       :       175
LOCMODE        :       176

INDIRECT       :       177
NOLOAD         :       178

EXTERNAL.SYMBOL:       179

SYMB           :       180
SYMBTYPE       :       181
EXPVAL         :       182
EXPTYPE        :       183
TAGV           :       184
CH             :       185
TAGTABLE       :       186
STVEC          :       187
STVECP         :       188
DOTEXP         :       189

OUTBUF         :       190
OUTBUFP        :       191
NERRS          :       192
PRINTRES       :       193
CHARPOS        :       194

ENDED          :       195
PASS1          :       196
PASS2          :       197

PRSTART        :       198
PREND          :       199

NONULL         :       200
PACKLR         :       201
RADIX          :       202

TITLE.NOT.READ :       203

VALPNTR        :       205
TYPEPNTR       :       206

NUMERRS        :       207
ERRORS.FOUND   :       208

//ARGV           :       210
SOURCESTREAM   :       211
LISTSTREAM     :       212
CODESTREAM     :       213
LISTING        :       214
FAILED         :       215
SYSOUT         :       217

COMPLAIN       :       220
FREEVECTORS    :       221
SKIPREST       :       222
SKIPOPERAND    :       223
READSYMB       :       224
OUTCODE        :       225
CHANGEMODE     :       226
SETLOC         :       227
BUILD          :       228
UNRCH          :       229
PUTLOC         :       230
PUTVAL         :       231
PUTN           :       232
PCH            :       233
CLEARBITS      :       234
ERROR          :       235
DECLSYSWORDS   :       236
REPORTUNDEFS   :       237
RCH            :       238
SKIPLAYOUT     :       240
STVECUPB       :       241
INHDR          :       242
HDRSTREAM      :       243
TIDY.UP.ON.FAIL:       244
WARN           :       245

       $)

MANIFEST
$(
   return.severe        =  20
   return.hard          =  10
   return.soft          =   5
   return.ok            =   0
   flag.break           =   1
   flag.commbreak       =   2
$)

MANIFEST
$(

// BASIC CONSTANTS
STVDEFSIZE   =   3000
TAGTABLESIZE =    200
ARGVUPB      =     50
MAXINT       =  32767
LOCATIONLIMIT= MAXINT
OUTBUFLIM    =    120

YES          =   TRUE
NO           =  FALSE

// SYMBOL TYPES
S.ABS   =  1     // TAG WITH ABSOLUTE VALUE
S.DIR   =  2     // ASSEMBLER DIRECTIVE
S.DOT   =  3     // LOCATION COUNTER SYMBOL
S.INSTR =  4     // INSTRUCTION MNEMONIC
S.NEW   =  5
S.NONE  =  6     // NO SYMBOL FOUND BEFORE END OF LINE
S.SKPMN =  7     // SKIP MNEMONIC FOR ALC INSTS.
S.REL   =  8     // TAG WITH RELOCATABLE VALUE
S.LBR   =  9     // LEFT PARENTHESIS
S.NUMBER= 10     // NUMBER (123. OR "E, SAY)
S.MONOP = 11     // MONADIC OPERATOR
S.EXT   = 12     // EXTERNAL REFERENCE


// OPERATORS

OP.PLUS   = 1
OP.MINUS  = 2
OP.TIMES  = 3
OP.OVER   = 4
OP.AND    = 5
OP.OR     = 6

// INSTRUCTION TYPES

I.MRA   =  1     // MEMORY REFERENCE WITH AC
I.MR    =  2     // MR WITHOUT AC
I.ALC   =  3     // ARITHMETIC AND LOGICAL
I.IOA   =  4     // I/O WITH AC AND DEVICE
I.IO    =  5     // I/O WITH DEVICE
I.ION   =  6     // I/O WITH AC
I.ZOP   =  7     // NO OPERANDS (HALT)

// SYMBOL TABLE TYPES FOR INSTRUCTIONS.
// THESE ARE THE TYPE FIELDS FOR ENTRIES FOR THE
// INSTRUCTION TAGS IN THE SYMBOL TABLE.

STI.MRA   = (I.MRA   << 10) + S.INSTR
STI.MR    = (I.MR    << 10) + S.INSTR
STI.ALC   = (I.ALC   << 10) + S.INSTR
STI.IOA   = (I.IOA   << 10) + S.INSTR
STI.IO    = (I.IO    << 10) + S.INSTR
STI.ION   = (I.ION   << 10) + S.INSTR
STI.ZOP   = (I.ZOP   << 10) + S.INSTR

// DIRECTIVES

D.BLK   =  1
D.END   =  2
D.LOC   =  3
D.NREL  =  4
D.RDX   =  5
D.TXT   =  6
D.TXTM  =  7
D.TXTN  =  8
D.ZREL  =  9
D.ENT   = 10
D.EXTN  = 11
D.TITL  = 12
D.LIST  = 13

D.UNIMP = 20

TAGCHARS=  5     // MAX. NUMBER OF CHARS IN A TAG
TAGSIZE = (TAGCHARS + BYTESPERWORD - 1)/BYTESPERWORD
                 // NUMBER OF WORDS NEEDED FOR TAG
TAGBYTEUPB = (TAGSIZE * BYTESPERWORD) - 1
                 // LAST BYTE OFFSET IN TAGSIZE WORDS
FATAL   =  YES  // FOR ERROR
CONTINUE = NO // FOR ERROR


// SYMBOL TABLE ENTRY OFFSETS

ST.TYPE  = TAGSIZE + 1
ST.VALUE = TAGSIZE + 2

// SYMBOL TABLE TYPE BITS

STB.ENT      =   #40
STB.SETEVER  =  #100
STB.SETNOW   =  #200
STB.MULDEF   =  #400
STB.TEMP     = #1000

ST.TYPE.MASK =   #X1F

// OBJECT MODULE IDENTIFIERS.

T.HUNK    = 1000
T.RELOC   = 1001
T.END     = 1002
T.ABSHUNK = 1003
T.ABSREL  = 1004
T.EXT     = 1005

// EXTERNAL SYMBOL TYPE BYTES

EXT.ENTREL =   1
EXT.ENTABS =   2
EXT.EXT    = 129


$)


LET START(ARG) BE
  $( LET OB = VEC 60
     LET TGV = VEC 2



     SYSOUT   := OUTPUT()
     TAGV     := TGV

     OUTBUF := OB


     // READ THE PARAMETERS AND OPEN FILES.

     SOURCESTREAM,LISTSTREAM,CODESTREAM:=0,0,0

     FAILED := NO

     TEST (ARGC < 4) THEN
       STVECUPB := STVDEFSIZE
      ELSE
       STVECUPB := STRINGTONUM(ARGV!5,STVDEFSIZE)

     STVEC := GETVEC(STVECUPB + TAGTABLESIZE)
     STVECP := STVEC + STVECUPB + 1

     IF STVEC = 0 THEN
       $( WRITES("INSUFFICIENT HEAP FOR SYMBOL TABLE*N")
          STOP(RETURN.HARD)
       $)

     TAGTABLE := STVECP
     FOR J = 0 TO TAGTABLESIZE - 1 DO
       TAGTABLE ! J := 0

     SOURCESTREAM := FINDINPUT(ARGV!1)
     CHECKOPEN(SOURCESTREAM,ARGV!1,"PROG")
     IF (ARGC > 3) & (ARGV!4 \= 0) THEN
       $( HDRSTREAM := FINDINPUT(ARGV!4)
          CHECKOPEN(HDRSTREAM,ARGV!4,"HDR")
       $)
     LISTING := ARGC > 2
     IF LISTING & (ARGV!3) \= 0 THEN
       $( LISTSTREAM := FINDOUTPUT(ARGV!3)
          CHECKOPEN(LISTSTREAM,ARGV!3,"VER")
       $)
     IF (ARGC > 1) & (ARGV!2) \= 0 THEN
       $( CODESTREAM := FINDOUTPUT(ARGV!2)
          CHECKOPEN(CODESTREAM,ARGV!2,"CODE")
       $)

     IF FAILED THEN // ONE OR MORE OPEN FAILS
       TIDY.UP.ON.FAIL()

     DECLSYSWORDS()
     IF LISTSTREAM \= 0 THEN
       SELECTOUTPUT(LISTSTREAM)
     WRITES("TRIPOS NOVA ASSEMBLER VERSION 2*E")

     FIRSTPASS()

     SECONDPASS()

     STOP(NUMERRS > 0 -> RETURN.SOFT,0)

  $)

and WRITEWORDS(V, N) be
$(
    FOR I = 0 TO N-1 DO
    $(  WRCH( [(V!I) >> 8] & 255)
        WRCH(  (V!I)       & 255)
    $)
$)

AND CHECKOPEN(STREAM,FILE,KEY) BE
  IF STREAM = 0 THEN
    $( WRITEF("CAN'T OPEN %S FILE *"%S*"*N",KEY,FILE)
       FAILED := YES
    $)


AND TIDY.UP.ON.FAIL() BE
  $( FREEVEC(STVEC)
     IF HDRSTREAM \= 0 THEN
       $( SELECTINPUT(HDRSTREAM)
          ENDREAD()
       $)
     IF SOURCESTREAM \= 0 THEN
       $( SELECTINPUT(SOURCESTREAM)
          ENDREAD()
       $)
     IF LISTSTREAM \= 0 THEN
       $( SELECTOUTPUT(LISTSTREAM)
          ENDWRITE()
       $)
     IF CODESTREAM \= 0 THEN
       $( SELECTOUTPUT(CODESTREAM)
          ENDWRITE()
       $)
     STOP(RETURN.HARD)
  $)







AND FIRSTPASS() BE
  $( ABSMIN, RELMIN := MAXINT,MAXINT
     ABSMAX, RELMAX := 0,0
     ABSRP,  RELRP  := 0,0
     ABSLOC, RELLOC := 0,0
     LOCMODE, RELP := S.ABS, ABSRP
     MINLOC, MAXLOC, LOCATION := ABSMIN,ABSMAX,ABSLOC

     CHARPOS := 1
     ENDED := NO; PASS1 := YES; PASS2 := NO
     RADIX, PACKLR, NONULL := 8, NO, NO

     INHDR := NO

     TITLE.NOT.READ := YES

     SELECTINPUT(SOURCESTREAM)

     UNTIL ENDED DO
       $( OUTBUFP := 22
          ERRORS.FOUND := NO
          DOLINE()
// TESTFLAGS not found
//          IF TESTFLAGS(1) THEN
//            $( // SELECTOUTPUT(SYSOUT)
//               WRITES("*N****BREAK - NO CODE PRODUCED*N")
//               FREEVECTORS()
//               TIDY.UP.ON.FAIL()
//            $)
       $)

     // FORCE SAVING OF FINAL LOCATION VALUES

     CHANGEMODE((LOCMODE = S.ABS -> S.REL, S.ABS))

     IF ABSMAX = 0 THEN ABSMIN := 0

     WRITES("*N*N")

     ENDREAD()

     IF INHDR THEN
       $( SELECTINPUT(SOURCESTREAM)
          ENDREAD()
       $)

     SOURCESTREAM, HDRSTREAM := 0, 0

  $)



AND SECONDPASS() BE
  $( // GET THE CODE VECTORS

     AVEC, RELVEC, ABSRVEC, RELRVEC := 0,0,0,0
     AVEC    := GVEC(ABSMAX - ABSMIN - 1)
     ABSVEC  := AVEC - ABSMIN
     ABSRVEC := GVEC(ABSRP - 1)
     RELVEC  := GVEC(RELMAX - 1)
     RELRVEC := GVEC(RELRP - 1)

     // CLEAR THE ABSOLUTE VECTOR.
     // THIS IS FOR SYSLINK.

     FOR J = ABSMIN TO ABSMAX - 1 DO
       ABSVEC ! J := 0

     ABSMIN, RELMIN := MAXINT,MAXINT
     ABSMAX, RELMAX := 0,0
     ABSRP,  RELRP  := 0,0
     ABSLOC, RELLOC := 0,0

     LOCMODE := S.ABS

     CHARPOS := 1
     CODEVEC, RELOCVEC, RELP := ABSVEC, ABSRVEC, ABSRP
     MINLOC, MAXLOC, LOCATION := ABSMIN,ABSMAX,ABSLOC
     ENDED := NO; PASS1 := NO
     RADIX, PACKLR, NONULL, PASS2 := 8,NO,NO,YES

     CLEARBITS()

     SOURCESTREAM := FINDINPUT(ARGV!1)

     INHDR := NO
     SELECTINPUT(SOURCESTREAM)

     NUMERRS := 0

     TITLE.NOT.READ := YES

     UNTIL ENDED DO
       $( OUTBUFP := 22; NERRS := 0
          PRSTART, PREND := 0, -1
          ERRORS.FOUND := NO

//          IF TESTFLAGS(1) THEN
//            $( SELECTOUTPUT(SYSOUT)
//               WRITES("*N****BREAK - NO CODE PRODUCED*N")
//               FREEVECTORS()
//               TIDY.UP.ON.FAIL()
//            $)

          FOR J = 0 TO OUTBUFLIM DO
            PUTBYTE(OUTBUF,J,' ')

          DOLINE()

          IF ERRORS.FOUND THEN
            NUMERRS := NUMERRS + 1

          UNLESS LISTING | NERRS > 0 THEN
            LOOP

          FOR J = 0 TO OUTBUFP - 1 DO
            WRCH(GETBYTE(OUTBUF,J))

          IF LISTING THEN
            FOR P = PRSTART TO PREND DO
              WRITEF("              %O6*N",CODEVEC!P)
       $)

     ENDREAD()

     IF INHDR THEN
       $( SELECTINPUT(SOURCESTREAM)
          ENDREAD()
       $)

     // OUTPUT FINAL MESSAGES.
     CHANGEMODE(LOCMODE = S.ABS -> S.REL,S.ABS)

     TEST NUMERRS > 0 THEN
       WRITEF("*N*N%N LINE(S) WITH ERRORS*N",NUMERRS)
      ELSE
       WRITES("*N*NNO ERRORS DETECTED*N")

     REPORTUNDEFS()

     IF ABSMAX = 0 THEN
       ABSMIN := 0

     WRITEF("*N*NASSEMBLY STATISTICS (WORDS): *N*N*
             *           ABSOLUTE   RELOCATABLE*N*N*
             *CODE        %I5       %I5*N*
             *RELOC. INFO %I5       %I5*N",
         ABSMAX - ABSMIN, RELMAX,ABSRP,RELRP)

     WRITEF("*N%N WORDS OF SYMBOL TABLE WERE USED*N",
         STVEC+STVECUPB+1-STVECP)


       WRITES("*N*NASSEMBLY COMPLETE*N")

     // CLOSE LISTING STREAM:

     IF LISTSTREAM \= 0 THEN
       ENDWRITE()

     IF ARGC > 1 THEN
       OUTCODE()

     // FREE THE CODE VECTORS

     FREEVECTORS()

     FREEVEC(STVEC)

  $)


AND GVEC(U) = VALOF
  $( IF U < 0 THEN
       RESULTIS 0
     $( LET V = GETVEC(U)
        IF V = 0 THEN
          ERROR("INSUFFICIENT STORE FOR VECTORS*N")
        RESULTIS V
     $)
  $)



AND FREEVECTORS() BE
  $( IF AVEC \= 0 THEN
       FREEVEC(AVEC)
     IF ABSRVEC \= 0 THEN
       FREEVEC(ABSRVEC)
     IF RELVEC \= 0 THEN
       FREEVEC(RELVEC)
     IF RELRVEC \= 0 THEN
       FREEVEC(RELRVEC)
  $)



AND COMPLAIN(CODE) BE
  $( ERRORS.FOUND := YES
     WARN(CODE)
  $)



AND WARN(CODE) BE
  $( UNLESS NERRS >= 5 | PASS1 THEN
       $( FOR I = 0 TO 4 DO
            IF CODE = GETBYTE(OUTBUF,I) THEN
              RETURN
          PUTBYTE(OUTBUF,NERRS,CODE)
          NERRS := NERRS + 1
       $)
  $)



AND DOLINE() BE
  $( // THIS ROUTINE HANDLES A COMPLETE LINE OF SOURCE.
     // FIRST THE LABELS ARE SET, AND THEN READREST IS
     // CALLED, EITHER FOR AN EQUIVALENCE LINE, OR FOR
     // AN ORDINARY LINE. (DIRECTIVE, INSTRUCTION OR
     // DATA WORD).
     INDIRECT, NOLOAD := NO, NO
     SETLABELS()
     TEST CH = '=' THEN // EQUIVALENCE LINE
       $( LET S,ST = SYMB,SYMBTYPE
          RCH(); READSYMB(); READREST(NO)
          SETSYMB(S,ST,EXPVAL,EXPTYPE)
       $)
      ELSE
       $( READREST(YES)
          IF (EXPTYPE = S.ABS) | (EXPTYPE = S.REL) THEN
            $( PUTLOC()
               BUILD(EXPVAL,EXPTYPE)
            $)
       $)
     IF PRINTRES THEN  // VALUE NEEDED IN LISTING
       PUTVAL(EXPVAL,EXPTYPE)

     // READ TO END OF LINE
     SKIPREST()
  $)


AND SETLABELS() BE // SETS LABELS ON LINE
  WHILE VALOF $(
    RCH(); READSYMB(); SKIPLAYOUT()
    RESULTIS CH = ':' $) DO
    SETSYMB(SYMB,SYMBTYPE,LOCATION,LOCMODE)


AND SKIPLAYOUT() BE
  $( // SKIPS OVER SPACES, TABS, COMMAS AND COMMENTS
     //  SETS INDIRECT OR NOLOAD IF @ OR # ARE FOUND.
     WHILE (CH = '*S') | (CH = '*T') | (CH = ',') |
       (CH = '@') | (CH = '#') DO
       $( IF CH = '@' THEN INDIRECT := YES
          IF CH = '#' THEN NOLOAD   := YES
          RCH()
       $)
     IF CH = ';' THEN // SKIP COMMENT
       SKIPREST()
  $)


AND SKIP.OPERAND() BE
  UNTIL CH = ' ' | CH = '*N' | CH = ',' |
        CH = ';' | CH = '*T' | CH = '#' | CH = '@' DO
    RCH()


AND SKIPREST() BE
  UNTIL (CH = '*N') DO RCH()



AND SETSYMB(S,ST,V,T) BE
  $( IF (ST \= S.ABS) & (ST \= S.REL) &
        (ST \= S.NEW)  & (ST \= S.EXT) THEN
       $( COMPLAIN('Q')
          RETURN
       $)
     IF (T \= S.ABS) & (T \= S.REL) & (T \= S.EXT) THEN
       $( COMPLAIN('Q')
          T := S.ABS; V := 0
       $)
     TEST (ST.TYPE ! S & STB.MULDEF) > 0 THEN
       COMPLAIN('M')
      ELSE
       TEST (ST.TYPE ! S & STB.SETNOW) > 0 THEN
         ST.TYPE ! S := ST.TYPE ! S | STB.MULDEF
        ELSE
         TEST T = S.EXT & (ST.TYPE!S & STB.ENT) \= 0 THEN
           COMPLAIN('I')
          ELSE
           $( ST.TYPE ! S := (ST.TYPE ! S & #177740) | T
              ST.VALUE ! S := V
              ST.TYPE ! S := ST.TYPE ! S | STB.SETNOW |
                STB.SETEVER
           $)
  $)


AND READREST(FREFS) BE
  $( // READ REST OF LINE, WHICH MAY BE:
     //       INSTRUCTION
     //       DATA WORD
     //       DIRECTIVE
     //       EMPTY
     // THE ROUTINE IS ENTERED WITH THE FIRST SYMBOL
     // DESCRIBED BY SYMB AND SYMBTYPE.
     LET S = SYMB



     PRINTRES := YES

     SWITCHON SYMBTYPE INTO

       $(
          CASE S.NONE:
            PRINTRES := NO
            EXPTYPE := S.NONE
           RETURN

          CASE S.DIR:
            DODIR()
            ENDCASE

          CASE S.INSTR:
            DOINSTR(FREFS)
            ENDCASE


          DEFAULT:
            READEXP(FREFS)
            IF ERRORS.FOUND THEN
              EXPTYPE := S.REL
            EXPVAL := EXPVAL + (INDIRECT -> #100000,0)
            INDIRECT := NO
            READSYMB()

       $)

     IF SYMBTYPE \= S.NONE THEN
       COMPLAIN('Q')

     IF NOLOAD | INDIRECT THEN
       COMPLAIN('Q')

     RETURN


  $)





AND DODIR() BE

  // HANDLE AN ASSEMBLER DIRECTIVE.
  //  THE ONLY DIRECTIVES RECOGNISED CURRENTLY ARE:
  //     .LOC
  //     .ZREL
  //     .BLK
  //     .NREL
  //     .TXTN
  //     .TXTM
  //     .RDX
  //     .END
  //     .TXT
  //     .ENT
  //     .EXTN
  //     .TITL
  //
  // OTHERS ARE FLAGGED WITH Y AS WARNINGS.
  //
  $( LET T = ST.VALUE ! SYMB

     SWITCHON T INTO

       $( CASE D.LOC:
            READSYMB()
            READEXP(NO)
            CHANGEMODE(EXPTYPE)
            SETLOC(EXPVAL)
            ENDCASE

          CASE D.ZREL: // FUDGED TO BE .LOC 50
            PRINTRES := NO
            CHANGEMODE(S.ABS)
            SETLOC(40)
            EXPVAL := 40
            ENDCASE

          CASE D.BLK:
            READSYMB()
            READABSEXP(NO)
            SETLOC(LOCATION + EXPVAL)
            ENDCASE

          CASE D.NREL:
            PRINTRES := NO
            CHANGEMODE(S.REL)
            ENDCASE

          CASE D.TXTN:
            READSYMB()
            NONULL := READABSEXP(NO) \= 0
            ENDCASE

          CASE D.TXTM:
            READSYMB()
            PACKLR := READABSEXP(NO) \= 0
            ENDCASE

          CASE D.RDX:
            RADIX := 10; READSYMB()
            READABSEXP(NO)
            TEST 2 <= EXPVAL <= 10 THEN
              RADIX := EXPVAL
             ELSE
              COMPLAIN('R')
            ENDCASE

          CASE D.END:
            ENDED := YES
            PRINTRES := NO
            ENDCASE

          CASE D.TXT:
            $( LET WORD, TERM, LEN = 0,0,0
               PUTLOC()
               PRSTART := LOCATION + 1
               SKIPLAYOUT()
               TERM := CH; RCH()
               UNTIL (CH = TERM) | (CH = '*N') DO
                 $( LET C = READSTRCH()
                    LEN := LEN + 1
                    TEST (LEN REM 2) = 0 THEN
                      $( WORD := WORD + C
                         UNLESS PACKLR THEN
                           WORD := (WORD >> 8) +
                              ((WORD & 255) << 8)
                         BUILD(WORD,S.ABS)
                         WORD := 0
                      $)
                     ELSE
                      WORD := C << 8
                 $)
               UNLESS PACKLR THEN // FINAL WORD
                 WORD := (WORD>>8)+((WORD & 255)<<8)
               UNLESS NONULL & (LEN REM 2 = 0) THEN
                 BUILD(WORD,S.ABS)
               PREND := LOCATION - 1
               IF (LEN \= 0) & PASS2 THEN
                 EXPVAL := CODEVEC ! (PRSTART - 1)
            $)
            SKIPREST()
            ENDCASE

          CASE D.ENT:
          CASE D.EXTN:
            // READ A LIST OF NAMES.
            $( READSYMB()
               SKIPLAYOUT()
               TEST T = D.EXTN THEN
                 SETSYMB(SYMB, SYMBTYPE, 0, S.EXT)
                ELSE
                 TEST SYMBTYPE = S.EXT THEN
                   COMPLAIN('I')
                  ELSE
                   // MUST CHECK THAT THE SYMBOL IS A TAG
                   TEST SYMBTYPE=S.ABS | SYMBTYPE=S.REL |
                        SYMBTYPE=S.NEW THEN
                     ST.TYPE!SYMB := ST.TYPE!SYMB | STB.ENT
                    ELSE
                     COMPLAIN('Q')
            $) REPEATUNTIL CH = '*N'

            ENDCASE

          CASE D.TITL:
            TEST TITLE.NOT.READ THEN
              $( TITLE.NOT.READ := NO
                 SKIPLAYOUT()
                 TEST PASS1 THEN
                   $( WRITES(" ASSEMBLING ")
                      UNTIL CH = ' ' | CH = '*N' |
                            CH = ';' | CH = '*T' DO
                        $( WRCH(CH)
                           RCH()
                        $)
                      WRCH('*E')
                   $)
                  ELSE
                   SKIPREST()
              $)
             ELSE
              $( SKIPREST()
                 WARN('T')
              $)
            SYMBTYPE := S.NONE
            ENDCASE

          CASE D.LIST:
            READSYMB()
            LISTING := READABSEXP(NO) = 0 & ARGV!2 \= 0
            ENDCASE

          CASE D.UNIMP:
            SKIPREST()
            PRINTRES := NO
            WARN('Y')
            SYMBTYPE := S.NONE
            ENDCASE

       $)

     READSYMB()
     EXPTYPE := S.DIR
  $)



AND DOINSTR(FREFS) BE
  $( LET I = (ST.TYPE ! SYMB) >> 10
     LET V = ST.VALUE ! SYMB

     EXPTYPE := S.ABS

     IF PASS1 & FREFS THEN
       // ONLY LOOK AT REST IN PASS2
       RETURN

     READSYMB()

     SWITCHON I INTO

       $( CASE I.MRA:
            V := V + (READAC(FREFS) << 11)
            READSYMB()

          CASE I.MR:
            V := V + READADDRESS(FREFS)
            V := V + (INDIRECT -> #2000,0)
            INDIRECT := NO
            ENDCASE

          CASE I.ALC:
            V := V+(READAC(FREFS)<<13); READSYMB()
            V := V+(READAC(FREFS)<<11); READSYMB()
            IF SYMBTYPE = S.SKPMN THEN
              $( V := V + ST.VALUE ! SYMB
                 READSYMB()
              $)
            V := V + (NOLOAD -> #10,0)
            NOLOAD := NO
            ENDCASE

          CASE I.IOA:
            V := V+(READAC(FREFS)<<11); READSYMB()


          CASE I.IO:
            V := V + READDEV(FREFS); READSYMB()
            ENDCASE

          CASE I.ION:
            V := V+(READAC(FREFS)<<11); READSYMB()
            ENDCASE

          CASE I.ZOP:
            ENDCASE

       $)

     EXPTYPE := S.ABS
     EXPVAL := V
  $)



AND READADDRESS(FREFS) = VALOF
  $( LET E = 0; READEXP(FREFS); E:=EXPVAL

     READSYMB()

     IF EXPTYPE = S.EXT THEN
       $( COMPLAIN('A')
          SKIPREST()
          RESULTIS 0
       $)

     TEST SYMBTYPE = S.NONE THEN
       $( LET D = E - LOCATION
          TEST (EXPTYPE = S.ABS) &
               ((NOT DOTEXP) |
                (D < -128) | (D > 127)) THEN
            $( IF 0 <= E <= 255 THEN
                 RESULTIS EXPVAL
               COMPLAIN('A')
               RESULTIS 0
            $)
           ELSE
            $( IF LOCMODE = S.ABS & EXPTYPE = S.REL THEN
                 $( COMPLAIN('A')
                    RESULTIS 0
                 $)
               IF -128 <= D < 128 THEN
                 RESULTIS ((1 << 8) + (D & 255))
               COMPLAIN('A')
               RESULTIS 0
            $)
       $)
      ELSE
       $( LET AC = READAC(FREFS); READSYMB()
          IF ((AC = 0) & (0 <= E <= 255)) |
             ((AC > 0) & (-128 <= E < 127)) THEN
             RESULTIS ((AC << 8) + (E & 255))
          COMPLAIN('A')
          RESULTIS 0
       $)
  $)



AND READAC(FREFS) = VALOF
  $( LET AC = READABSEXP(FREFS)
     IF 0 <= AC <= 3 THEN
       RESULTIS AC
     COMPLAIN('O')
     RESULTIS 0
  $)


AND READDEV(FREFS) = VALOF
  $( LET D = READABSEXP(FREFS)
     IF 0 <= D <= 63 THEN
       RESULTIS D
     COMPLAIN('O')
     RESULTIS 0
  $)


AND READABSEXP(FREFS) = VALOF
  $( LET E = 0; READEXP(FREFS); E := EXPVAL
     IF EXPTYPE = S.ABS THEN
       RESULTIS E
     COMPLAIN('R')
     EXPTYPE := S.ABS
     RESULTIS 0
  $)


AND READEXP(FREFS) BE
  $( LET ET, EDT, EVAL, OP = 0, 0, 0, OP.PLUS
     LET DOTPOS, UNKNOWN   = YES, NO

     IF SYMBTYPE = S.MONOP THEN
       $( OP := SYMB
          READSYMB()
       $)

     $( LET SVAL,SDT,ST = 0,0,0

        SWITCHON SYMBTYPE INTO

          $( CASE S.LBR:
               READSYMB()
               READEXP(FREFS); SVAL := EXPVAL
               ST := (EXPTYPE = S.REL -> 1,0)
               IF CH \= ')' THEN
                 $( COMPLAIN('E')
                    GOTO EXPERR
                 $)
               RCH()
               GOTO ELAB

             CASE S.DOT:
               SVAL := LOCATION
               ST := (LOCMODE = S.REL -> 1,0)
               SDT := 1
               GOTO ELAB

             CASE S.NUMBER:
               SVAL := SYMB
               GOTO ELAB

             CASE S.EXT:
               ST := 10000
               IF ~ FREFS THEN
                 COMPLAIN('F')
               EXTERNAL.SYMBOL := SYMB
               GOTO ELAB

             CASE S.REL:
               ST := 1
               GOTO ELB2

             CASE S.NEW:
               UNKNOWN := YES

             CASE S.INSTR:
             CASE S.ABS:
         ELB2: SVAL := ST.VALUE ! SYMB
               UNLESS (ST.TYPE ! SYMB & STB.SETNOW) > 0 |
                 FREFS THEN
                 COMPLAIN('F')

         ELAB: SWITCHON OP INTO
                 $( CASE OP.PLUS:
                      EVAL := EVAL + SVAL
                      ET   := ET   + ST
                      EDT  := EDT  + SDT
                      ENDCASE

                    CASE OP.MINUS:
                      IF ABS ST > 1 THEN
                        GOTO RELERR
                      EVAL := EVAL - SVAL
                      ET   := ET   - ST
                      EDT  := EDT  - SDT
                      ENDCASE

                    CASE OP.TIMES:
                      IF (ST > 0) & (ET > 0) THEN
                        GOTO RELERR
                      EVAL := EVAL * SVAL
                      ET   := ST*EVAL + ET*SVAL
                      EDT  := SDT*EVAL + EDT*SVAL
                      ENDCASE

                    CASE OP.OVER:
                      IF (ST>0) | (ET REM SVAL\=0) THEN
                        GOTO RELERR
                      EVAL := EVAL/SVAL
                      ET := ET/SVAL
                      DOTPOS := (EDT REM SVAL=0) & DOTPOS
                      EDT := EDT/SVAL
                      ENDCASE

                    CASE OP.OR: CASE OP.AND:
                      IF (ET > 0) | (ST > 0) THEN
                        GOTO RELERR
                      DOTPOS := DOTPOS & (EDT = SDT = 0)
                      EVAL := (OP = OP.OR -> EVAL | SVAL,
                                             EVAL & SVAL)
                      ENDCASE

                 $)
               ENDCASE

             DEFAULT:
               COMPLAIN('E')
               GOTO EXPERR

          $)

        OP := CHKOP()

        TEST OP > 0 THEN // ANOTHER OPERATOR
          $( RCH()
             IF CHKOP() > 0 THEN // TWO OPERATORS TOGETHER
               $( WARN('O')
                  SYMB,SYMBTYPE := 0,S.NUMBER
                  LOOP
               $)
             READSYMB()
          $)
         ELSE // NOT AN OPERATOR: EXPRESSION FINISHED
          BREAK

     $) REPEAT

     IF (ET \= 0) & (ET \= 1) & (ET \= 10000) THEN
       GOTO RELERR

     DOTEXP  := (EDT = 1) & DOTPOS
     EXPTYPE := (ET = 1 | UNKNOWN -> S.REL,
                 ET = 10000       -> S.EXT, S.ABS)
     EXPVAL := EVAL

     RETURN

RELERR:
     COMPLAIN('R')

EXPERR:
     EXPVAL, EXPTYPE := 0, S.ABS

  $)


AND CHKOP() = VALOF
  SWITCHON CH INTO
    $( CASE '+': RESULTIS OP.PLUS
       CASE '-': RESULTIS OP.MINUS
       CASE '**':RESULTIS OP.TIMES
       CASE '/': RESULTIS OP.OVER
       CASE '!': RESULTIS OP.OR
       CASE '&': RESULTIS OP.AND

       DEFAULT:  RESULTIS 0

    $)


AND READSTRCH() = VALOF
  $( LET R,C = RADIX,CH
     IF C = '<' THEN
       $( RADIX := 8
          RCH(); READSYMB()
          IF (SYMBTYPE \= S.NUMBER) |
             (CH       \= '>') THEN
            $( COMPLAIN('S')
               RADIX := R
               SKIPREST()
               RESULTIS 0
            $)
          C := SYMB
          RADIX := R
       $)
     RCH()
     RESULTIS C
  $)


AND STRINGTONUM(STRING, DEF) = VALOF
$( LET N = 0
   FOR I = 1 TO STRING%0 DO
   $( LET CH = STRING%I
      UNLESS '0'<=CH<='9' RESULTIS DEF
      N := 10*N + CH-'0'
   $)
   RESULTIS N
$)




LET READSYMB() BE
  $( // READS SYMBOL, SETTING SYMB AND SYMBTYPE
     $( SKIPLAYOUT()
        SWITCHON CH INTO

          $( CASE '*N': SYMBTYPE := S.NONE; RETURN


             CASE '0':CASE '1':CASE'2':CASE'3':CASE'4':
             CASE'5':CASE'6':CASE'7':CASE'8': CASE'9':
                SYMB := READNUM()
                SYMBTYPE := S.NUMBER
                RETURN

              CASE '(':
                SYMBTYPE := S.LBR
                BREAK

              CASE '"':
                RCH()
                SYMBTYPE := S.NUMBER
                TEST CH = '*N' THEN
                  SYMB := #15
                 ELSE
                  $( SYMB := CH
                     RCH()
                  $)
                RETURN

              CASE '+': CASE '-':
                SYMBTYPE := S.MONOP
                SYMB := (CH = '+' -> OP.PLUS,OP.MINUS)
                BREAK

              CASE '.':
                RCH()
                UNLESS ('A' <= CH <= 'Z') |
                       ('0' <= CH <= '9') |
                       (CH     =     '.') THEN
                  $( SYMBTYPE := S.DOT
                     RETURN
                  $)
                UNRCH(); CH := '.'

              CASE'A':CASE'B':CASE'C':CASE'D':CASE'E':
              CASE'F':CASE'G':CASE'H':CASE'I':CASE'J':
              CASE'K':CASE'L':CASE'M':CASE'N':CASE'O':
              CASE'P':CASE'Q':CASE'R':CASE'S':CASE'T':
              CASE'U':CASE'V':CASE'W':CASE'X':CASE'Y':
              CASE 'Z':
                READTAG()
                RETURN

              DEFAULT:
                COMPLAIN('B')
                UNRCH(); PCH('[')
                RCH(); PCH(']')
                RCH()

           $)

        $) REPEAT

     RCH()
  $)


AND READTAG() BE
    $(
    // READS THE TAG WHOSE FIRST CHARACTER IS IN CH,
    // LOOKS IT UP IN THE SYMBOL TABLE, AND CREATES
    // AN ENTRY FOR IT IF NECESSARY. SYMB IS SET
    // TO POINT TO THE ENTRY; SYMBTYPE IS SET FROM
    // THE TYPE FIELD IF THE ENTRY EXISTED, OTHERWISE
    // TO S.NEW

    LET LEN = 0

    WHILE ('A' <= CH <= 'Z') | ('0' <= CH <= '9') |
          (CH = '.')
    DO
        $(
        UNLESS LEN = TAGCHARS
        THEN
            $( // ONLY KEEP FIRST TAGCHARS CHARS
            PUTBYTE(TAGV, LEN, CH)
            LEN := LEN + 1
            $)

        RCH()
        $)

    // PAD OUT WITH SPACES
    FOR K = LEN TO TAGBYTEUPB DO PUTBYTE(TAGV, K, ' ')

    LOOKUP()
    $)


AND READNUM() = VALOF
  $( LET V = VEC 40
     LET N,R,P = 0,RADIX,-1

     $( P := P + 1
        PUTBYTE(V,P,CH)
        RCH()
     $) REPEATWHILE '0' <= CH <= '9'

     IF CH = '.' THEN
       $( R := 10; RCH() $)

     FOR J = 0 TO P DO
       $( LET D = GETBYTE(V,J) - '0'
          TEST 0 <= D < R THEN
            N := N * R + D
           ELSE
            COMPLAIN('N')
       $)

     RESULTIS N
  $)



AND CHANGEMODE(MODE) BE
  IF MODE \= LOCMODE THEN
    $( TEST LOCMODE = S.ABS THEN
         $( ABSMIN := MINLOC
            ABSMAX := MAXLOC
            ABSLOC := LOCATION
            ABSRP := RELP
            MINLOC := RELMIN
            MAXLOC := RELMAX
            LOCATION := RELLOC
            CODEVEC := RELVEC
            RELOCVEC := RELRVEC
            RELP := RELRP
         $)
        ELSE
         $( RELMIN := MINLOC
            RELMAX := MAXLOC
            RELLOC := LOCATION
            RELRP := RELP
            MINLOC := ABSMIN
            MAXLOC := ABSMAX
            LOCATION := ABSLOC
            CODEVEC := ABSVEC
            RELOCVEC := ABSRVEC
            RELP := ABSRP
         $)
       LOCMODE := MODE
    $)



AND SETLOC(NEWLOC) BE
  $( IF (NEWLOC > LOCATIONLIMIT) | (NEWLOC < 0) THEN
       ERROR("LOCATION OUT OF RANGE*N")
     IF NEWLOC > MAXLOC THEN
       MAXLOC := NEWLOC
     LOCATION := NEWLOC
  $)



AND BUILD(V,T) BE
  $( IF PASS2 THEN
       $( CODEVEC ! LOCATION := V
          IF T = S.REL THEN
            RELOCVEC ! RELP := LOCATION
          IF T = S.EXT THEN
            // ADD REFERENCE TO EXTERNAL SYMBOL
            $( LET V = NEWVEC(2)
               V!0 := ST.VALUE ! EXTERNAL.SYMBOL
               V!1 := LOCATION
               V!2 := LOCMODE
               ST.VALUE ! EXTERNAL.SYMBOL := V
            $)
       $)
     IF T = S.REL THEN
       RELP := RELP + 1
     IF LOCATION < MINLOC THEN
       MINLOC := LOCATION
     SETLOC(LOCATION + 1)
  $)



AND OUTCODE() BE
  $( // OUTPUT THE CODE.

     SELECTOUTPUT(CODESTREAM)

     // FIRST THE RELOCATABLE HUNK.
     OUTWORD(T.HUNK)
     OUTWORD(RELMAX)
     WRITEWORDS(RELVEC,RELMAX)

     // SECOND THE RELOCATION INFO.
     IF RELRP > 0 THEN
       $( OUTWORD(T.RELOC)
          OUTWORD(RELRP)
          WRITEWORDS(RELRVEC,RELRP)
       $)

     // NEXT THE EXTERNAL SYMBOL INFORMATION
     //  FOR THE RELOCATABLE HUNK.
     OUTEXT(S.REL)

     // NEXT THE ABSOLUTE HUNK.
     IF ABSMAX \= 0 THEN
       $( OUTWORD(T.ABSHUNK)
          OUTWORD(ABSMIN)
          OUTWORD(ABSMAX - ABSMIN)

     WRITEWORDS(ABSVEC + ABSMIN,ABSMAX - ABSMIN)
       $)

     // NEXT THE ABSHUNK RELOC. INFO
     IF ABSRP > 0 THEN
       $( OUTWORD(T.ABSREL)
          OUTWORD(ABSRP)
          WRITEWORDS(ABSRVEC,ABSRP)
       $)

     // FINALLY THE EXTERNAL SYMBOL INFORMATION
     //  FOR THE ABSOLUTE HUNK.
     OUTEXT(S.ABS)

     OUTWORD(T.END)

     ENDWRITE()

  $)


AND OUTWORD(N) BE WRITEWORDS(@ N,1)


AND OUTEXT(RELTYPE) BE
  // OUTPUT THE EXTERNAL SYMBOL INFORMATION.
  // THIS INFORMATION IS OF TWO TYPES:
  //  1. DEFINITION OF EXTERNAL SYMBOLS
  //  2. REFERENCES TO EXTERNAL SYMBOLS
  // THE FIRST TYPE IS PRODUCED WHEN RELTYPE
  //  IS S.REL, FOR SYMBOLS WITH BOTH
  //  ABSOLUTE AND RELOCATABLE VALUES.
  // THE SECOND TYPE IS PRODUCED WHEN THE
  //  REFERENCE TYPE MATCHES RELTYPE.
  $( LET EXTOUT = NO
     FOR J = 0 TO TAGTABLESIZE - 1 DO
       $( LET S = TAGTABLE ! J
          UNTIL S = 0 DO
            $( LET T1 = ST.TYPE ! S
               LET T2 = T1 & ST.TYPE.MASK
               TEST (T1 & STB.ENT) \= 0 &
                    RELTYPE = S.REL THEN
                 $( UNLESS EXTOUT THEN
                      $( OUTWORD(T.EXT)
                         EXTOUT := YES
                      $)
                    OUTNAME(S, T2, YES, ST.VALUE ! S)
                 $)
                ELSE
                 IF T2 = S.EXT THEN
                   $( LET EC = 0
                      FOR J = 0 TO 1 DO
                        $( LET L = ST.VALUE ! S
                           UNTIL L = 0 DO
                             $( IF L!2 = RELTYPE THEN
                                  TEST J = 0 THEN
                                    EC := EC + 1
                                   ELSE
                                    OUTWORD(L!1)
                                L := !L
                             $)
                           IF (J = 0) & (EC \= 0) THEN
                             $( UNLESS EXTOUT THEN
                                  $( OUTWORD(T.EXT)
                                     EXTOUT := YES
                                  $)
                                OUTNAME(S, 0, NO, EC)
                             $)
                        $)
                   $)
               S := !S
            $)
       $)
     IF EXTOUT THEN
       OUTWORD(0)
  $)


AND OUTNAME(S, TYPE, NAME, VALUE) BE
  $( LET V = NAME -> (TYPE = S.REL -> EXT.ENTREL,
                                      EXT.ENTABS
                     ), EXT.EXT
     FOR J = 0 TO 6 DO
       $( LET C = (J >= TAGCHARS -> ' ', (S + 1) % J)
          V := (V << 8) + C
          IF (J & 1) = 0 THEN
            $( OUTWORD(V)
               V := 0
            $)
       $)
     OUTWORD(VALUE)
  $)





AND RCH() BE
  $( TEST CHARPOS >= 73 THEN
       $( CHARPOS := 0; SKIPREST() $)
      ELSE
       $( CH := RDCH()
          IF CH = ENDSTREAMCH THEN
            TEST INHDR THEN
              $( INHDR := NO
                 ENDREAD()
                 HDRSTREAM := 0
                 SELECTINPUT(SOURCESTREAM)
                 CH := RDCH()
              $)
             ELSE
              $( ENDED := YES
                 CH := '*N'
              $)
          PCH(CH)
          CHARPOS := CHARPOS + 1
       $)
     IF CH = '*N' THEN
       CHARPOS := 1
  $)


AND UNRCH() BE
  $( UNRDCH()
     OUTBUFP := OUTBUFP - 1
  $)


AND PUTLOC() BE
  $( PUTN(6,LOCATION)
     IF LOCMODE = S.ABS THEN
       PUTBYTE(OUTBUF,12,'#')
  $)


AND PUTVAL(V,T) BE
  $( PUTN(14,V)
     IF T = S.REL THEN
       PUTBYTE(OUTBUF,20,'*'')
     IF T = S.EXT THEN
       PUTBYTE(OUTBUF,20,'X')
  $)

AND PUTN(P,N) BE
  FOR J = P + 5 TO P BY -1 DO
    $( PUTBYTE(OUTBUF,J,(N & 7) + '0')
       N := N >> 3
    $)


AND PCH(CH) BE
  TEST OUTBUFP > OUTBUFLIM THEN
    WARN('L')
   ELSE
    $( PUTBYTE(OUTBUF,OUTBUFP,CH)
       OUTBUFP := OUTBUFP + 1
    $)

AND DECLARE(WORDS) BE
    $(
    // TAKES THE WORDS SEPARATED BY '/' FROM THE
    // STRING WORDS, AND CREATES SYMBOL TABLE ENTRIES
    // FOR THEM.
    // A NULL WORD MARKS THE END OF WORDS.
    // THE VALUES AND TYPES ARE TAKEN FROM VALPNTR
    // AND TYPEPNTR.

    LET I, LENGTH = 1, 0

        $(  // MAIN LOOP
        LET CH = GETBYTE(WORDS, I)

        TEST CH = '/'
        THEN
            $(  // HAVE READ A COMPLETE WORD
            IF LENGTH = 0 THEN RETURN

            // FILL REST OF TAG AREA WITH SPACES
            FOR K = LENGTH TO TAGBYTEUPB
            DO PUTBYTE(TAGV, K, ' ')

            LOOKUP()
            SYMB!ST.VALUE := !VALPNTR
            VALPNTR := VALPNTR + 1
            SYMB!ST.TYPE := !TYPEPNTR | STB.SETEVER |
                                        STB.SETNOW
            TYPEPNTR := TYPEPNTR + 1
            LENGTH := 0
            $)
        ELSE
            $(  // READ NEXT CHARACTER, TRUSTING THAT NO
                // WORD IS LONGER THAN TAGCHARS CHARS
            PUTBYTE(TAGV, LENGTH, CH)
            LENGTH := LENGTH + 1
            $)

        I := I + 1
        $) REPEAT
    $)

AND LOOKUP() BE
    $(
    // LOOKS UP THE TAG IN TAGV IN THE SYMBOL TABLE.
    // IF IT IS NOT ALREADY THERE, A NEW ENTRY IS
    // CREATED, WITH TYPE S.NEW AND VALUE ZERO.
    // RETURNS WITH SYMB POINTING TO THE ENTRY.

    LET HASHVAL = ((TAGV!0 + TAGV!(TAGSIZE-1)) >> 1) REM
                  TAGTABLESIZE
    LET I = 0

    SYMB := TAGTABLE!HASHVAL

    UNTIL (SYMB = 0) | (I >= TAGSIZE)
    DO
        $(
        TEST SYMB!(I + 1) = TAGV!I
        THEN I := I + 1
        ELSE SYMB, I := !SYMB, 0
        $)

        TEST SYMB = 0
        THEN
            $(  // NEED TO CREATE A NEW ENTRY
            IF PASS2 THEN COMPLAIN('U')
            SYMB := NEWVEC(TAGSIZE + 2)
            !SYMB := TAGTABLE!HASHVAL   // LINK IT IN
            TAGTABLE!HASHVAL := SYMB

            // AND COPY IN THE TAG
            FOR K = 0 TO TAGSIZE - 1
            DO SYMB!(K + 1) := TAGV!K

            // SET INITIAL VALUE AND TYPE
            SYMB!ST.TYPE := S.NEW |  STB.TEMP
            SYMB!ST.VALUE := 0
            $)
        ELSE
            IF (ST.TYPE ! SYMB & STB.SETEVER) = 0 THEN
              IF PASS2 THEN COMPLAIN('U')
    SYMBTYPE := (SYMB ! ST.TYPE) & ST.TYPE.MASK
    $)




AND NEWVEC(N) = VALOF
    $(
    // ALLOCATES A VECTOR WITH UPPERBOUND N FROM STVEC.
    STVECP := STVECP - N - 1
    IF (STVECP - STVEC) <= 0
    THEN ERROR("NOT ENOUGH ROOM FOR SYMBOL TABLE*N")

    RESULTIS STVECP
    $)




AND REPORTUNDEFS() BE
  $( // PRINT LIST OF UNDEFINED SYMBOLS
     LET MESSNOTOUT, NUMONLINE = YES,0

     FOR J = 0 TO TAGTABLESIZE - 1 DO
       $( LET P = TAGTABLE ! J
          UNTIL P = 0 DO
            $( IF (ST.TYPE!P & ST.TYPE.MASK) = S.NEW THEN
                 $( IF MESSNOTOUT THEN
                      $( MESSNOTOUT := NO
                         WRITES("*N*NTHE FOLLOWING SYMBOL*
                            *(S) ARE UNDEFINED:*N*N")
                      $)
                    WRITES("   ")
                    FOR J = 0 TO TAGCHARS - 1 DO
                      WRCH(GETBYTE(@ P ! 1,J))
                    NUMONLINE :=NUMONLINE + 1
                    IF NUMONLINE >= 8 THEN
                      $( NEWLINE()
                         NUMONLINE := 0
                      $)
                 $)
               P := !P
            $)
       $)
     IF NUMONLINE \= 0 THEN NEWLINE()
  $)



AND CLEARBITS() BE
  FOR J = 0 TO TAGTABLESIZE - 1 DO
    $( LET P = TAGTABLE ! J
       UNTIL P = 0 DO
         $( IF (ST.TYPE ! P & STB.TEMP) > 0 THEN
              ST.TYPE ! P := ST.TYPE ! P &
                             NOT STB.SETNOW
            P := !P
         $)
    $)


AND ERROR(B) BE
  $( WRITES("*N*N")
     WRITES(B)
     FREEVECTORS()
     TIDY.UP.ON.FAIL()
  $)



AND DECLSYSWORDS() BE
  $(  // DITTO.........



   VALPNTR := TABLE #020000, #040000,
                    #010000, #014000, #000000, #004000,

                    #102000, #102100, #102200, #102300,
                    #102060, #102160, #102260, #102360,
                    #102040, #102140, #102240, #102340,
                    #102020, #102120, #102220, #102320,

                    #103000, #103100, #103200, #103300,
                    #103060, #103160, #103260, #103360,
                    #103040, #103140, #103240, #103340,
                    #103020, #103120, #103220, #103320,

                    #103400, #103500, #103600, #103700,
                    #103460, #103560, #103660, #103760,
                    #103440, #103540, #103640, #103740,
                    #103420, #103520, #103620, #103720,

                    #100000, #100100, #100200, #100300,
                    #100060, #100160, #100260, #100360,
                    #100040, #100140, #100240, #100340,
                    #100020, #100120, #100220, #100320,

                    #101400, #101500, #101600, #101700,
                    #101460, #101560, #101660, #101760,
                    #101440, #101540, #101640, #101740,
                    #101420, #101520, #101620, #101720,

                    #101000, #101100, #101200, #101300,
                    #101060, #101160, #101260, #101360,
                    #101040, #101140, #101240, #101340,
                    #101020, #101120, #101220, #101320,

                    #100400, #100500, #100600, #100700,
                    #100460, #100560, #100660, #100760,
                    #100440, #100540, #100640, #100740,
                    #100420, #100520, #100620, #100720,

                    #102400, #102500, #102600, #102700,
                    #102460, #102560, #102660, #102760,
                    #102440, #102540, #102640, #102740,
                    #102420, #102520, #102620, #102720,

                    #060400, #060600, #060700, #060500,
                    #061000, #061200, #061300, #061100,
                    #061400, #061600, #061700, #061500,
                    #062000, #062200, #062300, #062100,
                    #062400, #062600, #062700, #062500,
                    #063000, #063200, #063300, #063100,

                    #060000, #060200, #060300, #060100,
                    #063500, #063400, #063700, #063600,
                    #060477, #061477, #062077,
                    #062677, #060177, #060277, #063077,
                    #073301, #073101,

                    // DIRECTIVE VALUES.

                    D.BLK,   D.UNIMP, D.UNIMP, D.UNIMP,
                    D.UNIMP, D.UNIMP, D.UNIMP, D.UNIMP,
                    D.UNIMP, D.UNIMP, D.END,   D.UNIMP,
                    D.ENT,   D.UNIMP, D.UNIMP, D.UNIMP,
                    D.EXTN,  D.UNIMP, D.UNIMP, D.UNIMP,
                    D.UNIMP, D.UNIMP, D.UNIMP, D.UNIMP,
                    D.LOC,   D.NREL,  D.RDX,   D.TITL,
                    D.TXT,   D.UNIMP, D.UNIMP, D.UNIMP,
                    D.TXTM,  D.TXTN,  D.UNIMP, D.UNIMP,
                    D.ZREL,  D.LIST,

                    // DEVICE CODES

                    #01, #10, #11, #12, #13, #14, #15,
                    #16, #17, #20, #21, #22, #23, #24,
                    #30, #31, #32, #33, #50, #51, #52,
                    #53, #57, #60, #62, #70, #73, #77,

                    // SKIP MNEMONICS

                    1, 2, 3, 4, 5, 6, 7

   TYPEPNTR := TABLE STI.MRA, STI.MRA,
                     STI.MR,  STI.MR,  STI.MR,  STI.MR,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,
                     STI.ALC, STI.ALC, STI.ALC, STI.ALC,

                     STI.IOA, STI.IOA, STI.IOA, STI.IOA,
                     STI.IOA, STI.IOA, STI.IOA, STI.IOA,
                     STI.IOA, STI.IOA, STI.IOA, STI.IOA,
                     STI.IOA, STI.IOA, STI.IOA, STI.IOA,
                     STI.IOA, STI.IOA, STI.IOA, STI.IOA,
                     STI.IOA, STI.IOA, STI.IOA, STI.IOA,

                     STI.IO,  STI.IO,  STI.IO,  STI.IO,
                     STI.IO,  STI.IO,  STI.IO,  STI.IO,
                     STI.ION, STI.ION, STI.ION,
                     STI.ZOP, STI.ZOP, STI.ZOP, STI.ZOP,
                     STI.ZOP, STI.ZOP,

                     // DIRECTIVES (38 - I HOPE)

                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,   S.DIR,   S.DIR,
                     S.DIR,   S.DIR,

                     // DEVICES (ABSOLUTE SYMBOLS)

                     S.ABS,   S.ABS,   S.ABS,   S.ABS,
                     S.ABS,   S.ABS,   S.ABS,   S.ABS,
                     S.ABS,   S.ABS,   S.ABS,   S.ABS,
                     S.ABS,   S.ABS,   S.ABS,   S.ABS,
                     S.ABS,   S.ABS,   S.ABS,   S.ABS,
                     S.ABS,   S.ABS,   S.ABS,   S.ABS,
                     S.ABS,   S.ABS,   S.ABS,   S.ABS,

                     // SKIP MNEMONICS FOR ALC'S

                     S.SKPMN, S.SKPMN, S.SKPMN, S.SKPMN,
                     S.SKPMN, S.SKPMN, S.SKPMN, S.SKPMN


   DECLARE("LDA/STA/ISZ/DSZ/JMP/JSR//")

   DECLARE("ADC/ADCL/ADCR/ADCS/*
           *ADCC/ADCCL/ADCCR/ADCCS/*
           *ADCO/ADCOL/ADCOR/ADCOS/*
           *ADCZ/ADCZL/ADCZR/ADCZS/*
           *ADD/ADDL/ADDR/ADDS/*
           *ADDC/ADDCL/ADDCR/ADDCS/*
           *ADDO/ADDOL/ADDOR/ADDOS/*
           *ADDZ/ADDZL/ADDZR/ADDZS//")

   DECLARE("AND/ANDL/ANDR/ANDS/*
           *ANDC/ANDCL/ANDCR/ANDCS/*
           *ANDO/ANDOL/ANDOR/ANDOS/*
           *ANDZ/ANDZL/ANDZR/ANDZS/*
           *COM/COML/COMR/COMS/*
           *COMC/COMCL/COMCR/COMCS/*
           *COMO/COMOL/COMOR/COMOS/*
           *COMZ/COMZL/COMZR/COMZS//")

   DECLARE("INC/INCL/INCR/INCS/*
           *INCC/INCCL/INCCR/INCCS/*
           *INCO/INCOL/INCOR/INCOS/*
           *INCZ/INCZL/INCZR/INCZS/*
           *MOV/MOVL/MOVR/MOVS/*
           *MOVC/MOVCL/MOVCR/MOVCS/*
           *MOVO/MOVOL/MOVOR/MOVOS/*
           *MOVZ/MOVZL/MOVZR/MOVZS//")

   DECLARE("NEG/NEGL/NEGR/NEGS/*
           *NEGC/NEGCL/NEGCR/NEGCS/*
           *NEGO/NEGOL/NEGOR/NEGOS/*
           *NEGZ/NEGZL/NEGZR/NEGZS/*
           *SUB/SUBL/SUBR/SUBS/*
           *SUBC/SUBCL/SUBCR/SUBCS/*
           *SUBO/SUBOL/SUBOR/SUBOS/*
           *SUBZ/SUBZL/SUBZR/SUBZS//")


   DECLARE("DIA/DIAC/DIAP/DIAS/*
           *DOA/DOAC/DOAP/DOAS/*
           *DIB/DIBC/DIBP/DIBS/*
           *DOB/DOBC/DOBP/DOBS/*
           *DIC/DICC/DICP/DICS/*
           *DOC/DOCC/DOCP/DOCS//")

   DECLARE("NIO/NIOC/NIOP/NIOS/*
           *SKPBZ/SKPBN/SKPDZ/SKPDN/*
           *READS/INTA/MSKO/*
           *IORST/INTEN/INTDS/HALT/*
           *MUL/DIV//")


   // ASSEMBLER DIRECTIVES: MOST ARE IGNORED.

   DECLARE(".BLK/.COMM/.CSIZ/.DALC/.DIAC/*
           *.DIO/.DIOA/.DMR/.DMRA/.DUSR/*
           *.END/.ENDC/.ENT/.ENTO/.EOT/*
           *.EXTD/.EXTN/.EXTU/.GADD/.GLOC/*
           *.IFE/.IFG/.IFL/.IFN/.LOC/.NREL//")

   DECLARE(".RDX/.TITL/.TXT/.TXTE/.TXTF/*
           *.TXTF/.TXTM/.TXTN/.TXTO/.XPNG/*
           *.ZREL/.LIST//")


   // DEVICE CODES: INITIALLY ONLY A FEW.

   DECLARE("MDV/TTI/TTO/PTR/PTP/RTC/PLT/CDR/*
           *LPT/DSK/ADCV/MTA/DACV/DCM/QTY/*
           *IBM1/IBM2/DKP/TTI1/TTO1/PTR1/PTP1/*
           *LPT1/DSK1/MTA1/QTY1/DKP1/CPU//")


   // SKIP MNEMONICS FOR ALC INSTRUCTIONS

   DECLARE("SKP/SZC/SNC/SZR/SNR/SEZ/SBN//")
  $)


