// (C) Copyright 1979 Tripos Research Group
//     University of Cambridge
//     Computer Laboratory

GET "LIBHDR"


MANIFEST
$(
reportmax=10
$)

GET "global.b"

LET start() BE
 $( LET args = "SYS:L.BCPL-ARGS"
    LET syn  = "SYS:L.BCPL-SYN"
    LET trn  = "SYS:L.BCPL-TRN"
    LET err  = "SYS:L.BCPL-ERR"
    LET cg = 0
    LET oldoutput = output()
    LET v1 = VEC 3*reportmax
    LET v2 = VEC 14

    errvec := v1
    datvec := v2
    fromfile := 0
    ocodefile := 0

    cg := callbcplseg(args)
    UNLESS rc=0 GOTO fail

    UNLESS sourcestream=0 DO
    $( $( LET a = callbcplseg(syn)
          IF a=0 BREAK
          IF printtree DO callbcplseg(err, a)
          callbcplseg(trn, a)
          IF errcount>0 DO callbcplseg(err, 0)
       $) REPEATUNTIL ch=endstreamch | rc>=20

       endread()
       selectoutput(ocodestream)
       endwrite()
       ocodestream := 0
       selectoutput(verstream)
       freevec(treevec)
       UNLESS charcode=0 DO freevec(charcode)
    $)


    UNLESS codestream=0 DO
    $( IF rc=0 DO callbcplseg(cg)
       selectoutput(codestream)
       endwrite()
       selectoutput(verstream)
    $)

    UNLESS verstream=oldoutput DO endwrite()
fail:
    UNLESS fromfile=0 DO freevec(fromfile)
//    UNLESS ocodefile=0 DO
//    $( UNLESS keepocode DO deleteobj(ocodefile)
//       freevec(ocodefile)
//    $)
    UNLESS cg=0 DO freevec(cg)
    stop(rc)
 $)


AND callbcplseg(s, a) = VALOF
$( let overseg = loadseg(s)
   IF overseg=0 | globin(overseg)=0 DO
   $( writef("Can't load %S*N",s)
      rc := 20
      RESULTIS 0 $)
   a := start(a)
   unloadseg(overseg)
   RESULTIS a
$)

AND smallnumber(x) =  0<x<256 -> TRUE, FALSE


