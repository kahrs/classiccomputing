/* Copyright (c) 2012 Robert Nordier.  All rights reserved. */

#include "oc.h"

/*
 * Byte 0: Unused here
 * Byte 1: Pops from stack (0, 1, 2)
 * Byte 2: Pushes onto stack (0, 1, 2 = set to n, 3 = set to n + 1)
 * Byte 3: 0-7: force; 1-7: loadreg; 8: load op; 9: data op
 */

const char optab[][OPATTR] = {
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {1, 0, 1, 8},   /* TRUE */
    {1, 0, 1, 8},   /* FALSE */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {1, 1, 1, 3},   /* RV */
    {0, 0, 0, 0},
    {2, 1, 3, 1},   /* FNAP */
    {1, 2, 1, 5},   /* MULT */
    {1, 2, 1, 5},   /* DIV */
    {1, 2, 1, 5},   /* REM */
    {1, 2, 1, 4},   /* PLUS */
    {1, 2, 1, 4},   /* MINUS */
    {1, 0, 1, 8},   /* QUERY */
    {1, 1, 1, 3},   /* NEG */
    {0, 0, 0, 0},
    {1, 1, 1, 3},   /* ABS */
    {1, 2, 1, 4},   /* EQ */
    {1, 2, 1, 4},   /* NE */
    {1, 2, 1, 4},   /* LS */
    {1, 2, 1, 4},   /* GR */
    {1, 2, 1, 4},   /* LE */
    {1, 2, 1, 4},   /* GE */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {1, 1, 1, 3},   /* NOT */
    {1, 2, 1, 6},   /* LSHIFT */
    {1, 2, 1, 6},   /* RSHIFT */
    {1, 2, 1, 4},   /* LOGAND */
    {1, 2, 1, 4},   /* LOGOR */
    {1, 2, 1, 4},   /* EQV */
    {1, 2, 1, 4},   /* NEQV */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {2, 0, 1, 8},   /* LP */
    {2, 0, 1, 8},   /* LG */
    {2, 0, 1, 8},   /* LN */
    {4, 0, 1, 8},   /* LSTR */
    {3, 0, 1, 8},   /* LL */
    {2, 0, 1, 8},   /* LLP */
    {2, 0, 1, 8},   /* LLG */
    {3, 0, 1, 8},   /* LLL */
    {0, 0, 0, 0},
    {4, 0, 0, 0},   /* SECTION */
    {0, 0, 0, 0},
    {2, 1, 2, 1},   /* RTAP */
    {1, 1, 0, 1},   /* GOTO */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {1, 0, 0, 0},   /* FINISH */
    {0, 0, 0, 0},
    {7, 1, 0, 3},   /* SWITCHON */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {6, 0, 0, 0},   /* GLOBAL */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {2, 1, 0, 2},   /* SP */
    {2, 1, 0, 2},   /* SG */
    {3, 1, 0, 2},   /* SL */
    {1, 2, 0, 6},   /* STIND */
    {0, 0, 0, 0},
    {3, 0, 0, 0},   /* JUMP */
    {3, 1, 0, 3},   /* JT */
    {3, 1, 0, 3},   /* JF */
    {3, 2, 0, 4},   /* ENDFOR */
    {3, 0, 0, 0},   /* BLAB */
    {3, 0, 0, 0},   /* LAB */
    {2, 0, 2, 0},   /* STACK */
    {1, 0, 0, 0},   /* STORE */
    {2, 0, 3, 0},   /* RSTACK */
    {5, 0, 0, 0},   /* ENTRY */
    {2, 0, 2, 0},   /* SAVE */
    {1, 1, 0, 3},   /* FNRN */
    {1, 0, 0, 0},   /* RTRN */
    {3, 1, 0, 3},   /* RES */
    {0, 0, 0, 0},
    {3, 0, 0, 9},   /* DATALAB */
    {3, 0, 0, 9},   /* ITEML */
    {2, 0, 0, 9},   /* ITEMN */
    {2, 0, 0, 0},   /* ENDPROC */
    {1, 0, 0, 0},   /* END */
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {1, 2, 1, 4},   /* GETBYTE */
    {1, 2, 0, 4}    /* PUTBYTE (actually 3 args) */
};
