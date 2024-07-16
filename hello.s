; a "hello, world" program for Nova running RDOS
    ; uses PCHAR system call
    .titl hello
    .nrel
    .ent start

 start:
 dochar:
    lda    0,@pmsg  ; load ac0 with next character,
    mov#   0,0,snr  ; test ac0; skip if nonzero (don't load result)
    jmp    done
    .systm
    .pchar          ; print first
    jmp    er       ; skipped if OK
    movs   0,0      ; swap bytes
    .systm
    .pchar          ; print second
    jmp    er       ; skipped if OK
    isz    pmsg     ; point to next character
    jmp    dochar   ; go around again

 done:
    .systm          ; normal exit
    .rtn
 er:
    .systm          ; error exit
    .ertn
    halt

 pmsg:
    .+1             ; pointer to first character of string
                    ; note bytes are packed right-to-left by default
                    ; <15><12> denotes a CR LF pair.
    .txt /Hello, world.<15><12>/
    0               ; flag word to end string

    .end start
