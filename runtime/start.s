.start
    prep start2
    call 0
    prep main
    call 0
    halt

.putchar
    send 
    push 0
    ret

.getchar
    recv
    ret
