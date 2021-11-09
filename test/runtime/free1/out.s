
.pow
    resn  2
    push 1
    dup
    set  3 ; res
    drop
    push 0
    dup
    set  2 ; i
    drop
.start_loop_1
    get  2 ; i
    get  1 ; exp
    cmplt
    not
    jumpf cond_end_1
    jump end_loop_1
.cond_end_1
    get  3 ; res
    get  0 ; nb
    mul
    dup
    set  3 ; res
    drop
.cont_loop_1
    get  2 ; i
    push 1
    add
    dup
    set  2 ; i
    drop
    jump start_loop_1
.end_loop_1
    get  3 ; res
    ret
    push 0
    ret

.local_printf
    resn  1
    get  0 ; nb
    push 0
    cmpne
    jumpf cond_end_2
    get  0 ; nb
    push 10
    mod
    dup
    set  1 ; c
    drop
    get  0 ; nb
    push 10
    div
    dup
    set  0 ; nb
    drop
    prep local_printf
    get  0 ; nb
    call  1
    drop
    prep putchar
    get  1 ; c
    push 48
    add
    call  1
    drop
.cond_end_2
    push 0
    ret

.printf
    resn  0
    get  0 ; nb
    push 0
    cmpeq
    jumpf cond_else_3
    prep putchar
    push 48
    call  1
    drop
    jump cond_end_3
.cond_else_3
    get  0 ; nb
    push 0
    cmplt
    jumpf cond_else_4
    prep putchar
    push 45
    call  1
    drop
    prep local_printf
    push 0
    get  0 ; nb
    sub
    call  1
    drop
    jump cond_end_4
.cond_else_4
    prep local_printf
    get  0 ; nb
    call  1
    drop
.cond_end_4
.cond_end_3
    prep putchar
    push 10
    call  1
    drop
    push 0
    ret

.scanf
    resn  2
    push 0
    dup
    set  1 ; res
    prep getchar
    call  0
    dup
    set  0 ; c
    drop
.start_loop_2
.cont_loop_2
    get  0 ; c
    push 10
    cmpne
    jumpf cond_else_5
    get  1 ; res
    push 10
    mul
    get  0 ; c
    push 48
    sub
    add
    dup
    set  1 ; res
    drop
    prep getchar
    call  0
    dup
    set  0 ; c
    drop
    jump cond_end_5
.cond_else_5
    jump end_loop_2
.cond_end_5
    jump start_loop_2
.end_loop_2
    get  1 ; res
    ret
    push 0
    ret

.abs
    resn  0
    get  0 ; nb
    push 0
    cmpge
    jumpf cond_else_6
    get  0 ; nb
    ret
    jump cond_end_6
.cond_else_6
    push 0
    get  0 ; nb
    sub
    ret
.cond_end_6
    push 0
    ret

.malloc
    resn  11
    get  0 ; s
    push 2
    cmplt
    jumpf cond_end_7
    push 2
    dup
    set  0 ; s
    drop
.cond_end_7
    push 0
    read
    dup
    set  1 ; firstFree
.start_loop_3
    get  1 ; firstFree
    push 0
    push 1
    sub
    add
    read
    dup
    set  2 ; blockSize
    get  2 ; blockSize
    get  0 ; s
    cmpge
    jumpf cond_end_8
    get  2 ; blockSize
    get  1 ; firstFree
    get  2 ; blockSize
    add
    read
    cmpne
    jumpf cond_end_9
    prep printf
    push 0
    push 1
    sub
    call  1
    drop
    push 1
    push 0
    div
    dup
    set  3 ; err
.cond_end_9
    get  2 ; blockSize
    get  0 ; s
    push 4
    add
    cmple
    jumpf cond_else_10
    push 0
    get  2 ; blockSize
    sub
    dup
    get  1 ; firstFree
    push 0
    push 1
    sub
    add
    write
    drop
    push 0
    get  2 ; blockSize
    sub
    dup
    get  1 ; firstFree
    get  2 ; blockSize
    add
    write
    drop
    get  1 ; firstFree
    push 0
    add
    read
    dup
    set  4 ; LeftBlockPtr
    get  1 ; firstFree
    get  2 ; blockSize
    push 1
    sub
    add
    read
    dup
    set  5 ; RightBlockPtr
    get  4 ; LeftBlockPtr
    jumpf cond_else_11
    get  4 ; LeftBlockPtr
    push 0
    push 1
    sub
    add
    read
    dup
    set  6 ; LeftBlockSize
    get  5 ; RightBlockPtr
    dup
    get  4 ; LeftBlockPtr
    get  6 ; LeftBlockSize
    push 1
    sub
    add
    write
    drop
    jump cond_end_11
.cond_else_11
    get  5 ; RightBlockPtr
    dup
    push 0
    write
    drop
.cond_end_11
    get  5 ; RightBlockPtr
    jumpf cond_end_12
    get  4 ; LeftBlockPtr
    dup
    get  5 ; RightBlockPtr
    push 0
    add
    write
    drop
.cond_end_12
    get  1 ; firstFree
    ret
    jump cond_end_10
.cond_else_10
    get  2 ; blockSize
    get  0 ; s
    sub
    push 2
    sub
    dup
    set  7 ; size
    push 0
    get  0 ; s
    sub
    dup
    get  1 ; firstFree
    push 0
    push 1
    sub
    add
    write
    drop
    push 0
    get  0 ; s
    sub
    dup
    get  1 ; firstFree
    get  0 ; s
    add
    write
    drop
    get  1 ; firstFree
    get  0 ; s
    add
    push 2
    add
    dup
    set  8 ; newBlockPtr
    get  7 ; size
    dup
    get  8 ; newBlockPtr
    get  7 ; size
    add
    write
    drop
    get  7 ; size
    dup
    get  8 ; newBlockPtr
    push 0
    push 1
    sub
    add
    write
    drop
    get  1 ; firstFree
    push 0
    add
    read
    dup
    set  9 ; LeftBlockPtr
    get  1 ; firstFree
    get  2 ; blockSize
    push 1
    sub
    add
    read
    dup
    set  10 ; RightBlockPtr
    get  9 ; LeftBlockPtr
    jumpf cond_else_13
    get  9 ; LeftBlockPtr
    push 0
    push 1
    sub
    add
    read
    dup
    set  11 ; LeftBlockSize
    get  8 ; newBlockPtr
    dup
    get  9 ; LeftBlockPtr
    get  11 ; LeftBlockSize
    push 1
    sub
    add
    write
    drop
    jump cond_end_13
.cond_else_13
    get  8 ; newBlockPtr
    dup
    push 0
    write
    drop
.cond_end_13
    get  10 ; RightBlockPtr
    jumpf cond_end_14
    get  8 ; newBlockPtr
    dup
    get  10 ; RightBlockPtr
    push 0
    add
    write
    drop
.cond_end_14
    get  1 ; firstFree
    ret
.cond_end_10
.cond_end_8
    get  1 ; firstFree
    push 0
    get  2 ; blockSize
    sub
    push 1
    sub
    add
    read
    dup
    set  1 ; firstFree
    drop
.cont_loop_3
    get  1 ; firstFree
    push 0
    cmpne
    not
    jumpf cond_end_15
    jump end_loop_3
.cond_end_15
    jump start_loop_3
.end_loop_3
    push 0
    ret
    push 0
    ret

.free
    resn  2
    get  0 ; p
    push 0
    cmpeq
    jumpf cond_end_16
    push 0
    ret
.cond_end_16
    push 0
    get  0 ; p
    push 0
    push 1
    sub
    add
    read
    sub
    dup
    set  1 ; size
    get  0 ; p
    push 0
    push 1
    sub
    add
    read
    push 0
    cmpge
    get  0 ; p
    push 0
    push 1
    sub
    add
    read
    get  0 ; p
    prep abs
    get  1 ; size
    call  1
    add
    read
    cmpne
    or
    jumpf cond_end_17
    push 1
    push 0
    div
    dup
    set  2 ; err
.cond_end_17
    get  1 ; size
    dup
    get  0 ; p
    get  1 ; size
    add
    write
    drop
    get  1 ; size
    dup
    get  0 ; p
    push 0
    push 1
    sub
    add
    write
    drop
    push 0
    dup
    get  0 ; p
    push 0
    add
    write
    drop
    push 0
    read
    dup
    get  0 ; p
    get  1 ; size
    push 1
    sub
    add
    write
    drop
    get  0 ; p
    dup
    push 0
    read
    write
    drop
    get  0 ; p
    dup
    push 0
    write
    drop
    push 0
    ret

.start2
    resn  0
    push 0
    read
    push 1
    add
    dup
    push 0
    write
    drop
    push 0
    dup
    push 0
    read
    write
    drop
    push 0
    dup
    push 0
    read
    push 4096
    add
    push 1
    sub
    write
    drop
    push 4096
    dup
    push 0
    read
    push 1
    sub
    write
    drop
    push 4096
    dup
    push 0
    read
    push 4096
    add
    write
    drop
    push 0
    ret
    push 0
    ret


.main
    resn  1
    prep malloc
    push 12
    call  1
    dup
    set  0 ; a
    prep printf
    get  0 ; a
    push 0
    push 1
    sub
    add
    read
    call  1
    drop
    prep printf
    get  0 ; a
    push 12
    add
    read
    call  1
    drop
    prep free
    get  0 ; a
    call  1
    drop
    prep printf
    get  0 ; a
    push 0
    push 1
    sub
    add
    read
    call  1
    drop
    prep printf
    get  0 ; a
    push 12
    add
    read
    call  1
    drop
    push 0
    ret
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

