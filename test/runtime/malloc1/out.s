
.start2
    resn  1
    push 0
    dup
    push 0
    read
    push 1
    add
    write
    drop
    push 0
    dup
    push 0
    read
    push 4096
    add
    write
    drop
    push 4096
    dup
    push 0
    read
    write
    drop
    push 4096
    dup
    push 0
    read
    push 4097
    add
    write
    drop
    push 0
    read
    push 1
    add
    dup
    set  0
    get  0
    write
    dup
    push 0
    write
    drop
    push 0
    ret
    push 0
    ret

.pow
    resn  2
    push 1
    dup
    set  3
    drop
    push 0
    dup
    set  2
    drop
.start_loop_1
    get  2
    get  1
    cmplt
    not
    jumpf cond_end_1
    jump end_loop_1
.cond_end_1
    get  3
    get  0
    mul
    dup
    set  3
    drop
.cont_loop_1
    get  2
    push 1
    add
    dup
    set  2
    drop
    jump start_loop_1
.end_loop_1
    get  3
    ret
    push 0
    ret

.local_printf
    resn  1
    get  0
    push 0
    cmpne
    jumpf cond_end_2
    get  0
    push 10
    mod
    dup
    set  1
    drop
    get  0
    push 10
    div
    dup
    set  0
    drop
    prep local_printf
    get  0
    call  1
    drop
    prep putchar
    get  1
    push 48
    add
    call  1
    drop
.cond_end_2
    push 0
    ret

.printf
    resn  0
    get  0
    push 0
    cmpeq
    jumpf cond_else_3
    prep putchar
    push 48
    call  1
    drop
    jump cond_end_3
.cond_else_3
    get  0
    push 0
    cmplt
    jumpf cond_else_4
    prep putchar
    push 45
    call  1
    drop
    prep local_printf
    push 0
    get  0
    sub
    call  1
    drop
    jump cond_end_4
.cond_else_4
    prep local_printf
    get  0
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
    set  1
    prep getchar
    call  0
    dup
    set  0
    drop
.start_loop_2
.cont_loop_2
    get  0
    push 10
    cmpne
    jumpf cond_else_5
    get  1
    push 10
    mul
    get  0
    push 48
    sub
    add
    dup
    set  1
    drop
    prep getchar
    call  0
    dup
    set  0
    drop
    jump cond_end_5
.cond_else_5
    jump end_loop_2
.cond_end_5
    jump start_loop_2
.end_loop_2
    get  1
    ret
    push 0
    ret

.abs
    resn  0
    get  0
    push 0
    cmpge
    jumpf cond_else_6
    get  0
    ret
    jump cond_end_6
.cond_else_6
    push 0
    get  0
    sub
    ret
.cond_end_6
    push 0
    ret

.malloc
    resn  12
    get  0
    push 2
    cmplt
    jumpf cond_end_7
    push 2
    dup
    set  0
    drop
.cond_end_7
    push 0
    read
    write
    dup
    set  1
    prep printf
    get  1
    call  1
    drop
.start_loop_3
    get  1
    push 1
    sub
    write
    dup
    set  2
    prep printf
    get  2
    call  1
    drop
    get  2
    get  0
    cmpge
    jumpf cond_end_8
    get  2
    get  1
    get  2
    add
    read
    cmpne
    jumpf cond_end_9
    prep printf
    push 0
    call  1
    drop
    push 1
    push 0
    div
    dup
    set  3
.cond_end_9
    get  2
    get  0
    push 4
    add
    cmple
    jumpf cond_else_10
    push 0
    get  2
    sub
    dup
    get  1
    push 1
    sub
    write
    drop
    push 0
    get  2
    sub
    dup
    get  1
    get  2
    add
    write
    drop
    get  1
    write
    dup
    set  4
    get  4
    push 1
    sub
    write
    dup
    set  5
    get  1
    get  2
    add
    write
    dup
    set  6
    get  6
    dup
    get  4
    get  5
    add
    push 1
    sub
    write
    drop
    get  4
    dup
    get  6
    write
    drop
    get  1
    ret
    jump cond_end_10
.cond_else_10
    get  2
    get  0
    sub
    push 2
    sub
    dup
    set  7
    push 0
    get  0
    sub
    dup
    get  1
    push 1
    sub
    write
    drop
    push 0
    get  0
    sub
    dup
    get  1
    get  0
    add
    write
    drop
    get  1
    get  0
    add
    push 2
    add
    write
    dup
    set  8
    push 0
    get  7
    sub
    dup
    get  8
    get  7
    add
    write
    drop
    push 0
    get  7
    sub
    dup
    get  8
    push 1
    sub
    write
    drop
    get  1
    write
    dup
    set  9
    get  9
    push 1
    sub
    write
    dup
    set  10
    get  1
    get  2
    add
    write
    dup
    set  11
    get  11
    push 1
    sub
    write
    dup
    set  12
    get  8
    dup
    get  9
    get  10
    add
    push 1
    sub
    write
    drop
    get  8
    dup
    get  11
    write
    drop
    get  9
    dup
    get  8
    write
    drop
    get  1
    ret
.cond_end_10
.cond_end_8
    get  1
    get  2
    add
    push 1
    sub
    dup
    set  1
    drop
.cont_loop_3
    get  1
    push 0
    cmpne
    not
    jumpf cond_end_11
    jump end_loop_3
.cond_end_11
    jump start_loop_3
.end_loop_3
    push 0
    ret
    push 0
    ret

.free
    resn  0
    push 0
    ret


.main
    resn  1
    prep malloc
    push 12
    call  1
    dup
    set  0
    prep printf
    get  0
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

