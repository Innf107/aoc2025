BITS 64
GLOBAL _start

; r15 ~ number of characters leaft to read
; r14 ~ actual buffer pointer
read_char:
    cmp r15, 0
    jne .skip_refill

.refill:
    ; the buffer is empty so we need to re-fill it
    mov rax, 0 ; read(fd, input_buffer, 4096)
    mov rdi, [fd]
    mov rsi, input_buffer
    mov rdx, 4096
    syscall
    
    mov r15, rax

    test rax, rax
    jz .on_eof

    mov r14, input_buffer

.skip_refill:
    xor rax, rax
    mov al, [r14]
    inc r14
    dec r15
    ret
.on_eof:
    ret


print_unsigned_int:
    ; we assume that every integer fits into the output buffer
    
    test rax, rax
    jz .print_zero

    ; rcx ~ current pointer into the output buffer.
    ; We fill the output buffer *from the right*
    mov rcx, output_buffer
    add rcx, 4095
    mov [rcx], BYTE 10 ; we can immediately append a trailing newline
    dec rcx

.conversion_loop:
    test rax, rax
    jz .string_complete

    xor rdx, rdx
    mov rbx, 10
    div rbx

    add rdx, 48; convert the digit's value (in 0..9) to an ascii digit
    mov [rcx], BYTE dl

    dec rcx
    jmp .conversion_loop
.string_complete:
    mov rdx, output_buffer
    add rdx, 4096
    sub rdx, rcx ; rdx now contains the remaining size after rcx

    ; for simplicity, we assume that one write syscall is enough to print the result
    mov rax, 1 ; write
    mov rdi, 1 ; stdout
    mov rsi, rcx
    syscall
    ret
.print_zero:
    mov rsi, output_buffer
    mov [rsi], WORD 0x0A30 ; 0x0A30 = 0x30 0x0A (little endian) = '0\n'
    mov rax, 1 ; write
    mov rdi, 1 ; stdout
    mov rdx, 2 ; length
    syscall
    ret

; Algorithm Overview
; =====================
; We stream the input one character at a time.
; Instead of counting the number of surrounding paper rolls, we count the number of *empty* spaces.
; That way we can immediately increment our counter (and blackhole the found number) whenever we hit 5 or more empty spaces.
; We still need to make sure htat the tile we hit actually contains a paper roll however!
;
; Crucially, for this to work we need a few additional pieces of machinery
;   1) every tile at the edges (left and right) starts off with 3 adjacent blank spaces
;   2) the initial current_line_spaces array is initialized to `5 3 ... 3 5`
;   3) at the end, we iterate through the last line and increment for every value >= 2
;
; [previous_line_spaces] contains the number of spaces per tile in the previous line.
; Every empty space at position i increments previous_line_spaces[i - 1], previous_line_spaces[i] and previous_line_spaces[i + 1]
; For each of these, we increment if they have hit their threshold.
; We need to blackhole previous_line_spaces[i] and [i + 1], but not previous_line_spaces[i - 1], since
; that position will never be reached again by anything 
;
; [current_line_spaces] contains the number of spaces we already know for the current line.
; Every empty space at position i increments current_line_spaces for positions `i - 1` and `i + 1`
; We only need to increment for current_line_spaces[i - 1] since we are going to process current_line_spaces[i + 1]
; again anyway. We do need to blackhole it though
; Additionally, we can blackhole current_line_spaces[i] so that further code doesn't try to increment from it
; (which would be invalid since it is not a roll!) 
;
; [next_line_spaces] contains the effect the current line will have on the next line.
; Every empty space at position i increments next_line_spaces at position i - 1, i and i + 1
; We don't need to increment anything here

; Move all the arrays over by one
; and reset the new next_line_spaces array to 3 0 ... 0 3 to account for the edges
shift_and_reset:
    ; we move the arrays first
    mov rax, [next_line_spaces]
    mov rbx, [previous_line_spaces]
    mov rcx, [current_line_spaces]
    mov [next_line_spaces], rbx
    mov [current_line_spaces], rax
    mov [previous_line_spaces], rcx

    ; reset the sentinels
    mov BYTE [rax-1], -100
    mov BYTE [rax+line_length], -100
    mov BYTE [rbx-1], -100
    mov BYTE [rbx+line_length], -100
    mov BYTE [rcx-1], -100
    mov BYTE [rcx+line_length], -100

    ; then reset what is *now* in next_line_spaces (in rbx)
    lea rcx, [rbx+line_length] ; save the address one past the last element so we know when to stop

    .zero_loop:
    ; ymm0 is never used so it's always 0 here
    vmovdqu [rbx], ymm0
    add rbx, 32
    cmp rbx, rcx
    jl .zero_loop

    mov rbx, [next_line_spaces]

    ; fill in the edges
    mov BYTE [rbx], 3
    mov BYTE [rbx+line_length - 1], 3
    ret

process_next_line:
    xor r13, r13 ; r13 ~ index into the current line

    call read_char
    cmp rax, 0 ; EOF
    jne .after_read
    ret

.line_loop:
    inc r13
    call read_char
    cmp rax, 0xA ; '\n'
    je .end_of_line
.after_read:
    cmp rax, 64 ; '@'
    je .check_roll
    ; we have hit a space ('.') and need to increment
    ; current_line_spaces[i + 1], current_line_spaces[i - 1],
    ; next_line_spaces[i - 1], next_line_spaces[i], next_line_spaces[i + 1]
    ; previous_line_spaces[i - 1], previous_line_spaces[i] and previous_line_spaces[i + 1]
    mov rbx, [next_line_spaces]
    inc BYTE [rbx+r13-1]
    inc BYTE [rbx+r13]
    inc BYTE [rbx+r13+1]

    mov rbx, [current_line_spaces]
    mov rcx, [previous_line_spaces]
    add rbx, r13
    add rcx, r13
    ; blackhole the current tile since it is not a roll
    mov BYTE [rbx], -100
    inc BYTE [rbx+1]

    inc BYTE [rbx-1]
    inc BYTE [rcx-1]
    inc BYTE [rcx]
    inc BYTE [rcx+1]

    cmp BYTE [rbx-1], 5
    jl .skip_cur_n1
    inc QWORD [number_reachable]
    mov BYTE [rbx-1], -100
.skip_cur_n1:
    cmp BYTE [rcx-1], 5
    jl .skip_prev_n1
    inc QWORD [number_reachable]
    ; we don't need to blackhole this one
.skip_prev_n1:
    cmp BYTE [rcx], 5
    jl .skip_prev
    inc QWORD [number_reachable]
    mov BYTE [rcx], -100
.skip_prev:
    cmp BYTE [rcx+1], 5
    jl .line_loop
    inc QWORD [number_reachable]
    mov BYTE [rcx+1], -100
    jmp .line_loop

.check_roll:
    ; We only need to check if we have already hit the threshhold (5) here.
    mov rcx, [current_line_spaces]
    add rcx, r13

    cmp BYTE [rcx], 5
    jl .line_loop
    inc QWORD [number_reachable]
    mov BYTE [rcx], -100
    
    jmp .line_loop

.end_of_line:
    call shift_and_reset
    jmp process_next_line


process_remaining_in_last_line:
    mov rbx, [previous_line_spaces]
    lea rcx, [rbx+line_length]

    ; we only need to check if they're at least 2 now
.check_loop:
    cmp rbx, rcx
    jge .done

    cmp BYTE [rbx], 2
    jl .skip_inc
    inc QWORD [number_reachable]
.skip_inc:
    inc rbx
    jmp .check_loop
.done:
    ret

_start:
    ; initialize the input
    mov rax, 2 ; fd = open(input_file, O_RDONLY, 0)
    mov rdi, input_file
    xor rsi, rsi ; flags = 0
    xor rdx, rdx ; mode = 0
    syscall
    mov [fd], rax

    ; Initialize the initial array pointers. We will swap these after every line
    mov rax, linearray1
    mov [current_line_spaces], rax
    mov rax, linearray2
    mov [next_line_spaces], rax
    mov rax, linearray3
    mov [previous_line_spaces], rax

    call process_next_line

    call process_remaining_in_last_line

    mov rax, [number_reachable]
    call print_unsigned_int

    mov rax, 60
    mov rdi, 0
    syscall


section .data
    align 8
    input_buffer: times 4096 db 0
    input_file: db "input.txt", 0
    
    align 8
    output_buffer: times 4096 db 0
    align 8
    fd: dq 0

    line_length: equ 137

    ; (initially current_line_spaces)
    db 0 ; we include an unused 0 so that we can write just before the array without corrupting anything
    align 8, db 0
    linearray1: db 5
                times (line_length - 2) db 3
                db 5
                times 128 db 0 ; include a bit more space for good measure (and so we can easily zero more than one byte at a time)

    ; (initially next_line_spaces)
    db 0 ; we include an unused 0 so that we can write just before the array without corrupting anything
    align 8, db 0
    linearray2: db 3
                times (line_length - 2) db 0
                db 3
                times 128 db 0 ; include a bit more space for good measure (and so we can easily zero more than one byte at a time)

    ; (initially previous_line_spaces)
    db 0 ; we include an unused 0 so that we can write just before the array without corrupting anything
    align 8, db 0
    linearray3: times (line_length) db 0
                times 128 db 0 ; include a bit more space for good measure (and so we can easily zero more than one byte at a time)

    previous_line_spaces:  dq 0
    current_line_spaces: dq 0
    next_line_spaces:  dq 0

    number_reachable: dq 0

