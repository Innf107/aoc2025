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
; That way we can immediately increment our counter (and blackhole the found number) whenever we hit 4 or more empty spaces.
; We still need to make sure htat the tile we hit actually contains a paper roll however!
;
; Crucially, for this to work we need a few additional pieces of machinery
;   1) every tile at the edges (left and right) starts off with 3 adjacent blank spaces
;   2) the initial current_line_spaces array is initialized to `5 3 ... 3 5`
;   3) we add `2 3 ... 3 2` to the last current_line_spaces array before processing the last line
;
; [previous_spaces] contains the 
;
; [current_line_spaces] contains the number of spaces we already know for the current line.
; every empty space at position i increments current_line_spaces for positions `i - 1` and `i + 1`
;
; [next_line_spaces] contains the effect the current line will have on the next line.
; every empty space at position i increments next_line_spaces at position i

; Move next_line_spaces to current_line_spaces
; and reset the new next_line_spaces array to 3 0 ... 0 3 to account for the edges
swap_and_reset:
    ; we swap first
    mov rax, [next_line_spaces]
    mov rbx, [current_line_spaces]
    mov [next_line_spaces], rbx
    mov [current_line_spaces], rax

    ; reset the (left) sentinels
    mov BYTE [rax-1], -100
    mov BYTE [rbx-1], -100

    ; then reset what is *now* in next_line_spaces (in rbx)
    lea rcx, [rbx+line_length] ; save the address one past the last element so we know when to stop

    ; (technically we could use SIMD here but that's confusing sooo it's just SWAR for now i guess)
    .zero_loop:
    mov QWORD [rbx], 0
    add rbx, 8
    cmp rbx, rcx
    jl .zero_loop

    mov rbx, [next_line_spaces]

    ; fill in the edges
    mov BYTE [rbx], 3
    mov BYTE [rbx+line_length - 1], 3
    ret

process_next_line:
    xor r13, r13 ; r13 ~ index into the current line
    xor r12, r12 ; we remember in r12 if the previous character was a roll or not

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
    ; next_line_spaces[i]
    ; we also need to check if current_line_spaces[i - 1] has hit its threshhold
    mov rbx, [next_line_spaces]
    inc BYTE [rbx+r13]

    mov rbx, [current_line_spaces]
    add rbx, r13
    inc BYTE [rbx+1]
    ; We are not going to access current_line_spaces[i - 1] again, so we don't need to literally
    ; increment it in the array (RIP debugging), but we do need to check if it is at exactly 3
    ; (in which case we increment the counter).
    ; If it is higher, it should have already been counted on the last iteration, so 
    ; this way, we can avoid having to blackhole it    
    test r12, r12
    jz .line_loop ; r12 is already 0 so we can skip the bit that zeroes it

    ; We can skip a branch by conditionally incrementing like this
    mov rax, [number_reachable]
    lea rdx, [rax+1]
    cmp BYTE [rbx-1], 3
    cmove rax, rdx
    mov [number_reachable], rax

    xor r12, r12
    jmp .line_loop

.check_roll:
    ; We only need to check if we have already hit the threshhold (4) here.
    mov rcx, [current_line_spaces]
    add rcx, r13

    ; We can skip a branch by conditionally incrementing like this
    ; (idk if that's actually that much faster though. number_reachable should be in cache i guess)
    mov rax, [number_reachable]
    lea rdx, [rax+1]
    cmp BYTE [rcx], 4
    cmovge rax, rdx
    mov [number_reachable], rax
    
    jmp .line_loop

.end_of_line:
    call swap_and_reset
    jmp process_next_line



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

    call process_next_line

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

    db 0 ; we include an unused 0 so that we can write just before the array without corrupting anything
    align 8, db 0
    linearray1: db 5
                times (line_length - 2) db 3
                db 5
                times 128 db 0 ; include a bit more space for good measure (and so we can easily zero more than one byte at a time)

    db 0 ; we include an unused 0 so that we can write just before the array without corrupting anything
    align 8, db 0
    linearray2: db 3
                times (line_length - 2) db 0
                db 3
                times 128 db 0 ; include a bit more space for good measure (and so we can easily zero more than one byte at a time)

    current_line_spaces: dq 0
    next_line_spaces:  dq 0

    number_reachable: dq 0

