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

process_input:
    push 0 ; we push a sentinel value so we know when we have exhausted our stack
process_next_line:
    xor r13, r13 ; r13 ~ index into the current line

    call read_char
    cmp rax, 0 ; EOF
    jne .after_read
    ; because we use the call stack to track values we need to process recursively, we cannot actually
    ; return here. Instead, we tail-call into the next function, which then cleans up the stack
    ; and returns back to _entry
    jmp process_recursively

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
    ;
    ; we also blackhole the current tile
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
    lea rdx, [rbx-1]
    push rdx
    mov BYTE [rbx-1], -100
.skip_cur_n1:
    cmp BYTE [rcx-1], 5
    jl .skip_prev_n1
    inc QWORD [number_reachable]
    lea rdx, [rcx-1]
    push rdx
    mov BYTE [rcx-1], -100
.skip_prev_n1:
    cmp BYTE [rcx], 5
    jl .skip_prev
    inc QWORD [number_reachable]
    push rcx
    mov BYTE [rcx], -100
.skip_prev:
    cmp BYTE [rcx+1], 5
    jl .line_loop
    inc QWORD [number_reachable]
    lea rdx, [rcx+1]
    push rdx
    mov BYTE [rcx+1], -100
    jmp .line_loop

.check_roll:
    ; We only need to check if we have already hit the threshhold (5) here.
    mov rcx, [current_line_spaces]
    add rcx, r13

    cmp BYTE [rcx], 5
    jl .line_loop
    inc QWORD [number_reachable]
    push rcx
    mov BYTE [rcx], -100
    
    jmp .line_loop

.end_of_line:
    ; We don't actually need to do anything interesting here in part 2, since we have a
    ; full, actual array and we only need to shift the pointers into it over by one row
    add QWORD [previous_line_spaces], array_row_length
    add QWORD [current_line_spaces], array_row_length
    add QWORD [next_line_spaces], array_row_length
    jmp process_next_line


initialize_array:
    ; We need to fill in the left and right edges of everything but the first and last row
    ; (everything else has been initialized statically already)
    ; We could fill everything after with black holes as well, but that's not necessary since none of the values there will
    ; ever hit 5 anyway
    mov rbx, array+array_row_length
    lea rcx, [array+array_row_length*(height-1)]
.loop:
    mov BYTE [rbx], 3
    mov BYTE [rbx+line_length-1], 3
    
    add rbx, array_row_length
    cmp rbx, rcx
    jl .loop
    ret

process_recursively:
    pop rax
    test rax, rax
    jz .done
    
    inc BYTE [rax-1]
    inc BYTE [rax+1]
    inc BYTE [rax-array_row_length-1]
    inc BYTE [rax-array_row_length]
    inc BYTE [rax-array_row_length+1]
    inc BYTE [rax+array_row_length-1]
    inc BYTE [rax+array_row_length]
    inc BYTE [rax+array_row_length+1]

    cmp BYTE [rax-1], 5
    jl .skip1
    inc QWORD [number_reachable]
    lea rbx, [rax-1]
    mov BYTE [rbx], -100
    push rbx
.skip1:
    cmp BYTE [rax+1], 5
    jl .skip2
    inc QWORD [number_reachable]
    lea rbx, [rax+1]
    mov BYTE [rbx], -100
    push rbx
.skip2:
    cmp BYTE [rax-array_row_length-1], 5
    jl .skip3
    inc QWORD [number_reachable]
    lea rbx, [rax-array_row_length-1]
    mov BYTE [rbx], -100
    push rbx
.skip3:
    cmp BYTE [rax-array_row_length], 5
    jl .skip4
    inc QWORD [number_reachable]
    lea rbx, [rax-array_row_length]
    mov BYTE [rbx], -100
    push rbx
.skip4:
    cmp BYTE [rax-array_row_length+1], 5
    jl .skip5
    inc QWORD [number_reachable]
    lea rbx, [rax-array_row_length+1]
    mov BYTE [rbx], -100
    push rbx
.skip5:
    cmp BYTE [rax+array_row_length-1], 5
    jl .skip6
    inc QWORD [number_reachable]
    lea rbx, [rax+array_row_length-1]
    mov BYTE [rbx], -100
    push rbx
.skip6:
    cmp BYTE [rax+array_row_length], 5
    jl .skip7
    inc QWORD [number_reachable]
    lea rbx, [rax+array_row_length]
    mov BYTE [rbx], -100
    push rbx
.skip7:
    cmp BYTE [rax+array_row_length+1], 5
    jl process_recursively
    inc QWORD [number_reachable]
    lea rbx, [rax+array_row_length+1]
    mov BYTE [rbx], -100
    push rbx

    jmp process_recursively
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

    call initialize_array

    ; Initialize the initial array pointers. We will swap these after every line
    mov QWORD [previous_line_spaces], (array - array_row_length)
    mov QWORD [current_line_spaces], array
    mov QWORD [next_line_spaces], (array + array_row_length)

    call process_input

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
    height: equ 137
    array_row_length: equ 256

        ; + 1 since we need to include the curner at (-1,-1)
        times (array_row_length + 2) db -100
    array:  
        db 5
        times (line_length - 2) db 3
        db 5
        times (array_row_length - line_length) db -100

        times ((height - 2) * array_row_length) db 0

        db 5
        times (line_length - 2) db 3
        db 5
        times (array_row_length - line_length) db -100
    ; sentinel
        times (array_row_length + 1) db -100

    previous_line_spaces:  dq 0
    current_line_spaces: dq 0
    next_line_spaces:  dq 0

    number_reachable: dq 0

