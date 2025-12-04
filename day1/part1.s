BITS 64
GLOBAL _start

; r12 ~ accumulator
; r13 ~ total number of times 0 was reached

; r15 ~ number of characters leaft to read
; r14 ~ actual buffer pointer
read_char:
    push rdi
    cmp r15, 0
    jne .skip_refill
    ; the buffer is empty so we need to re-fill it
    mov rax, 0 ; read(fd, input_buffer, 4096)
    mov rdi, [fd]
    mov rsi, input_buffer
    mov rdx, 4096
    syscall

    mov r15, rax
    mov r14, input_buffer

.skip_refill:
    xor rax, rax
    mov al, [r14]
    inc r14
    dec r15
    pop rdi
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

parse_line:
    call read_char
    cmp rax, 10
    je .eof
    mov rbx, rax
    sub rbx, 76 ; 76 = L
    ; now rbx = 0 iff the first char was L (i.e. we need to negate the result)
    push rbx
    
    xor rdi, rdi ; rdi (non-volatile) is our accumulator for the final number
.parse_loop:
    call read_char
    cmp rax, 10 ; 10 = '\n'
    je .done

    sub rax, 48 ; convert from an ascii digit to its numerical value
    mov rcx, rax

    mov rax, rdi
    xor rdx, rdx
    mov rbx, 10
    mul rbx
    add rax, rcx
    mov rdi, rax
    jmp .parse_loop
.done:
    mov rax, rdi

    xor rdx, rdx
    sub rdx, rax

    pop rbx
    test rbx, rbx
    cmovz rax, rdx
    ret
.eof:
    xor rax, rax
    ret

accumulate_loop:
    call parse_line
    test rax, rax
    jz .done
    add r12, rax
    
    mov rbx, r12 ; wrap r12 around if it's < 0
    mov rcx, r12 ; wrap r12 around if it's > 99 (we interleave these two to enable instruction level parallelism)
    
    add rbx, 100 ; wrap r12 around if it's < 0
    cmp r12, 0
    cmovl r12, rbx

    sub rcx, 100 ; wrap r12 around if it's > 99
    cmp r12, 99
    cmovg r12, rcx

    mov rbx, r13 ; if r12 = 0: r13 = r13 + 1
    inc rbx
    test r12, r12
    cmovz r13, rbx
    jmp accumulate_loop
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

    mov r12, 50 ; "the dial stars by pointing at 50"

    call accumulate_loop

    mov rax, r13
    call print_unsigned_int

    mov rax, 60
    mov rdi, 12
    syscall


section .data
    input_buffer: times 4096 db 0
    input_file: db "input.txt", 0
    
    output_buffer: times 4096 db 0
    fd: db 0
