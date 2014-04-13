xorq %rcx, %rcx
.loop:
cmpq %rdx, %rcx
je .done
movb (%rsi, %rcx, 1), %al
movb %al, (%rdi, %rcx, 1)
incq %rcx
jmpq .loop
.done:
retq
