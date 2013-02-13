.loop:
cmpq $0x0, %rdx
je .done
movb (%rsi), %al
movb %al, (%rdi)
decq %rdx
jmpq .loop
.done:
retq
