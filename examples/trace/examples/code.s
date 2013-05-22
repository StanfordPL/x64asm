xorq %rax, %rax
incq %rax
incq %rax
movq %rax, %r14
movd %eax, %xmm0
movd %eax, %xmm1
pcmpeqb %xmm2, %xmm2
retq
