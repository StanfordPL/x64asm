, 4		// adcb $0x4, %al
, 6		// adcw $0x4050, %ax
, 4		// adcl $0xc0decafe, %eax
, 17		// adcw $0x4050, (%rsi)
, 18		// adcw $0x4, (%rsi)
, 18		// adcw %cx, (%rsi)
, 18		// adcl $0xc0decafe, (%rsi)
, 18		// adcl $0x4, (%rsi)
, 18		// adcl %edx, (%rsi)
, 18		// adcq $0xc0decafe, (%rsi)
, 18		// adcq $0x4, (%rsi)
, 18		// adcq %rdx, (%rsi)
, 17		// adcb $0x4, (%rsi)
, 18		// adcb %dl, (%rsi)
, 20		// adcb %ch, (%rsi)
, 6		// adcw $0x4050, %cx
, 4		// adcw $0x4, %cx
, 4		// adcw (%rsi), %cx
, 4		// adcw %cx, %cx
, 4		// adcw %cx, %cx
, 4		// adcl $0xc0decafe, %edx
, 4		// adcl $0x4, %edx
, 4		// adcl (%rsi), %edx
, 4		// adcl %edx, %edx
, 4		// adcl %edx, %edx
, 4		// adcq $0xc0decafe, %rdx
, 4		// adcq $0x4, %rdx
, 4		// adcq (%rsi), %rdx
, 4		// adcq %rdx, %rdx
, 4		// adcq %rdx, %rdx
, 4		// adcb $0x4, %dl
, 4		// adcb (%rsi), %dl
, 4		// adcb %dl, %dl
, 4		// adcb %dl, %dl
, 6		// adcb %ch, %dl
, 6		// adcb %ch, %dl
, 4		// adcq $0xc0decafe, %rax
, 4		// adcb $0x4, %ch
, 4		// adcb (%rsi), %ch
, 4		// adcb %dl, %ch
, 4		// adcb %dl, %ch
, 4		// adcb %ch, %ch
, 4		// adcb %ch, %ch
, 1		// addb $0x4, %al
, 7		// addw $0x4050, %ax
, 1		// addl $0xc0decafe, %eax
, 16		// addw $0x4050, (%rsi)
, 16		// addw $0x4, (%rsi)
, 14		// addw %cx, (%rsi)
, 16		// addl $0xc0decafe, (%rsi)
, 17		// addl $0x4, (%rsi)
, 14		// addl %edx, (%rsi)
, 16		// addq $0xc0decafe, (%rsi)
, 16		// addq $0x4, (%rsi)
, 14		// addq %rdx, (%rsi)
, 16		// addb $0x4, (%rsi)
, 14		// addb %dl, (%rsi)
, 18		// addb %ch, (%rsi)
, 7		// addw $0x4050, %cx
, 1		// addw $0x4, %cx
, 1		// addw (%rsi), %cx
, 1		// addw %cx, %cx
, 1		// addw %cx, %cx
, 1		// addl $0xc0decafe, %edx
, 1		// addl $0x4, %edx
, 1		// addl (%rsi), %edx
, 1		// addl %edx, %edx
, 1		// addl %edx, %edx
, 1		// addq $0xc0decafe, %rdx
, 1		// addq $0x4, %rdx
, 1		// addq (%rsi), %rdx
, 1		// addq %rdx, %rdx
, 1		// addq %rdx, %rdx
, 1		// addb $0x4, %dl
, 1		// addb (%rsi), %dl
, 1		// addb %dl, %dl
, 1		// addb %dl, %dl
, 4		// addb %ch, %dl
, 4		// addb %ch, %dl
, 1		// addq $0xc0decafe, %rax
, 1		// addb $0x4, %ch
, 1		// addb (%rsi), %ch
, 1		// addb %dl, %ch
, 1		// addb %dl, %ch
, 1		// addb %ch, %ch
, 1		// addb %ch, %ch
, 7		// addpd (%rsi), %xmm0
, 7		// addpd %xmm0, %xmm0
, 7		// addps (%rsi), %xmm0
, 7		// addps %xmm0, %xmm0
, 7		// addsd (%rsi), %xmm0
, 7		// addsd %xmm0, %xmm0
, 7		// addss (%rsi), %xmm0
, 7		// addss %xmm0, %xmm0
, 7		// addsubpd (%rsi), %xmm0
, 7		// addsubpd %xmm0, %xmm0
, 7		// addsubps (%rsi), %xmm0
, 7		// addsubps %xmm0, %xmm0
, 17		// aesdec (%rsi), %xmm0
, 17		// aesdec %xmm0, %xmm0
, 17		// aesdeclast (%rsi), %xmm0
, 17		// aesdeclast %xmm0, %xmm0
, 17		// aesenc (%rsi), %xmm0
, 17		// aesenc %xmm0, %xmm0
, 17		// aesenclast (%rsi), %xmm0
, 17		// aesenclast %xmm0, %xmm0
, 4		// aesimc (%rsi), %xmm0
, 35		// aesimc %xmm0, %xmm0
, 19		// aeskeygenassist $0x4, (%rsi), %xmm0
, 25		// aeskeygenassist $0x4, %xmm0, %xmm0
, 1		// andb $0x4, %al
, 7		// andw $0x4050, %ax
, 1		// andl $0xc0decafe, %eax
, 17		// andw $0x4050, (%rsi)
, 16		// andw $0x4, (%rsi)
, 14		// andw %cx, (%rsi)
, 17		// andl $0xc0decafe, (%rsi)
, 16		// andl $0x4, (%rsi)
, 14		// andl %edx, (%rsi)
, 16		// andq $0xc0decafe, (%rsi)
, 17		// andq $0x4, (%rsi)
, 14		// andq %rdx, (%rsi)
, 17		// andb $0x4, (%rsi)
, 14		// andb %dl, (%rsi)
, 18		// andb %ch, (%rsi)
, 7		// andw $0x4050, %cx
, 1		// andw $0x4, %cx
, 1		// andw (%rsi), %cx
, 1		// andw %cx, %cx
, 1		// andw %cx, %cx
, 1		// andl $0xc0decafe, %edx
, 1		// andl $0x4, %edx
, 1		// andl (%rsi), %edx
, 1		// andl %edx, %edx
, 1		// andl %edx, %edx
, 1		// andq $0xc0decafe, %rdx
, 1		// andq $0x4, %rdx
, 1		// andq (%rsi), %rdx
, 1		// andq %rdx, %rdx
, 1		// andq %rdx, %rdx
, 1		// andb $0x4, %dl
, 1		// andb (%rsi), %dl
, 1		// andb %dl, %dl
, 1		// andb %dl, %dl
, 4		// andb %ch, %dl
, 4		// andb %ch, %dl
, 1		// andq $0xc0decafe, %rax
, 1		// andb $0x4, %ch
, 1		// andb (%rsi), %ch
, 1		// andb %dl, %ch
, 1		// andb %dl, %ch
, 1		// andb %ch, %ch
, 1		// andb %ch, %ch
, 1		// andnl (%rsi), %edx, %edx
, 1		// andnl %edx, %edx, %edx
, 1		// andnq (%rsi), %rdx, %rdx
, 1		// andnq %rdx, %rdx, %rdx
, 1		// andnpd (%rsi), %xmm0
, 1		// andnpd %xmm0, %xmm0
, 1		// andnps (%rsi), %xmm0
, 1		// andnps %xmm0, %xmm0
, 1		// andpd (%rsi), %xmm0
, 1		// andpd %xmm0, %xmm0
, 1		// andps (%rsi), %xmm0
, 1		// andps %xmm0, %xmm0
, 4		// bextrl %edx, (%rsi), %edx
, 4		// bextrl %edx, %edx, %edx
, 4		// bextrq %rdx, (%rsi), %rdx
, 4		// bextrq %rdx, %rdx, %rdx
, 1		// blendpd $0x4, (%rsi), %xmm0
, 1		// blendpd $0x4, %xmm0, %xmm0
, 1		// blendps $0x4, (%rsi), %xmm0
, 1		// blendps $0x4, %xmm0, %xmm0
, 4		// blendvpd %xmm0, (%rsi), %xmm0
, 4		// blendvpd %xmm0, %xmm0, %xmm0
, 4		// blendvps %xmm0, (%rsi), %xmm0
, 4		// blendvps %xmm0, %xmm0, %xmm0
, 1		// blsil (%rsi), %edx
, 1		// blsil %edx, %edx
, 1		// blsiq (%rsi), %rdx
, 1		// blsiq %rdx, %rdx
, 1		// blsmskl (%rsi), %edx
, 1		// blsmskl %edx, %edx
, 1		// blsmskq (%rsi), %rdx
, 1		// blsmskq %rdx, %rdx
, 1		// blsrl (%rsi), %edx
, 1		// blsrl %edx, %edx
, 1		// blsrq (%rsi), %rdx
, 1		// blsrq %rdx, %rdx
, 6		// bsfl (%rsi), %edx
, 6		// bsfl %edx, %edx
, 6		// bsfq (%rsi), %rdx
, 6		// bsfq %rdx, %rdx
, 6		// bsrw (%rsi), %cx
, 6		// bsrw %cx, %cx
, 6		// bsrl (%rsi), %edx
, 6		// bsrl %edx, %edx
, 6		// bsrq (%rsi), %rdx
, 6		// bsrq %rdx, %rdx
, 1		// bswap %edx
, 4		// bswap %rdx
, 1		// btw $0x4, (%rsi)
, 12		// btw %cx, (%rsi)
, 1		// btl $0x4, (%rsi)
, 999		// btl %eax, es:(%rax,%rax,1)
, 1		// btq $0x4, (%rsi)
, 999		// btq %rax, es:(%rax,%rax,1)
, 1		// btw $0x4, %cx
, 1		// btw %cx, %cx
, 1		// btl $0x4, %edx
, 1		// btl %edx, %edx
, 1		// btq $0x4, %rdx
, 1		// btq %rdx, %rdx
, 14		// btcw $0x4, (%rsi)
, 15		// btcw %cx, (%rsi)
, 14		// btcl $0x4, (%rsi)
, 999		// btcl %eax, es:(%rax,%rax,1)
, 14		// btcq $0x4, (%rsi)
, 999		// btcq %rax, es:(%rax,%rax,1)
, 1		// btcw $0x4, %cx
, 1		// btcw %cx, %cx
, 1		// btcl $0x4, %edx
, 1		// btcl %edx, %edx
, 1		// btcq $0x4, %rdx
, 1		// btcq %rdx, %rdx
, 14		// btrw $0x4, (%rsi)
, 15		// btrw %cx, (%rsi)
, 14		// btrl $0x4, (%rsi)
, 999		// btrl %eax, es:(%rax,%rax,1)
, 14		// btrq $0x4, (%rsi)
, 999		// btrq %rax, es:(%rax,%rax,1)
, 1		// btrw $0x4, %cx
, 1		// btrw %cx, %cx
, 1		// btrl $0x4, %edx
, 1		// btrl %edx, %edx
, 1		// btrq $0x4, %rdx
, 1		// btrq %rdx, %rdx
, 14		// btsw $0x4, (%rsi)
, 15		// btsw %cx, (%rsi)
, 14		// btsl $0x4, (%rsi)
, 999		// btsl %eax, es:(%rax,%rax,1)
, 14		// btsq $0x4, (%rsi)
, 999		// btsq %rax, es:(%rax,%rax,1)
, 1		// btsw $0x4, %cx
, 1		// btsw %cx, %cx
, 1		// btsl $0x4, %edx
, 1		// btsl %edx, %edx
, 1		// btsq $0x4, %rdx
, 1		// btsq %rdx, %rdx
, 1		// bzhil %edx, (%rsi), %edx
, 1		// bzhil %edx, %edx, %edx
, 1		// bzhiq %rdx, (%rsi), %rdx
, 1		// bzhiq %rdx, %rdx, %rdx
, 0		// callq es:(%rax,%rax,1)
, 0		// callq es:(%rax,%rax,1)
, 0		// callq es:(%rax,%rax,1)
, 0		// callq 
