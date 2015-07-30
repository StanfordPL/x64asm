, 4		// adcb $0x4, %al
, 6		// adcw $0x4050, %ax
, 4		// adcl $0xc0decafe, %eax
, 18		// adcw $0x4050, (%rsi)
, 17		// adcw $0x4, (%rsi)
, 18		// adcw %cx, (%rsi)
, 18		// adcl $0xc0decafe, (%rsi)
, 17		// adcl $0x4, (%rsi)
, 18		// adcl %edx, (%rsi)
, 18		// adcq $0xc0decafe, (%rsi)
, 18		// adcq $0x4, (%rsi)
, 18		// adcq %rdx, (%rsi)
, 18		// adcb $0x4, (%rsi)
, 17		// adcb %dl, (%rsi)
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
, 17		// addw $0x4050, (%rsi)
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
, 24		// aeskeygenassist $0x4, %xmm0, %xmm0
, 1		// andb $0x4, %al
, 7		// andw $0x4050, %ax
, 1		// andl $0xc0decafe, %eax
, 17		// andw $0x4050, (%rsi)
, 16		// andw $0x4, (%rsi)
, 14		// andw %cx, (%rsi)
, 16		// andl $0xc0decafe, (%rsi)
, 17		// andl $0x4, (%rsi)
, 14		// andl %edx, (%rsi)
, 16		// andq $0xc0decafe, (%rsi)
, 16		// andq $0x4, (%rsi)
, 14		// andq %rdx, (%rsi)
, 16		// andb $0x4, (%rsi)
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
, 0		// callq es:(%rax,%rax,1)
, 0		// callq %rax
, 0		// callq 0
, 1		// cbtw 
, 1		// cltd 
, 1		// cltq 
, 1		// clc 
, 9		// cld 
, 156		// clflush (%rsi)
, 999		// cli 
, 1		// cmc 
, 1		// cmovaw (%rsi), %cx
, 4		// cmovaw %cx, %cx
, 1		// cmoval (%rsi), %edx
, 4		// cmoval %edx, %edx
, 1		// cmovaq (%rsi), %rdx
, 4		// cmovaq %rdx, %rdx
, 1		// cmovaew (%rsi), %cx
, 4		// cmovaew %cx, %cx
, 1		// cmovael (%rsi), %edx
, 4		// cmovael %edx, %edx
, 1		// cmovaeq (%rsi), %rdx
, 4		// cmovaeq %rdx, %rdx
, 1		// cmovbw (%rsi), %cx
, 4		// cmovbw %cx, %cx
, 1		// cmovbl (%rsi), %edx
, 4		// cmovbl %edx, %edx
, 1		// cmovbq (%rsi), %rdx
, 4		// cmovbq %rdx, %rdx
, 1		// cmovbew (%rsi), %cx
, 4		// cmovbew %cx, %cx
, 1		// cmovbel (%rsi), %edx
, 4		// cmovbel %edx, %edx
, 1		// cmovbeq (%rsi), %rdx
, 4		// cmovbeq %rdx, %rdx
, 1		// cmovcw (%rsi), %cx
, 4		// cmovcw %cx, %cx
, 1		// cmovcl (%rsi), %edx
, 4		// cmovcl %edx, %edx
, 1		// cmovcq (%rsi), %rdx
, 4		// cmovcq %rdx, %rdx
, 1		// cmovew (%rsi), %cx
, 4		// cmovew %cx, %cx
, 1		// cmovel (%rsi), %edx
, 4		// cmovel %edx, %edx
, 1		// cmoveq (%rsi), %rdx
, 4		// cmoveq %rdx, %rdx
, 1		// cmovgw (%rsi), %cx
, 4		// cmovgw %cx, %cx
, 1		// cmovgl (%rsi), %edx
, 4		// cmovgl %edx, %edx
, 1		// cmovgq (%rsi), %rdx
, 4		// cmovgq %rdx, %rdx
, 1		// cmovgew (%rsi), %cx
, 4		// cmovgew %cx, %cx
, 1		// cmovgel (%rsi), %edx
, 4		// cmovgel %edx, %edx
, 1		// cmovgeq (%rsi), %rdx
, 4		// cmovgeq %rdx, %rdx
, 1		// cmovlw (%rsi), %cx
, 4		// cmovlw %cx, %cx
, 1		// cmovll (%rsi), %edx
, 4		// cmovll %edx, %edx
, 1		// cmovlq (%rsi), %rdx
, 4		// cmovlq %rdx, %rdx
, 1		// cmovlew (%rsi), %cx
, 4		// cmovlew %cx, %cx
, 1		// cmovlel (%rsi), %edx
, 4		// cmovlel %edx, %edx
, 1		// cmovleq (%rsi), %rdx
, 4		// cmovleq %rdx, %rdx
, 1		// cmovnaw (%rsi), %cx
, 4		// cmovnaw %cx, %cx
, 1		// cmovnal (%rsi), %edx
, 4		// cmovnal %edx, %edx
, 1		// cmovnaq (%rsi), %rdx
, 4		// cmovnaq %rdx, %rdx
, 1		// cmovnaew (%rsi), %cx
, 4		// cmovnaew %cx, %cx
, 1		// cmovnael (%rsi), %edx
, 4		// cmovnael %edx, %edx
, 1		// cmovnaeq (%rsi), %rdx
, 4		// cmovnaeq %rdx, %rdx
, 1		// cmovnbw (%rsi), %cx
, 4		// cmovnbw %cx, %cx
, 1		// cmovnbl (%rsi), %edx
, 4		// cmovnbl %edx, %edx
, 1		// cmovnbq (%rsi), %rdx
, 4		// cmovnbq %rdx, %rdx
, 1		// cmovnbew (%rsi), %cx
, 4		// cmovnbew %cx, %cx
, 1		// cmovnbel (%rsi), %edx
, 4		// cmovnbel %edx, %edx
, 1		// cmovnbeq (%rsi), %rdx
, 4		// cmovnbeq %rdx, %rdx
, 1		// cmovncw (%rsi), %cx
, 4		// cmovncw %cx, %cx
, 1		// cmovncl (%rsi), %edx
, 4		// cmovncl %edx, %edx
, 1		// cmovncq (%rsi), %rdx
, 4		// cmovncq %rdx, %rdx
, 1		// cmovnew (%rsi), %cx
, 4		// cmovnew %cx, %cx
, 1		// cmovnel (%rsi), %edx
, 4		// cmovnel %edx, %edx
, 1		// cmovneq (%rsi), %rdx
, 4		// cmovneq %rdx, %rdx
, 1		// cmovngw (%rsi), %cx
, 4		// cmovngw %cx, %cx
, 1		// cmovngl (%rsi), %edx
, 4		// cmovngl %edx, %edx
, 1		// cmovngq (%rsi), %rdx
, 4		// cmovngq %rdx, %rdx
, 1		// cmovngew (%rsi), %cx
, 4		// cmovngew %cx, %cx
, 1		// cmovngel (%rsi), %edx
, 4		// cmovngel %edx, %edx
, 1		// cmovngeq (%rsi), %rdx
, 4		// cmovngeq %rdx, %rdx
, 1		// cmovnlw (%rsi), %cx
, 4		// cmovnlw %cx, %cx
, 1		// cmovnll (%rsi), %edx
, 4		// cmovnll %edx, %edx
, 1		// cmovnlq (%rsi), %rdx
, 4		// cmovnlq %rdx, %rdx
, 1		// cmovnlew (%rsi), %cx
, 4		// cmovnlew %cx, %cx
, 1		// cmovnlel (%rsi), %edx
, 4		// cmovnlel %edx, %edx
, 1		// cmovnleq (%rsi), %rdx
, 4		// cmovnleq %rdx, %rdx
, 1		// cmovnow (%rsi), %cx
, 4		// cmovnow %cx, %cx
, 1		// cmovnol (%rsi), %edx
, 4		// cmovnol %edx, %edx
, 1		// cmovnoq (%rsi), %rdx
, 4		// cmovnoq %rdx, %rdx
, 1		// cmovnpw (%rsi), %cx
, 4		// cmovnpw %cx, %cx
, 1		// cmovnpl (%rsi), %edx
, 4		// cmovnpl %edx, %edx
, 1		// cmovnpq (%rsi), %rdx
, 4		// cmovnpq %rdx, %rdx
, 1		// cmovnsw (%rsi), %cx
, 4		// cmovnsw %cx, %cx
, 1		// cmovnsl (%rsi), %edx
, 4		// cmovnsl %edx, %edx
, 1		// cmovnsq (%rsi), %rdx
, 4		// cmovnsq %rdx, %rdx
, 1		// cmovnzw (%rsi), %cx
, 4		// cmovnzw %cx, %cx
, 1		// cmovnzl (%rsi), %edx
, 4		// cmovnzl %edx, %edx
, 1		// cmovnzq (%rsi), %rdx
, 4		// cmovnzq %rdx, %rdx
, 1		// cmovow (%rsi), %cx
, 4		// cmovow %cx, %cx
, 1		// cmovol (%rsi), %edx
, 4		// cmovol %edx, %edx
, 1		// cmovoq (%rsi), %rdx
, 4		// cmovoq %rdx, %rdx
, 1		// cmovpw (%rsi), %cx
, 4		// cmovpw %cx, %cx
, 1		// cmovpl (%rsi), %edx
, 4		// cmovpl %edx, %edx
, 1		// cmovpq (%rsi), %rdx
, 4		// cmovpq %rdx, %rdx
, 1		// cmovpew (%rsi), %cx
, 4		// cmovpew %cx, %cx
, 1		// cmovpel (%rsi), %edx
, 4		// cmovpel %edx, %edx
, 1		// cmovpeq (%rsi), %rdx
, 4		// cmovpeq %rdx, %rdx
, 1		// cmovpow (%rsi), %cx
, 4		// cmovpow %cx, %cx
, 1		// cmovpol (%rsi), %edx
, 4		// cmovpol %edx, %edx
, 1		// cmovpoq (%rsi), %rdx
, 4		// cmovpoq %rdx, %rdx
, 1		// cmovsw (%rsi), %cx
, 4		// cmovsw %cx, %cx
, 2		// cmovsl (%rsi), %edx
, 4		// cmovsl %edx, %edx
, 2		// cmovsq (%rsi), %rdx
, 4		// cmovsq %rdx, %rdx
, 2		// cmovzw (%rsi), %cx
, 4		// cmovzw %cx, %cx
, 2		// cmovzl (%rsi), %edx
, 4		// cmovzl %edx, %edx
, 2		// cmovzq (%rsi), %rdx
, 4		// cmovzq %rdx, %rdx
, 1		// cmpb $0x4, %al
, 7		// cmpw $0x4050, %ax
, 1		// cmpl $0xc0decafe, %eax
, 4		// cmpw $0x4050, (%rsi)
, 1		// cmpw $0x4, (%rsi)
, 1		// cmpw %cx, (%rsi)
, 1		// cmpl $0xc0decafe, (%rsi)
, 1		// cmpl $0x4, (%rsi)
, 1		// cmpl %edx, (%rsi)
, 1		// cmpq $0xc0decafe, (%rsi)
, 1		// cmpq $0x4, (%rsi)
, 1		// cmpq %rdx, (%rsi)
, 1		// cmpb $0x4, (%rsi)
, 1		// cmpb %dl, (%rsi)
, 1		// cmpb %ch, (%rsi)
, 7		// cmpw $0x4050, %cx
, 1		// cmpw $0x4, %cx
, 1		// cmpw (%rsi), %cx
, 1		// cmpw %cx, %cx
, 1		// cmpw %cx, %cx
, 1		// cmpl $0xc0decafe, %edx
, 1		// cmpl $0x4, %edx
, 1		// cmpl (%rsi), %edx
, 1		// cmpl %edx, %edx
, 1		// cmpl %edx, %edx
, 1		// cmpq $0xc0decafe, %rdx
, 1		// cmpq $0x4, %rdx
, 1		// cmpq (%rsi), %rdx
, 1		// cmpq %rdx, %rdx
, 1		// cmpq %rdx, %rdx
, 1		// cmpb $0x4, %dl
, 1		// cmpb (%rsi), %dl
, 1		// cmpb %dl, %dl
, 1		// cmpb %dl, %dl
, 1		// cmpb %ch, %dl
, 1		// cmpb %ch, %dl
, 1		// cmpq $0xc0decafe, %rax
, 1		// cmpb $0x4, %ch
, 1		// cmpb (%rsi), %ch
, 1		// cmpb %dl, %ch
, 1		// cmpb %dl, %ch
, 1		// cmpb %ch, %ch
, 1		// cmpb %ch, %ch
, 7		// cmppd $0x4, (%rsi), %xmm0
, 7		// cmppd $0x4, %xmm0, %xmm0
, 7		// cmpps $0x4, (%rsi), %xmm0
, 7		// cmpps $0x4, %xmm0, %xmm0
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmpsb 
, 999		// cmpsl 
, 7		// cmpsd $0x4, (%rsi), %xmm0
, 7		// cmpsd $0x4, %xmm0, %xmm0
, 999		// cmpsq 
, 7		// cmpss $0x4, (%rsi), %xmm0
, 7		// cmpss $0x4, %xmm0, %xmm0
, 999		// cmpsw 
, 19		// cmpxchgw %cx, (%rsi)
, 19		// cmpxchgl %edx, (%rsi)
, 19		// cmpxchgq %rdx, (%rsi)
, 19		// cmpxchgb %dl, (%rsi)
, 19		// cmpxchgb %ch, (%rsi)
, 12		// cmpxchgw %cx, %cx
, 12		// cmpxchgl %edx, %edx
, 12		// cmpxchgq %rdx, %rdx
, 12		// cmpxchgb %dl, %dl
, 12		// cmpxchgb %ch, %dl
, 12		// cmpxchgb %dl, %ch
, 12		// cmpxchgb %ch, %ch
, 38		// cmpxchg16b (%rsi)
, 21		// cmpxchg8b (%rsi)
, 1		// comisd (%rsi), %xmm0
, 1		// comisd %xmm0, %xmm0
, 1		// comiss (%rsi), %xmm0
, 1		// comiss %xmm0, %xmm0
, 504		// cpuid 
, 1		// cqto 
, 6		// crc32w (%rsi), %edx
, 6		// crc32l (%rsi), %edx
, 6		// crc32b (%rsi), %edx
, 6		// crc32w %cx, %edx
, 6		// crc32l %edx, %edx
, 6		// crc32b %dl, %edx
, 9		// crc32b %ch, %edx
, 6		// crc32q (%rsi), %rdx
, 6		// crc32b (%rsi), %rdx
, 6		// crc32q %rdx, %rdx
, 6		// crc32b %dl, %rdx
, 1		// cvtdq2pd (%rsi), %xmm0
, 9		// cvtdq2pd %xmm0, %xmm0
, 1		// cvtdq2ps (%rsi), %xmm0
, 7		// cvtdq2ps %xmm0, %xmm0
, 1		// cvtpd2dq (%rsi), %xmm0
, 9		// cvtpd2dq %xmm0, %xmm0
, 1		// cvtpd2pi (%rsi), %mm0
, 1		// cvtpd2pi %xmm0, %mm0
, 1		// cvtpd2ps (%rsi), %xmm0
, 9		// cvtpd2ps %xmm0, %xmm0
, 1		// cvtpi2pd (%rsi), %xmm0
, 1		// cvtpi2pd %mm0, %xmm0
, 7		// cvtpi2ps (%rsi), %xmm0
, 9		// cvtpi2ps %mm0, %xmm0
, 1		// cvtps2dq (%rsi), %xmm0
, 7		// cvtps2dq %xmm0, %xmm0
, 1		// cvtps2pd (%rsi), %xmm0
, 4		// cvtps2pd %xmm0, %xmm0
, 1		// cvtps2pi (%rsi), %mm0
, 1		// cvtps2pi %xmm0, %mm0
, 1		// cvtsd2si (%rsi), %edx
, 1		// cvtsd2si %xmm0, %edx
, 1		// cvtsd2si (%rsi), %rdx
, 1		// cvtsd2si %xmm0, %rdx
, 1		// cvtsd2ss (%rsi), %xmm0
, 9		// cvtsd2ss %xmm0, %xmm0
, 7		// cvtsi2sdl (%rsi), %xmm0
, 7		// cvtsi2sdq (%rsi), %xmm0
, 7		// cvtsi2sdl %edx, %xmm0
, 7		// cvtsi2sdq %rdx, %xmm0
, 7		// cvtsi2ssl (%rsi), %xmm0
, 1		// cvtsi2ssq (%rsi), %xmm0
, 7		// cvtsi2ssl %edx, %xmm0
, 4		// cvtsi2ssq %rdx, %xmm0
, 2		// cvtss2sd (%rsi), %xmm0
, 4		// cvtss2sd %xmm0, %xmm0
, 2		// cvtss2sil (%rsi), %edx
, 2		// cvtss2sil %xmm0, %edx
, 2		// cvtss2siq (%rsi), %rdx
, 2		// cvtss2siq %xmm0, %rdx
, 2		// cvttpd2dq (%rsi), %xmm0
, 9		// cvttpd2dq %xmm0, %xmm0
, 2		// cvttpd2pi (%rsi), %mm0
, 2		// cvttpd2pi %xmm0, %mm0
, 1		// cvttps2dq (%rsi), %xmm0
, 7		// cvttps2dq %xmm0, %xmm0
, 2		// cvttps2pi (%rsi), %mm0
, 2		// cvttps2pi %xmm0, %mm0
, 2		// cvttsd2si (%rsi), %edx
, 1		// cvttsd2si %xmm0, %edx
, 1		// cvttsd2siq (%rsi), %rdx
, 1		// cvttsd2siq %xmm0, %rdx
, 1		// cvttss2si (%rsi), %edx
, 1		// cvttss2si %xmm0, %edx
, 1		// cvttss2siq (%rsi), %rdx
, 1		// cvttss2siq %xmm0, %rdx
, 1		// cwtd 
, 1		// cwtl 
, 14		// decw (%rsi)
, 14		// decl (%rsi)
, 14		// decq (%rsi)
, 14		// decb (%rsi)
, 1		// decw %cx
, 1		// decl %edx
, 1		// decq %rdx
, 1		// decb %dl
, 1		// decb %ch
, 999		// divw es:(%rax,%rax,1)
, 999		// divl es:(%rax,%rax,1)
, 999		// divq es:(%rax,%rax,1)
, 999		// divb es:(%rax,%rax,1)
, 999		// divw %ax
, 999		// divl %eax
, 999		// divq %rax
, 999		// divb %al
, 999		// divb  
, 24		// divpd (%rsi), %xmm0
, 25		// divpd %xmm0, %xmm0
, 24		// divps (%rsi), %xmm0
, 25		// divps %xmm0, %xmm0
, 24		// divsd (%rsi), %xmm0
, 25		// divsd %xmm0, %xmm0
, 24		// divss (%rsi), %xmm0
, 25		// divss %xmm0, %xmm0
, 22		// dppd $0x4, (%rsi), %xmm0
, 22		// dppd $0x4, %xmm0, %xmm0
, 35		// dpps $0x4, (%rsi), %xmm0
, 35		// dpps $0x4, %xmm0, %xmm0
, 32		// emms 
, 0		// enterq $0x0, $0x0
, 0		// enterq $0x0, $0x0
, 0		// enterq $0x0, $0x0
, 1		// extractps $0x4, %xmm0, (%rsi)
, 1		// extractps $0x4, %xmm0, %edx
, 1		// extractps $0x4, %xmm0, %rdx
, 876		// f2xm1 
, 1		// fabs 
, 1134		// fadds (%rsi)
, 1153		// faddl (%rsi)
, 999		// fadd %st, %st
, 999		// fadd %st, %st
, 806		// faddp 
, 999		// faddp %st, %st
, 967		// fbld (%rsi)
, 907		// fbstp (%rsi)
, 1		// fchs 
, 64		// fclex 
, 999		// fcmovb %st, %st
, 999		// fcmovbe %st, %st
, 999		// fcmove %st, %st
, 999		// fcmovnb %st, %st
, 999		// fcmovnbe %st, %st
, 999		// fcmovne %st, %st
, 999		// fcmovnu %st, %st
, 999		// fcmovu %st, %st
, 738		// fcom 
, 1032		// fcoms (%rsi)
, 1049		// fcoml (%rsi)
, 999		// fcom %st
, 999		// fcomi %st, %st
, 999		// fcomip %st, %st
, 792		// fcomp 
, 1083		// fcomps (%rsi)
, 1092		// fcompl (%rsi)
, 999		// fcomp %st
, 828		// fcompp 
, 876		// fcos 
, 1		// fdecstp 
, 1184		// fdivs (%rsi)
, 1170		// fdivl (%rsi)
, 999		// fdiv %st, %st
, 999		// fdiv %st, %st
, 817		// fdivp 
, 999		// fdivp %st, %st
, 931		// fdivrs (%rsi)
, 918		// fdivrl (%rsi)
, 999		// fdivr %st, %st
, 999		// fdivr %st, %st
, 817		// fdivrp 
, 999		// fdivrp %st, %st
, 999		// ffreep %st
, 1050		// fiadd (%rsi)
, 1043		// fiaddl (%rsi)
, 994		// ficom (%rsi)
, 995		// ficoml (%rsi)
, 1046		// ficomp (%rsi)
, 1042		// ficompl (%rsi)
, 1072		// fidiv (%rsi)
, 1064		// fidivl (%rsi)
, 1063		// fidivr (%rsi)
, 1071		// fidivrl (%rsi)
, 1112		// fild (%rsi)
, 1111		// fildl (%rsi)
, 1122		// fildll (%rsi)
, 1055		// fimul (%rsi)
, 1060		// fimull (%rsi)
, 1		// fincstp 
, 216		// finit 
, 862		// fist (%rsi)
, 874		// fistl (%rsi)
, 856		// fistp (%rsi)
, 867		// fistpl (%rsi)
, 835		// fistpll (%rsi)
, 819		// fisttp (%rsi)
, 819		// fisttpl (%rsi)
, 826		// fisttpll (%rsi)
, 1051		// fisub (%rsi)
, 1044		// fisubl (%rsi)
, 1048		// fisubr (%rsi)
, 1052		// fisubrl (%rsi)
, 900		// flds (%rsi)
, 895		// fldl (%rsi)
, 863		// fldt (%rsi)
, 999		// fld %st
, 851		// fld1 
, 999		// fldcw es:(%rax,%rax,1)
, 344		// fldenvl (%rsi)
, 999		// fldl2e 
, 999		// fldl2t 
, 999		// fldlg2 
, 999		// fldln2 
, 999		// fldpi 
, 999		// fldz 
, 999		// fmuls es:(%rax,%rax,1)
, 999		// fmull es:(%rax,%rax,1)
, 999		// fmul %st, %st
, 999		// fmul %st, %st
, 999		// fmulp 
, 999		// fmulp %st, %st
, 54		// fnclex 
, 213		// fninit 
, 1		// fnop 
, 441		// fnsave (%rsi)
, 2		// fnstcw (%rsi)
, 190		// fnstenvl (%rsi)
, 2		// fnstsw %ax
, 1		// fnstsw (%rsi)
, 846		// fpatan 
, 847		// fprem 
, 846		// fprem1 
, 850		// fptan 
, 886		// frndint 
, 420		// frstor (%rsi)
, 423		// fsave (%rsi)
, 798		// fscale 
, 886		// fsin 
, 859		// fsincos 
, 855		// fsqrt 
, 1		// fsts (%rsi)
, 1		// fstl (%rsi)
, 999		// fst %st
, 4		// fstcw (%rsi)
, 181		// fstenvl (%rsi)
, 1106		// fstps (%rsi)
, 1114		// fstpl (%rsi)
, 904		// fstpt (%rsi)
, 999		// fstp %st
, 4		// fstsw %ax
, 4		// fstsw (%rsi)
, 1239		// fsubs (%rsi)
, 1141		// fsubl (%rsi)
, 999		// fsub %st, %st
, 999		// fsub %st, %st
, 823		// fsubp 
, 999		// fsubp %st, %st
, 1256		// fsubrs (%rsi)
, 1145		// fsubrl (%rsi)
, 999		// fsubr %st, %st
, 999		// fsubr %st, %st
, 820		// fsubrp 
, 999		// fsubrp %st, %st
, 742		// ftst 
, 738		// fucom 
, 999		// fucom %st
, 999		// fucomi %st, %st
, 999		// fucomip %st, %st
, 789		// fucomp 
, 999		// fucomp %st
, 825		// fucompp 
, 1		// fwait 
, 636		// fxam 
, 1		// fxch 
, 999		// fxch %st
, 999		// fxrstor es:(%rax,%rax,1)
, 999		// fxrstor64 es:(%rax,%rax,1)
, 169		// fxsave (%rsi)
, 169		// fxsave64 (%rsi)
, 847		// fxtract 
, 842		// fyl2x 
, 852		// fyl2xp1 
, 12		// haddpd (%rsi), %xmm0
, 12		// haddpd %xmm0, %xmm0
, 12		// haddps (%rsi), %xmm0
, 12		// haddps %xmm0, %xmm0
, 12		// hsubpd (%rsi), %xmm0
, 12		// hsubpd %xmm0, %xmm0
, 12		// hsubps (%rsi), %xmm0
, 12		// hsubps %xmm0, %xmm0
, 999		// idivw es:(%rax,%rax,1)
, 999		// idivl es:(%rax,%rax,1)
, 999		// idivq es:(%rax,%rax,1)
, 999		// idivb es:(%rax,%rax,1)
, 999		// idivw %ax
, 999		// idivl %eax
, 999		// idivq %rax
, 999		// idivb %al
, 999		// idivb  
, 9		// imulw (%rsi)
, 10		// imull (%rsi)
, 6		// imulq (%rsi)
, 6		// imulb (%rsi)
, 9		// imulw %cx
, 6		// imulw (%rsi), %cx
, 5		// imulw $0x4050, (%rsi), %cx
, 2		// imulw $0x4, (%rsi), %cx
, 6		// imulw %cx, %cx
, 9		// imulw $0x4050, %cx, %cx
, 9		// imulw $0x4, %cx, %cx
, 10		// imull %edx
, 6		// imull (%rsi), %edx
, 2		// imull $0xc0decafe, (%rsi), %edx
, 1		// imull $0x4, (%rsi), %edx
, 6		// imull %edx, %edx
, 6		// imull $0xc0decafe, %edx, %edx
, 6		// imull $0x4, %edx, %edx
, 9		// imulq %rdx
, 6		// imulq (%rsi), %rdx
, 1		// imulq $0xc0decafe, (%rsi), %rdx
, 1		// imulq $0x4, (%rsi), %rdx
, 6		// imulq %rdx, %rdx
, 6		// imulq $0xc0decafe, %rdx, %rdx
, 6		// imulq $0x4, %rdx, %rdx
, 6		// imulb %dl
, 9		// imulb %ch
, 999		// inb %ax, %al
, 999		// inb $0x0, %al
, 999		// inw %ax, %ax
, 999		// inw $0x0, %ax
, 999		// inl %ax, %eax
, 999		// inl $0x0, %eax
, 14		// incw (%rsi)
, 14		// incl (%rsi)
, 14		// incq (%rsi)
, 14		// incb (%rsi)
, 1		// incw %cx
, 1		// incl %edx
, 1		// incq %rdx
, 1		// incb %dl
, 1		// incb %ch
, 999		// ins %ax, es:(%rax,%rax,1)
, 999		// ins %ax, es:(%rax,%rax,1)
, 999		// ins %ax, es:(%rax,%rax,1)
, 999		// insb 
, 999		// insl 
, 1		// insertps $0x4, (%rsi), %xmm0
, 1		// insertps $0x4, %xmm0, %xmm0
, 999		// insw 
, 999		// int $0x0
, 999		// int $0x0
, 999		// invpcid es:(%rax,%rax,1), %rax
, 0		// iretw 
, 0		// iretl 
, 0		// iretq 
, 0		// ja 
, 0		// ja 
, 0		// ja , 
, 0		// ja , 
, 0		// ja 0
, 0		// ja , 0
, 0		// ja 0
, 0		// ja , 0
, 0		// jae 
, 0		// jae 
, 0		// jae , 
, 0		// jae , 
, 0		// jae 0
, 0		// jae , 0
, 0		// jae 0
, 0		// jae , 0
, 0		// jb 
, 0		// jb 
, 0		// jb , 
, 0		// jb , 
, 0		// jb 0
, 0		// jb , 0
, 0		// jb 0
, 0		// jb , 0
, 0		// jbe 
, 0		// jbe 
, 0		// jbe , 
, 0		// jbe , 
, 0		// jbe 0
, 0		// jbe , 0
, 0		// jbe 0
, 0		// jbe , 0
, 0		// jc 
, 0		// jc 
, 0		// jc , 
, 0		// jc , 
, 0		// jc 0
, 0		// jc , 0
, 0		// jc 0
, 0		// jc , 0
, 0		// je 
, 0		// je 
, 0		// je , 
, 0		// je , 
, 0		// je 0
, 0		// je , 0
, 0		// je 0
, 0		// je , 0
, 0		// jecxz 
, 0		// jecxz , 
, 0		// jecxz 0
, 0		// jecxz , 0
, 0		// jg 
, 0		// jg 
, 0		// jg , 
, 0		// jg , 
, 0		// jg 0
, 0		// jg , 0
, 0		// jg 0
, 0		// jg , 0
, 0		// jge 
, 0		// jge 
, 0		// jge , 
, 0		// jge , 
, 0		// jge 0
, 0		// jge , 0
, 0		// jge 0
, 0		// jge , 0
, 0		// jl 
, 0		// jl 
, 0		// jl , 
, 0		// jl , 
, 0		// jl 0
, 0		// jl , 0
, 0		// jl 0
, 0		// jl , 0
, 0		// jle 
, 0		// jle 
, 0		// jle , 
, 0		// jle , 
, 0		// jle 0
, 0		// jle , 0
, 0		// jle 0
, 0		// jle , 0
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq 
, 0		// jmpq 
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq %rax
, 0		// jmpq 0
, 0		// jmpq 0
, 0		// jna 
, 0		// jna 
, 0		// jna , 
, 0		// jna , 
, 0		// jna 0
, 0		// jna , 0
, 0		// jna 0
, 0		// jna , 0
, 0		// jnae 
, 0		// jnae 
, 0		// jnae , 
, 0		// jnae , 
, 0		// jnae 0
, 0		// jnae , 0
, 0		// jnae 0
, 0		// jnae , 0
, 0		// jnb 
, 0		// jnb 
, 0		// jnb , 
, 0		// jnb , 
, 0		// jnb 0
, 0		// jnb , 0
, 0		// jnb 0
, 0		// jnb , 0
, 0		// jnbe 
, 0		// jnbe 
, 0		// jnbe , 
, 0		// jnbe , 
, 0		// jnbe 0
, 0		// jnbe , 0
, 0		// jnbe 0
, 0		// jnbe , 0
, 0		// jnc 
, 0		// jnc 
, 0		// jnc , 
, 0		// jnc , 
, 0		// jnc 0
, 0		// jnc , 0
, 0		// jnc 0
, 0		// jnc , 0
, 0		// jne 
, 0		// jne 
, 0		// jne , 
, 0		// jne , 
, 0		// jne 0
, 0		// jne , 0
, 0		// jne 0
, 0		// jne , 0
, 0		// jng 
, 0		// jng 
, 0		// jng , 
, 0		// jng , 
, 0		// jng 0
, 0		// jng , 0
, 0		// jng 0
, 0		// jng , 0
, 0		// jnge 
, 0		// jnge 
, 0		// jnge , 
, 0		// jnge , 
, 0		// jnge 0
, 0		// jnge , 0
, 0		// jnge 0
, 0		// jnge , 0
, 0		// jnl 
, 0		// jnl 
, 0		// jnl , 
, 0		// jnl , 
, 0		// jnl 0
, 0		// jnl , 0
, 0		// jnl 0
, 0		// jnl , 0
, 0		// jnle 
, 0		// jnle 
, 0		// jnle , 
, 0		// jnle , 
, 0		// jnle 0
, 0		// jnle , 0
, 0		// jnle 0
, 0		// jnle , 0
, 0		// jno 
, 0		// jno 
, 0		// jno , 
, 0		// jno , 
, 0		// jno 0
, 0		// jno , 0
, 0		// jno 0
, 0		// jno , 0
, 0		// jnp 
, 0		// jnp 
, 0		// jnp , 
, 0		// jnp , 
, 0		// jnp 0
, 0		// jnp , 0
, 0		// jnp 0
, 0		// jnp , 0
, 0		// jns 
, 0		// jns 
, 0		// jns , 
, 0		// jns , 
, 0		// jns 0
, 0		// jns , 0
, 0		// jns 0
, 0		// jns , 0
, 0		// jnz 
, 0		// jnz 
, 0		// jnz , 
, 0		// jnz , 
, 0		// jnz 0
, 0		// jnz , 0
, 0		// jnz 0
, 0		// jnz , 0
, 0		// jo 
, 0		// jo 
, 0		// jo , 
, 0		// jo , 
, 0		// jo 0
, 0		// jo , 0
, 0		// jo 0
, 0		// jo , 0
, 0		// jp 
, 0		// jp 
, 0		// jp , 
, 0		// jp , 
, 0		// jp 0
, 0		// jp , 0
, 0		// jp 0
, 0		// jp , 0
, 0		// jpe 
, 0		// jpe 
, 0		// jpe , 
, 0		// jpe , 
, 0		// jpe 0
, 0		// jpe , 0
, 0		// jpe 0
, 0		// jpe , 0
, 0		// jpo 
, 0		// jpo 
, 0		// jpo , 
, 0		// jpo , 
, 0		// jpo 0
, 0		// jpo , 0
, 0		// jpo 0
, 0		// jpo , 0
, 0		// jrcxz 
, 0		// jrcxz , 
, 0		// jrcxz 0
, 0		// jrcxz , 0
, 0		// js 
, 0		// js 
, 0		// js , 
, 0		// js , 
, 0		// js 0
, 0		// js , 0
, 0		// js 0
, 0		// js , 0
, 0		// jz 
, 0		// jz 
, 0		// jz , 
, 0		// jz , 
, 0		// jz 0
, 0		// jz , 0
, 0		// jz 0
, 0		// jz , 0
, 1		// lahf 
, 377		// larw (%rsi), %cx
, 360		// larw %cx, %cx
, 372		// lar (%rsi), %edx
, 368		// lar %edx, %edx
, 369		// lar (%rsi), %rdx
, 360		// lar %edx, %rdx
, 1		// lddqu (%rsi), %xmm0
, 999		// ldmxcsr es:(%rax,%rax,1)
, 1		// leaw (%rsi), %cx
, 1		// leaw (%rsi), %cx
, 1		// leaw (%rsi), %cx
, 1		// leal (%rsi), %edx
, 1		// leal (%rsi), %edx
, 1		// leal (%rsi), %edx
, 1		// leaq (%rsi), %rdx
, 1		// leaq (%rsi), %rdx
, 1		// leaq (%rsi), %rdx
, 0		// leaveq 
, 0		// leavew 
, 14		// lfence 
, 999		// lfsw es:(%rax,%rax,1), %ax
, 999		// lfsl es:(%rax,%rax,1), %eax
, 999		// lfs es:(%rax,%rax,1), %rax
, 999		// lgsw es:(%rax,%rax,1), %ax
, 999		// lgsl es:(%rax,%rax,1), %eax
, 999		// lgs es:(%rax,%rax,1), %rax
, 999		// lock 
, 999		// lods es:(%rax,%rax,1)
, 999		// lods es:(%rax,%rax,1)
, 999		// lods es:(%rax,%rax,1)
, 999		// lods es:(%rax,%rax,1)
, 999		// lodsb 
, 999		// lodsl 
, 999		// lodsq 
, 999		// lodsw 
, 0		// loop 
, 0		// loop 0
, 0		// loope 
, 0		// loope 0
, 0		// loopne 
, 0		// loopne 0
, 388		// lslw (%rsi), %cx
, 360		// lslw %cx, %cx
, 367		// lsl (%rsi), %edx
, 361		// lsl %edx, %edx
, 393		// lsl (%rsi), %rdx
, 360		// lsl %edx, %rdx
, 999		// lssw es:(%rax,%rax,1), %ax
, 999		// lssl es:(%rax,%rax,1), %eax
, 999		// lss es:(%rax,%rax,1), %rax
, 6		// lzcntw (%rsi), %cx
, 6		// lzcntw %cx, %cx
, 6		// lzcntl (%rsi), %edx
, 6		// lzcntl %edx, %edx
, 6		// lzcntq (%rsi), %rdx
, 6		// lzcntq %rdx, %rdx
, 14		// maskmovdqu %xmm0, %xmm0
, 1		// maskmovq %mm0, %mm0
, 7		// maxpd (%rsi), %xmm0
, 7		// maxpd %xmm0, %xmm0
, 7		// maxps (%rsi), %xmm0
, 7		// maxps %xmm0, %xmm0
, 7		// maxsd (%rsi), %xmm0
, 7		// maxsd %xmm0, %xmm0
, 7		// maxss (%rsi), %xmm0
, 7		// maxss %xmm0, %xmm0
, 90		// mfence 
, 7		// minpd (%rsi), %xmm0
, 7		// minpd %xmm0, %xmm0
, 7		// minps (%rsi), %xmm0
, 7		// minps %xmm0, %xmm0
, 7		// minsd (%rsi), %xmm0
, 7		// minsd %xmm0, %xmm0
, 7		// minss (%rsi), %xmm0
, 7		// minss %xmm0, %xmm0
, 999		// monitor 
, 999		// movabsb es:0x0, %al
, 999		// movabsb , es:0x0, %al
, 999		// movabsw es:0x0, %ax
, 999		// movabsl es:0x0, %eax
, 4		// movw $0x4050, (%rsi)
, 1		// movw %cx, (%rsi)
, 999		// movw es, (%rsi)
, 1		// movl $0xc0decafe, (%rsi)
, 1		// movl %edx, (%rsi)
, 1		// movq $0xc0decafe, (%rsi)
, 1		// movq %rdx, (%rsi)
, 999		// movq es, (%rsi)
, 1		// movb $0x4, (%rsi)
, 1		// movb %dl, (%rsi)
, 1		// movb %ch, (%rsi)
, 999		// movabsw %ax, es:0x0
, 999		// movabsl %eax, es:0x0
, 999		// movabsq %rax, es:0x0
, 999		// movabsb %al, es:0x0
, 999		// movabsb , %al, es:0x0
, 5		// movw $0x4050, %cx
, 4		// movw $0x4050, %cx
, 1		// movw (%rsi), %cx
, 1		// movw %cx, %cx
, 1		// movw %cx, %cx
, 999		// movw es, %cx
, 1		// movl $0xc0decafe, %edx
, 1		// movl $0xc0decafe, %edx
, 1		// movl (%rsi), %edx
, 1		// movl %edx, %edx
, 1		// movl %edx, %edx
, 1		// movq $0xc0decafe, %rdx
, 1		// movq $0xbadeb0dec0decafe, %rdx
, 1		// movq (%rsi), %rdx
, 1		// movq %rdx, %rdx
, 1		// movq %rdx, %rdx
, 999		// movq es, %rdx
, 1		// movb $0x4, %dl
, 1		// movb $0x4, %dl
, 1		// movb (%rsi), %dl
, 1		// movb %dl, %dl
, 1		// movb %dl, %dl
, 4		// movb %ch, %dl
, 4		// movb %ch, %dl
, 999		// movabsq es:0x0, %rax
, 1		// movb $0x4, %ch
, 1		// movb $0x4, %ch
, 1		// movb (%rsi), %ch
, 1		// movb %dl, %ch
, 1		// movb %dl, %ch
, 1		// movb %ch, %ch
, 1		// movb %ch, %ch
, 999		// mov (%rsi), es
, 999		// mov (%rsi), es
, 999		// mov %ax, es
, 999		// mov %rax, es
, 1		// movapd %xmm0, (%rsi)
, 1		// movapd (%rsi), %xmm0
, 1		// movapd %xmm0, %xmm0
, 1		// movapd %xmm0, %xmm0
, 1		// movaps %xmm0, (%rsi)
, 1		// movaps (%rsi), %xmm0
, 1		// movaps %xmm0, %xmm0
, 1		// movaps %xmm0, %xmm0
, 1		// movbew %cx, (%rsi)
, 1		// movbel %edx, (%rsi)
, 1		// movbeq %rdx, (%rsi)
, 1		// movbew (%rsi), %cx
, 1		// movbel (%rsi), %edx
, 1		// movbeq (%rsi), %rdx
, 1		// movd %mm0, (%rsi)
, 1		// movd %xmm0, (%rsi)
, 1		// movd (%rsi), %mm0
, 1		// movd %edx, %mm0
, 1		// movd %mm0, %edx
, 1		// movd %xmm0, %edx
, 1		// movd (%rsi), %xmm0
, 1		// movd %edx, %xmm0
, 1		// movddup (%rsi), %xmm0
, 1		// movddup %xmm0, %xmm0
, 1		// movdq2q %xmm0, %mm0
, 1		// movdqa %xmm0, (%rsi)
, 1		// movdqa (%rsi), %xmm0
, 1		// movdqa %xmm0, %xmm0
, 1		// movdqa %xmm0, %xmm0
, 1		// movdqu %xmm0, (%rsi)
, 1		// movdqu (%rsi), %xmm0
, 1		// movdqu %xmm0, %xmm0
, 1		// movdqu %xmm0, %xmm0
, 1		// movhlps %xmm0, %xmm0
, 1		// movhpd %xmm0, (%rsi)
, 1		// movhpd (%rsi), %xmm0
, 1		// movhps %xmm0, (%rsi)
, 1		// movhps (%rsi), %xmm0
, 1		// movlhps %xmm0, %xmm0
, 1		// movlpd %xmm0, (%rsi)
, 1		// movlpd (%rsi), %xmm0
, 1		// movlps %xmm0, (%rsi)
, 1		// movlps (%rsi), %xmm0
, 1		// movmskpd %xmm0, %edx
, 1		// movmskpd %xmm0, %rdx
, 1		// movmskps %xmm0, %edx
, 1		// movmskps %xmm0, %rdx
, 1		// movntdq %xmm0, (%rsi)
, 1		// movntdq %ymm0, (%rsi)
, 1		// movntdqa (%rsi), %xmm0
, 1		// movnti %edx, (%rsi)
, 1		// movnti %rdx, (%rsi)
, 1		// movntpd %xmm0, (%rsi)
, 1		// movntps %xmm0, (%rsi)
, 1		// movntq %mm0, (%rsi)
, 1		// movq %mm0, (%rsi)
, 1		// movq %mm0, (%rsi)
, 1		// movq %xmm0, (%rsi)
, 1		// movq %xmm0, (%rsi)
, 1		// movq (%rsi), %mm0
, 1		// movq (%rsi), %mm0
, 1		// movq %mm0, %mm0
, 1		// movq %mm0, %mm0
, 1		// movq %rdx, %mm0
, 1		// movq %mm0, %rdx
, 1		// movq %xmm0, %rdx
, 1		// movq (%rsi), %xmm0
, 1		// movq (%rsi), %xmm0
, 1		// movq %rdx, %xmm0
, 1		// movq %xmm0, %xmm0
, 1		// movq %xmm0, %xmm0
, 1		// movq2dq %mm0, %xmm0
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movsb 
, 999		// movsl 
, 3		// movsd %xmm0, (%rsi)
, 1		// movsd (%rsi), %xmm0
, 1		// movsd %xmm0, %xmm0
, 1		// movsd %xmm0, %xmm0
, 1		// movshdup (%rsi), %xmm0
, 1		// movshdup %xmm0, %xmm0
, 1		// movsldup (%rsi), %xmm0
, 1		// movsldup %xmm0, %xmm0
, 999		// movsq 
, 3		// movss %xmm0, (%rsi)
, 1		// movss (%rsi), %xmm0
, 1		// movss %xmm0, %xmm0
, 1		// movss %xmm0, %xmm0
, 999		// movsw 
, 1		// movsbw (%rsi), %cx
, 1		// movsbw %dl, %cx
, 4		// movsbw %ch, %cx
, 1		// movswl (%rsi), %edx
, 1		// movsbl (%rsi), %edx
, 1		// movswl %cx, %edx
, 1		// movsbl %dl, %edx
, 1		// movsbl %ch, %edx
, 1		// movswq (%rsi), %rdx
, 1		// movsbq (%rsi), %rdx
, 1		// movswq %cx, %rdx
, 1		// movsbq %dl, %rdx
, 1		// movslq (%rsi), %rdx
, 1		// movslq %edx, %rdx
, 3		// movupd %xmm0, (%rsi)
, 1		// movupd (%rsi), %xmm0
, 1		// movupd %xmm0, %xmm0
, 1		// movupd %xmm0, %xmm0
, 1		// movups %xmm0, (%rsi)
, 1		// movups (%rsi), %xmm0
, 1		// movups %xmm0, %xmm0
, 1		// movups %xmm0, %xmm0
, 1		// movzbw (%rsi), %cx
, 1		// movzbw %dl, %cx
, 4		// movzbw %ch, %cx
, 1		// movzwl (%rsi), %edx
, 1		// movzbl (%rsi), %edx
, 1		// movzwl %cx, %edx
, 1		// movzbl %dl, %edx
, 1		// movzbl %ch, %edx
, 1		// movzwq (%rsi), %rdx
, 1		// movzbq (%rsi), %rdx
, 1		// movzwq %cx, %rdx
, 1		// movzbq %dl, %rdx
, 14		// mpsadbw $0x4, (%rsi), %xmm0
, 17		// mpsadbw $0x4, %xmm0, %xmm0
, 10		// mulw (%rsi)
, 10		// mull (%rsi)
, 6		// mulq (%rsi)
, 6		// mulb (%rsi)
, 10		// mulw %cx
, 10		// mull %edx
, 9		// mulq %rdx
, 6		// mulb %dl
, 9		// mulb %ch
, 12		// mulpd (%rsi), %xmm0
, 12		// mulpd %xmm0, %xmm0
, 12		// mulps (%rsi), %xmm0
, 12		// mulps %xmm0, %xmm0
, 12		// mulsd (%rsi), %xmm0
, 12		// mulsd %xmm0, %xmm0
, 12		// mulss (%rsi), %xmm0
, 12		// mulss %xmm0, %xmm0
, 10		// mulxl (%rsi), %edx, %edx
, 10		// mulxl %edx, %edx, %edx
, 9		// mulxq (%rsi), %rdx, %rdx
, 9		// mulxq %rdx, %rdx, %rdx
, 999		// mwait 
, 17		// negw (%rsi)
, 16		// negl (%rsi)
, 16		// negq (%rsi)
, 16		// negb (%rsi)
, 1		// negw %cx
, 1		// negl %edx
, 1		// negq %rdx
, 1		// negb %dl
, 1		// negb %ch
, 1		// nop 
, 1		// nopw (%rsi)
, 1		// nopl (%rsi)
, 1		// nopw %cx
, 1		// nopl %edx
, 14		// notw (%rsi)
, 14		// notl (%rsi)
, 14		// notq (%rsi)
, 14		// notb (%rsi)
, 1		// notw %cx
, 1		// notl %edx
, 1		// notq %rdx
, 1		// notb %dl
, 1		// notb %ch
, 1		// orb $0x4, %al
, 5		// orw $0x4050, %ax
, 1		// orl $0xc0decafe, %eax
, 16		// orw $0x4050, (%rsi)
, 16		// orw $0x4, (%rsi)
, 14		// orw %cx, (%rsi)
, 16		// orl $0xc0decafe, (%rsi)
, 16		// orl $0x4, (%rsi)
, 14		// orl %edx, (%rsi)
, 16		// orq $0xc0decafe, (%rsi)
, 16		// orq $0x4, (%rsi)
, 14		// orq %rdx, (%rsi)
, 17		// orb $0x4, (%rsi)
, 15		// orb %dl, (%rsi)
, 18		// orb %ch, (%rsi)
, 6		// orw $0x4050, %cx
, 1		// orw $0x4, %cx
, 1		// orw (%rsi), %cx
, 1		// orw %cx, %cx
, 1		// orw %cx, %cx
, 1		// orl $0xc0decafe, %edx
, 1		// orl $0x4, %edx
, 1		// orl (%rsi), %edx
, 1		// orl %edx, %edx
, 1		// orl %edx, %edx
, 1		// orq $0xc0decafe, %rdx
, 1		// orq $0x4, %rdx
, 1		// orq (%rsi), %rdx
, 1		// orq %rdx, %rdx
, 1		// orq %rdx, %rdx
, 1		// orb $0x4, %dl
, 1		// orb (%rsi), %dl
, 1		// orb %dl, %dl
, 1		// orb %dl, %dl
, 4		// orb %ch, %dl
, 4		// orb %ch, %dl
, 1		// orq $0xc0decafe, %rax
, 1		// orb $0x4, %ch
, 1		// orb (%rsi), %ch
, 1		// orb %dl, %ch
, 1		// orb %dl, %ch
, 1		// orb %ch, %ch
, 1		// orb %ch, %ch
, 1		// orpd (%rsi), %xmm0
, 1		// orpd %xmm0, %xmm0
, 1		// orps (%rsi), %xmm0
, 1		// orps %xmm0, %xmm0
, 999		// outb %al, %ax
, 999		// outw %ax, %ax
, 999		// outl %eax, %ax
, 999		// outb %al, $0x0
, 999		// outw %ax, $0x0
, 999		// outl %eax, $0x0
, 999		// outs es:(%rax,%rax,1), %ax
, 999		// outs es:(%rax,%rax,1), %ax
, 999		// outs es:(%rax,%rax,1), %ax
, 999		// outsb 
, 999		// outsl 
, 999		// outsw 
, 1		// pabsb (%rsi), %mm0
, 1		// pabsb %mm0, %mm0
, 1		// pabsb (%rsi), %xmm0
, 1		// pabsb %xmm0, %xmm0
, 1		// pabsd (%rsi), %mm0
, 1		// pabsd %mm0, %mm0
, 1		// pabsd (%rsi), %xmm0
, 1		// pabsd %xmm0, %xmm0
, 1		// pabsw (%rsi), %mm0
, 1		// pabsw %mm0, %mm0
, 1		// pabsw (%rsi), %xmm0
, 1		// pabsw %xmm0, %xmm0
, 4		// packssdw (%rsi), %mm0
, 4		// packssdw %mm0, %mm0
, 1		// packssdw (%rsi), %xmm0
, 1		// packssdw %xmm0, %xmm0
, 4		// packsswb (%rsi), %mm0
, 4		// packsswb %mm0, %mm0
, 1		// packsswb (%rsi), %xmm0
, 1		// packsswb %xmm0, %xmm0
, 1		// packusdw (%rsi), %xmm0
, 1		// packusdw %xmm0, %xmm0
, 4		// packuswb (%rsi), %mm0
, 4		// packuswb %mm0, %mm0
, 1		// packuswb (%rsi), %xmm0
, 1		// packuswb %xmm0, %xmm0
, 1		// paddb (%rsi), %mm0
, 1		// paddb %mm0, %mm0
, 1		// paddb (%rsi), %xmm0
, 1		// paddb %xmm0, %xmm0
, 1		// paddd (%rsi), %mm0
, 1		// paddd %mm0, %mm0
, 1		// paddd (%rsi), %xmm0
, 1		// paddd %xmm0, %xmm0
, 1		// paddq (%rsi), %mm0
, 1		// paddq %mm0, %mm0
, 1		// paddq (%rsi), %xmm0
, 1		// paddq %xmm0, %xmm0
, 1		// paddsb (%rsi), %mm0
, 1		// paddsb %mm0, %mm0
, 1		// paddsb (%rsi), %xmm0
, 1		// paddsb %xmm0, %xmm0
, 1		// paddsw (%rsi), %mm0
, 1		// paddsw %mm0, %mm0
, 1		// paddsw (%rsi), %xmm0
, 1		// paddsw %xmm0, %xmm0
, 1		// paddusb (%rsi), %mm0
, 1		// paddusb %mm0, %mm0
, 1		// paddusb (%rsi), %xmm0
, 1		// paddusb %xmm0, %xmm0
, 1		// paddusw (%rsi), %mm0
, 1		// paddusw %mm0, %mm0
, 1		// paddusw (%rsi), %xmm0
, 1		// paddusw %xmm0, %xmm0
, 1		// paddw (%rsi), %mm0
, 1		// paddw %mm0, %mm0
, 1		// paddw (%rsi), %xmm0
, 1		// paddw %xmm0, %xmm0
, 1		// palignr $0x4, (%rsi), %mm0
, 1		// palignr $0x4, %mm0, %mm0
, 1		// palignr $0x4, (%rsi), %xmm0
, 1		// palignr $0x4, %xmm0, %xmm0
, 1		// pand (%rsi), %mm0
, 1		// pand %mm0, %mm0
, 1		// pand (%rsi), %xmm0
, 1		// pand %xmm0, %xmm0
, 1		// pandn (%rsi), %mm0
, 1		// pandn %mm0, %mm0
, 1		// pandn (%rsi), %xmm0
, 1		// pandn %xmm0, %xmm0
, 23		// pause 
, 1		// pavgb (%rsi), %mm0
, 1		// pavgb %mm0, %mm0
, 1		// pavgb (%rsi), %xmm0
, 1		// pavgb %xmm0, %xmm0
, 1		// pavgw (%rsi), %mm0
, 1		// pavgw %mm0, %mm0
, 1		// pavgw (%rsi), %xmm0
, 1		// pavgw %xmm0, %xmm0
, 4		// pblendvb %xmm0, (%rsi), %xmm0
, 4		// pblendvb %xmm0, %xmm0, %xmm0
, 1		// pblendw $0x4, (%rsi), %xmm0
, 1		// pblendw $0x4, %xmm0, %xmm0
, 17		// pclmulqdq $0x4, (%rsi), %xmm0
, 17		// pclmulqdq $0x4, %xmm0, %xmm0
, 1		// pcmpeqb (%rsi), %mm0
, 1		// pcmpeqb %mm0, %mm0
, 1		// pcmpeqb (%rsi), %xmm0
, 1		// pcmpeqb %xmm0, %xmm0
, 1		// pcmpeqd (%rsi), %mm0
, 1		// pcmpeqd %mm0, %mm0
, 1		// pcmpeqd (%rsi), %xmm0
, 1		// pcmpeqd %xmm0, %xmm0
, 1		// pcmpeqq (%rsi), %xmm0
, 1		// pcmpeqq %xmm0, %xmm0
, 1		// pcmpeqw (%rsi), %mm0
, 1		// pcmpeqw %mm0, %mm0
, 1		// pcmpeqw (%rsi), %xmm0
, 1		// pcmpeqw %xmm0, %xmm0
, 9		// pcmpestri $0x4, (%rsi), %xmm0
, 9		// pcmpestri $0x4, %xmm0, %xmm0
, 27		// pcmpestrm $0x4, (%rsi), %xmm0
, 27		// pcmpestrm $0x4, %xmm0, %xmm0
, 1		// pcmpgtb (%rsi), %mm0
, 1		// pcmpgtb %mm0, %mm0
, 1		// pcmpgtb (%rsi), %xmm0
, 1		// pcmpgtb %xmm0, %xmm0
, 1		// pcmpgtd (%rsi), %mm0
, 1		// pcmpgtd %mm0, %mm0
, 1		// pcmpgtd (%rsi), %xmm0
, 1		// pcmpgtd %xmm0, %xmm0
, 12		// pcmpgtq (%rsi), %xmm0
, 2		// pcmpgtq %xmm0, %xmm0
, 1		// pcmpgtw (%rsi), %mm0
, 1		// pcmpgtw %mm0, %mm0
, 1		// pcmpgtw (%rsi), %xmm0
, 1		// pcmpgtw %xmm0, %xmm0
, 7		// pcmpistri $0x4, (%rsi), %xmm0
, 7		// pcmpistri $0x4, %xmm0, %xmm0
, 27		// pcmpistrm $0x4, (%rsi), %xmm0
, 26		// pcmpistrm $0x4, %xmm0, %xmm0
, 6		// pdepl (%rsi), %edx, %edx
, 6		// pdepl %edx, %edx, %edx
, 6		// pdepq (%rsi), %rdx, %rdx
, 6		// pdepq %rdx, %rdx, %rdx
, 6		// pextl (%rsi), %edx, %edx
, 6		// pextl %edx, %edx, %edx
, 6		// pextq (%rsi), %rdx, %rdx
, 6		// pextq %rdx, %rdx, %rdx
, 1		// pextrb $0x4, %xmm0, (%rsi)
, 1		// pextrb $0x4, %xmm0, %edx
, 1		// pextrb $0x4, %xmm0, %rdx
, 1		// pextrd $0x4, %xmm0, (%rsi)
, 1		// pextrd $0x4, %xmm0, %edx
, 1		// pextrq $0x4, %xmm0, (%rsi)
, 1		// pextrq $0x4, %xmm0, %rdx
, 1		// pextrw $0x4, %xmm0, (%rsi)
, 1		// pextrw $0x4, %mm0, %edx
, 1		// pextrw $0x4, %xmm0, %edx
, 1		// pextrw $0x4, %xmm0, %edx
, 1		// pextrw $0x4, %mm0, %rdx
, 1		// pextrw $0x4, %xmm0, %rdx
, 1		// pextrw $0x4, %xmm0, %rdx
, 7		// phaddd (%rsi), %mm0
, 7		// phaddd %mm0, %mm0
, 7		// phaddd (%rsi), %xmm0
, 7		// phaddd %xmm0, %xmm0
, 7		// phaddsw (%rsi), %mm0
, 7		// phaddsw %mm0, %mm0
, 7		// phaddsw (%rsi), %xmm0
, 7		// phaddsw %xmm0, %xmm0
, 7		// phaddw (%rsi), %mm0
, 7		// phaddw %mm0, %mm0
, 7		// phaddw (%rsi), %xmm0
, 7		// phaddw %xmm0, %xmm0
, 1		// phminposuw (%rsi), %xmm0
, 12		// phminposuw %xmm0, %xmm0
, 7		// phsubd (%rsi), %mm0
, 7		// phsubd %mm0, %mm0
, 7		// phsubd (%rsi), %xmm0
, 7		// phsubd %xmm0, %xmm0
, 7		// phsubsw (%rsi), %mm0
, 7		// phsubsw %mm0, %mm0
, 7		// phsubsw (%rsi), %xmm0
, 7		// phsubsw %xmm0, %xmm0
, 7		// phsubw (%rsi), %mm0
, 7		// phsubw %mm0, %mm0
, 7		// phsubw (%rsi), %xmm0
, 7		// phsubw %xmm0, %xmm0
, 1		// pinsrb $0x4, (%rsi), %xmm0
, 4		// pinsrb $0x4, %edx, %xmm0
, 1		// pinsrd $0x4, (%rsi), %xmm0
, 4		// pinsrd $0x4, %edx, %xmm0
, 1		// pinsrw $0x4, (%rsi), %mm0
, 4		// pinsrw $0x4, %edx, %mm0
, 1		// pinsrw $0x4, (%rsi), %xmm0
, 4		// pinsrw $0x4, %edx, %xmm0
, 12		// pmaddubsw (%rsi), %mm0
, 12		// pmaddubsw %mm0, %mm0
, 12		// pmaddubsw (%rsi), %xmm0
, 12		// pmaddubsw %xmm0, %xmm0
, 12		// pmaddwd (%rsi), %mm0
, 12		// pmaddwd %mm0, %mm0
, 12		// pmaddwd (%rsi), %xmm0
, 12		// pmaddwd %xmm0, %xmm0
, 1		// pmaxsb (%rsi), %xmm0
, 1		// pmaxsb %xmm0, %xmm0
, 1		// pmaxsd (%rsi), %xmm0
, 1		// pmaxsd %xmm0, %xmm0
, 1		// pmaxsw (%rsi), %mm0
, 1		// pmaxsw %mm0, %mm0
, 1		// pmaxsw (%rsi), %xmm0
, 1		// pmaxsw %xmm0, %xmm0
, 1		// pmaxub (%rsi), %mm0
, 1		// pmaxub %mm0, %mm0
, 1		// pmaxub (%rsi), %xmm0
, 1		// pmaxub %xmm0, %xmm0
, 1		// pmaxud (%rsi), %xmm0
, 1		// pmaxud %xmm0, %xmm0
, 1		// pmaxuw (%rsi), %xmm0
, 1		// pmaxuw %xmm0, %xmm0
, 1		// pminsb (%rsi), %xmm0
, 1		// pminsb %xmm0, %xmm0
, 1		// pminsd (%rsi), %xmm0
, 1		// pminsd %xmm0, %xmm0
, 1		// pminsw (%rsi), %mm0
, 1		// pminsw %mm0, %mm0
, 1		// pminsw (%rsi), %xmm0
, 1		// pminsw %xmm0, %xmm0
, 1		// pminub (%rsi), %mm0
, 1		// pminub %mm0, %mm0
, 1		// pminub (%rsi), %xmm0
, 1		// pminub %xmm0, %xmm0
, 1		// pminud (%rsi), %xmm0
, 1		// pminud %xmm0, %xmm0
, 1		// pminuw (%rsi), %xmm0
, 1		// pminuw %xmm0, %xmm0
, 1		// pmovmskb %mm0, %edx
, 1		// pmovmskb %xmm0, %edx
, 1		// pmovmskb %mm0, %rdx
, 1		// pmovmskb %xmm0, %rdx
, 1		// pmovsxbd (%rsi), %xmm0
, 1		// pmovsxbd %xmm0, %xmm0
, 1		// pmovsxbq (%rsi), %xmm0
, 1		// pmovsxbq %xmm0, %xmm0
, 1		// pmovsxbw (%rsi), %xmm0
, 1		// pmovsxbw %xmm0, %xmm0
, 1		// pmovsxdq (%rsi), %xmm0
, 1		// pmovsxdq %xmm0, %xmm0
, 1		// pmovsxwd (%rsi), %xmm0
, 1		// pmovsxwd %xmm0, %xmm0
, 1		// pmovsxwq (%rsi), %xmm0
, 1		// pmovsxwq %xmm0, %xmm0
, 1		// pmovzxbd (%rsi), %xmm0
, 1		// pmovzxbd %xmm0, %xmm0
, 1		// pmovzxbq (%rsi), %xmm0
, 1		// pmovzxbq %xmm0, %xmm0
, 1		// pmovzxbw (%rsi), %xmm0
, 1		// pmovzxbw %xmm0, %xmm0
, 1		// pmovzxdq (%rsi), %xmm0
, 1		// pmovzxdq %xmm0, %xmm0
, 1		// pmovzxwd (%rsi), %xmm0
, 1		// pmovzxwd %xmm0, %xmm0
, 1		// pmovzxwq (%rsi), %xmm0
, 1		// pmovzxwq %xmm0, %xmm0
, 12		// pmuldq (%rsi), %xmm0
, 12		// pmuldq %xmm0, %xmm0
, 12		// pmulhrsw (%rsi), %mm0
, 12		// pmulhrsw %mm0, %mm0
, 12		// pmulhrsw (%rsi), %xmm0
, 12		// pmulhrsw %xmm0, %xmm0
, 12		// pmulhuw (%rsi), %mm0
, 12		// pmulhuw %mm0, %mm0
, 12		// pmulhuw (%rsi), %xmm0
, 12		// pmulhuw %xmm0, %xmm0
, 12		// pmulhw (%rsi), %mm0
, 12		// pmulhw %mm0, %mm0
, 12		// pmulhw (%rsi), %xmm0
, 12		// pmulhw %xmm0, %xmm0
, 24		// pmulld (%rsi), %xmm0
, 24		// pmulld %xmm0, %xmm0
, 12		// pmullw (%rsi), %mm0
, 12		// pmullw %mm0, %mm0
, 12		// pmullw (%rsi), %xmm0
, 12		// pmullw %xmm0, %xmm0
, 12		// pmuludq (%rsi), %mm0
, 12		// pmuludq %mm0, %mm0
, 12		// pmuludq (%rsi), %xmm0
, 12		// pmuludq %xmm0, %xmm0
, 999		// popq es
, 999		// popq , es
, 999		// popq es
, 999		// popq , es
, 999		// popw es:(%rax,%rax,1)
, 999		// popq es:(%rax,%rax,1)
, 999		// popw %ax
, 999		// popw %ax
, 999		// popq %rax
, 999		// popq %rax
, 6		// popcntw (%rsi), %cx
, 6		// popcntw %cx, %cx
, 6		// popcntl (%rsi), %edx
, 6		// popcntl %edx, %edx
, 6		// popcntq (%rsi), %rdx
, 6		// popcntq %rdx, %rdx
, 999		// popf 
, 999		// popfq 
, 1		// por (%rsi), %mm0
, 1		// por %mm0, %mm0
, 1		// por (%rsi), %xmm0
, 1		// por %xmm0, %xmm0
, 1		// prefetchnta (%rsi)
, 1		// prefetcht0 (%rsi)
, 1		// prefetcht1 (%rsi)
, 1		// prefetcht2 (%rsi)
, 12		// psadbw (%rsi), %mm0
, 12		// psadbw %mm0, %mm0
, 12		// psadbw (%rsi), %xmm0
, 12		// psadbw %xmm0, %xmm0
, 1		// pshufb (%rsi), %mm0
, 1		// pshufb %mm0, %mm0
, 1		// pshufb (%rsi), %xmm0
, 1		// pshufb %xmm0, %xmm0
, 1		// pshufd $0x4, (%rsi), %xmm0
, 1		// pshufd $0x4, %xmm0, %xmm0
, 1		// pshufhw $0x4, (%rsi), %xmm0
, 1		// pshufhw $0x4, %xmm0, %xmm0
, 1		// pshuflw $0x4, (%rsi), %xmm0
, 1		// pshuflw $0x4, %xmm0, %xmm0
, 1		// pshufw $0x4, (%rsi), %mm0
, 1		// pshufw $0x4, %mm0, %mm0
, 1		// psignb (%rsi), %mm0
, 1		// psignb %mm0, %mm0
, 1		// psignb (%rsi), %xmm0
, 1		// psignb %xmm0, %xmm0
, 1		// psignd (%rsi), %mm0
, 1		// psignd %mm0, %mm0
, 1		// psignd (%rsi), %xmm0
, 1		// psignd %xmm0, %xmm0
, 1		// psignw (%rsi), %mm0
, 1		// psignw %mm0, %mm0
, 1		// psignw (%rsi), %xmm0
, 1		// psignw %xmm0, %xmm0
, 1		// pslld $0x4, %mm0
, 1		// pslld (%rsi), %mm0
, 1		// pslld %mm0, %mm0
, 1		// pslld $0x4, %xmm0
, 2		// pslld (%rsi), %xmm0
, 4		// pslld %xmm0, %xmm0
, 1		// pslldq $0x4, %xmm0
, 1		// psllq $0x4, %mm0
, 1		// psllq (%rsi), %mm0
, 1		// psllq %mm0, %mm0
, 1		// psllq $0x4, %xmm0
, 1		// psllq (%rsi), %xmm0
, 4		// psllq %xmm0, %xmm0
, 1		// psllw $0x4, %mm0
, 1		// psllw (%rsi), %mm0
, 1		// psllw %mm0, %mm0
, 1		// psllw $0x4, %xmm0
, 1		// psllw (%rsi), %xmm0
, 4		// psllw %xmm0, %xmm0
, 1		// psrad $0x4, %mm0
, 1		// psrad (%rsi), %mm0
, 1		// psrad %mm0, %mm0
, 1		// psrad $0x4, %xmm0
, 1		// psrad (%rsi), %xmm0
, 4		// psrad %xmm0, %xmm0
, 1		// psraw $0x4, %mm0
, 1		// psraw (%rsi), %mm0
, 1		// psraw %mm0, %mm0
, 1		// psraw $0x4, %xmm0
, 1		// psraw (%rsi), %xmm0
, 4		// psraw %xmm0, %xmm0
, 1		// psrld $0x4, %mm0
, 1		// psrld (%rsi), %mm0
, 1		// psrld %mm0, %mm0
, 1		// psrld $0x4, %xmm0
, 1		// psrld (%rsi), %xmm0
, 4		// psrld %xmm0, %xmm0
, 1		// psrldq $0x4, %xmm0
, 1		// psrlq $0x4, %mm0
, 1		// psrlq (%rsi), %mm0
, 1		// psrlq %mm0, %mm0
, 1		// psrlq $0x4, %xmm0
, 1		// psrlq (%rsi), %xmm0
, 4		// psrlq %xmm0, %xmm0
, 1		// psrlw $0x4, %mm0
, 1		// psrlw (%rsi), %mm0
, 1		// psrlw %mm0, %mm0
, 1		// psrlw $0x4, %xmm0
, 1		// psrlw (%rsi), %xmm0
, 4		// psrlw %xmm0, %xmm0
, 1		// psubb (%rsi), %mm0
, 1		// psubb %mm0, %mm0
, 1		// psubb (%rsi), %xmm0
, 1		// psubb %xmm0, %xmm0
, 1		// psubd (%rsi), %mm0
, 1		// psubd %mm0, %mm0
, 1		// psubd (%rsi), %xmm0
, 1		// psubd %xmm0, %xmm0
, 1		// psubq (%rsi), %mm0
, 1		// psubq %mm0, %mm0
, 1		// psubq (%rsi), %xmm0
, 1		// psubq %xmm0, %xmm0
, 1		// psubsb (%rsi), %mm0
, 1		// psubsb %mm0, %mm0
, 1		// psubsb (%rsi), %xmm0
, 1		// psubsb %xmm0, %xmm0
, 1		// psubsw (%rsi), %mm0
, 1		// psubsw %mm0, %mm0
, 1		// psubsw (%rsi), %xmm0
, 1		// psubsw %xmm0, %xmm0
, 1		// psubusb (%rsi), %mm0
, 1		// psubusb %mm0, %mm0
, 1		// psubusb (%rsi), %xmm0
, 1		// psubusb %xmm0, %xmm0
, 1		// psubusw (%rsi), %mm0
, 1		// psubusw %mm0, %mm0
, 1		// psubusw (%rsi), %xmm0
, 1		// psubusw %xmm0, %xmm0
, 1		// psubw (%rsi), %mm0
, 1		// psubw %mm0, %mm0
, 1		// psubw (%rsi), %xmm0
, 1		// psubw %xmm0, %xmm0
, 1		// ptest (%rsi), %xmm0
, 1		// ptest %xmm0, %xmm0
, 1		// punpckhbw (%rsi), %mm0
, 1		// punpckhbw %mm0, %mm0
, 1		// punpckhbw (%rsi), %xmm0
, 1		// punpckhbw %xmm0, %xmm0
, 1		// punpckhdq (%rsi), %mm0
, 1		// punpckhdq %mm0, %mm0
, 1		// punpckhdq (%rsi), %xmm0
, 1		// punpckhdq %xmm0, %xmm0
, 1		// punpckhqdq (%rsi), %xmm0
, 1		// punpckhqdq %xmm0, %xmm0
, 1		// punpckhwd (%rsi), %mm0
, 1		// punpckhwd %mm0, %mm0
, 1		// punpckhwd (%rsi), %xmm0
, 1		// punpckhwd %xmm0, %xmm0
, 1		// punpcklbw (%rsi), %mm0
, 1		// punpcklbw %mm0, %mm0
, 1		// punpcklbw (%rsi), %xmm0
, 1		// punpcklbw %xmm0, %xmm0
, 1		// punpckldq (%rsi), %mm0
, 1		// punpckldq %mm0, %mm0
, 1		// punpckldq (%rsi), %xmm0
, 1		// punpckldq %xmm0, %xmm0
, 1		// punpcklqdq (%rsi), %xmm0
, 1		// punpcklqdq %xmm0, %xmm0
, 1		// punpcklwd (%rsi), %mm0
, 1		// punpcklwd %mm0, %mm0
, 1		// punpcklwd (%rsi), %xmm0
, 1		// punpcklwd %xmm0, %xmm0
, 999		// pushq es
, 999		// pushq es
, 999		// pushq $0x0
, 999		// pushq $0x0
, 999		// pushq $0x0
, 999		// pushw es:(%rax,%rax,1)
, 999		// pushq es:(%rax,%rax,1)
, 999		// pushw %ax
, 999		// pushw %ax
, 999		// pushq %rax
, 999		// pushq %rax
, 999		// pushf 
, 999		// pushfq 
, 1		// pxor (%rsi), %mm0
, 1		// pxor %mm0, %mm0
, 1		// pxor (%rsi), %xmm0
, 1		// pxor %xmm0, %xmm0
, 30		// rclw %cl, (%rsi)
, 30		// rclw $0x4, (%rsi)
, 19		// rclw $0x1, (%rsi)
, 30		// rcll %cl, (%rsi)
, 30		// rcll $0x4, (%rsi)
, 19		// rcll $0x1, (%rsi)
, 30		// rclq %cl, (%rsi)
, 30		// rclq $0x4, (%rsi)
, 19		// rclq $0x1, (%rsi)
, 26		// rclb %cl, (%rsi)
, 26		// rclb $0x4, (%rsi)
, 19		// rclb $0x1, (%rsi)
, 17		// rclw %cl, %cx
, 17		// rclw $0x4, %cx
, 4		// rclw $0x1, %cx
, 17		// rcll %cl, %edx
, 17		// rcll $0x4, %edx
, 4		// rcll $0x1, %edx
, 17		// rclq %cl, %rdx
, 17		// rclq $0x4, %rdx
, 4		// rclq $0x1, %rdx
, 15		// rclb %cl, %dl
, 15		// rclb $0x4, %dl
, 4		// rclb $0x1, %dl
, 15		// rclb %cl, %ch
, 15		// rclb $0x4, %ch
, 4		// rclb $0x1, %ch
, 1		// rcpps (%rsi), %xmm0
, 12		// rcpps %xmm0, %xmm0
, 12		// rcpss (%rsi), %xmm0
, 12		// rcpss %xmm0, %xmm0
, 27		// rcrw %cl, (%rsi)
, 27		// rcrw $0x4, (%rsi)
, 19		// rcrw $0x1, (%rsi)
, 27		// rcrl %cl, (%rsi)
, 27		// rcrl $0x4, (%rsi)
, 19		// rcrl $0x1, (%rsi)
, 27		// rcrq %cl, (%rsi)
, 27		// rcrq $0x4, (%rsi)
, 19		// rcrq $0x1, (%rsi)
, 30		// rcrb %cl, (%rsi)
, 30		// rcrb $0x4, (%rsi)
, 19		// rcrb $0x1, (%rsi)
, 17		// rcrw %cl, %cx
, 14		// rcrw $0x4, %cx
, 4		// rcrw $0x1, %cx
, 14		// rcrl %cl, %edx
, 14		// rcrl $0x4, %edx
, 4		// rcrl $0x1, %edx
, 14		// rcrq %cl, %rdx
, 14		// rcrq $0x4, %rdx
, 4		// rcrq $0x1, %rdx
, 20		// rcrb %cl, %dl
, 20		// rcrb $0x4, %dl
, 4		// rcrb $0x1, %dl
, 20		// rcrb %cl, %ch
, 20		// rcrb $0x4, %ch
, 4		// rcrb $0x1, %ch
, 999		// rdfsbase %eax
, 999		// rdfsbase %rax
, 999		// rdgsbase %eax
, 999		// rdgsbase %rax
, 864		// rdrand %cx
, 861		// rdrand %edx
, 862		// rdrand %rdx
, 999		// rep insw %ax, es:(%rax,%rax,1)
, 999		// rep insl %ax, es:(%rax,%rax,1)
, 999		// rep insq %ax, es:(%rax,%rax,1)
, 999		// rep insb %ax, es:(%rax,%rax,1)
, 999		// rep insb %ax, es:(%rax,%rax,1)
, 999		// rep lodsb %al
, 999		// rep lodsb %al
, 999		// rep lodsw %ax
, 999		// rep lodsl %eax
, 999		// rep lodsq %rax
, 999		// rep movsw es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// rep movsl es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// rep movsq es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// rep movsb es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// rep movsb es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// rep outsw es:(%rax,%rax,1), %ax
, 999		// rep outsl es:(%rax,%rax,1), %ax
, 999		// rep outsq es:(%rax,%rax,1), %ax
, 999		// rep outsb es:(%rax,%rax,1), %ax
, 999		// rep outsb es:(%rax,%rax,1), %ax
, 999		// rep stosw es:(%rax,%rax,1)
, 999		// rep stosl es:(%rax,%rax,1)
, 999		// rep stosq es:(%rax,%rax,1)
, 999		// rep stosb es:(%rax,%rax,1)
, 999		// rep stosb es:(%rax,%rax,1)
, 999		// repz cmpsw es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repz cmpsl es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repz cmpsq es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repz cmpsb es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repz cmpsb es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repz scasw es:(%rax,%rax,1)
, 999		// repz scasl es:(%rax,%rax,1)
, 999		// repz scasq es:(%rax,%rax,1)
, 999		// repz scasb es:(%rax,%rax,1)
, 999		// repz scasb es:(%rax,%rax,1)
, 999		// repnz cmpsw es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repnz cmpsl es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repnz cmpsq es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repnz cmpsb es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repnz cmpsb es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// repnz scasw es:(%rax,%rax,1)
, 999		// repnz scasl es:(%rax,%rax,1)
, 999		// repnz scasq es:(%rax,%rax,1)
, 999		// repnz scasb es:(%rax,%rax,1)
, 999		// repnz scasb es:(%rax,%rax,1)
, 0		// retq 
, 0		// lretl 
, 0		// retq $0x0
, 0		// retq , $0x0
, 14		// rolw %cl, (%rsi)
, 15		// rolw $0x4, (%rsi)
, 15		// rolw $0x1, (%rsi)
, 14		// roll %cl, (%rsi)
, 15		// roll $0x4, (%rsi)
, 15		// roll $0x1, (%rsi)
, 14		// rolq %cl, (%rsi)
, 15		// rolq $0x4, (%rsi)
, 15		// rolq $0x1, (%rsi)
, 14		// rolb %cl, (%rsi)
, 15		// rolb $0x4, (%rsi)
, 15		// rolb $0x1, (%rsi)
, 4		// rolw %cl, %cx
, 1		// rolw $0x4, %cx
, 1		// rolw $0x1, %cx
, 4		// roll %cl, %edx
, 1		// roll $0x4, %edx
, 1		// roll $0x1, %edx
, 4		// rolq %cl, %rdx
, 1		// rolq $0x4, %rdx
, 1		// rolq $0x1, %rdx
, 4		// rolb %cl, %dl
, 1		// rolb $0x4, %dl
, 1		// rolb $0x1, %dl
, 4		// rolb %cl, %ch
, 1		// rolb $0x4, %ch
, 1		// rolb $0x1, %ch
, 14		// rorw %cl, (%rsi)
, 15		// rorw $0x4, (%rsi)
, 15		// rorw $0x1, (%rsi)
, 14		// rorl %cl, (%rsi)
, 15		// rorl $0x4, (%rsi)
, 15		// rorl $0x1, (%rsi)
, 14		// rorq %cl, (%rsi)
, 15		// rorq $0x4, (%rsi)
, 15		// rorq $0x1, (%rsi)
, 14		// rorb %cl, (%rsi)
, 15		// rorb $0x4, (%rsi)
, 15		// rorb $0x1, (%rsi)
, 4		// rorw %cl, %cx
, 1		// rorw $0x4, %cx
, 1		// rorw $0x1, %cx
, 4		// rorl %cl, %edx
, 1		// rorl $0x4, %edx
, 1		// rorl $0x1, %edx
, 4		// rorq %cl, %rdx
, 1		// rorq $0x4, %rdx
, 1		// rorq $0x1, %rdx
, 4		// rorb %cl, %dl
, 1		// rorb $0x4, %dl
, 1		// rorb $0x1, %dl
, 4		// rorb %cl, %ch
, 1		// rorb $0x4, %ch
, 1		// rorb $0x1, %ch
, 1		// rorxl $0x4, (%rsi), %edx
, 1		// rorxl $0x4, %edx, %edx
, 1		// rorxq $0x4, (%rsi), %rdx
, 1		// rorxq $0x4, %rdx, %rdx
, 4		// roundpd $0x4, (%rsi), %xmm0
, 14		// roundpd $0x4, %xmm0, %xmm0
, 4		// roundps $0x4, (%rsi), %xmm0
, 14		// roundps $0x4, %xmm0, %xmm0
, 14		// roundsd $0x4, (%rsi), %xmm0
, 14		// roundsd $0x4, %xmm0, %xmm0
, 14		// roundss $0x4, (%rsi), %xmm0
, 14		// roundss $0x4, %xmm0, %xmm0
, 1		// rsqrtps (%rsi), %xmm0
, 12		// rsqrtps %xmm0, %xmm0
, 12		// rsqrtss (%rsi), %xmm0
, 12		// rsqrtss %xmm0, %xmm0
, 4		// sahf 
, 14		// salw %cl, (%rsi)
, 14		// salw $0x4, (%rsi)
, 14		// salw $0x1, (%rsi)
, 14		// sall %cl, (%rsi)
, 14		// sall $0x4, (%rsi)
, 14		// sall $0x1, (%rsi)
, 14		// salq %cl, (%rsi)
, 14		// salq $0x4, (%rsi)
, 14		// salq $0x1, (%rsi)
, 14		// salb %cl, (%rsi)
, 14		// salb $0x4, (%rsi)
, 14		// salb $0x1, (%rsi)
, 4		// salw %cl, %cx
, 1		// salw $0x4, %cx
, 1		// salw $0x1, %cx
, 4		// sall %cl, %edx
, 1		// sall $0x4, %edx
, 1		// sall $0x1, %edx
, 4		// salq %cl, %rdx
, 1		// salq $0x4, %rdx
, 1		// salq $0x1, %rdx
, 4		// salb %cl, %dl
, 1		// salb $0x4, %dl
, 1		// salb $0x1, %dl
, 4		// salb %cl, %ch
, 1		// salb $0x4, %ch
, 1		// salb $0x1, %ch
, 14		// sarw %cl, (%rsi)
, 14		// sarw $0x4, (%rsi)
, 14		// sarw $0x1, (%rsi)
, 14		// sarl %cl, (%rsi)
, 14		// sarl $0x4, (%rsi)
, 14		// sarl $0x1, (%rsi)
, 14		// sarq %cl, (%rsi)
, 14		// sarq $0x4, (%rsi)
, 14		// sarq $0x1, (%rsi)
, 14		// sarb %cl, (%rsi)
, 14		// sarb $0x4, (%rsi)
, 14		// sarb $0x1, (%rsi)
, 4		// sarw %cl, %cx
, 1		// sarw $0x4, %cx
, 1		// sarw $0x1, %cx
, 4		// sarl %cl, %edx
, 1		// sarl $0x4, %edx
, 1		// sarl $0x1, %edx
, 4		// sarq %cl, %rdx
, 1		// sarq $0x4, %rdx
, 1		// sarq $0x1, %rdx
, 4		// sarb %cl, %dl
, 1		// sarb $0x4, %dl
, 1		// sarb $0x1, %dl
, 4		// sarb %cl, %ch
, 1		// sarb $0x4, %ch
, 1		// sarb $0x1, %ch
, 1		// sarxl %edx, (%rsi), %edx
, 1		// sarxl %edx, %edx, %edx
, 1		// sarxq %rdx, (%rsi), %rdx
, 1		// sarxq %rdx, %rdx, %rdx
, 4		// sbbb $0x4, %al
, 6		// sbbw $0x4050, %ax
, 4		// sbbl $0xc0decafe, %eax
, 17		// sbbw $0x4050, (%rsi)
, 18		// sbbw $0x4, (%rsi)
, 18		// sbbw %cx, (%rsi)
, 18		// sbbl $0xc0decafe, (%rsi)
, 18		// sbbl $0x4, (%rsi)
, 18		// sbbl %edx, (%rsi)
, 17		// sbbq $0xc0decafe, (%rsi)
, 18		// sbbq $0x4, (%rsi)
, 18		// sbbq %rdx, (%rsi)
, 17		// sbbb $0x4, (%rsi)
, 18		// sbbb %dl, (%rsi)
, 20		// sbbb %ch, (%rsi)
, 6		// sbbw $0x4050, %cx
, 4		// sbbw $0x4, %cx
, 4		// sbbw (%rsi), %cx
, 4		// sbbw %cx, %cx
, 4		// sbbw %cx, %cx
, 4		// sbbl $0xc0decafe, %edx
, 4		// sbbl $0x4, %edx
, 4		// sbbl (%rsi), %edx
, 4		// sbbl %edx, %edx
, 4		// sbbl %edx, %edx
, 4		// sbbq $0xc0decafe, %rdx
, 4		// sbbq $0x4, %rdx
, 4		// sbbq (%rsi), %rdx
, 4		// sbbq %rdx, %rdx
, 4		// sbbq %rdx, %rdx
, 4		// sbbb $0x4, %dl
, 4		// sbbb (%rsi), %dl
, 4		// sbbb %dl, %dl
, 4		// sbbb %dl, %dl
, 6		// sbbb %ch, %dl
, 6		// sbbb %ch, %dl
, 4		// sbbq $0xc0decafe, %rax
, 4		// sbbb $0x4, %ch
, 4		// sbbb (%rsi), %ch
, 4		// sbbb %dl, %ch
, 4		// sbbb %dl, %ch
, 4		// sbbb %ch, %ch
, 4		// sbbb %ch, %ch
, 999		// scas es:(%rax,%rax,1)
, 999		// scas es:(%rax,%rax,1)
, 999		// scas es:(%rax,%rax,1)
, 999		// scas es:(%rax,%rax,1)
, 999		// scasb 
, 999		// scasl 
, 999		// scasq 
, 999		// scasw 
, 1		// seta (%rsi)
, 1		// seta %dl
, 1		// seta %ch
, 1		// setae (%rsi)
, 1		// setae %dl
, 1		// setae %ch
, 1		// setb (%rsi)
, 1		// setb %dl
, 1		// setb %ch
, 1		// setbe (%rsi)
, 1		// setbe %dl
, 1		// setbe %ch
, 1		// setc (%rsi)
, 1		// setc %dl
, 1		// setc %ch
, 1		// sete (%rsi)
, 1		// sete %dl
, 1		// sete %ch
, 1		// setg (%rsi)
, 1		// setg %dl
, 1		// setg %ch
, 1		// setge (%rsi)
, 1		// setge %dl
, 1		// setge %ch
, 1		// setl (%rsi)
, 1		// setl %dl
, 1		// setl %ch
, 1		// setle (%rsi)
, 1		// setle %dl
, 1		// setle %ch
, 1		// setna (%rsi)
, 1		// setna %dl
, 1		// setna %ch
, 1		// setnae (%rsi)
, 1		// setnae %dl
, 1		// setnae %ch
, 1		// setnb (%rsi)
, 1		// setnb %dl
, 1		// setnb %ch
, 1		// setnbe (%rsi)
, 1		// setnbe %dl
, 1		// setnbe %ch
, 1		// setnc (%rsi)
, 1		// setnc %dl
, 1		// setnc %ch
, 1		// setne (%rsi)
, 1		// setne %dl
, 1		// setne %ch
, 1		// setng (%rsi)
, 1		// setng %dl
, 1		// setng %ch
, 1		// setnge (%rsi)
, 1		// setnge %dl
, 1		// setnge %ch
, 1		// setnl (%rsi)
, 1		// setnl %dl
, 1		// setnl %ch
, 1		// setnle (%rsi)
, 1		// setnle %dl
, 1		// setnle %ch
, 1		// setno (%rsi)
, 1		// setno %dl
, 1		// setno %ch
, 1		// setnp (%rsi)
, 1		// setnp %dl
, 1		// setnp %ch
, 1		// setns (%rsi)
, 1		// setns %dl
, 1		// setns %ch
, 1		// setnz (%rsi)
, 1		// setnz %dl
, 1		// setnz %ch
, 1		// seto (%rsi)
, 1		// seto %dl
, 1		// seto %ch
, 1		// setp (%rsi)
, 1		// setp %dl
, 1		// setp %ch
, 1		// setpe (%rsi)
, 1		// setpe %dl
, 1		// setpe %ch
, 1		// setpo (%rsi)
, 1		// setpo %dl
, 1		// setpo %ch
, 1		// sets (%rsi)
, 1		// sets %dl
, 1		// sets %ch
, 1		// setz (%rsi)
, 1		// setz %dl
, 1		// setz %ch
, 14		// sfence 
, 14		// shlw %cl, (%rsi)
, 14		// shlw $0x4, (%rsi)
, 14		// shlw $0x1, (%rsi)
, 14		// shll %cl, (%rsi)
, 14		// shll $0x4, (%rsi)
, 14		// shll $0x1, (%rsi)
, 14		// shlq %cl, (%rsi)
, 14		// shlq $0x4, (%rsi)
, 14		// shlq $0x1, (%rsi)
, 14		// shlb %cl, (%rsi)
, 14		// shlb $0x4, (%rsi)
, 14		// shlb $0x1, (%rsi)
, 4		// shlw %cl, %cx
, 1		// shlw $0x4, %cx
, 1		// shlw $0x1, %cx
, 4		// shll %cl, %edx
, 1		// shll $0x4, %edx
, 1		// shll $0x1, %edx
, 4		// shlq %cl, %rdx
, 1		// shlq $0x4, %rdx
, 1		// shlq $0x1, %rdx
, 4		// shlb %cl, %dl
, 1		// shlb $0x4, %dl
, 1		// shlb $0x1, %dl
, 4		// shlb %cl, %ch
, 1		// shlb $0x4, %ch
, 1		// shlb $0x1, %ch
, 19		// shldw %cl, %cx, (%rsi)
, 19		// shldw $0x4, %cx, (%rsi)
, 19		// shldl %cl, %edx, (%rsi)
, 19		// shldl $0x4, %edx, (%rsi)
, 19		// shldq %cl, %rdx, (%rsi)
, 19		// shldq $0x4, %rdx, (%rsi)
, 9		// shldw %cl, %cx, %cx
, 1		// shldw $0x4, %cx, %cx
, 9		// shldl %cl, %edx, %edx
, 1		// shldl $0x4, %edx, %edx
, 9		// shldq %cl, %rdx, %rdx
, 1		// shldq $0x4, %rdx, %rdx
, 1		// shlxl %edx, (%rsi), %edx
, 1		// shlxl %edx, %edx, %edx
, 1		// shlxq %rdx, (%rsi), %rdx
, 1		// shlxq %rdx, %rdx, %rdx
, 14		// shrw %cl, (%rsi)
, 14		// shrw $0x4, (%rsi)
, 14		// shrw $0x1, (%rsi)
, 14		// shrl %cl, (%rsi)
, 14		// shrl $0x4, (%rsi)
, 14		// shrl $0x1, (%rsi)
, 14		// shrq %cl, (%rsi)
, 14		// shrq $0x4, (%rsi)
, 14		// shrq $0x1, (%rsi)
, 14		// shrb %cl, (%rsi)
, 14		// shrb $0x4, (%rsi)
, 14		// shrb $0x1, (%rsi)
, 4		// shrw %cl, %cx
, 1		// shrw $0x4, %cx
, 1		// shrw $0x1, %cx
, 4		// shrl %cl, %edx
, 1		// shrl $0x4, %edx
, 1		// shrl $0x1, %edx
, 4		// shrq %cl, %rdx
, 1		// shrq $0x4, %rdx
, 1		// shrq $0x1, %rdx
, 4		// shrb %cl, %dl
, 1		// shrb $0x4, %dl
, 1		// shrb $0x1, %dl
, 4		// shrb %cl, %ch
, 1		// shrb $0x4, %ch
, 1		// shrb $0x1, %ch
, 22		// shrdw %cl, %cx, (%rsi)
, 22		// shrdw $0x4, %cx, (%rsi)
, 22		// shrdl %cl, %edx, (%rsi)
, 22		// shrdl $0x4, %edx, (%rsi)
, 22		// shrdq %cl, %rdx, (%rsi)
, 22		// shrdq $0x4, %rdx, (%rsi)
, 9		// shrdw %cl, %cx, %cx
, 1		// shrdw $0x4, %cx, %cx
, 9		// shrdl %cl, %edx, %edx
, 1		// shrdl $0x4, %edx, %edx
, 9		// shrdq %cl, %rdx, %rdx
, 1		// shrdq $0x4, %rdx, %rdx
, 1		// shrxl %edx, (%rsi), %edx
, 1		// shrxl %edx, %edx, %edx
, 1		// shrxq %rdx, (%rsi), %rdx
, 1		// shrxq %rdx, %rdx, %rdx
, 1		// shufpd $0x4, (%rsi), %xmm0
, 1		// shufpd $0x4, %xmm0, %xmm0
, 1		// shufps $0x4, (%rsi), %xmm0
, 1		// shufps $0x4, %xmm0, %xmm0
, 19		// sqrtpd (%rsi), %xmm0
, 25		// sqrtpd %xmm0, %xmm0
, 17		// sqrtps (%rsi), %xmm0
, 25		// sqrtps %xmm0, %xmm0
, 24		// sqrtsd (%rsi), %xmm0
, 25		// sqrtsd %xmm0, %xmm0
, 24		// sqrtss (%rsi), %xmm0
, 25		// sqrtss %xmm0, %xmm0
, 1		// stc 
, 999		// std 
, 999		// sti 
, 1		// stmxcsr (%rsi)
, 999		// stosw es:(%rax,%rax,1)
, 999		// stosl es:(%rax,%rax,1)
, 999		// stosq es:(%rax,%rax,1)
, 999		// stosb es:(%rax,%rax,1)
, 999		// stosb 
, 999		// stosl 
, 999		// stosq 
, 999		// stosw 
, 1		// subb $0x4, %al
, 7		// subw $0x4050, %ax
, 1		// subl $0xc0decafe, %eax
, 15		// subw $0x4050, (%rsi)
, 16		// subw $0x4, (%rsi)
, 14		// subw %cx, (%rsi)
, 16		// subl $0xc0decafe, (%rsi)
, 16		// subl $0x4, (%rsi)
, 14		// subl %edx, (%rsi)
, 16		// subq $0xc0decafe, (%rsi)
, 16		// subq $0x4, (%rsi)
, 14		// subq %rdx, (%rsi)
, 16		// subb $0x4, (%rsi)
, 14		// subb %dl, (%rsi)
, 18		// subb %ch, (%rsi)
, 7		// subw $0x4050, %cx
, 1		// subw $0x4, %cx
, 1		// subw (%rsi), %cx
, 1		// subw %cx, %cx
, 1		// subw %cx, %cx
, 1		// subl $0xc0decafe, %edx
, 1		// subl $0x4, %edx
, 1		// subl (%rsi), %edx
, 1		// subl %edx, %edx
, 1		// subl %edx, %edx
, 1		// subq $0xc0decafe, %rdx
, 1		// subq $0x4, %rdx
, 1		// subq (%rsi), %rdx
, 1		// subq %rdx, %rdx
, 1		// subq %rdx, %rdx
, 1		// subb $0x4, %dl
, 1		// subb (%rsi), %dl
, 1		// subb %dl, %dl
, 1		// subb %dl, %dl
, 4		// subb %ch, %dl
, 4		// subb %ch, %dl
, 1		// subq $0xc0decafe, %rax
, 1		// subb $0x4, %ch
, 1		// subb (%rsi), %ch
, 1		// subb %dl, %ch
, 1		// subb %dl, %ch
, 1		// subb %ch, %ch
, 1		// subb %ch, %ch
, 7		// subpd (%rsi), %xmm0
, 7		// subpd %xmm0, %xmm0
, 7		// subps (%rsi), %xmm0
, 7		// subps %xmm0, %xmm0
, 7		// subsd (%rsi), %xmm0
, 7		// subsd %xmm0, %xmm0
, 7		// subss (%rsi), %xmm0
, 7		// subss %xmm0, %xmm0
, 999		// swapgs 
, 0		// syscall 
, 999		// sysenter 
, 999		// sysexit 
, 999		// sysexit 
, 0		// sysret 
, 0		// sysret 
, 1		// testb $0x4, %al
, 7		// testw $0x4050, %ax
, 1		// testl $0xc0decafe, %eax
, 4		// testw $0x4050, (%rsi)
, 1		// testw %cx, (%rsi)
, 1		// testl $0xc0decafe, (%rsi)
, 1		// testl %edx, (%rsi)
, 1		// testq $0xc0decafe, (%rsi)
, 1		// testq %rdx, (%rsi)
, 1		// testb $0x4, (%rsi)
, 1		// testb %dl, (%rsi)
, 1		// testb %ch, (%rsi)
, 7		// testw $0x4050, %cx
, 1		// testw %cx, %cx
, 1		// testl $0xc0decafe, %edx
, 1		// testl %edx, %edx
, 1		// testq $0xc0decafe, %rdx
, 1		// testq %rdx, %rdx
, 1		// testb $0x4, %dl
, 1		// testb %dl, %dl
, 1		// testb %ch, %dl
, 1		// testq $0xc0decafe, %rax
, 1		// testb $0x4, %ch
, 1		// testb %dl, %ch
, 1		// testb %ch, %ch
, 6		// tzcntw (%rsi), %cx
, 6		// tzcntw %cx, %cx
, 6		// tzcntl (%rsi), %edx
, 6		// tzcntl %edx, %edx
, 6		// tzcntq (%rsi), %rdx
, 6		// tzcntq %rdx, %rdx
, 1		// ucomisd (%rsi), %xmm0
, 1		// ucomisd %xmm0, %xmm0
, 1		// ucomiss (%rsi), %xmm0
, 1		// ucomiss %xmm0, %xmm0
, 999		// ud2 
, 1		// unpckhpd (%rsi), %xmm0
, 1		// unpckhpd %xmm0, %xmm0
, 1		// unpckhps (%rsi), %xmm0
, 1		// unpckhps %xmm0, %xmm0
, 1		// unpcklpd (%rsi), %xmm0
, 1		// unpcklpd %xmm0, %xmm0
, 1		// unpcklps (%rsi), %xmm0
, 1		// unpcklps %xmm0, %xmm0
, 7		// vaddpd (%rsi), %xmm0, %xmm0
, 7		// vaddpd %xmm0, %xmm0, %xmm0
, 7		// vaddpd (%rsi), %ymm0, %ymm0
, 7		// vaddpd %ymm0, %ymm0, %ymm0
, 7		// vaddps (%rsi), %xmm0, %xmm0
, 7		// vaddps %xmm0, %xmm0, %xmm0
, 7		// vaddps (%rsi), %ymm0, %ymm0
, 7		// vaddps %ymm0, %ymm0, %ymm0
, 7		// vaddsd (%rsi), %xmm0, %xmm0
, 7		// vaddsd %xmm0, %xmm0, %xmm0
, 7		// vaddss (%rsi), %xmm0, %xmm0
, 7		// vaddss %xmm0, %xmm0, %xmm0
, 7		// vaddsubpd (%rsi), %xmm0, %xmm0
, 7		// vaddsubpd %xmm0, %xmm0, %xmm0
, 7		// vaddsubpd (%rsi), %ymm0, %ymm0
, 7		// vaddsubpd %ymm0, %ymm0, %ymm0
, 7		// vaddsubps (%rsi), %xmm0, %xmm0
, 7		// vaddsubps %xmm0, %xmm0, %xmm0
, 7		// vaddsubps (%rsi), %ymm0, %ymm0
, 7		// vaddsubps %ymm0, %ymm0, %ymm0
, 17		// vaesdec (%rsi), %xmm0, %xmm0
, 17		// vaesdec %xmm0, %xmm0, %xmm0
, 17		// vaesdeclast (%rsi), %xmm0, %xmm0
, 17		// vaesdeclast %xmm0, %xmm0, %xmm0
, 17		// vaesenc (%rsi), %xmm0, %xmm0
, 17		// vaesenc %xmm0, %xmm0, %xmm0
, 17		// vaesenclast (%rsi), %xmm0, %xmm0
, 17		// vaesenclast %xmm0, %xmm0, %xmm0
, 4		// vaesimc (%rsi), %xmm0
, 35		// vaesimc %xmm0, %xmm0
, 19		// vaeskeygenassist $0x4, (%rsi), %xmm0
, 24		// vaeskeygenassist $0x4, %xmm0, %xmm0
, 1		// vandnpd (%rsi), %xmm0, %xmm0
, 1		// vandnpd %xmm0, %xmm0, %xmm0
, 1		// vandnpd (%rsi), %ymm0, %ymm0
, 1		// vandnpd %ymm0, %ymm0, %ymm0
, 1		// vandnps (%rsi), %xmm0, %xmm0
, 1		// vandnps %xmm0, %xmm0, %xmm0
, 1		// vandnps (%rsi), %ymm0, %ymm0
, 1		// vandnps %ymm0, %ymm0, %ymm0
, 1		// vandpd (%rsi), %xmm0, %xmm0
, 1		// vandpd %xmm0, %xmm0, %xmm0
, 1		// vandpd (%rsi), %ymm0, %ymm0
, 1		// vandpd %ymm0, %ymm0, %ymm0
, 1		// vandps (%rsi), %xmm0, %xmm0
, 1		// vandps %xmm0, %xmm0, %xmm0
, 1		// vandps (%rsi), %ymm0, %ymm0
, 1		// vandps %ymm0, %ymm0, %ymm0
, 1		// vblendpd $0x4, (%rsi), %xmm0, %xmm0
, 1		// vblendpd $0x4, %xmm0, %xmm0, %xmm0
, 1		// vblendpd $0x4, (%rsi), %ymm0, %ymm0
, 1		// vblendpd $0x4, %ymm0, %ymm0, %ymm0
, 1		// vblendps $0x4, (%rsi), %xmm0, %xmm0
, 1		// vblendps $0x4, %xmm0, %xmm0, %xmm0
, 1		// vblendps $0x4, (%rsi), %ymm0, %ymm0
, 1		// vblendps $0x4, %ymm0, %ymm0, %ymm0
, 4		// vblendvpd %xmm0, (%rsi), %xmm0, %xmm0
, 4		// vblendvpd %xmm0, %xmm0, %xmm0, %xmm0
, 4		// vblendvpd %ymm0, (%rsi), %ymm0, %ymm0
, 4		// vblendvpd %ymm0, %ymm0, %ymm0, %ymm0
, 4		// vblendvps %xmm0, (%rsi), %xmm0, %xmm0
, 4		// vblendvps %xmm0, %xmm0, %xmm0, %xmm0
, 4		// vblendvps %ymm0, (%rsi), %ymm0, %ymm0
, 4		// vblendvps %ymm0, %ymm0, %ymm0, %ymm0
, 1		// vbroadcastf128 (%rsi), %ymm0
, 1		// vpbroadcasti128 (%rsi), %ymm0
, 1		// vbroadcastsd (%rsi), %ymm0
, 7		// vbroadcastsd %xmm0, %ymm0
, 1		// vbroadcastss (%rsi), %xmm0
, 1		// vbroadcastss %xmm0, %xmm0
, 1		// vbroadcastss (%rsi), %ymm0
, 7		// vbroadcastss %xmm0, %ymm0
, 7		// vcmppd $0x4, (%rsi), %xmm0, %xmm0
, 7		// vcmppd $0x4, %xmm0, %xmm0, %xmm0
, 7		// vcmppd $0x4, (%rsi), %ymm0, %ymm0
, 7		// vcmppd $0x4, %ymm0, %ymm0, %ymm0
, 7		// vcmpps $0x4, (%rsi), %xmm0, %xmm0
, 7		// vcmpps $0x4, %xmm0, %xmm0, %xmm0
, 7		// vcmpps $0x4, (%rsi), %ymm0, %ymm0
, 7		// vcmpps $0x4, %ymm0, %ymm0, %ymm0
, 7		// vcmpsd $0x4, (%rsi), %xmm0, %xmm0
, 7		// vcmpsd $0x4, %xmm0, %xmm0, %xmm0
, 7		// vcmpss $0x4, (%rsi), %xmm0, %xmm0
, 7		// vcmpss $0x4, %xmm0, %xmm0, %xmm0
, 1		// vcomisd (%rsi), %xmm0
, 1		// vcomisd %xmm0, %xmm0
, 1		// vcomiss (%rsi), %xmm0
, 1		// vcomiss %xmm0, %xmm0
, 1		// vcvtdq2pd (%rsi), %xmm0
, 9		// vcvtdq2pd %xmm0, %xmm0
, 2		// vcvtdq2pd (%rsi), %ymm0
, 16		// vcvtdq2pd %ymm0, %ymm0
, 1		// vcvtdq2ps (%rsi), %xmm0
, 7		// vcvtdq2ps %xmm0, %xmm0
, 2		// vcvtdq2ps (%rsi), %ymm0
, 7		// vcvtdq2ps %ymm0, %ymm0
, 1		// vcvtpd2dqx (%rsi), %xmm0
, 2		// vcvtpd2dq (%rsi), %xmm0
, 9		// vcvtpd2dqx %xmm0, %xmm0
, 16		// vcvtpd2dq %ymm0, %xmm0
, 1		// vcvtpd2ps (%rsi), %xmm0
, 2		// vcvtpd2ps (%rsi), %xmm0
, 9		// vcvtpd2ps %xmm0, %xmm0
, 16		// vcvtpd2ps %ymm0, %xmm0
, 2		// vcvtph2ps (%rsi), %xmm0
, 9		// vcvtph2ps %xmm0, %xmm0
, 3		// vcvtph2ps (%rsi), %ymm0
, 16		// vcvtph2ps %xmm0, %ymm0
, 1		// vcvtps2dq (%rsi), %xmm0
, 7		// vcvtps2dq %xmm0, %xmm0
, 2		// vcvtps2dq (%rsi), %ymm0
, 7		// vcvtps2dq %ymm0, %ymm0
, 1		// vcvtps2pd (%rsi), %xmm0
, 4		// vcvtps2pd %xmm0, %xmm0
, 2		// vcvtps2pd (%rsi), %ymm0
, 10		// vcvtps2pd %xmm0, %ymm0
, 2		// vcvtps2ph $0x4, %ymm0, (%rsi)
, 1		// vcvtps2ph $0x4, %xmm0, (%rsi)
, 9		// vcvtps2ph $0x4, %xmm0, %xmm0
, 16		// vcvtps2ph $0x4, %ymm0, %xmm0
, 1		// vcvtsd2sil (%rsi), %edx
, 1		// vcvtsd2sil %xmm0, %edx
, 1		// vcvtsd2siq (%rsi), %rdx
, 1		// vcvtsd2siq %xmm0, %rdx
, 1		// vcvtsd2ss (%rsi), %xmm0, %xmm0
, 9		// vcvtsd2ss %xmm0, %xmm0, %xmm0
, 7		// vcvtsi2sdl (%rsi), %xmm0, %xmm0
, 7		// vcvtsi2sdq (%rsi), %xmm0, %xmm0
, 7		// vcvtsi2sdl %edx, %xmm0, %xmm0
, 7		// vcvtsi2sdq %rdx, %xmm0, %xmm0
, 7		// vcvtsi2ssl (%rsi), %xmm0, %xmm0
, 1		// vcvtsi2ssq (%rsi), %xmm0, %xmm0
, 7		// vcvtsi2ssl %edx, %xmm0, %xmm0
, 4		// vcvtsi2ssq %rdx, %xmm0, %xmm0
, 1		// vcvtss2sd (%rsi), %xmm0, %xmm0
, 4		// vcvtss2sd %xmm0, %xmm0, %xmm0
, 1		// vcvtss2sil (%rsi), %edx
, 1		// vcvtss2sil %xmm0, %edx
, 1		// vcvtss2siq (%rsi), %rdx
, 1		// vcvtss2siq %xmm0, %rdx
, 1		// vcvttpd2dq (%rsi), %xmm0
, 2		// vcvttpd2dq (%rsi), %xmm0
, 9		// vcvttpd2dq %xmm0, %xmm0
, 16		// vcvttpd2dq %ymm0, %xmm0
, 1		// vcvttps2dq (%rsi), %xmm0
, 7		// vcvttps2dq %xmm0, %xmm0
, 2		// vcvttps2dq (%rsi), %ymm0
, 7		// vcvttps2dq %ymm0, %ymm0
, 1		// vcvttsd2si (%rsi), %edx
, 1		// vcvttsd2si %xmm0, %edx
, 1		// vcvttsd2siq (%rsi), %rdx
, 1		// vcvttsd2siq %xmm0, %rdx
, 1		// vcvttss2si (%rsi), %edx
, 1		// vcvttss2si %xmm0, %edx
, 1		// vcvttss2siq (%rsi), %rdx
, 1		// vcvttss2siq %xmm0, %rdx
, 24		// vdivpd (%rsi), %xmm0, %xmm0
, 25		// vdivpd %xmm0, %xmm0, %xmm0
, 48		// vdivpd (%rsi), %ymm0, %ymm0
, 48		// vdivpd %ymm0, %ymm0, %ymm0
, 24		// vdivps (%rsi), %xmm0, %xmm0
, 25		// vdivps %xmm0, %xmm0, %xmm0
, 46		// vdivps (%rsi), %ymm0, %ymm0
, 45		// vdivps %ymm0, %ymm0, %ymm0
, 24		// vdivsd (%rsi), %xmm0, %xmm0
, 25		// vdivsd %xmm0, %xmm0, %xmm0
, 24		// vdivss (%rsi), %xmm0, %xmm0
, 25		// vdivss %xmm0, %xmm0, %xmm0
, 22		// vdppd $0x4, (%rsi), %xmm0, %xmm0
, 22		// vdppd $0x4, %xmm0, %xmm0, %xmm0
, 35		// vdpps $0x4, (%rsi), %xmm0, %xmm0
, 35		// vdpps $0x4, %xmm0, %xmm0, %xmm0
, 39		// vdpps $0x4, (%rsi), %ymm0, %ymm0
, 39		// vdpps $0x4, %ymm0, %ymm0, %ymm0
, 176		// verr (%rsi)
, 367		// verr %cx
, 172		// verw (%rsi)
, 358		// verw %cx
, 1		// vextractf128 $0x4, %ymm0, (%rsi)
, 7		// vextractf128 $0x4, %ymm0, %xmm0
, 7		// vextracti128 $0x4, %ymm0, (%rsi)
, 7		// vextracti128 $0x4, %ymm0, %xmm0
, 1		// vextractps $0x4, %xmm0, (%rsi)
, 1		// vextractps $0x4, %xmm0, %edx
, 13		// vfmadd132pd (%rsi), %xmm0, %xmm0
, 13		// vfmadd132pd %xmm0, %xmm0, %xmm0
, 13		// vfmadd132pd (%rsi), %ymm0, %ymm0
, 13		// vfmadd132pd %ymm0, %ymm0, %ymm0
, 13		// vfmadd132ps (%rsi), %xmm0, %xmm0
, 13		// vfmadd132ps %xmm0, %xmm0, %xmm0
, 13		// vfmadd132ps (%rsi), %ymm0, %ymm0
, 13		// vfmadd132ps %ymm0, %ymm0, %ymm0
, 12		// vfmadd132sd (%rsi), %xmm0, %xmm0
, 12		// vfmadd132sd %xmm0, %xmm0, %xmm0
, 12		// vfmadd132ss (%rsi), %xmm0, %xmm0
, 12		// vfmadd132ss %xmm0, %xmm0, %xmm0
, 13		// vfmadd213pd (%rsi), %xmm0, %xmm0
, 13		// vfmadd213pd %xmm0, %xmm0, %xmm0
, 13		// vfmadd213pd (%rsi), %ymm0, %ymm0
, 13		// vfmadd213pd %ymm0, %ymm0, %ymm0
, 13		// vfmadd213ps (%rsi), %xmm0, %xmm0
, 13		// vfmadd213ps %xmm0, %xmm0, %xmm0
, 13		// vfmadd213ps (%rsi), %ymm0, %ymm0
, 13		// vfmadd213ps %ymm0, %ymm0, %ymm0
, 12		// vfmadd213sd (%rsi), %xmm0, %xmm0
, 12		// vfmadd213sd %xmm0, %xmm0, %xmm0
, 12		// vfmadd213ss (%rsi), %xmm0, %xmm0
, 12		// vfmadd213ss %xmm0, %xmm0, %xmm0
, 13		// vfmadd231pd (%rsi), %xmm0, %xmm0
, 13		// vfmadd231pd %xmm0, %xmm0, %xmm0
, 13		// vfmadd231pd (%rsi), %ymm0, %ymm0
, 13		// vfmadd231pd %ymm0, %ymm0, %ymm0
, 13		// vfmadd231ps (%rsi), %xmm0, %xmm0
, 13		// vfmadd231ps %xmm0, %xmm0, %xmm0
, 13		// vfmadd231ps (%rsi), %ymm0, %ymm0
, 13		// vfmadd231ps %ymm0, %ymm0, %ymm0
, 12		// vfmadd231sd (%rsi), %xmm0, %xmm0
, 12		// vfmadd231sd %xmm0, %xmm0, %xmm0
, 12		// vfmadd231ss (%rsi), %xmm0, %xmm0
, 12		// vfmadd231ss %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub132pd (%rsi), %xmm0, %xmm0
, 13		// vfmaddsub132pd %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub132pd (%rsi), %ymm0, %ymm0
, 13		// vfmaddsub132pd %ymm0, %ymm0, %ymm0
, 13		// vfmaddsub132ps (%rsi), %xmm0, %xmm0
, 13		// vfmaddsub132ps %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub132ps (%rsi), %ymm0, %ymm0
, 13		// vfmaddsub132ps %ymm0, %ymm0, %ymm0
, 13		// vfmaddsub213pd (%rsi), %xmm0, %xmm0
, 13		// vfmaddsub213pd %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub213pd (%rsi), %ymm0, %ymm0
, 13		// vfmaddsub213pd %ymm0, %ymm0, %ymm0
, 13		// vfmaddsub213ps (%rsi), %xmm0, %xmm0
, 13		// vfmaddsub213ps %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub213ps (%rsi), %ymm0, %ymm0
, 13		// vfmaddsub213ps %ymm0, %ymm0, %ymm0
, 13		// vfmaddsub231pd (%rsi), %xmm0, %xmm0
, 13		// vfmaddsub231pd %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub231pd (%rsi), %ymm0, %ymm0
, 13		// vfmaddsub231pd %ymm0, %ymm0, %ymm0
, 13		// vfmaddsub231ps (%rsi), %xmm0, %xmm0
, 13		// vfmaddsub231ps %xmm0, %xmm0, %xmm0
, 13		// vfmaddsub231ps (%rsi), %ymm0, %ymm0
, 13		// vfmaddsub231ps %ymm0, %ymm0, %ymm0
, 13		// vfmsub132pd (%rsi), %xmm0, %xmm0
, 13		// vfmsub132pd %xmm0, %xmm0, %xmm0
, 13		// vfmsub132pd (%rsi), %ymm0, %ymm0
, 13		// vfmsub132pd %ymm0, %ymm0, %ymm0
, 13		// vfmsub132ps (%rsi), %xmm0, %xmm0
, 13		// vfmsub132ps %xmm0, %xmm0, %xmm0
, 13		// vfmsub132ps (%rsi), %ymm0, %ymm0
, 13		// vfmsub132ps %ymm0, %ymm0, %ymm0
, 12		// vfmsub132sd (%rsi), %xmm0, %xmm0
, 12		// vfmsub132sd %xmm0, %xmm0, %xmm0
, 12		// vfmsub132ss (%rsi), %xmm0, %xmm0
, 12		// vfmsub132ss %xmm0, %xmm0, %xmm0
, 13		// vfmsub213pd (%rsi), %xmm0, %xmm0
, 13		// vfmsub213pd %xmm0, %xmm0, %xmm0
, 13		// vfmsub213pd (%rsi), %ymm0, %ymm0
, 13		// vfmsub213pd %ymm0, %ymm0, %ymm0
, 13		// vfmsub213ps (%rsi), %xmm0, %xmm0
, 13		// vfmsub213ps %xmm0, %xmm0, %xmm0
, 13		// vfmsub213ps (%rsi), %ymm0, %ymm0
, 13		// vfmsub213ps %ymm0, %ymm0, %ymm0
, 12		// vfmsub213sd (%rsi), %xmm0, %xmm0
, 12		// vfmsub213sd %xmm0, %xmm0, %xmm0
, 12		// vfmsub213ss (%rsi), %xmm0, %xmm0
, 12		// vfmsub213ss %xmm0, %xmm0, %xmm0
, 13		// vfmsub231pd (%rsi), %xmm0, %xmm0
, 13		// vfmsub231pd %xmm0, %xmm0, %xmm0
, 13		// vfmsub231pd (%rsi), %ymm0, %ymm0
, 13		// vfmsub231pd %ymm0, %ymm0, %ymm0
, 13		// vfmsub231ps (%rsi), %xmm0, %xmm0
, 13		// vfmsub231ps %xmm0, %xmm0, %xmm0
, 13		// vfmsub231ps (%rsi), %ymm0, %ymm0
, 13		// vfmsub231ps %ymm0, %ymm0, %ymm0
, 12		// vfmsub231sd (%rsi), %xmm0, %xmm0
, 12		// vfmsub231sd %xmm0, %xmm0, %xmm0
, 12		// vfmsub231ss (%rsi), %xmm0, %xmm0
, 12		// vfmsub231ss %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd132pd (%rsi), %xmm0, %xmm0
, 13		// vfmsubadd132pd %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd132pd (%rsi), %ymm0, %ymm0
, 13		// vfmsubadd132pd %ymm0, %ymm0, %ymm0
, 13		// vfmsubadd132ps (%rsi), %xmm0, %xmm0
, 13		// vfmsubadd132ps %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd132ps (%rsi), %ymm0, %ymm0
, 13		// vfmsubadd132ps %ymm0, %ymm0, %ymm0
, 13		// vfmsubadd213pd (%rsi), %xmm0, %xmm0
, 13		// vfmsubadd213pd %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd213pd (%rsi), %ymm0, %ymm0
, 13		// vfmsubadd213pd %ymm0, %ymm0, %ymm0
, 13		// vfmsubadd213ps (%rsi), %xmm0, %xmm0
, 13		// vfmsubadd213ps %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd213ps (%rsi), %ymm0, %ymm0
, 13		// vfmsubadd213ps %ymm0, %ymm0, %ymm0
, 13		// vfmsubadd231pd (%rsi), %xmm0, %xmm0
, 13		// vfmsubadd231pd %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd231pd (%rsi), %ymm0, %ymm0
, 13		// vfmsubadd231pd %ymm0, %ymm0, %ymm0
, 13		// vfmsubadd231ps (%rsi), %xmm0, %xmm0
, 13		// vfmsubadd231ps %xmm0, %xmm0, %xmm0
, 13		// vfmsubadd231ps (%rsi), %ymm0, %ymm0
, 13		// vfmsubadd231ps %ymm0, %ymm0, %ymm0
, 13		// vfnmadd132pd (%rsi), %xmm0, %xmm0
, 13		// vfnmadd132pd %xmm0, %xmm0, %xmm0
, 13		// vfnmadd132pd (%rsi), %ymm0, %ymm0
, 13		// vfnmadd132pd %ymm0, %ymm0, %ymm0
, 13		// vfnmadd132ps (%rsi), %xmm0, %xmm0
, 13		// vfnmadd132ps %xmm0, %xmm0, %xmm0
, 13		// vfnmadd132ps (%rsi), %ymm0, %ymm0
, 13		// vfnmadd132ps %ymm0, %ymm0, %ymm0
, 12		// vfnmadd132sd (%rsi), %xmm0, %xmm0
, 12		// vfnmadd132sd %xmm0, %xmm0, %xmm0
, 12		// vfnmadd132ss (%rsi), %xmm0, %xmm0
, 12		// vfnmadd132ss %xmm0, %xmm0, %xmm0
, 13		// vfnmadd213pd (%rsi), %xmm0, %xmm0
, 13		// vfnmadd213pd %xmm0, %xmm0, %xmm0
, 13		// vfnmadd213pd (%rsi), %ymm0, %ymm0
, 13		// vfnmadd213pd %ymm0, %ymm0, %ymm0
, 13		// vfnmadd213ps (%rsi), %xmm0, %xmm0
, 13		// vfnmadd213ps %xmm0, %xmm0, %xmm0
, 13		// vfnmadd213ps (%rsi), %ymm0, %ymm0
, 13		// vfnmadd213ps %ymm0, %ymm0, %ymm0
, 12		// vfnmadd213sd (%rsi), %xmm0, %xmm0
, 12		// vfnmadd213sd %xmm0, %xmm0, %xmm0
, 12		// vfnmadd213ss (%rsi), %xmm0, %xmm0
, 12		// vfnmadd213ss %xmm0, %xmm0, %xmm0
, 13		// vfnmadd231pd (%rsi), %xmm0, %xmm0
, 13		// vfnmadd231pd %xmm0, %xmm0, %xmm0
, 13		// vfnmadd231pd (%rsi), %ymm0, %ymm0
, 13		// vfnmadd231pd %ymm0, %ymm0, %ymm0
, 13		// vfnmadd231ps (%rsi), %xmm0, %xmm0
, 13		// vfnmadd231ps %xmm0, %xmm0, %xmm0
, 13		// vfnmadd231ps (%rsi), %ymm0, %ymm0
, 13		// vfnmadd231ps %ymm0, %ymm0, %ymm0
, 12		// vfnmadd231sd (%rsi), %xmm0, %xmm0
, 12		// vfnmadd231sd %xmm0, %xmm0, %xmm0
, 12		// vfnmadd231ss (%rsi), %xmm0, %xmm0
, 12		// vfnmadd231ss %xmm0, %xmm0, %xmm0
, 13		// vfnmsub132pd (%rsi), %xmm0, %xmm0
, 13		// vfnmsub132pd %xmm0, %xmm0, %xmm0
, 13		// vfnmsub132pd (%rsi), %ymm0, %ymm0
, 13		// vfnmsub132pd %ymm0, %ymm0, %ymm0
, 13		// vfnmsub132ps (%rsi), %xmm0, %xmm0
, 13		// vfnmsub132ps %xmm0, %xmm0, %xmm0
, 13		// vfnmsub132ps (%rsi), %ymm0, %ymm0
, 13		// vfnmsub132ps %ymm0, %ymm0, %ymm0
, 12		// vfnmsub132sd (%rsi), %xmm0, %xmm0
, 12		// vfnmsub132sd %xmm0, %xmm0, %xmm0
, 12		// vfnmsub132ss (%rsi), %xmm0, %xmm0
, 12		// vfnmsub132ss %xmm0, %xmm0, %xmm0
, 13		// vfnmsub213pd (%rsi), %xmm0, %xmm0
, 13		// vfnmsub213pd %xmm0, %xmm0, %xmm0
, 13		// vfnmsub213pd (%rsi), %ymm0, %ymm0
, 13		// vfnmsub213pd %ymm0, %ymm0, %ymm0
, 13		// vfnmsub213ps (%rsi), %xmm0, %xmm0
, 13		// vfnmsub213ps %xmm0, %xmm0, %xmm0
, 13		// vfnmsub213ps (%rsi), %ymm0, %ymm0
, 13		// vfnmsub213ps %ymm0, %ymm0, %ymm0
, 12		// vfnmsub213sd (%rsi), %xmm0, %xmm0
, 12		// vfnmsub213sd %xmm0, %xmm0, %xmm0
, 12		// vfnmsub213ss (%rsi), %xmm0, %xmm0
, 12		// vfnmsub213ss %xmm0, %xmm0, %xmm0
, 13		// vfnmsub231pd (%rsi), %xmm0, %xmm0
, 381		// vfnmsub231pd %xmm0, %xmm0, %xmm0
, 13		// vfnmsub231pd (%rsi), %ymm0, %ymm0
, 375		// vfnmsub231pd %ymm0, %ymm0, %ymm0
, 13		// vfnmsub231ps (%rsi), %xmm0, %xmm0
, 13		// vfnmsub231ps %xmm0, %xmm0, %xmm0
, 13		// vfnmsub231ps (%rsi), %ymm0, %ymm0
, 13		// vfnmsub231ps %ymm0, %ymm0, %ymm0
, 12		// vfnmsub231sd (%rsi), %xmm0, %xmm0
, 366		// vfnmsub231sd %xmm0, %xmm0, %xmm0
, 12		// vfnmsub231ss (%rsi), %xmm0, %xmm0
, 12		// vfnmsub231ss %xmm0, %xmm0, %xmm0
, 999		// vgatherdpd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherdpd %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vgatherdps %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherdps %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vgatherqpd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherqpd %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vgatherqps %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherqps %xmm0, es:(%rax,%rax,1), %xmm0
, 12		// vhaddpd (%rsi), %xmm0, %xmm0
, 12		// vhaddpd %xmm0, %xmm0, %xmm0
, 13		// vhaddpd (%rsi), %ymm0, %ymm0
, 13		// vhaddpd %ymm0, %ymm0, %ymm0
, 12		// vhaddps (%rsi), %xmm0, %xmm0
, 12		// vhaddps %xmm0, %xmm0, %xmm0
, 13		// vhaddps (%rsi), %ymm0, %ymm0
, 13		// vhaddps %ymm0, %ymm0, %ymm0
, 12		// vhsubpd (%rsi), %xmm0, %xmm0
, 12		// vhsubpd %xmm0, %xmm0, %xmm0
, 13		// vhsubpd (%rsi), %ymm0, %ymm0
, 13		// vhsubpd %ymm0, %ymm0, %ymm0
, 12		// vhsubps (%rsi), %xmm0, %xmm0
, 12		// vhsubps %xmm0, %xmm0, %xmm0
, 13		// vhsubps (%rsi), %ymm0, %ymm0
, 13		// vhsubps %ymm0, %ymm0, %ymm0
, 1		// vinsertf128 $0x4, (%rsi), %ymm0, %ymm0
, 7		// vinsertf128 $0x4, %xmm0, %ymm0, %ymm0
, 1		// vinserti128 $0x4, (%rsi), %ymm0, %ymm0
, 7		// vinserti128 $0x4, %xmm0, %ymm0, %ymm0
, 1		// vinsertps $0x4, (%rsi), %xmm0, %xmm0
, 1		// vinsertps $0x4, %xmm0, %xmm0, %xmm0
, 1		// vlddqu (%rsi), %xmm0
, 1		// vlddqu (%rsi), %ymm0
, 5		// vldmxcsr (%rsi)
, 14		// vmaskmovdqu %xmm0, %xmm0
, 1		// vmaskmovpd %xmm0, %xmm0, (%rsi)
, 1		// vmaskmovpd %ymm0, %ymm0, (%rsi)
, 4		// vmaskmovpd (%rsi), %xmm0, %xmm0
, 4		// vmaskmovpd (%rsi), %ymm0, %ymm0
, 1		// vmaskmovps %xmm0, %xmm0, (%rsi)
, 1		// vmaskmovps %ymm0, %ymm0, (%rsi)
, 4		// vmaskmovps (%rsi), %xmm0, %xmm0
, 4		// vmaskmovps (%rsi), %ymm0, %ymm0
, 7		// vmaxpd (%rsi), %xmm0, %xmm0
, 7		// vmaxpd %xmm0, %xmm0, %xmm0
, 7		// vmaxpd (%rsi), %ymm0, %ymm0
, 7		// vmaxpd %ymm0, %ymm0, %ymm0
, 7		// vmaxps (%rsi), %xmm0, %xmm0
, 7		// vmaxps %xmm0, %xmm0, %xmm0
, 7		// vmaxps (%rsi), %ymm0, %ymm0
, 7		// vmaxps %ymm0, %ymm0, %ymm0
, 7		// vmaxsd (%rsi), %xmm0, %xmm0
, 7		// vmaxsd %xmm0, %xmm0, %xmm0
, 7		// vmaxss (%rsi), %xmm0, %xmm0
, 7		// vmaxss %xmm0, %xmm0, %xmm0
, 7		// vminpd (%rsi), %xmm0, %xmm0
, 7		// vminpd %xmm0, %xmm0, %xmm0
, 7		// vminpd (%rsi), %ymm0, %ymm0
, 7		// vminpd %ymm0, %ymm0, %ymm0
, 7		// vminps (%rsi), %xmm0, %xmm0
, 7		// vminps %xmm0, %xmm0, %xmm0
, 7		// vminps (%rsi), %ymm0, %ymm0
, 7		// vminps %ymm0, %ymm0, %ymm0
, 7		// vminsd (%rsi), %xmm0, %xmm0
, 7		// vminsd %xmm0, %xmm0, %xmm0
, 7		// vminss (%rsi), %xmm0, %xmm0
, 7		// vminss %xmm0, %xmm0, %xmm0
, 1		// vmovapd %xmm0, (%rsi)
, 1		// vmovapd %ymm0, (%rsi)
, 1		// vmovapd (%rsi), %xmm0
, 1		// vmovapd %xmm0, %xmm0
, 1		// vmovapd %xmm0, %xmm0
, 1		// vmovapd (%rsi), %ymm0
, 1		// vmovapd %ymm0, %ymm0
, 1		// vmovapd %ymm0, %ymm0
, 1		// vmovaps %xmm0, (%rsi)
, 1		// vmovaps %ymm0, (%rsi)
, 1		// vmovaps (%rsi), %xmm0
, 1		// vmovaps %xmm0, %xmm0
, 1		// vmovaps %xmm0, %xmm0
, 1		// vmovaps (%rsi), %ymm0
, 1		// vmovaps %ymm0, %ymm0
, 1		// vmovaps %ymm0, %ymm0
, 1		// vmovd %xmm0, (%rsi)
, 1		// vmovd %xmm0, %edx
, 1		// vmovd (%rsi), %xmm0
, 1		// vmovd %edx, %xmm0
, 1		// vmovddup (%rsi), %xmm0
, 1		// vmovddup %xmm0, %xmm0
, 1		// vmovddup (%rsi), %ymm0
, 1		// vmovddup %ymm0, %ymm0
, 1		// vmovdqa %xmm0, (%rsi)
, 1		// vmovdqa %ymm0, (%rsi)
, 1		// vmovdqa (%rsi), %xmm0
, 1		// vmovdqa %xmm0, %xmm0
, 1		// vmovdqa %xmm0, %xmm0
, 1		// vmovdqa (%rsi), %ymm0
, 1		// vmovdqa %ymm0, %ymm0
, 1		// vmovdqa %ymm0, %ymm0
, 1		// vmovdqu %xmm0, (%rsi)
, 1		// vmovdqu %ymm0, (%rsi)
, 1		// vmovdqu (%rsi), %xmm0
, 1		// vmovdqu %xmm0, %xmm0
, 1		// vmovdqu %xmm0, %xmm0
, 1		// vmovdqu (%rsi), %ymm0
, 1		// vmovdqu %ymm0, %ymm0
, 1		// vmovdqu %ymm0, %ymm0
, 1		// vmovhlps %xmm0, %xmm0, %xmm0
, 1		// vmovhpd %xmm0, (%rsi)
, 1		// vmovhpd (%rsi), %xmm0, %xmm0
, 2		// vmovhps %xmm0, (%rsi)
, 1		// vmovhps (%rsi), %xmm0, %xmm0
, 1		// vmovlhps %xmm0, %xmm0, %xmm0
, 3		// vmovlpd %xmm0, (%rsi)
, 1		// vmovlpd (%rsi), %xmm0, %xmm0
, 3		// vmovlps %xmm0, (%rsi)
, 1		// vmovlps (%rsi), %xmm0, %xmm0
, 1		// vmovmskpd %xmm0, %edx
, 1		// vmovmskpd %ymm0, %edx
, 1		// vmovmskpd %xmm0, %rdx
, 1		// vmovmskpd %ymm0, %rdx
, 1		// vmovmskps %xmm0, %edx
, 1		// vmovmskps %ymm0, %edx
, 1		// vmovmskps %xmm0, %rdx
, 1		// vmovmskps %ymm0, %rdx
, 1		// vmovntdqa %xmm0, (%rsi)
, 1		// vmovntdqa (%rsi), %xmm0
, 1		// vmovntdqa (%rsi), %ymm0
, 1		// vmovntpd %xmm0, (%rsi)
, 1		// vmovntpd %ymm0, (%rsi)
, 1		// vmovntps %xmm0, (%rsi)
, 1		// vmovntps %ymm0, (%rsi)
, 1		// vmovq %xmm0, (%rsi)
, 1		// vmovq %xmm0, (%rsi)
, 1		// vmovq %xmm0, %rdx
, 1		// vmovq (%rsi), %xmm0
, 1		// vmovq (%rsi), %xmm0
, 1		// vmovq %rdx, %xmm0
, 1		// vmovq %xmm0, %xmm0
, 1		// vmovq %xmm0, %xmm0
, 1		// vmovsd %xmm0, (%rsi)
, 1		// vmovsd (%rsi), %xmm0
, 1		// vmovsd %xmm0, %xmm0, %xmm0
, 1		// vmovsd %xmm0, %xmm0, %xmm0
, 1		// vmovshdup (%rsi), %xmm0
, 1		// vmovshdup %xmm0, %xmm0
, 1		// vmovshdup (%rsi), %ymm0
, 1		// vmovshdup %ymm0, %ymm0
, 1		// vmovsldup (%rsi), %xmm0
, 1		// vmovsldup %xmm0, %xmm0
, 1		// vmovsldup (%rsi), %ymm0
, 1		// vmovsldup %ymm0, %ymm0
, 1		// vmovss %xmm0, (%rsi)
, 1		// vmovss (%rsi), %xmm0
, 1		// vmovss %xmm0, %xmm0, %xmm0
, 1		// vmovss %xmm0, %xmm0, %xmm0
, 1		// vmovupd %xmm0, (%rsi)
, 1		// vmovupd %ymm0, (%rsi)
, 1		// vmovupd (%rsi), %xmm0
, 1		// vmovupd %xmm0, %xmm0
, 1		// vmovupd %xmm0, %xmm0
, 1		// vmovupd (%rsi), %ymm0
, 1		// vmovupd %ymm0, %ymm0
, 1		// vmovupd %ymm0, %ymm0
, 1		// vmovups %xmm0, (%rsi)
, 1		// vmovups %ymm0, (%rsi)
, 1		// vmovups (%rsi), %xmm0
, 1		// vmovups %xmm0, %xmm0
, 1		// vmovups %xmm0, %xmm0
, 1		// vmovups (%rsi), %ymm0
, 1		// vmovups %ymm0, %ymm0
, 1		// vmovups %ymm0, %ymm0
, 14		// vmpsadbw $0x4, (%rsi), %xmm0, %xmm0
, 17		// vmpsadbw $0x4, %xmm0, %xmm0, %xmm0
, 14		// vmpsadbw $0x4, (%rsi), %ymm0, %ymm0
, 17		// vmpsadbw $0x4, %ymm0, %ymm0, %ymm0
, 999		// vmulpd es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmulpd %xmm0, %xmm0, %xmm0
, 999		// vmulpd es:(%rax,%rax,1), %ymm0, %ymm0
, 999		// vmulpd %ymm0, %ymm0, %ymm0
, 999		// vmulps es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmulps %xmm0, %xmm0, %xmm0
, 999		// vmulps es:(%rax,%rax,1), %ymm0, %ymm0
, 999		// vmulps %ymm0, %ymm0, %ymm0
, 999		// vmulsd es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmulsd %xmm0, %xmm0, %xmm0
, 999		// vmulss es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmulss %xmm0, %xmm0, %xmm0
, 1		// vorpd (%rsi), %xmm0, %xmm0
, 1		// vorpd %xmm0, %xmm0, %xmm0
, 1		// vorpd (%rsi), %ymm0, %ymm0
, 1		// vorpd %ymm0, %ymm0, %ymm0
, 1		// vorps (%rsi), %xmm0, %xmm0
, 1		// vorps %xmm0, %xmm0, %xmm0
, 1		// vorps (%rsi), %ymm0, %ymm0
, 1		// vorps %ymm0, %ymm0, %ymm0
, 1		// vpabsb (%rsi), %xmm0
, 1		// vpabsb %xmm0, %xmm0
, 1		// vpabsb (%rsi), %ymm0
, 1		// vpabsb %ymm0, %ymm0
, 1		// vpabsd (%rsi), %xmm0
, 1		// vpabsd %xmm0, %xmm0
, 1		// vpabsd (%rsi), %ymm0
, 1		// vpabsd %ymm0, %ymm0
, 1		// vpabsw (%rsi), %xmm0
, 1		// vpabsw %xmm0, %xmm0
, 1		// vpabsw (%rsi), %ymm0
, 1		// vpabsw %ymm0, %ymm0
, 1		// vpackssdw (%rsi), %xmm0, %xmm0
, 1		// vpackssdw %xmm0, %xmm0, %xmm0
, 1		// vpackssdw (%rsi), %ymm0, %ymm0
, 1		// vpackssdw %ymm0, %ymm0, %ymm0
, 1		// vpacksswb (%rsi), %xmm0, %xmm0
, 1		// vpacksswb %xmm0, %xmm0, %xmm0
, 1		// vpacksswb (%rsi), %ymm0, %ymm0
, 1		// vpacksswb %ymm0, %ymm0, %ymm0
, 1		// vpackusdw (%rsi), %xmm0, %xmm0
, 1		// vpackusdw %xmm0, %xmm0, %xmm0
, 1		// vpackusdw (%rsi), %ymm0, %ymm0
, 1		// vpackusdw %ymm0, %ymm0, %ymm0
, 1		// vpackuswb (%rsi), %xmm0, %xmm0
, 1		// vpackuswb %xmm0, %xmm0, %xmm0
, 1		// vpackuswb (%rsi), %ymm0, %ymm0
, 1		// vpackuswb %ymm0, %ymm0, %ymm0
, 1		// vpaddb (%rsi), %xmm0, %xmm0
, 1		// vpaddb %xmm0, %xmm0, %xmm0
, 1		// vpaddb (%rsi), %ymm0, %ymm0
, 1		// vpaddb %ymm0, %ymm0, %ymm0
, 1		// vpaddd (%rsi), %xmm0, %xmm0
, 1		// vpaddd %xmm0, %xmm0, %xmm0
, 1		// vpaddd (%rsi), %ymm0, %ymm0
, 1		// vpaddd %ymm0, %ymm0, %ymm0
, 1		// vpaddq (%rsi), %xmm0, %xmm0
, 1		// vpaddq %xmm0, %xmm0, %xmm0
, 1		// vpaddq (%rsi), %ymm0, %ymm0
, 1		// vpaddq %ymm0, %ymm0, %ymm0
, 1		// vpaddsb (%rsi), %xmm0, %xmm0
, 1		// vpaddsb %xmm0, %xmm0, %xmm0
, 1		// vpaddsb (%rsi), %ymm0, %ymm0
, 1		// vpaddsb %ymm0, %ymm0, %ymm0
, 1		// vpaddsw (%rsi), %xmm0, %xmm0
, 1		// vpaddsw %xmm0, %xmm0, %xmm0
, 1		// vpaddsw (%rsi), %ymm0, %ymm0
, 1		// vpaddsw %ymm0, %ymm0, %ymm0
, 1		// vpaddusb (%rsi), %xmm0, %xmm0
, 1		// vpaddusb %xmm0, %xmm0, %xmm0
, 1		// vpaddusb (%rsi), %ymm0, %ymm0
, 1		// vpaddusb %ymm0, %ymm0, %ymm0
, 1		// vpaddusw (%rsi), %xmm0, %xmm0
, 1		// vpaddusw %xmm0, %xmm0, %xmm0
, 1		// vpaddusw (%rsi), %ymm0, %ymm0
, 1		// vpaddusw %ymm0, %ymm0, %ymm0
, 1		// vpaddw (%rsi), %xmm0, %xmm0
, 1		// vpaddw %xmm0, %xmm0, %xmm0
, 1		// vpaddw (%rsi), %ymm0, %ymm0
, 1		// vpaddw %ymm0, %ymm0, %ymm0
, 1		// vpalignr $0x4, (%rsi), %xmm0, %xmm0
, 1		// vpalignr $0x4, %xmm0, %xmm0, %xmm0
, 1		// vpalignr $0x4, (%rsi), %ymm0, %ymm0
, 1		// vpalignr $0x4, %ymm0, %ymm0, %ymm0
, 1		// vpand (%rsi), %xmm0, %xmm0
, 1		// vpand %xmm0, %xmm0, %xmm0
, 1		// vpand (%rsi), %ymm0, %ymm0
, 1		// vpand %ymm0, %ymm0, %ymm0
, 1		// vpandn (%rsi), %xmm0, %xmm0
, 1		// vpandn %xmm0, %xmm0, %xmm0
, 1		// vpandn (%rsi), %ymm0, %ymm0
, 1		// vpandn %ymm0, %ymm0, %ymm0
, 1		// vpavgb (%rsi), %xmm0, %xmm0
, 1		// vpavgb %xmm0, %xmm0, %xmm0
, 1		// vpavgb (%rsi), %ymm0, %ymm0
, 1		// vpavgb %ymm0, %ymm0, %ymm0
, 1		// vpavgw (%rsi), %xmm0, %xmm0
, 1		// vpavgw %xmm0, %xmm0, %xmm0
, 1		// vpavgw (%rsi), %ymm0, %ymm0
, 1		// vpavgw %ymm0, %ymm0, %ymm0
, 1		// vpblendd $0x4, (%rsi), %xmm0, %xmm0
, 1		// vpblendd $0x4, %xmm0, %xmm0, %xmm0
, 1		// vpblendd $0x4, (%rsi), %ymm0, %ymm0
, 1		// vpblendd $0x4, %ymm0, %ymm0, %ymm0
, 4		// vpblendvb %xmm0, (%rsi), %xmm0, %xmm0
, 4		// vpblendvb %xmm0, %xmm0, %xmm0, %xmm0
, 4		// vpblendvb %ymm0, (%rsi), %ymm0, %ymm0
, 4		// vpblendvb %ymm0, %ymm0, %ymm0, %ymm0
, 1		// vpblendw $0x4, (%rsi), %xmm0, %xmm0
, 1		// vpblendw $0x4, %xmm0, %xmm0, %xmm0
, 1		// vpblendw $0x4, (%rsi), %ymm0, %ymm0
, 1		// vpblendw $0x4, %ymm0, %ymm0, %ymm0
, 1		// vpbroadcastb (%rsi), %xmm0
, 1		// vpbroadcastb %xmm0, %xmm0
, 1		// vpbroadcastb (%rsi), %ymm0
, 7		// vpbroadcastb %xmm0, %ymm0
, 1		// vpbroadcastd (%rsi), %xmm0
, 1		// vpbroadcastd %xmm0, %xmm0
, 1		// vpbroadcastd (%rsi), %ymm0
, 7		// vpbroadcastd %xmm0, %ymm0
, 1		// vpbroadcastq (%rsi), %xmm0
, 1		// vpbroadcastq %xmm0, %xmm0
, 1		// vpbroadcastq (%rsi), %ymm0
, 7		// vpbroadcastq %xmm0, %ymm0
, 1		// vpbroadcastw (%rsi), %xmm0
, 1		// vpbroadcastw %xmm0, %xmm0
, 1		// vpbroadcastw (%rsi), %ymm0
, 7		// vpbroadcastw %xmm0, %ymm0
, 17		// vpclmulqdq $0x4, (%rsi), %xmm0, %xmm0
, 17		// vpclmulqdq $0x4, %xmm0, %xmm0, %xmm0
, 1		// vpcmpeqb (%rsi), %xmm0, %xmm0
, 1		// vpcmpeqb %xmm0, %xmm0, %xmm0
, 1		// vpcmpeqb (%rsi), %ymm0, %ymm0
, 1		// vpcmpeqb %ymm0, %ymm0, %ymm0
, 1		// vpcmpeqd (%rsi), %xmm0, %xmm0
, 1		// vpcmpeqd %xmm0, %xmm0, %xmm0
, 1		// vpcmpeqd (%rsi), %ymm0, %ymm0
, 1		// vpcmpeqd %ymm0, %ymm0, %ymm0
, 1		// vpcmpeqq (%rsi), %xmm0, %xmm0
, 1		// vpcmpeqq %xmm0, %xmm0, %xmm0
, 1		// vpcmpeqq (%rsi), %ymm0, %ymm0
, 1		// vpcmpeqq %ymm0, %ymm0, %ymm0
, 1		// vpcmpeqw (%rsi), %xmm0, %xmm0
, 1		// vpcmpeqw %xmm0, %xmm0, %xmm0
, 1		// vpcmpeqw (%rsi), %ymm0, %ymm0
, 1		// vpcmpeqw %ymm0, %ymm0, %ymm0
, 9		// vpcmpestri $0x4, (%rsi), %xmm0
, 9		// vpcmpestri $0x4, %xmm0, %xmm0
, 27		// vpcmpestrm $0x4, (%rsi), %xmm0
, 27		// vpcmpestrm $0x4, %xmm0, %xmm0
, 1		// vpcmpgtb (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtb %xmm0, %xmm0, %xmm0
, 1		// vpcmpgtb (%rsi), %ymm0, %ymm0
, 1		// vpcmpgtb %ymm0, %ymm0, %ymm0
, 1		// vpcmpgtd (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtd %xmm0, %xmm0, %xmm0
, 1		// vpcmpgtd (%rsi), %ymm0, %ymm0
, 1		// vpcmpgtd %ymm0, %ymm0, %ymm0
, 12		// vpcmpgtq (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtq %xmm0, %xmm0, %xmm0
, 12		// vpvmpgtq (%rsi), %ymm0, %ymm0
, 1		// vpvmpgtq %ymm0, %ymm0, %ymm0
, 1		// vpcmpgtw (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtw %xmm0, %xmm0, %xmm0
, 1		// vpcmpgtw (%rsi), %ymm0, %ymm0
, 1		// vpcmpgtw %ymm0, %ymm0, %ymm0
, 7		// vpcmpistri $0x4, (%rsi), %xmm0
, 7		// vpcmpistri $0x4, %xmm0, %xmm0
, 27		// vpcmpistrm $0x4, (%rsi), %xmm0
, 26		// vpcmpistrm $0x4, %xmm0, %xmm0
, 7		// vperm2f128 $0x4, (%rsi), %ymm0, %ymm0
, 7		// vperm2f128 $0x4, %ymm0, %ymm0, %ymm0
, 7		// vperm2i128 $0x4, (%rsi), %ymm0, %ymm0
, 7		// vperm2i128 $0x4, %ymm0, %ymm0, %ymm0
, 7		// vpermd (%rsi), %ymm0, %ymm0
, 7		// vpermd %ymm0, %ymm0, %ymm0
, 1		// vpermilpd $0x4, (%rsi), %xmm0
, 1		// vpermilpd $0x4, %xmm0, %xmm0
, 1		// vpermilpd (%rsi), %xmm0, %xmm0
, 1		// vpermilpd %xmm0, %xmm0, %xmm0
, 1		// vpermilpd $0x4, (%rsi), %ymm0
, 1		// vpermilpd $0x4, %ymm0, %ymm0
, 1		// vpermilpd (%rsi), %ymm0, %ymm0
, 1		// vpermilpd %ymm0, %ymm0, %ymm0
, 1		// vpermilps $0x4, (%rsi), %xmm0
, 1		// vpermilps $0x4, %xmm0, %xmm0
, 1		// vpermilps (%rsi), %xmm0, %xmm0
, 1		// vpermilps %xmm0, %xmm0, %xmm0
, 1		// vpermilps $0x4, (%rsi), %ymm0
, 1		// vpermilps $0x4, %ymm0, %ymm0
, 1		// vpermilps (%rsi), %ymm0, %ymm0
, 1		// vpermilps %ymm0, %ymm0, %ymm0
, 1		// vpermpd $0x4, (%rsi), %ymm0
, 7		// vpermpd $0x4, %ymm0, %ymm0
, 7		// vpermps (%rsi), %ymm0, %ymm0
, 7		// vpermps %ymm0, %ymm0, %ymm0
, 1		// vpermq $0x4, (%rsi), %ymm0
, 7		// vpermq $0x4, %ymm0, %ymm0
, 1		// vpextrb $0x4, %xmm0, (%rsi)
, 1		// vpextrb $0x4, %xmm0, %edx
, 1		// vpextrb $0x4, %xmm0, %rdx
, 1		// vpextrd $0x4, %xmm0, (%rsi)
, 1		// vpextrd $0x4, %xmm0, %edx
, 1		// vpextrq $0x4, %xmm0, (%rsi)
, 1		// vpextrq $0x4, %xmm0, %rdx
, 1		// vpextrw $0x4, %xmm0, (%rsi)
, 1		// vpextrw $0x4, %xmm0, %edx
, 1		// vpextrw $0x4, %xmm0, %edx
, 1		// vpextrw $0x4, %xmm0, %rdx
, 1		// vpextrw $0x4, %xmm0, %rdx
, 999		// vpgatherdd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherdd %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vpgatherdq %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherdq %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vpgatherqd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqq %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqq %ymm0, es:(%rax,%rax,1), %ymm0
, 7		// vphaddd (%rsi), %xmm0, %xmm0
, 7		// vphaddd %xmm0, %xmm0, %xmm0
, 7		// vphaddd (%rsi), %ymm0, %ymm0
, 7		// vphaddd %ymm0, %ymm0, %ymm0
, 7		// vphaddsw (%rsi), %xmm0, %xmm0
, 7		// vphaddsw %xmm0, %xmm0, %xmm0
, 7		// vphaddsw (%rsi), %ymm0, %ymm0
, 7		// vphaddsw %ymm0, %ymm0, %ymm0
, 7		// vphaddw (%rsi), %xmm0, %xmm0
, 7		// vphaddw %xmm0, %xmm0, %xmm0
, 7		// vphaddw (%rsi), %ymm0, %ymm0
, 7		// vphaddw %ymm0, %ymm0, %ymm0
, 1		// vphminposuw (%rsi), %xmm0
, 12		// vphminposuw %xmm0, %xmm0
, 7		// vphsubd (%rsi), %xmm0, %xmm0
, 7		// vphsubd %xmm0, %xmm0, %xmm0
, 7		// vphsubd (%rsi), %ymm0, %ymm0
, 7		// vphsubd %ymm0, %ymm0, %ymm0
, 7		// vphsubsw (%rsi), %xmm0, %xmm0
, 7		// vphsubsw %xmm0, %xmm0, %xmm0
, 7		// vphsubsw (%rsi), %ymm0, %ymm0
, 7		// vphsubsw %ymm0, %ymm0, %ymm0
, 7		// vphsubw (%rsi), %xmm0, %xmm0
, 7		// vphsubw %xmm0, %xmm0, %xmm0
, 7		// vphsubw (%rsi), %ymm0, %ymm0
, 7		// vphsubw %ymm0, %ymm0, %ymm0
, 1		// vpinsrb $0x4, (%rsi), %xmm0, %xmm0
, 4		// vpinsrb $0x4, %edx, %xmm0, %xmm0
, 1		// vpinsrd $0x4, (%rsi), %xmm0, %xmm0
, 4		// vpinsrd $0x4, %edx, %xmm0, %xmm0
, 1		// vpinsrq $0x4, (%rsi), %xmm0, %xmm0
, 4		// vpinsrq $0x4, %rdx, %xmm0, %xmm0
, 1		// vpinsrw $0x4, (%rsi), %xmm0, %xmm0
, 4		// vpinsrw $0x4, %edx, %xmm0, %xmm0
, 12		// vpmaddubsw (%rsi), %xmm0, %xmm0
, 12		// vpmaddubsw %xmm0, %xmm0, %xmm0
, 12		// vpmaddubsw (%rsi), %ymm0, %ymm0
, 12		// vpmaddubsw %ymm0, %ymm0, %ymm0
, 12		// vpmaddwd (%rsi), %xmm0, %xmm0
, 12		// vpmaddwd %xmm0, %xmm0, %xmm0
, 12		// vpmaddwd (%rsi), %ymm0, %ymm0
, 12		// vpmaddwd %ymm0, %ymm0, %ymm0
, 1		// vpmaskmovd %xmm0, %xmm0, (%rsi)
, 1		// vpmaskmovd %ymm0, %ymm0, (%rsi)
, 4		// vpmaskmovd (%rsi), %xmm0, %xmm0
, 4		// vpmaskmovd (%rsi), %ymm0, %ymm0
, 1		// vpmaskmovq %xmm0, %xmm0, (%rsi)
, 1		// vpmaskmovq %ymm0, %ymm0, (%rsi)
, 4		// vpmaskmovq (%rsi), %xmm0, %xmm0
, 4		// vpmaskmovq (%rsi), %ymm0, %ymm0
, 1		// vpmaxsb (%rsi), %xmm0, %xmm0
, 1		// vpmaxsb %xmm0, %xmm0, %xmm0
, 1		// vpmaxsb (%rsi), %ymm0, %ymm0
, 1		// vpmaxsb %ymm0, %ymm0, %ymm0
, 1		// vpmaxsd (%rsi), %xmm0, %xmm0
, 1		// vpmaxsd %xmm0, %xmm0, %xmm0
, 1		// vpmaxsd (%rsi), %ymm0, %ymm0
, 1		// vpmaxsd %ymm0, %ymm0, %ymm0
, 1		// vpmaxsw (%rsi), %xmm0, %xmm0
, 1		// vpmaxsw %xmm0, %xmm0, %xmm0
, 1		// vpmaxsw (%rsi), %ymm0, %ymm0
, 1		// vpmaxsw %ymm0, %ymm0, %ymm0
, 1		// vpmaxub (%rsi), %xmm0, %xmm0
, 1		// vpmaxub %xmm0, %xmm0, %xmm0
, 1		// vpmaxub (%rsi), %ymm0, %ymm0
, 1		// vpmaxub %ymm0, %ymm0, %ymm0
, 1		// vpmaxud (%rsi), %xmm0, %xmm0
, 1		// vpmaxud %xmm0, %xmm0, %xmm0
, 1		// vpmaxud (%rsi), %ymm0, %ymm0
, 1		// vpmaxud %ymm0, %ymm0, %ymm0
, 1		// vpmaxuw (%rsi), %xmm0, %xmm0
, 1		// vpmaxuw %xmm0, %xmm0, %xmm0
, 1		// vpmaxuw (%rsi), %ymm0, %ymm0
, 1		// vpmaxuw %ymm0, %ymm0, %ymm0
, 1		// vpminsb (%rsi), %xmm0, %xmm0
, 1		// vpminsb %xmm0, %xmm0, %xmm0
, 1		// vpminsb (%rsi), %ymm0, %ymm0
, 1		// vpminsb %ymm0, %ymm0, %ymm0
, 1		// vpminsd (%rsi), %xmm0, %xmm0
, 1		// vpminsd %xmm0, %xmm0, %xmm0
, 1		// vpminsd (%rsi), %ymm0, %ymm0
, 1		// vpminsd %ymm0, %ymm0, %ymm0
, 1		// vpminsw (%rsi), %xmm0, %xmm0
, 1		// vpminsw %xmm0, %xmm0, %xmm0
, 1		// vpminub (%rsi), %xmm0, %xmm0
, 1		// vpminub %xmm0, %xmm0, %xmm0
, 1		// vpminub (%rsi), %ymm0, %ymm0
, 1		// vpminub %ymm0, %ymm0, %ymm0
, 2		// vpminud (%rsi), %xmm0, %xmm0
, 2		// vpminud %xmm0, %xmm0, %xmm0
, 2		// vpminud (%rsi), %ymm0, %ymm0
, 1		// vpminud %ymm0, %ymm0, %ymm0
, 1		// vpminuw (%rsi), %xmm0, %xmm0
, 1		// vpminuw %xmm0, %xmm0, %xmm0
, 1		// vpminuw (%rsi), %ymm0, %ymm0
, 1		// vpminuw %ymm0, %ymm0, %ymm0
, 1		// vpmovmskb %xmm0, %edx
, 1		// vpmovmskb %ymm0, %edx
, 1		// vpmovmskb %xmm0, %rdx
, 1		// vpmovmskb %ymm0, %rdx
, 1		// vpmovsxbd (%rsi), %xmm0
, 1		// vpmovsxbd %xmm0, %xmm0
, 1		// vpmovsxbd (%rsi), %ymm0
, 7		// vpmovsxbd %xmm0, %ymm0
, 1		// vpmovsxbq (%rsi), %xmm0
, 1		// vpmovsxbq %xmm0, %xmm0
, 1		// vpmovsxbq (%rsi), %ymm0
, 7		// vpmovsxbq %xmm0, %ymm0
, 1		// vpmovsxbw (%rsi), %xmm0
, 1		// vpmovsxbw %xmm0, %xmm0
, 1		// vpmovsxbw (%rsi), %ymm0
, 7		// vpmovsxbw %xmm0, %ymm0
, 1		// vpmovsxdq (%rsi), %xmm0
, 1		// vpmovsxdq %xmm0, %xmm0
, 1		// vpmovsxdq (%rsi), %ymm0
, 7		// vpmovsxdq %xmm0, %ymm0
, 1		// vpmovsxwd (%rsi), %xmm0
, 1		// vpmovsxwd %xmm0, %xmm0
, 1		// vpmovsxwd (%rsi), %ymm0
, 7		// vpmovsxwd %xmm0, %ymm0
, 1		// vpmovsxwq (%rsi), %xmm0
, 1		// vpmovsxwq %xmm0, %xmm0
, 1		// vpmovsxwq (%rsi), %ymm0
, 7		// vpmovsxwq %xmm0, %ymm0
, 1		// vpmovzxbd (%rsi), %xmm0
, 1		// vpmovzxbd %xmm0, %xmm0
, 1		// vpmovzxbd (%rsi), %ymm0
, 7		// vpmovzxbd %xmm0, %ymm0
, 1		// vpmovzxbq (%rsi), %xmm0
, 1		// vpmovzxbq %xmm0, %xmm0
, 1		// vpmovzxbq (%rsi), %ymm0
, 7		// vpmovzxbq %xmm0, %ymm0
, 2		// vpmovzxbw (%rsi), %xmm0
, 2		// vpmovzxbw %xmm0, %xmm0
, 2		// vpmovzxbw (%rsi), %ymm0
, 7		// vpmovzxbw %xmm0, %ymm0
, 1		// vpmovzxdq (%rsi), %xmm0
, 1		// vpmovzxdq %xmm0, %xmm0
, 1		// vpmovzxdq (%rsi), %ymm0
, 7		// vpmovzxdq %xmm0, %ymm0
, 1		// vpmovzxwd (%rsi), %xmm0
, 1		// vpmovzxwd %xmm0, %xmm0
, 1		// vpmovzxwd (%rsi), %ymm0
, 7		// vpmovzxwd %xmm0, %ymm0
, 1		// vpmovzxwq (%rsi), %xmm0
, 1		// vpmovzxwq %xmm0, %xmm0
, 1		// vpmovzxwq (%rsi), %ymm0
, 7		// vpmovzxwq %xmm0, %ymm0
, 12		// vpmuldq (%rsi), %xmm0, %xmm0
, 12		// vpmuldq %xmm0, %xmm0, %xmm0
, 12		// vpmuldq (%rsi), %ymm0, %ymm0
, 12		// vpmuldq %ymm0, %ymm0, %ymm0
, 12		// vpmulhrsw (%rsi), %xmm0, %xmm0
, 12		// vpmulhrsw %xmm0, %xmm0, %xmm0
, 12		// vpmulhrsw (%rsi), %ymm0, %ymm0
, 12		// vpmulhrsw %ymm0, %ymm0, %ymm0
, 12		// vpmulhuw (%rsi), %xmm0, %xmm0
, 12		// vpmulhuw %xmm0, %xmm0, %xmm0
, 12		// vpmulhuw (%rsi), %ymm0, %ymm0
, 12		// vpmulhuw %ymm0, %ymm0, %ymm0
, 12		// vpmulhw (%rsi), %xmm0, %xmm0
, 12		// vpmulhw %xmm0, %xmm0, %xmm0
, 12		// vpmulhw (%rsi), %ymm0, %ymm0
, 12		// vpmulhw %ymm0, %ymm0, %ymm0
, 24		// vpmulld (%rsi), %xmm0, %xmm0
, 24		// vpmulld %xmm0, %xmm0, %xmm0
, 24		// vpmulld (%rsi), %ymm0, %ymm0
, 24		// vpmulld %ymm0, %ymm0, %ymm0
, 12		// vpmullw (%rsi), %xmm0, %xmm0
, 12		// vpmullw %xmm0, %xmm0, %xmm0
, 12		// vpmullw (%rsi), %ymm0, %ymm0
, 12		// vpmullw %ymm0, %ymm0, %ymm0
, 12		// vpmuludq (%rsi), %xmm0, %xmm0
, 12		// vpmuludq %xmm0, %xmm0, %xmm0
, 12		// vpmuludq (%rsi), %ymm0, %ymm0
, 12		// vpmuludq %ymm0, %ymm0, %ymm0
, 1		// vpor (%rsi), %xmm0, %xmm0
, 1		// vpor %xmm0, %xmm0, %xmm0
, 1		// vpor (%rsi), %ymm0, %ymm0
, 1		// vpor %ymm0, %ymm0, %ymm0
, 12		// vpsadbw (%rsi), %xmm0, %xmm0
, 12		// vpsadbw %xmm0, %xmm0, %xmm0
, 12		// vpsadbw (%rsi), %ymm0, %ymm0
, 12		// vpsadbw %ymm0, %ymm0, %ymm0
, 1		// vpshufb (%rsi), %xmm0, %xmm0
, 1		// vpshufb %xmm0, %xmm0, %xmm0
, 1		// vpshufb (%rsi), %ymm0, %ymm0
, 1		// vpshufb %ymm0, %ymm0, %ymm0
, 1		// vpshufd $0x4, (%rsi), %xmm0
, 1		// vpshufd $0x4, %xmm0, %xmm0
, 1		// vpshufd $0x4, (%rsi), %ymm0
, 1		// vpshufd $0x4, %ymm0, %ymm0
, 1		// vpshufhw $0x4, (%rsi), %xmm0
, 1		// vpshufhw $0x4, %xmm0, %xmm0
, 1		// vpshufhw $0x4, (%rsi), %ymm0
, 1		// vpshufhw $0x4, %ymm0, %ymm0
, 1		// vpshuflw $0x4, (%rsi), %xmm0
, 1		// vpshuflw $0x4, %xmm0, %xmm0
, 1		// vpshuflw $0x4, (%rsi), %ymm0
, 1		// vpshuflw $0x4, %ymm0, %ymm0
, 1		// vpsignb (%rsi), %xmm0, %xmm0
, 1		// vpsignb %xmm0, %xmm0, %xmm0
, 1		// vpsignd (%rsi), %xmm0, %xmm0
, 1		// vpsignd %xmm0, %xmm0, %xmm0
, 1		// vpsignw (%rsi), %xmm0, %xmm0
, 1		// vpsignw %xmm0, %xmm0, %xmm0
, 1		// vpslld $0x4, %xmm0, %xmm0
, 1		// vpslld (%rsi), %xmm0, %xmm0
, 4		// vpslld %xmm0, %xmm0, %xmm0
, 1		// vpslld $0x4, %ymm0, %ymm0
, 1		// vpslld (%rsi), %ymm0, %ymm0
, 9		// vpslld %xmm0, %ymm0, %ymm0
, 1		// vpslldq $0x4, %xmm0, %xmm0
, 1		// vpslldq $0x4, %ymm0, %ymm0
, 1		// vpsllq $0x4, %xmm0, %xmm0
, 1		// vpsllq (%rsi), %xmm0, %xmm0
, 4		// vpsllq %xmm0, %xmm0, %xmm0
, 1		// vpsllq $0x4, %ymm0, %ymm0
, 1		// vpsllq (%rsi), %ymm0, %ymm0
, 9		// vpsllq %xmm0, %ymm0, %ymm0
, 4		// vpsllvd (%rsi), %xmm0, %xmm0
, 4		// vpsllvd %xmm0, %xmm0, %xmm0
, 4		// vpsllvd (%rsi), %ymm0, %ymm0
, 4		// vpsllvd %ymm0, %ymm0, %ymm0
, 1		// vpsllvq (%rsi), %xmm0, %xmm0
, 1		// vpsllvq %xmm0, %xmm0, %xmm0
, 1		// vpsllvq (%rsi), %ymm0, %ymm0
, 1		// vpsllvq %ymm0, %ymm0, %ymm0
, 1		// vpsllw $0x4, %xmm0, %xmm0
, 1		// vpsllw (%rsi), %xmm0, %xmm0
, 4		// vpsllw %xmm0, %xmm0, %xmm0
, 1		// vpsllw $0x4, %ymm0, %ymm0
, 1		// vpsllw (%rsi), %ymm0, %ymm0
, 9		// vpsllw %xmm0, %ymm0, %ymm0
, 1		// vpsrad $0x4, %xmm0, %xmm0
, 1		// vpsrad (%rsi), %xmm0, %xmm0
, 4		// vpsrad %xmm0, %xmm0, %xmm0
, 1		// vpsrad $0x4, %ymm0, %ymm0
, 1		// vpsrad (%rsi), %ymm0, %ymm0
, 9		// vpsrad %xmm0, %ymm0, %ymm0
, 4		// vpsravd (%rsi), %xmm0, %xmm0
, 4		// vpsravd %xmm0, %xmm0, %xmm0
, 4		// vpsravd (%rsi), %ymm0, %ymm0
, 4		// vpsravd %ymm0, %ymm0, %ymm0
, 1		// vpsraw $0x4, %xmm0, %xmm0
, 1		// vpsraw (%rsi), %xmm0, %xmm0
, 4		// vpsraw %xmm0, %xmm0, %xmm0
, 1		// vpsraw $0x4, %ymm0, %ymm0
, 1		// vpsraw (%rsi), %ymm0, %ymm0
, 9		// vpsraw %xmm0, %ymm0, %ymm0
, 1		// vpsrld $0x4, %xmm0, %xmm0
, 1		// vpsrld (%rsi), %xmm0, %xmm0
, 4		// vpsrld %xmm0, %xmm0, %xmm0
, 1		// vpsrld $0x4, %ymm0, %ymm0
, 1		// vpsrld (%rsi), %ymm0, %ymm0
, 9		// vpsrld %xmm0, %ymm0, %ymm0
, 1		// vpsrldq $0x4, %xmm0, %xmm0
, 1		// vpsrldq $0x4, %ymm0, %ymm0
, 1		// vpsrlq $0x4, %xmm0, %xmm0
, 1		// vpsrlq (%rsi), %xmm0, %xmm0
, 4		// vpsrlq %xmm0, %xmm0, %xmm0
, 1		// vpsrlq $0x4, %ymm0, %ymm0
, 1		// vpsrlq (%rsi), %ymm0, %ymm0
, 9		// vpsrlq %xmm0, %ymm0, %ymm0
, 4		// vpsrlvd (%rsi), %xmm0, %xmm0
, 4		// vpsrlvd %xmm0, %xmm0, %xmm0
, 4		// vpsrlvd (%rsi), %ymm0, %ymm0
, 4		// vpsrlvd %ymm0, %ymm0, %ymm0
, 1		// vpsrlvq (%rsi), %xmm0, %xmm0
, 1		// vpsrlvq %xmm0, %xmm0, %xmm0
, 1		// vpsrlvq (%rsi), %ymm0, %ymm0
, 1		// vpsrlvq %ymm0, %ymm0, %ymm0
, 1		// vpsrlw $0x4, %xmm0, %xmm0
, 1		// vpsrlw (%rsi), %xmm0, %xmm0
, 4		// vpsrlw %xmm0, %xmm0, %xmm0
, 1		// vpsrlw $0x4, %ymm0, %ymm0
, 1		// vpsrlw (%rsi), %ymm0, %ymm0
, 9		// vpsrlw %xmm0, %ymm0, %ymm0
, 1		// vpsubb (%rsi), %xmm0, %xmm0
, 1		// vpsubb %xmm0, %xmm0, %xmm0
, 1		// vpsubb (%rsi), %ymm0, %ymm0
, 1		// vpsubb %ymm0, %ymm0, %ymm0
, 1		// vpsubd (%rsi), %xmm0, %xmm0
, 1		// vpsubd %xmm0, %xmm0, %xmm0
, 1		// vpsubd (%rsi), %ymm0, %ymm0
, 1		// vpsubd %ymm0, %ymm0, %ymm0
, 1		// vpsubq (%rsi), %xmm0, %xmm0
, 1		// vpsubq %xmm0, %xmm0, %xmm0
, 1		// vpsubq (%rsi), %ymm0, %ymm0
, 1		// vpsubq %ymm0, %ymm0, %ymm0
, 1		// vpsubsb (%rsi), %xmm0, %xmm0
, 1		// vpsubsb %xmm0, %xmm0, %xmm0
, 1		// vpsubsb (%rsi), %ymm0, %ymm0
, 1		// vpsubsb %ymm0, %ymm0, %ymm0
, 1		// vpsubsw (%rsi), %xmm0, %xmm0
, 1		// vpsubsw %xmm0, %xmm0, %xmm0
, 1		// vpsubsw (%rsi), %ymm0, %ymm0
, 1		// vpsubsw %ymm0, %ymm0, %ymm0
, 1		// vpsubusb (%rsi), %xmm0, %xmm0
, 1		// vpsubusb %xmm0, %xmm0, %xmm0
, 1		// vpsubusb (%rsi), %ymm0, %ymm0
, 1		// vpsubusb %ymm0, %ymm0, %ymm0
, 1		// vpsubusw (%rsi), %xmm0, %xmm0
, 1		// vpsubusw %xmm0, %xmm0, %xmm0
, 1		// vpsubusw (%rsi), %ymm0, %ymm0
, 1		// vpsubusw %ymm0, %ymm0, %ymm0
, 1		// vpsubw (%rsi), %xmm0, %xmm0
, 1		// vpsubw %xmm0, %xmm0, %xmm0
, 1		// vpsubw (%rsi), %ymm0, %ymm0
, 1		// vpsubw %ymm0, %ymm0, %ymm0
, 1		// vptest (%rsi), %xmm0
, 1		// vptest %xmm0, %xmm0
, 1		// vptest (%rsi), %ymm0
, 1		// vptest %ymm0, %ymm0
, 1		// vpunpckhbw (%rsi), %xmm0, %xmm0
, 1		// vpunpckhbw %xmm0, %xmm0, %xmm0
, 1		// vpunpckhbw (%rsi), %ymm0, %ymm0
, 1		// vpunpckhbw %ymm0, %ymm0, %ymm0
, 1		// vpunpckhdq (%rsi), %xmm0, %xmm0
, 1		// vpunpckhdq %xmm0, %xmm0, %xmm0
, 1		// vpunpckhdq (%rsi), %ymm0, %ymm0
, 1		// vpunpckhdq %ymm0, %ymm0, %ymm0
, 1		// vpunpckhqdq (%rsi), %xmm0, %xmm0
, 1		// vpunpckhqdq %xmm0, %xmm0, %xmm0
, 1		// vpunpckhqdq (%rsi), %ymm0, %ymm0
, 1		// vpunpckhqdq %ymm0, %ymm0, %ymm0
, 1		// vpunpckhwd (%rsi), %xmm0, %xmm0
, 1		// vpunpckhwd %xmm0, %xmm0, %xmm0
, 1		// vpunpckhwd (%rsi), %ymm0, %ymm0
, 1		// vpunpckhwd %ymm0, %ymm0, %ymm0
, 1		// vpunpcklbw (%rsi), %xmm0, %xmm0
, 1		// vpunpcklbw %xmm0, %xmm0, %xmm0
, 1		// vpunpcklbw (%rsi), %ymm0, %ymm0
, 1		// vpunpcklbw %ymm0, %ymm0, %ymm0
, 1		// vpunpckldq (%rsi), %xmm0, %xmm0
, 1		// vpunpckldq %xmm0, %xmm0, %xmm0
, 1		// vpunpckldq (%rsi), %ymm0, %ymm0
, 1		// vpunpckldq %ymm0, %ymm0, %ymm0
, 1		// vpunpcklqdq (%rsi), %xmm0, %xmm0
, 1		// vpunpcklqdq %xmm0, %xmm0, %xmm0
, 1		// vpunpcklqdq (%rsi), %ymm0, %ymm0
, 1		// vpunpcklqdq %ymm0, %ymm0, %ymm0
, 1		// vpunpcklwd (%rsi), %xmm0, %xmm0
, 1		// vpunpcklwd %xmm0, %xmm0, %xmm0
, 1		// vpunpcklwd (%rsi), %ymm0, %ymm0
, 1		// vpunpcklwd %ymm0, %ymm0, %ymm0
, 1		// vpxor (%rsi), %xmm0, %xmm0
, 1		// vpxor %xmm0, %xmm0, %xmm0
, 1		// vpxor (%rsi), %ymm0, %ymm0
, 1		// vpxor %ymm0, %ymm0, %ymm0
, 1		// vrcpps (%rsi), %xmm0
, 12		// vrcpps %xmm0, %xmm0
, 4		// vrcpps (%rsi), %ymm0
, 17		// vrcpps %ymm0, %ymm0
, 12		// vrcpss (%rsi), %xmm0, %xmm0
, 12		// vrcpss %xmm0, %xmm0, %xmm0
, 999		// vroundpd $0x0, es:(%rax,%rax,1), %xmm0
, 999		// vroundpd $0x0, %xmm0, %xmm0
, 999		// vroundpd $0x0, es:(%rax,%rax,1), %ymm0
, 999		// vroundpd $0x0, %ymm0, %ymm0
, 999		// vroundps $0x0, es:(%rax,%rax,1), %xmm0
, 999		// vroundps $0x0, %xmm0, %xmm0
, 999		// vroundps $0x0, es:(%rax,%rax,1), %ymm0
, 999		// vroundps $0x0, %ymm0, %ymm0
, 999		// vroundsd $0x0, es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vroundsd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vroundss $0x0, es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vroundss $0x0, %xmm0, %xmm0, %xmm0
, 1		// vrsqrtps (%rsi), %xmm0
, 12		// vrsqrtps %xmm0, %xmm0
, 4		// vrsqrtps (%rsi), %ymm0
, 17		// vrsqrtps %ymm0, %ymm0
, 12		// vrsqrtss (%rsi), %xmm0, %xmm0
, 12		// vrsqrtss %xmm0, %xmm0, %xmm0
, 1		// vshufpd $0x4, (%rsi), %xmm0, %xmm0
, 1		// vshufpd $0x4, %xmm0, %xmm0, %xmm0
, 1		// vshufpd $0x4, (%rsi), %ymm0, %ymm0
, 1		// vshufpd $0x4, %ymm0, %ymm0, %ymm0
, 1		// vshufps $0x4, (%rsi), %xmm0, %xmm0
, 1		// vshufps $0x4, %xmm0, %xmm0, %xmm0
, 1		// vshufps $0x4, (%rsi), %ymm0, %ymm0
, 1		// vshufps $0x4, %ymm0, %ymm0, %ymm0
, 999		// vsqrtpd es:(%rax,%rax,1), %xmm0
, 999		// vsqrtpd %xmm0, %xmm0
, 999		// vsqrtpd es:(%rax,%rax,1), %ymm0
, 999		// vsqrtpd %ymm0, %ymm0
, 999		// vsqrtps es:(%rax,%rax,1), %xmm0
, 999		// vsqrtps %xmm0, %xmm0
, 999		// vsqrtps es:(%rax,%rax,1), %ymm0
, 999		// vsqrtps %ymm0, %ymm0
, 999		// vsqrtsd es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vsqrtsd %xmm0, %xmm0, %xmm0
, 999		// vsqrtss es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vsqrtss %xmm0, %xmm0, %xmm0
, 1		// vstmxcsr (%rsi)
, 7		// vsubpd (%rsi), %xmm0, %xmm0
, 7		// vsubpd %xmm0, %xmm0, %xmm0
, 7		// vsubpd (%rsi), %ymm0, %ymm0
, 7		// vsubpd %ymm0, %ymm0, %ymm0
, 7		// vsubps (%rsi), %xmm0, %xmm0
, 7		// vsubps %xmm0, %xmm0, %xmm0
, 7		// vsubps (%rsi), %ymm0, %ymm0
, 7		// vsubps %ymm0, %ymm0, %ymm0
, 7		// vsubsd (%rsi), %xmm0, %xmm0
, 7		// vsubsd %xmm0, %xmm0, %xmm0
, 7		// vsubss (%rsi), %xmm0, %xmm0
, 7		// vsubss %xmm0, %xmm0, %xmm0
, 1		// vtestpd (%rsi), %xmm0
, 1		// vtestpd %xmm0, %xmm0
, 1		// vtestpd (%rsi), %ymm0
, 1		// vtestpd %ymm0, %ymm0
, 1		// vtestps (%rsi), %xmm0
, 1		// vtestps %xmm0, %xmm0
, 1		// vtestps (%rsi), %ymm0
, 1		// vtestps %ymm0, %ymm0
, 1		// vucomisd (%rsi), %xmm0
, 1		// vucomisd %xmm0, %xmm0
, 1		// vucomiss (%rsi), %xmm0
, 1		// vucomiss %xmm0, %xmm0
, 1		// vunpckhpd (%rsi), %xmm0, %xmm0
, 1		// vunpckhpd %xmm0, %xmm0, %xmm0
, 1		// vunpckhpd (%rsi), %ymm0, %ymm0
, 1		// vunpckhpd %ymm0, %ymm0, %ymm0
, 1		// vunpckhps (%rsi), %xmm0, %xmm0
, 1		// vunpckhps %xmm0, %xmm0, %xmm0
, 1		// vunpckhps (%rsi), %ymm0, %ymm0
, 1		// vunpckhps %ymm0, %ymm0, %ymm0
, 1		// vunpcklpd (%rsi), %xmm0, %xmm0
, 1		// vunpcklpd %xmm0, %xmm0, %xmm0
, 1		// vunpcklpd (%rsi), %ymm0, %ymm0
, 1		// vunpcklpd %ymm0, %ymm0, %ymm0
, 1		// vunpcklps (%rsi), %xmm0, %xmm0
, 1		// vunpcklps %xmm0, %xmm0, %xmm0
, 1		// vunpcklps (%rsi), %ymm0, %ymm0
, 1		// vunpcklps %ymm0, %ymm0, %ymm0
, 1		// vxorpd (%rsi), %xmm0, %xmm0
, 1		// vxorpd %xmm0, %xmm0, %xmm0
, 1		// vxorpd (%rsi), %ymm0, %ymm0
, 1		// vxorpd %ymm0, %ymm0, %ymm0
, 1		// vxorps (%rsi), %xmm0, %xmm0
, 1		// vxorps %xmm0, %xmm0, %xmm0
, 1		// vxorps (%rsi), %ymm0, %ymm0
, 1		// vxorps %ymm0, %ymm0, %ymm0
, 19		// vzeroall 
, 1		// vzeroupper 
, 1		// wait 
, 999		// wrfsbase %eax
, 999		// wrfsbase %rax
, 999		// wrgsbase %eax
, 999		// wrgsbase %rax
, 999		// xabort $0x0
, 999		// xacquire 
, 17		// xaddw %cx, (%rsi)
, 17		// xaddl %edx, (%rsi)
, 17		// xaddq %rdx, (%rsi)
, 17		// xaddb %dl, (%rsi)
, 17		// xaddb %ch, (%rsi)
, 4		// xaddw %cx, %cx
, 4		// xaddl %edx, %edx
, 4		// xaddq %rdx, %rdx
, 4		// xaddb %dl, %dl
, 4		// xaddb %ch, %dl
, 4		// xaddb %dl, %ch
, 4		// xaddb %ch, %ch
, 999		// xbegin 
, 999		// xbegin 0
, 3		// xchgw %cx, %ax
, 3		// xchgl %edx, %eax
, 48		// xchgw %cx, (%rsi)
, 48		// xchgl %edx, (%rsi)
, 48		// xchgq %rdx, (%rsi)
, 48		// xchgb %dl, (%rsi)
, 48		// xchgb %ch, (%rsi)
, 3		// xchgw %ax, %cx
, 48		// xchgw (%rsi), %cx
, 4		// xchgw %cx, %cx
, 4		// xchgw %cx, %cx
, 3		// xchgl %eax, %edx
, 48		// xchgl (%rsi), %edx
, 4		// xchgl %edx, %edx
, 4		// xchgl %edx, %edx
, 48		// xchgq (%rsi), %rdx
, 4		// xchgq %rdx, %rdx
, 4		// xchgq %rdx, %rdx
, 3		// xchgq %rax, %rdx
, 48		// xchgb (%rsi), %dl
, 4		// xchgb %dl, %dl
, 4		// xchgb %dl, %dl
, 3		// xchgb %ch, %dl
, 3		// xchgb %ch, %dl
, 3		// xchgq %rdx, %rax
, 48		// xchgb (%rsi), %ch
, 3		// xchgb %dl, %ch
, 3		// xchgb %dl, %ch
, 4		// xchgb %ch, %ch
, 4		// xchgb %ch, %ch
, 999		// xend 
, 999		// xgetbv 
, 999		// xlat es:(%rax,%rax,1)
, 999		// xlatb 
, 999		// xlatb 
, 1		// xorb $0x4, %al
, 5		// xorw $0x4050, %ax
, 1		// xorl $0xc0decafe, %eax
, 16		// xorw $0x4050, (%rsi)
, 17		// xorw $0x4, (%rsi)
, 14		// xorw %cx, (%rsi)
, 16		// xorl $0xc0decafe, (%rsi)
, 16		// xorl $0x4, (%rsi)
, 14		// xorl %edx, (%rsi)
, 16		// xorq $0xc0decafe, (%rsi)
, 15		// xorq $0x4, (%rsi)
, 15		// xorq %rdx, (%rsi)
, 16		// xorb $0x4, (%rsi)
, 14		// xorb %dl, (%rsi)
, 18		// xorb %ch, (%rsi)
, 4		// xorw $0x4050, %cx
, 1		// xorw $0x4, %cx
, 1		// xorw (%rsi), %cx
, 1		// xorw %cx, %cx
, 1		// xorw %cx, %cx
, 1		// xorl $0xc0decafe, %edx
, 1		// xorl $0x4, %edx
, 1		// xorl (%rsi), %edx
, 1		// xorl %edx, %edx
, 1		// xorl %edx, %edx
, 1		// xorq $0xc0decafe, %rdx
, 1		// xorq $0x4, %rdx
, 1		// xorq (%rsi), %rdx
, 1		// xorq %rdx, %rdx
, 1		// xorq %rdx, %rdx
, 1		// xorb $0x4, %dl
, 1		// xorb (%rsi), %dl
, 1		// xorb %dl, %dl
, 1		// xorb %dl, %dl
, 4		// xorb %ch, %dl
, 4		// xorb %ch, %dl
, 1		// xorq $0xc0decafe, %rax
, 1		// xorb $0x4, %ch
, 1		// xorb (%rsi), %ch
, 1		// xorb %dl, %ch
, 1		// xorb %dl, %ch
, 1		// xorb %ch, %ch
, 1		// xorb %ch, %ch
, 1		// xorpd (%rsi), %xmm0
, 1		// xorpd %xmm0, %xmm0
, 1		// xorps (%rsi), %xmm0
, 1		// xorps %xmm0, %xmm0
, 999		// xrelease 
, 999		// xrstor es:(%rax,%rax,1)
, 999		// xrstor es:(%rax,%rax,1)
, 999		// xrstor es:(%rax,%rax,1)
, 999		// xrstor64 es:(%rax,%rax,1)
, 999		// xrstor64 es:(%rax,%rax,1)
, 999		// xrstor64 es:(%rax,%rax,1)
, 999		// xsave es:(%rax,%rax,1)
, 999		// xsave es:(%rax,%rax,1)
, 999		// xsave es:(%rax,%rax,1)
, 999		// xsave64 es:(%rax,%rax,1)
, 999		// xsave64 es:(%rax,%rax,1)
, 999		// xsave64 es:(%rax,%rax,1)
, 999		// xsaveopt es:(%rax,%rax,1)
, 999		// xsaveopt es:(%rax,%rax,1)
, 999		// xsaveopt es:(%rax,%rax,1)
, 999		// xsaveopt64 es:(%rax,%rax,1)
, 999		// xsaveopt64 es:(%rax,%rax,1)
, 999		// xsaveopt64 es:(%rax,%rax,1)
, 999		// xtest 
