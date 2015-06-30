, 44		// adcb $0x4, %al
, 67		// adcw $0x4050, %ax
, 44		// adcl $0xc0decafe, %eax
, 184		// adcw $0x4050, (%rsi)
, 181		// adcw $0x4, (%rsi)
, 183		// adcw %cx, (%rsi)
, 183		// adcl $0xc0decafe, (%rsi)
, 182		// adcl $0x4, (%rsi)
, 187		// adcl %edx, (%rsi)
, 178		// adcq $0xc0decafe, (%rsi)
, 178		// adcq $0x4, (%rsi)
, 187		// adcq %rdx, (%rsi)
, 180		// adcb $0x4, (%rsi)
, 187		// adcb %dl, (%rsi)
, 201		// adcb %ch, (%rsi)
, 64		// adcw $0x4050, %cx
, 44		// adcw $0x4, %cx
, 44		// adcw (%rsi), %cx
, 44		// adcw %cx, %cx
, 44		// adcw %cx, %cx
, 44		// adcl $0xc0decafe, %edx
, 44		// adcl $0x4, %edx
, 44		// adcl (%rsi), %edx
, 44		// adcl %edx, %edx
, 44		// adcl %edx, %edx
, 44		// adcq $0xc0decafe, %rdx
, 44		// adcq $0x4, %rdx
, 44		// adcq (%rsi), %rdx
, 44		// adcq %rdx, %rdx
, 44		// adcq %rdx, %rdx
, 44		// adcb $0x4, %dl
, 44		// adcb (%rsi), %dl
, 44		// adcb %dl, %dl
, 44		// adcb %dl, %dl
, 69		// adcb %ch, %dl
, 69		// adcb %ch, %dl
, 44		// adcq $0xc0decafe, %rax
, 44		// adcb $0x4, %ch
, 44		// adcb (%rsi), %ch
, 44		// adcb %dl, %ch
, 44		// adcb %dl, %ch
, 44		// adcb %ch, %ch
, 44		// adcb %ch, %ch
, 18		// addb $0x4, %al
, 76		// addw $0x4050, %ax
, 18		// addl $0xc0decafe, %eax
, 165		// addw $0x4050, (%rsi)
, 163		// addw $0x4, (%rsi)
, 149		// addw %cx, (%rsi)
, 159		// addl $0xc0decafe, (%rsi)
, 168		// addl $0x4, (%rsi)
, 149		// addl %edx, (%rsi)
, 167		// addq $0xc0decafe, (%rsi)
, 168		// addq $0x4, (%rsi)
, 149		// addq %rdx, (%rsi)
, 170		// addb $0x4, (%rsi)
, 149		// addb %dl, (%rsi)
, 181		// addb %ch, (%rsi)
, 78		// addw $0x4050, %cx
, 18		// addw $0x4, %cx
, 18		// addw (%rsi), %cx
, 18		// addw %cx, %cx
, 18		// addw %cx, %cx
, 18		// addl $0xc0decafe, %edx
, 18		// addl $0x4, %edx
, 18		// addl (%rsi), %edx
, 18		// addl %edx, %edx
, 18		// addl %edx, %edx
, 18		// addq $0xc0decafe, %rdx
, 18		// addq $0x4, %rdx
, 18		// addq (%rsi), %rdx
, 18		// addq %rdx, %rdx
, 18		// addq %rdx, %rdx
, 18		// addb $0x4, %dl
, 18		// addb (%rsi), %dl
, 18		// addb %dl, %dl
, 18		// addb %dl, %dl
, 43		// addb %ch, %dl
, 43		// addb %ch, %dl
, 18		// addq $0xc0decafe, %rax
, 18		// addb $0x4, %ch
, 18		// addb (%rsi), %ch
, 18		// addb %dl, %ch
, 18		// addb %dl, %ch
, 18		// addb %ch, %ch
, 18		// addb %ch, %ch
, 70		// addpd (%rsi), %xmm0
, 70		// addpd %xmm0, %xmm0
, 70		// addps (%rsi), %xmm0
, 70		// addps %xmm0, %xmm0
, 70		// addsd (%rsi), %xmm0
, 70		// addsd %xmm0, %xmm0
, 70		// addss (%rsi), %xmm0
, 70		// addss %xmm0, %xmm0
, 70		// addsubpd (%rsi), %xmm0
, 70		// addsubpd %xmm0, %xmm0
, 70		// addsubps (%rsi), %xmm0
, 70		// addsubps %xmm0, %xmm0
, 172		// aesdec (%rsi), %xmm0
, 172		// aesdec %xmm0, %xmm0
, 172		// aesdeclast (%rsi), %xmm0
, 172		// aesdeclast %xmm0, %xmm0
, 173		// aesenc (%rsi), %xmm0
, 172		// aesenc %xmm0, %xmm0
, 172		// aesenclast (%rsi), %xmm0
, 172		// aesenclast %xmm0, %xmm0
, 44		// aesimc (%rsi), %xmm0
, 352		// aesimc %xmm0, %xmm0
, 194		// aeskeygenassist $0x4, (%rsi), %xmm0
, 249		// aeskeygenassist $0x4, %xmm0, %xmm0
, 18		// andb $0x4, %al
, 76		// andw $0x4050, %ax
, 18		// andl $0xc0decafe, %eax
, 166		// andw $0x4050, (%rsi)
, 169		// andw $0x4, (%rsi)
, 149		// andw %cx, (%rsi)
, 169		// andl $0xc0decafe, (%rsi)
, 166		// andl $0x4, (%rsi)
, 149		// andl %edx, (%rsi)
, 170		// andq $0xc0decafe, (%rsi)
, 173		// andq $0x4, (%rsi)
, 149		// andq %rdx, (%rsi)
, 168		// andb $0x4, (%rsi)
, 149		// andb %dl, (%rsi)
, 181		// andb %ch, (%rsi)
, 78		// andw $0x4050, %cx
, 18		// andw $0x4, %cx
, 18		// andw (%rsi), %cx
, 18		// andw %cx, %cx
, 18		// andw %cx, %cx
, 18		// andl $0xc0decafe, %edx
, 18		// andl $0x4, %edx
, 18		// andl (%rsi), %edx
, 18		// andl %edx, %edx
, 18		// andl %edx, %edx
, 18		// andq $0xc0decafe, %rdx
, 18		// andq $0x4, %rdx
, 18		// andq (%rsi), %rdx
, 18		// andq %rdx, %rdx
, 18		// andq %rdx, %rdx
, 18		// andb $0x4, %dl
, 18		// andb (%rsi), %dl
, 18		// andb %dl, %dl
, 18		// andb %dl, %dl
, 43		// andb %ch, %dl
, 43		// andb %ch, %dl
, 18		// andq $0xc0decafe, %rax
, 18		// andb $0x4, %ch
, 18		// andb (%rsi), %ch
, 18		// andb %dl, %ch
, 18		// andb %dl, %ch
, 18		// andb %ch, %ch
, 18		// andb %ch, %ch
, 18		// andnl (%rsi), %edx, %edx
, 18		// andnl %edx, %edx, %edx
, 18		// andnq (%rsi), %rdx, %rdx
, 18		// andnq %rdx, %rdx, %rdx
, 18		// andnpd (%rsi), %xmm0
, 18		// andnpd %xmm0, %xmm0
, 18		// andnps (%rsi), %xmm0
, 18		// andnps %xmm0, %xmm0
, 18		// andpd (%rsi), %xmm0
, 18		// andpd %xmm0, %xmm0
, 18		// andps (%rsi), %xmm0
, 18		// andps %xmm0, %xmm0
, 44		// bextrl %edx, (%rsi), %edx
, 44		// bextrl %edx, %edx, %edx
, 44		// bextrq %rdx, (%rsi), %rdx
, 44		// bextrq %rdx, %rdx, %rdx
, 18		// blendpd $0x4, (%rsi), %xmm0
, 18		// blendpd $0x4, %xmm0, %xmm0
, 18		// blendps $0x4, (%rsi), %xmm0
, 18		// blendps $0x4, %xmm0, %xmm0
, 44		// blendvpd %xmm0, (%rsi), %xmm0
, 44		// blendvpd %xmm0, %xmm0, %xmm0
, 44		// blendvps %xmm0, (%rsi), %xmm0
, 44		// blendvps %xmm0, %xmm0, %xmm0
, 6		// blsil (%rsi), %edx
, 18		// blsil %edx, %edx
, 6		// blsiq (%rsi), %rdx
, 18		// blsiq %rdx, %rdx
, 6		// blsmskl (%rsi), %edx
, 18		// blsmskl %edx, %edx
, 6		// blsmskq (%rsi), %rdx
, 18		// blsmskq %rdx, %rdx
, 6		// blsrl (%rsi), %edx
, 18		// blsrl %edx, %edx
, 6		// blsrq (%rsi), %rdx
, 18		// blsrq %rdx, %rdx
, 67		// bsfl (%rsi), %edx
, 67		// bsfl %edx, %edx
, 67		// bsfq (%rsi), %rdx
, 67		// bsfq %rdx, %rdx
, 67		// bsrw (%rsi), %cx
, 67		// bsrw %cx, %cx
, 67		// bsrl (%rsi), %edx
, 67		// bsrl %edx, %edx
, 67		// bsrq (%rsi), %rdx
, 67		// bsrq %rdx, %rdx
, 18		// bswap %edx
, 44		// bswap %rdx
, 15		// btw $0x4, (%rsi)
, 121		// btw %cx, (%rsi)
, 16		// btl $0x4, (%rsi)
, 999		// btl %eax, es:(%rax,%rax,1)
, 15		// btq $0x4, (%rsi)
, 999		// btq %rax, es:(%rax,%rax,1)
, 11		// btw $0x4, %cx
, 11		// btw %cx, %cx
, 11		// btl $0x4, %edx
, 6		// btl %edx, %edx
, 11		// btq $0x4, %rdx
, 11		// btq %rdx, %rdx
, 148		// btcw $0x4, (%rsi)
, 156		// btcw %cx, (%rsi)
, 148		// btcl $0x4, (%rsi)
, 999		// btcl %eax, es:(%rax,%rax,1)
, 148		// btcq $0x4, (%rsi)
, 999		// btcq %rax, es:(%rax,%rax,1)
, 18		// btcw $0x4, %cx
, 18		// btcw %cx, %cx
, 18		// btcl $0x4, %edx
, 18		// btcl %edx, %edx
, 18		// btcq $0x4, %rdx
, 18		// btcq %rdx, %rdx
, 148		// btrw $0x4, (%rsi)
, 156		// btrw %cx, (%rsi)
, 148		// btrl $0x4, (%rsi)
, 999		// btrl %eax, es:(%rax,%rax,1)
, 148		// btrq $0x4, (%rsi)
, 999		// btrq %rax, es:(%rax,%rax,1)
, 18		// btrw $0x4, %cx
, 18		// btrw %cx, %cx
, 18		// btrl $0x4, %edx
, 18		// btrl %edx, %edx
, 18		// btrq $0x4, %rdx
, 18		// btrq %rdx, %rdx
, 148		// btsw $0x4, (%rsi)
, 156		// btsw %cx, (%rsi)
, 148		// btsl $0x4, (%rsi)
, 999		// btsl %eax, es:(%rax,%rax,1)
, 148		// btsq $0x4, (%rsi)
, 999		// btsq %rax, es:(%rax,%rax,1)
, 18		// btsw $0x4, %cx
, 18		// btsw %cx, %cx
, 18		// btsl $0x4, %edx
, 18		// btsl %edx, %edx
, 18		// btsq $0x4, %rdx
, 18		// btsq %rdx, %rdx
, 18		// bzhil %edx, (%rsi), %edx
, 18		// bzhil %edx, %edx, %edx
, 18		// bzhiq %rdx, (%rsi), %rdx
, 18		// bzhiq %rdx, %rdx, %rdx
, 0		// callq es:(%rax,%rax,1)
, 0		// callq es:(%rax,%rax,1)
, 0		// callq es:(%rax,%rax,1)
, 0		// callq es:(%rax,%rax,1)
, 0		// callq %rax
, 0		// callq 0
, 0		// callq 
, 18		// cbtw 
, 19		// cltd 
, 18		// cltq 
, 1		// clc 
, 96		// cld 
, 1687		// clflush (%rsi)
, 999		// cli 
, 18		// cmc 
, 19		// cmovaw (%rsi), %cx
, 44		// cmovaw %cx, %cx
, 19		// cmoval (%rsi), %edx
, 44		// cmoval %edx, %edx
, 19		// cmovaq (%rsi), %rdx
, 44		// cmovaq %rdx, %rdx
, 18		// cmovaew (%rsi), %cx
, 44		// cmovaew %cx, %cx
, 19		// cmovael (%rsi), %edx
, 44		// cmovael %edx, %edx
, 18		// cmovaeq (%rsi), %rdx
, 44		// cmovaeq %rdx, %rdx
, 18		// cmovbw (%rsi), %cx
, 44		// cmovbw %cx, %cx
, 19		// cmovbl (%rsi), %edx
, 44		// cmovbl %edx, %edx
, 18		// cmovbq (%rsi), %rdx
, 44		// cmovbq %rdx, %rdx
, 19		// cmovbew (%rsi), %cx
, 44		// cmovbew %cx, %cx
, 19		// cmovbel (%rsi), %edx
, 44		// cmovbel %edx, %edx
, 19		// cmovbeq (%rsi), %rdx
, 44		// cmovbeq %rdx, %rdx
, 18		// cmovcw (%rsi), %cx
, 44		// cmovcw %cx, %cx
, 19		// cmovcl (%rsi), %edx
, 44		// cmovcl %edx, %edx
, 18		// cmovcq (%rsi), %rdx
, 44		// cmovcq %rdx, %rdx
, 18		// cmovew (%rsi), %cx
, 44		// cmovew %cx, %cx
, 19		// cmovel (%rsi), %edx
, 44		// cmovel %edx, %edx
, 18		// cmoveq (%rsi), %rdx
, 44		// cmoveq %rdx, %rdx
, 18		// cmovgw (%rsi), %cx
, 44		// cmovgw %cx, %cx
, 19		// cmovgl (%rsi), %edx
, 44		// cmovgl %edx, %edx
, 18		// cmovgq (%rsi), %rdx
, 44		// cmovgq %rdx, %rdx
, 18		// cmovgew (%rsi), %cx
, 44		// cmovgew %cx, %cx
, 19		// cmovgel (%rsi), %edx
, 44		// cmovgel %edx, %edx
, 18		// cmovgeq (%rsi), %rdx
, 44		// cmovgeq %rdx, %rdx
, 18		// cmovlw (%rsi), %cx
, 44		// cmovlw %cx, %cx
, 19		// cmovll (%rsi), %edx
, 44		// cmovll %edx, %edx
, 18		// cmovlq (%rsi), %rdx
, 44		// cmovlq %rdx, %rdx
, 18		// cmovlew (%rsi), %cx
, 44		// cmovlew %cx, %cx
, 19		// cmovlel (%rsi), %edx
, 44		// cmovlel %edx, %edx
, 18		// cmovleq (%rsi), %rdx
, 44		// cmovleq %rdx, %rdx
, 19		// cmovnaw (%rsi), %cx
, 44		// cmovnaw %cx, %cx
, 19		// cmovnal (%rsi), %edx
, 44		// cmovnal %edx, %edx
, 19		// cmovnaq (%rsi), %rdx
, 44		// cmovnaq %rdx, %rdx
, 18		// cmovnaew (%rsi), %cx
, 44		// cmovnaew %cx, %cx
, 19		// cmovnael (%rsi), %edx
, 44		// cmovnael %edx, %edx
, 18		// cmovnaeq (%rsi), %rdx
, 44		// cmovnaeq %rdx, %rdx
, 18		// cmovnbw (%rsi), %cx
, 44		// cmovnbw %cx, %cx
, 19		// cmovnbl (%rsi), %edx
, 44		// cmovnbl %edx, %edx
, 18		// cmovnbq (%rsi), %rdx
, 44		// cmovnbq %rdx, %rdx
, 19		// cmovnbew (%rsi), %cx
, 44		// cmovnbew %cx, %cx
, 19		// cmovnbel (%rsi), %edx
, 44		// cmovnbel %edx, %edx
, 19		// cmovnbeq (%rsi), %rdx
, 44		// cmovnbeq %rdx, %rdx
, 18		// cmovncw (%rsi), %cx
, 44		// cmovncw %cx, %cx
, 19		// cmovncl (%rsi), %edx
, 44		// cmovncl %edx, %edx
, 18		// cmovncq (%rsi), %rdx
, 44		// cmovncq %rdx, %rdx
, 18		// cmovnew (%rsi), %cx
, 44		// cmovnew %cx, %cx
, 19		// cmovnel (%rsi), %edx
, 44		// cmovnel %edx, %edx
, 18		// cmovneq (%rsi), %rdx
, 44		// cmovneq %rdx, %rdx
, 18		// cmovngw (%rsi), %cx
, 44		// cmovngw %cx, %cx
, 19		// cmovngl (%rsi), %edx
, 44		// cmovngl %edx, %edx
, 18		// cmovngq (%rsi), %rdx
, 44		// cmovngq %rdx, %rdx
, 18		// cmovngew (%rsi), %cx
, 44		// cmovngew %cx, %cx
, 19		// cmovngel (%rsi), %edx
, 44		// cmovngel %edx, %edx
, 18		// cmovngeq (%rsi), %rdx
, 44		// cmovngeq %rdx, %rdx
, 18		// cmovnlw (%rsi), %cx
, 44		// cmovnlw %cx, %cx
, 19		// cmovnll (%rsi), %edx
, 44		// cmovnll %edx, %edx
, 18		// cmovnlq (%rsi), %rdx
, 44		// cmovnlq %rdx, %rdx
, 18		// cmovnlew (%rsi), %cx
, 44		// cmovnlew %cx, %cx
, 19		// cmovnlel (%rsi), %edx
, 44		// cmovnlel %edx, %edx
, 18		// cmovnleq (%rsi), %rdx
, 44		// cmovnleq %rdx, %rdx
, 18		// cmovnow (%rsi), %cx
, 44		// cmovnow %cx, %cx
, 19		// cmovnol (%rsi), %edx
, 44		// cmovnol %edx, %edx
, 18		// cmovnoq (%rsi), %rdx
, 44		// cmovnoq %rdx, %rdx
, 18		// cmovnpw (%rsi), %cx
, 44		// cmovnpw %cx, %cx
, 19		// cmovnpl (%rsi), %edx
, 44		// cmovnpl %edx, %edx
, 18		// cmovnpq (%rsi), %rdx
, 44		// cmovnpq %rdx, %rdx
, 18		// cmovnsw (%rsi), %cx
, 44		// cmovnsw %cx, %cx
, 19		// cmovnsl (%rsi), %edx
, 44		// cmovnsl %edx, %edx
, 18		// cmovnsq (%rsi), %rdx
, 44		// cmovnsq %rdx, %rdx
, 18		// cmovnzw (%rsi), %cx
, 44		// cmovnzw %cx, %cx
, 19		// cmovnzl (%rsi), %edx
, 44		// cmovnzl %edx, %edx
, 18		// cmovnzq (%rsi), %rdx
, 44		// cmovnzq %rdx, %rdx
, 18		// cmovow (%rsi), %cx
, 44		// cmovow %cx, %cx
, 19		// cmovol (%rsi), %edx
, 44		// cmovol %edx, %edx
, 18		// cmovoq (%rsi), %rdx
, 44		// cmovoq %rdx, %rdx
, 18		// cmovpw (%rsi), %cx
, 44		// cmovpw %cx, %cx
, 19		// cmovpl (%rsi), %edx
, 44		// cmovpl %edx, %edx
, 18		// cmovpq (%rsi), %rdx
, 44		// cmovpq %rdx, %rdx
, 18		// cmovpew (%rsi), %cx
, 44		// cmovpew %cx, %cx
, 19		// cmovpel (%rsi), %edx
, 44		// cmovpel %edx, %edx
, 18		// cmovpeq (%rsi), %rdx
, 44		// cmovpeq %rdx, %rdx
, 18		// cmovpow (%rsi), %cx
, 44		// cmovpow %cx, %cx
, 19		// cmovpol (%rsi), %edx
, 44		// cmovpol %edx, %edx
, 18		// cmovpoq (%rsi), %rdx
, 44		// cmovpoq %rdx, %rdx
, 18		// cmovsw (%rsi), %cx
, 44		// cmovsw %cx, %cx
, 19		// cmovsl (%rsi), %edx
, 44		// cmovsl %edx, %edx
, 18		// cmovsq (%rsi), %rdx
, 44		// cmovsq %rdx, %rdx
, 18		// cmovzw (%rsi), %cx
, 44		// cmovzw %cx, %cx
, 19		// cmovzl (%rsi), %edx
, 44		// cmovzl %edx, %edx
, 18		// cmovzq (%rsi), %rdx
, 44		// cmovzq %rdx, %rdx
, 1		// cmpb $0x4, %al
, 76		// cmpw $0x4050, %ax
, 0		// cmpl $0xc0decafe, %eax
, 40		// cmpw $0x4050, (%rsi)
, 6		// cmpw $0x4, (%rsi)
, 6		// cmpw %cx, (%rsi)
, 9		// cmpl $0xc0decafe, (%rsi)
, 6		// cmpl $0x4, (%rsi)
, 6		// cmpl %edx, (%rsi)
, 9		// cmpq $0xc0decafe, (%rsi)
, 6		// cmpq $0x4, (%rsi)
, 6		// cmpq %rdx, (%rsi)
, 6		// cmpb $0x4, (%rsi)
, 6		// cmpb %dl, (%rsi)
, 6		// cmpb %ch, (%rsi)
, 78		// cmpw $0x4050, %cx
, 0		// cmpw $0x4, %cx
, 6		// cmpw (%rsi), %cx
, 1		// cmpw %cx, %cx
, 1		// cmpw %cx, %cx
, 0		// cmpl $0xc0decafe, %edx
, 0		// cmpl $0x4, %edx
, 6		// cmpl (%rsi), %edx
, 1		// cmpl %edx, %edx
, 1		// cmpl %edx, %edx
, 2		// cmpq $0xc0decafe, %rdx
, 0		// cmpq $0x4, %rdx
, 6		// cmpq (%rsi), %rdx
, 1		// cmpq %rdx, %rdx
, 1		// cmpq %rdx, %rdx
, 0		// cmpb $0x4, %dl
, 6		// cmpb (%rsi), %dl
, 1		// cmpb %dl, %dl
, 1		// cmpb %dl, %dl
, 0		// cmpb %ch, %dl
, 0		// cmpb %ch, %dl
, 1		// cmpq $0xc0decafe, %rax
, 1		// cmpb $0x4, %ch
, 6		// cmpb (%rsi), %ch
, 0		// cmpb %dl, %ch
, 0		// cmpb %dl, %ch
, 1		// cmpb %ch, %ch
, 1		// cmpb %ch, %ch
, 70		// cmppd $0x4, (%rsi), %xmm0
, 70		// cmppd $0x4, %xmm0, %xmm0
, 70		// cmpps $0x4, (%rsi), %xmm0
, 70		// cmpps $0x4, %xmm0, %xmm0
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmps es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// cmpsb 
, 999		// cmpsl 
, 70		// cmpsd $0x4, (%rsi), %xmm0
, 70		// cmpsd $0x4, %xmm0, %xmm0
, 999		// cmpsq 
, 70		// cmpss $0x4, (%rsi), %xmm0
, 70		// cmpss $0x4, %xmm0, %xmm0
, 999		// cmpsw 
, 194		// cmpxchgw %cx, (%rsi)
, 194		// cmpxchgl %edx, (%rsi)
, 195		// cmpxchgq %rdx, (%rsi)
, 194		// cmpxchgb %dl, (%rsi)
, 198		// cmpxchgb %ch, (%rsi)
, 121		// cmpxchgw %cx, %cx
, 121		// cmpxchgl %edx, %edx
, 121		// cmpxchgq %rdx, %rdx
, 121		// cmpxchgb %dl, %dl
, 121		// cmpxchgb %ch, %dl
, 121		// cmpxchgb %dl, %ch
, 121		// cmpxchgb %ch, %ch
, 384		// cmpxchg16b (%rsi)
, 217		// cmpxchg8b (%rsi)
, 18		// comisd (%rsi), %xmm0
, 18		// comisd %xmm0, %xmm0
, 19		// comiss (%rsi), %xmm0
, 18		// comiss %xmm0, %xmm0
, 5170		// cpuid 
, 6		// cqto 
, 67		// crc32w (%rsi), %edx
, 67		// crc32l (%rsi), %edx
, 67		// crc32b (%rsi), %edx
, 67		// crc32w %cx, %edx
, 67		// crc32l %edx, %edx
, 67		// crc32b %dl, %edx
, 92		// crc32b %ch, %edx
, 67		// crc32q (%rsi), %rdx
, 67		// crc32b (%rsi), %rdx
, 67		// crc32q %rdx, %rdx
, 67		// crc32b %dl, %rdx
, 18		// cvtdq2pd (%rsi), %xmm0
, 95		// cvtdq2pd %xmm0, %xmm0
, 18		// cvtdq2ps (%rsi), %xmm0
, 70		// cvtdq2ps %xmm0, %xmm0
, 19		// cvtpd2dq (%rsi), %xmm0
, 95		// cvtpd2dq %xmm0, %xmm0
, 19		// cvtpd2pi (%rsi), %mm0
, 18		// cvtpd2pi %xmm0, %mm0
, 19		// cvtpd2ps (%rsi), %xmm0
, 95		// cvtpd2ps %xmm0, %xmm0
, 18		// cvtpi2pd (%rsi), %xmm0
, 18		// cvtpi2pd %mm0, %xmm0
, 70		// cvtpi2ps (%rsi), %xmm0
, 95		// cvtpi2ps %mm0, %xmm0
, 18		// cvtps2dq (%rsi), %xmm0
, 70		// cvtps2dq %xmm0, %xmm0
, 19		// cvtps2pd (%rsi), %xmm0
, 44		// cvtps2pd %xmm0, %xmm0
, 19		// cvtps2pi (%rsi), %mm0
, 19		// cvtps2pi %xmm0, %mm0
, 19		// cvtsd2si (%rsi), %edx
, 18		// cvtsd2si %xmm0, %edx
, 19		// cvtsd2si (%rsi), %rdx
, 18		// cvtsd2si %xmm0, %rdx
, 19		// cvtsd2ss (%rsi), %xmm0
, 95		// cvtsd2ss %xmm0, %xmm0
, 70		// cvtsi2sdl (%rsi), %xmm0
, 70		// cvtsi2sdq (%rsi), %xmm0
, 70		// cvtsi2sdl %edx, %xmm0
, 70		// cvtsi2sdq %rdx, %xmm0
, 70		// cvtsi2ssl (%rsi), %xmm0
, 19		// cvtsi2ssq (%rsi), %xmm0
, 70		// cvtsi2ssl %edx, %xmm0
, 44		// cvtsi2ssq %rdx, %xmm0
, 18		// cvtss2sd (%rsi), %xmm0
, 44		// cvtss2sd %xmm0, %xmm0
, 19		// cvtss2sil (%rsi), %edx
, 18		// cvtss2sil %xmm0, %edx
, 19		// cvtss2siq (%rsi), %rdx
, 18		// cvtss2siq %xmm0, %rdx
, 19		// cvttpd2dq (%rsi), %xmm0
, 95		// cvttpd2dq %xmm0, %xmm0
, 19		// cvttpd2pi (%rsi), %mm0
, 18		// cvttpd2pi %xmm0, %mm0
, 18		// cvttps2dq (%rsi), %xmm0
, 70		// cvttps2dq %xmm0, %xmm0
, 19		// cvttps2pi (%rsi), %mm0
, 19		// cvttps2pi %xmm0, %mm0
, 19		// cvttsd2si (%rsi), %edx
, 18		// cvttsd2si %xmm0, %edx
, 19		// cvttsd2siq (%rsi), %rdx
, 18		// cvttsd2siq %xmm0, %rdx
, 19		// cvttss2si (%rsi), %edx
, 18		// cvttss2si %xmm0, %edx
, 19		// cvttss2siq (%rsi), %rdx
, 18		// cvttss2siq %xmm0, %rdx
, 19		// cwtd 
, 19		// cwtl 
, 148		// decw (%rsi)
, 148		// decl (%rsi)
, 148		// decq (%rsi)
, 149		// decb (%rsi)
, 18		// decw %cx
, 18		// decl %edx
, 18		// decq %rdx
, 18		// decb %dl
, 18		// decb %ch
, 999		// divw es:(%rax,%rax,1)
, 999		// divl es:(%rax,%rax,1)
, 999		// divq es:(%rax,%rax,1)
, 999		// divb es:(%rax,%rax,1)
, 999		// divw %ax
, 999		// divl %eax
, 999		// divq %rax
, 999		// divb %al
, 999		// divb  
, 249		// divpd (%rsi), %xmm0
, 250		// divpd %xmm0, %xmm0
, 249		// divps (%rsi), %xmm0
, 250		// divps %xmm0, %xmm0
, 249		// divsd (%rsi), %xmm0
, 250		// divsd %xmm0, %xmm0
, 249		// divss (%rsi), %xmm0
, 250		// divss %xmm0, %xmm0
, 224		// dppd $0x4, (%rsi), %xmm0
, 224		// dppd $0x4, %xmm0, %xmm0
, 352		// dpps $0x4, (%rsi), %xmm0
, 352		// dpps $0x4, %xmm0, %xmm0
, 327		// emms 
, 0		// enterq $0x0, $0x0
, 0		// enterq $0x0, $0x0
, 0		// enterq $0x0, $0x0
, 19		// extractps $0x4, %xmm0, (%rsi)
, 18		// extractps $0x4, %xmm0, %edx
, 18		// extractps $0x4, %xmm0, %rdx
, 8768		// f2xm1 
, 18		// fabs 
, 11340		// fadds (%rsi)
, 11340		// faddl (%rsi)
, 999		// fadd %st, %st
, 999		// fadd %st, %st
, 8000		// faddp 
, 999		// faddp %st, %st
, 9650		// fbld (%rsi)
, 9062		// fbstp (%rsi)
, 18		// fchs 
, 552		// fclex 
, 999		// fcmovb %st, %st
, 999		// fcmovbe %st, %st
, 999		// fcmove %st, %st
, 999		// fcmovnb %st, %st
, 999		// fcmovnbe %st, %st
, 999		// fcmovne %st, %st
, 999		// fcmovnu %st, %st
, 999		// fcmovu %st, %st
, 7372		// fcom 
, 10323		// fcoms (%rsi)
, 10323		// fcoml (%rsi)
, 999		// fcom %st
, 999		// fcomi %st, %st
, 999		// fcomip %st, %st
, 7838		// fcomp 
, 10832		// fcomps (%rsi)
, 10831		// fcompl (%rsi)
, 999		// fcomp %st
, 8253		// fcompp 
, 8767		// fcos 
, 5		// fdecstp 
, 11703		// fdivs (%rsi)
, 11704		// fdivl (%rsi)
, 999		// fdiv %st, %st
, 999		// fdiv %st, %st
, 8177		// fdivp 
, 999		// fdivp %st, %st
, 9000		// fdivrs (%rsi)
, 9001		// fdivrl (%rsi)
, 999		// fdivr %st, %st
, 999		// fdivr %st, %st
, 8178		// fdivrp 
, 999		// fdivrp %st, %st
, 999		// ffreep %st
, 10424		// fiadd (%rsi)
, 10424		// fiaddl (%rsi)
, 9890		// ficom (%rsi)
, 9889		// ficoml (%rsi)
, 10400		// ficomp (%rsi)
, 10399		// ficompl (%rsi)
, 10604		// fidiv (%rsi)
, 10603		// fidivl (%rsi)
, 10604		// fidivr (%rsi)
, 10604		// fidivrl (%rsi)
, 11044		// fild (%rsi)
, 11044		// fildl (%rsi)
, 11044		// fildll (%rsi)
, 10476		// fimul (%rsi)
, 10475		// fimull (%rsi)
, 5		// fincstp 
, 2149		// finit 
, 8512		// fist (%rsi)
, 8494		// fistl (%rsi)
, 8534		// fistp (%rsi)
, 8521		// fistpl (%rsi)
, 8124		// fistpll (%rsi)
, 8135		// fisttp (%rsi)
, 8175		// fisttpl (%rsi)
, 8175		// fisttpll (%rsi)
, 10423		// fisub (%rsi)
, 10423		// fisubl (%rsi)
, 10423		// fisubr (%rsi)
, 10422		// fisubrl (%rsi)
, 8887		// flds (%rsi)
, 8887		// fldl (%rsi)
, 8573		// fldt (%rsi)
, 999		// fld %st
, 8445		// fld1 
, 999		// fldcw es:(%rax,%rax,1)
, 3175		// fldenvl (%rsi)
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
, 545		// fnclex 
, 2137		// fninit 
, 5		// fnop 
, 4280		// fnsave (%rsi)
, 19		// fnstcw (%rsi)
, 1739		// fnstenvl (%rsi)
, 21		// fnstsw %ax
, 19		// fnstsw (%rsi)
, 8367		// fpatan 
, 8339		// fprem 
, 8365		// fprem1 
, 8495		// fptan 
, 8742		// frndint 
, 4202		// frstor (%rsi)
, 4228		// fsave (%rsi)
, 7930		// fscale 
, 8778		// fsin 
, 8470		// fsincos 
, 8472		// fsqrt 
, 19		// fsts (%rsi)
, 19		// fstl (%rsi)
, 999		// fst %st
, 44		// fstcw (%rsi)
, 1815		// fstenvl (%rsi)
, 11101		// fstps (%rsi)
, 11058		// fstpl (%rsi)
, 8969		// fstpt (%rsi)
, 999		// fstp %st
, 44		// fstsw %ax
, 44		// fstsw (%rsi)
, 12270		// fsubs (%rsi)
, 11340		// fsubl (%rsi)
, 999		// fsub %st, %st
, 999		// fsub %st, %st
, 7999		// fsubp 
, 999		// fsubp %st, %st
, 12275		// fsubrs (%rsi)
, 11340		// fsubrl (%rsi)
, 999		// fsubr %st, %st
, 999		// fsubr %st, %st
, 8000		// fsubrp 
, 999		// fsubrp %st, %st
, 7326		// ftst 
, 7369		// fucom 
, 999		// fucom %st
, 999		// fucomi %st, %st
, 999		// fucomip %st, %st
, 7835		// fucomp 
, 999		// fucomp %st
, 8253		// fucompp 
, 19		// fwait 
, 6342		// fxam 
, 6		// fxch 
, 999		// fxch %st
, 999		// fxrstor es:(%rax,%rax,1)
, 999		// fxrstor64 es:(%rax,%rax,1)
, 1693		// fxsave (%rsi)
, 1693		// fxsave64 (%rsi)
, 8368		// fxtract 
, 8393		// fyl2x 
, 8445		// fyl2xp1 
, 121		// haddpd (%rsi), %xmm0
, 121		// haddpd %xmm0, %xmm0
, 121		// haddps (%rsi), %xmm0
, 121		// haddps %xmm0, %xmm0
, 121		// hsubpd (%rsi), %xmm0
, 121		// hsubpd %xmm0, %xmm0
, 121		// hsubps (%rsi), %xmm0
, 121		// hsubps %xmm0, %xmm0
, 999		// idivw es:(%rax,%rax,1)
, 999		// idivl es:(%rax,%rax,1)
, 999		// idivq es:(%rax,%rax,1)
, 999		// idivb es:(%rax,%rax,1)
, 999		// idivw %ax
, 999		// idivl %eax
, 999		// idivq %rax
, 999		// idivb %al
, 999		// idivb  
, 96		// imulw (%rsi)
, 100		// imull (%rsi)
, 69		// imulq (%rsi)
, 67		// imulb (%rsi)
, 96		// imulw %cx
, 67		// imulw (%rsi), %cx
, 58		// imulw $0x4050, (%rsi), %cx
, 21		// imulw $0x4, (%rsi), %cx
, 67		// imulw %cx, %cx
, 94		// imulw $0x4050, %cx, %cx
, 94		// imulw $0x4, %cx, %cx
, 102		// imull %edx
, 67		// imull (%rsi), %edx
, 18		// imull $0xc0decafe, (%rsi), %edx
, 18		// imull $0x4, (%rsi), %edx
, 67		// imull %edx, %edx
, 67		// imull $0xc0decafe, %edx, %edx
, 67		// imull $0x4, %edx, %edx
, 94		// imulq %rdx
, 67		// imulq (%rsi), %rdx
, 18		// imulq $0xc0decafe, (%rsi), %rdx
, 18		// imulq $0x4, (%rsi), %rdx
, 67		// imulq %rdx, %rdx
, 67		// imulq $0xc0decafe, %rdx, %rdx
, 67		// imulq $0x4, %rdx, %rdx
, 67		// imulb %dl
, 92		// imulb %ch
, 999		// inb %ax, %al
, 999		// inb $0x0, %al
, 999		// inw %ax, %ax
, 999		// inw $0x0, %ax
, 999		// inl %ax, %eax
, 999		// inl $0x0, %eax
, 148		// incw (%rsi)
, 148		// incl (%rsi)
, 148		// incq (%rsi)
, 148		// incb (%rsi)
, 18		// incw %cx
, 18		// incl %edx
, 18		// incq %rdx
, 18		// incb %dl
, 18		// incb %ch
, 999		// ins %ax, es:(%rax,%rax,1)
, 999		// ins %ax, es:(%rax,%rax,1)
, 999		// ins %ax, es:(%rax,%rax,1)
, 999		// insb 
, 999		// insl 
, 18		// insertps $0x4, (%rsi), %xmm0
, 18		// insertps $0x4, %xmm0, %xmm0
, 999		// insw 
, 999		// int $0x0
, 999		// int $0x0
, 999		// invpcid es:(%rax,%rax,1), %rax
, 0		// iretw 
, 0		// iretl 
, 0		// iretq 
, 0		// ja 0
, 0		// ja 
, 0		// ja , 0
, 0		// ja , 
, 0		// ja 0
, 0		// ja , 0
, 0		// jae 0
, 0		// jae 
, 0		// jae , 0
, 0		// jae , 
, 0		// jae 0
, 0		// jae , 0
, 0		// jb 0
, 0		// jb 
, 0		// jb , 0
, 0		// jb , 
, 0		// jb 0
, 0		// jb , 0
, 0		// jbe 0
, 0		// jbe 
, 0		// jbe , 0
, 0		// jbe , 
, 0		// jbe 0
, 0		// jbe , 0
, 0		// jc 0
, 0		// jc 
, 0		// jc , 0
, 0		// jc , 
, 0		// jc 0
, 0		// jc , 0
, 0		// je 0
, 0		// je 
, 0		// je , 0
, 0		// je , 
, 0		// je 0
, 0		// je , 0
, 0		// jecxz 0
, 0		// jecxz , 0
, 0		// jg 0
, 0		// jg 
, 0		// jg , 0
, 0		// jg , 
, 0		// jg 0
, 0		// jg , 0
, 0		// jge 0
, 0		// jge 
, 0		// jge , 0
, 0		// jge , 
, 0		// jge 0
, 0		// jge , 0
, 0		// jl 0
, 0		// jl 
, 0		// jl , 0
, 0		// jl , 
, 0		// jl 0
, 0		// jl , 0
, 0		// jle 0
, 0		// jle 
, 0		// jle , 0
, 0		// jle , 
, 0		// jle 0
, 0		// jle , 0
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq es:(%rax,%rax,1)
, 0		// jmpq %rax
, 0		// jmpq 0
, 0		// jmpq 
, 0		// jmpq 0
, 0		// jna 0
, 0		// jna 
, 0		// jna , 0
, 0		// jna , 
, 0		// jna 0
, 0		// jna , 0
, 0		// jnae 0
, 0		// jnae 
, 0		// jnae , 0
, 0		// jnae , 
, 0		// jnae 0
, 0		// jnae , 0
, 0		// jnb 0
, 0		// jnb 
, 0		// jnb , 0
, 0		// jnb , 
, 0		// jnb 0
, 0		// jnb , 0
, 0		// jnbe 0
, 0		// jnbe 
, 0		// jnbe , 0
, 0		// jnbe , 
, 0		// jnbe 0
, 0		// jnbe , 0
, 0		// jnc 0
, 0		// jnc 
, 0		// jnc , 0
, 0		// jnc , 
, 0		// jnc 0
, 0		// jnc , 0
, 0		// jne 0
, 0		// jne 
, 0		// jne , 0
, 0		// jne , 
, 0		// jne 0
, 0		// jne , 0
, 0		// jng 0
, 0		// jng 
, 0		// jng , 0
, 0		// jng , 
, 0		// jng 0
, 0		// jng , 0
, 0		// jnge 0
, 0		// jnge 
, 0		// jnge , 0
, 0		// jnge , 
, 0		// jnge 0
, 0		// jnge , 0
, 0		// jnl 0
, 0		// jnl 
, 0		// jnl , 0
, 0		// jnl , 
, 0		// jnl 0
, 0		// jnl , 0
, 0		// jnle 0
, 0		// jnle 
, 0		// jnle , 0
, 0		// jnle , 
, 0		// jnle 0
, 0		// jnle , 0
, 0		// jno 0
, 0		// jno 
, 0		// jno , 0
, 0		// jno , 
, 0		// jno 0
, 0		// jno , 0
, 0		// jnp 0
, 0		// jnp 
, 0		// jnp , 0
, 0		// jnp , 
, 0		// jnp 0
, 0		// jnp , 0
, 0		// jns 0
, 0		// jns 
, 0		// jns , 0
, 0		// jns , 
, 0		// jns 0
, 0		// jns , 0
, 0		// jnz 0
, 0		// jnz 
, 0		// jnz , 0
, 0		// jnz , 
, 0		// jnz 0
, 0		// jnz , 0
, 0		// jo 0
, 0		// jo 
, 0		// jo , 0
, 0		// jo , 
, 0		// jo 0
, 0		// jo , 0
, 0		// jp 0
, 0		// jp 
, 0		// jp , 0
, 0		// jp , 
, 0		// jp 0
, 0		// jp , 0
, 0		// jpe 0
, 0		// jpe 
, 0		// jpe , 0
, 0		// jpe , 
, 0		// jpe 0
, 0		// jpe , 0
, 0		// jpo 0
, 0		// jpo 
, 0		// jpo , 0
, 0		// jpo , 
, 0		// jpo 0
, 0		// jpo , 0
, 0		// jrcxz 0
, 0		// jrcxz , 0
, 0		// js 0
, 0		// js 
, 0		// js , 0
, 0		// js , 
, 0		// js 0
, 0		// js , 0
, 0		// jz 0
, 0		// jz 
, 0		// jz , 0
, 0		// jz , 
, 0		// jz 0
, 0		// jz , 0
, 19		// lahf 
, 3683		// larw (%rsi), %cx
, 3606		// larw %cx, %cx
, 3683		// lar (%rsi), %edx
, 3609		// lar %edx, %edx
, 3690		// lar (%rsi), %rdx
, 3606		// lar %edx, %rdx
, 6		// lddqu (%rsi), %xmm0
, 999		// ldmxcsr es:(%rax,%rax,1)
, 19		// leaw (%rsi), %cx
, 19		// leaw (%rsi), %cx
, 19		// leaw (%rsi), %cx
, 6		// leal (%rsi), %edx
, 6		// leal (%rsi), %edx
, 6		// leal (%rsi), %edx
, 6		// leaq (%rsi), %rdx
, 6		// leaq (%rsi), %rdx
, 6		// leaq (%rsi), %rdx
, 0		// leaveq 
, 0		// leavew 
, 109		// lfence 
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
, 0		// loop 0
, 0		// loope 0
, 0		// loopne 0
, 3657		// lslw (%rsi), %cx
, 3606		// lslw %cx, %cx
, 3658		// lsl (%rsi), %edx
, 3610		// lsl %edx, %edx
, 3664		// lsl (%rsi), %rdx
, 3624		// lsl %edx, %rdx
, 999		// lssw es:(%rax,%rax,1), %ax
, 999		// lssl es:(%rax,%rax,1), %eax
, 999		// lss es:(%rax,%rax,1), %rax
, 68		// lzcntw (%rsi), %cx
, 67		// lzcntw %cx, %cx
, 68		// lzcntl (%rsi), %edx
, 67		// lzcntl %edx, %edx
, 67		// lzcntq (%rsi), %rdx
, 67		// lzcntq %rdx, %rdx
, 147		// maskmovdqu %xmm0, %xmm0
, 19		// maskmovq %mm0, %mm0
, 70		// maxpd (%rsi), %xmm0
, 70		// maxpd %xmm0, %xmm0
, 70		// maxps (%rsi), %xmm0
, 70		// maxps %xmm0, %xmm0
, 70		// maxsd (%rsi), %xmm0
, 70		// maxsd %xmm0, %xmm0
, 70		// maxss (%rsi), %xmm0
, 70		// maxss %xmm0, %xmm0
, 847		// mfence 
, 70		// minpd (%rsi), %xmm0
, 70		// minpd %xmm0, %xmm0
, 70		// minps (%rsi), %xmm0
, 70		// minps %xmm0, %xmm0
, 70		// minsd (%rsi), %xmm0
, 70		// minsd %xmm0, %xmm0
, 70		// minss (%rsi), %xmm0
, 70		// minss %xmm0, %xmm0
, 999		// monitor 
, 999		// movabsb es:0x0, %al
, 999		// movabsb , es:0x0, %al
, 999		// movabsw es:0x0, %ax
, 999		// movabsl es:0x0, %eax
, 45		// movw $0x4050, (%rsi)
, 19		// movw %cx, (%rsi)
, 999		// movw es, (%rsi)
, 19		// movl $0xc0decafe, (%rsi)
, 19		// movl %edx, (%rsi)
, 19		// movq $0xc0decafe, (%rsi)
, 19		// movq %rdx, (%rsi)
, 999		// movq es, (%rsi)
, 19		// movb $0x4, (%rsi)
, 19		// movb %dl, (%rsi)
, 19		// movb %ch, (%rsi)
, 999		// movabsw %ax, es:0x0
, 999		// movabsl %eax, es:0x0
, 999		// movabsq %rax, es:0x0
, 999		// movabsb %al, es:0x0
, 999		// movabsb , %al, es:0x0
, 43		// movw $0x4050, %cx
, 44		// movw $0x4050, %cx
, 18		// movw (%rsi), %cx
, 18		// movw %cx, %cx
, 18		// movw %cx, %cx
, 999		// movw es, %cx
, 0		// movl $0xc0decafe, %edx
, 1		// movl $0xc0decafe, %edx
, 6		// movl (%rsi), %edx
, 18		// movl %edx, %edx
, 18		// movl %edx, %edx
, 2		// movq $0xc0decafe, %rdx
, 12		// movq $0xbadeb0dec0decafe, %rdx
, 6		// movq (%rsi), %rdx
, 18		// movq %rdx, %rdx
, 18		// movq %rdx, %rdx
, 999		// movq es, %rdx
, 18		// movb $0x4, %dl
, 18		// movb $0x4, %dl
, 18		// movb (%rsi), %dl
, 18		// movb %dl, %dl
, 18		// movb %dl, %dl
, 43		// movb %ch, %dl
, 43		// movb %ch, %dl
, 999		// movabsq es:0x0, %rax
, 18		// movb $0x4, %ch
, 18		// movb $0x4, %ch
, 6		// movb (%rsi), %ch
, 0		// movb %dl, %ch
, 0		// movb %dl, %ch
, 18		// movb %ch, %ch
, 18		// movb %ch, %ch
, 999		// mov (%rsi), es
, 999		// mov (%rsi), es
, 999		// mov %ax, es
, 999		// mov %rax, es
, 19		// movapd %xmm0, (%rsi)
, 6		// movapd (%rsi), %xmm0
, 18		// movapd %xmm0, %xmm0
, 18		// movapd %xmm0, %xmm0
, 19		// movaps %xmm0, (%rsi)
, 6		// movaps (%rsi), %xmm0
, 18		// movaps %xmm0, %xmm0
, 18		// movaps %xmm0, %xmm0
, 19		// movbew %cx, (%rsi)
, 19		// movbel %edx, (%rsi)
, 19		// movbeq %rdx, (%rsi)
, 19		// movbew (%rsi), %cx
, 16		// movbel (%rsi), %edx
, 19		// movbeq (%rsi), %rdx
, 19		// movd %mm0, (%rsi)
, 19		// movd %xmm0, (%rsi)
, 6		// movd (%rsi), %mm0
, 18		// movd %edx, %mm0
, 18		// movd %mm0, %edx
, 18		// movd %xmm0, %edx
, 6		// movd (%rsi), %xmm0
, 18		// movd %edx, %xmm0
, 6		// movddup (%rsi), %xmm0
, 18		// movddup %xmm0, %xmm0
, 18		// movdq2q %xmm0, %mm0
, 19		// movdqa %xmm0, (%rsi)
, 6		// movdqa (%rsi), %xmm0
, 18		// movdqa %xmm0, %xmm0
, 18		// movdqa %xmm0, %xmm0
, 19		// movdqu %xmm0, (%rsi)
, 6		// movdqu (%rsi), %xmm0
, 18		// movdqu %xmm0, %xmm0
, 18		// movdqu %xmm0, %xmm0
, 18		// movhlps %xmm0, %xmm0
, 19		// movhpd %xmm0, (%rsi)
, 18		// movhpd (%rsi), %xmm0
, 19		// movhps %xmm0, (%rsi)
, 18		// movhps (%rsi), %xmm0
, 18		// movlhps %xmm0, %xmm0
, 19		// movlpd %xmm0, (%rsi)
, 18		// movlpd (%rsi), %xmm0
, 19		// movlps %xmm0, (%rsi)
, 18		// movlps (%rsi), %xmm0
, 18		// movmskpd %xmm0, %edx
, 18		// movmskpd %xmm0, %rdx
, 18		// movmskps %xmm0, %edx
, 18		// movmskps %xmm0, %rdx
, 19		// movntdq %xmm0, (%rsi)
, 19		// movntdq %ymm0, (%rsi)
, 12		// movntdqa (%rsi), %xmm0
, 19		// movnti %edx, (%rsi)
, 19		// movnti %rdx, (%rsi)
, 19		// movntpd %xmm0, (%rsi)
, 19		// movntps %xmm0, (%rsi)
, 19		// movntq %mm0, (%rsi)
, 19		// movq %mm0, (%rsi)
, 19		// movq %mm0, (%rsi)
, 19		// movq %xmm0, (%rsi)
, 19		// movq %xmm0, (%rsi)
, 6		// movq (%rsi), %mm0
, 6		// movq (%rsi), %mm0
, 18		// movq %mm0, %mm0
, 18		// movq %mm0, %mm0
, 18		// movq %rdx, %mm0
, 18		// movq %mm0, %rdx
, 18		// movq %xmm0, %rdx
, 6		// movq (%rsi), %xmm0
, 6		// movq (%rsi), %xmm0
, 18		// movq %rdx, %xmm0
, 18		// movq %xmm0, %xmm0
, 18		// movq %xmm0, %xmm0
, 1		// movq2dq %mm0, %xmm0
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movs es:(%rax,%rax,1), es:(%rax,%rax,1)
, 999		// movsb 
, 999		// movsl 
, 19		// movsd %xmm0, (%rsi)
, 6		// movsd (%rsi), %xmm0
, 18		// movsd %xmm0, %xmm0
, 18		// movsd %xmm0, %xmm0
, 6		// movshdup (%rsi), %xmm0
, 18		// movshdup %xmm0, %xmm0
, 6		// movsldup (%rsi), %xmm0
, 18		// movsldup %xmm0, %xmm0
, 999		// movsq 
, 19		// movss %xmm0, (%rsi)
, 6		// movss (%rsi), %xmm0
, 18		// movss %xmm0, %xmm0
, 18		// movss %xmm0, %xmm0
, 999		// movsw 
, 18		// movsbw (%rsi), %cx
, 18		// movsbw %dl, %cx
, 42		// movsbw %ch, %cx
, 6		// movswl (%rsi), %edx
, 6		// movsbl (%rsi), %edx
, 1		// movswl %cx, %edx
, 18		// movsbl %dl, %edx
, 1		// movsbl %ch, %edx
, 6		// movswq (%rsi), %rdx
, 6		// movsbq (%rsi), %rdx
, 1		// movswq %cx, %rdx
, 18		// movsbq %dl, %rdx
, 6		// movslq (%rsi), %rdx
, 18		// movslq %edx, %rdx
, 19		// movupd %xmm0, (%rsi)
, 6		// movupd (%rsi), %xmm0
, 18		// movupd %xmm0, %xmm0
, 18		// movupd %xmm0, %xmm0
, 20		// movups %xmm0, (%rsi)
, 6		// movups (%rsi), %xmm0
, 19		// movups %xmm0, %xmm0
, 18		// movups %xmm0, %xmm0
, 18		// movzbw (%rsi), %cx
, 18		// movzbw %dl, %cx
, 42		// movzbw %ch, %cx
, 6		// movzwl (%rsi), %edx
, 6		// movzbl (%rsi), %edx
, 1		// movzwl %cx, %edx
, 18		// movzbl %dl, %edx
, 1		// movzbl %ch, %edx
, 6		// movzwq (%rsi), %rdx
, 6		// movzbq (%rsi), %rdx
, 0		// movzwq %cx, %rdx
, 18		// movzbq %dl, %rdx
, 147		// mpsadbw $0x4, (%rsi), %xmm0
, 172		// mpsadbw $0x4, %xmm0, %xmm0
, 96		// mulw (%rsi)
, 100		// mull (%rsi)
, 69		// mulq (%rsi)
, 67		// mulb (%rsi)
, 96		// mulw %cx
, 102		// mull %edx
, 94		// mulq %rdx
, 67		// mulb %dl
, 92		// mulb %ch
, 121		// mulpd (%rsi), %xmm0
, 121		// mulpd %xmm0, %xmm0
, 121		// mulps (%rsi), %xmm0
, 121		// mulps %xmm0, %xmm0
, 121		// mulsd (%rsi), %xmm0
, 121		// mulsd %xmm0, %xmm0
, 121		// mulss (%rsi), %xmm0
, 121		// mulss %xmm0, %xmm0
, 101		// mulxl (%rsi), %edx, %edx
, 100		// mulxl %edx, %edx, %edx
, 94		// mulxq (%rsi), %rdx, %rdx
, 94		// mulxq %rdx, %rdx, %rdx
, 999		// mwait 
, 163		// negw (%rsi)
, 168		// negl (%rsi)
, 167		// negq (%rsi)
, 167		// negb (%rsi)
, 18		// negw %cx
, 18		// negl %edx
, 18		// negq %rdx
, 18		// negb %dl
, 18		// negb %ch
, 1		// nop 
, 1		// nopw (%rsi)
, 1		// nopl (%rsi)
, 1		// nopw %cx
, 1		// nopl %edx
, 148		// notw (%rsi)
, 149		// notl (%rsi)
, 148		// notq (%rsi)
, 148		// notb (%rsi)
, 18		// notw %cx
, 18		// notl %edx
, 18		// notq %rdx
, 18		// notb %dl
, 18		// notb %ch
, 18		// orb $0x4, %al
, 43		// orw $0x4050, %ax
, 18		// orl $0xc0decafe, %eax
, 168		// orw $0x4050, (%rsi)
, 165		// orw $0x4, (%rsi)
, 149		// orw %cx, (%rsi)
, 163		// orl $0xc0decafe, (%rsi)
, 167		// orl $0x4, (%rsi)
, 149		// orl %edx, (%rsi)
, 166		// orq $0xc0decafe, (%rsi)
, 165		// orq $0x4, (%rsi)
, 149		// orq %rdx, (%rsi)
, 165		// orb $0x4, (%rsi)
, 149		// orb %dl, (%rsi)
, 181		// orb %ch, (%rsi)
, 44		// orw $0x4050, %cx
, 18		// orw $0x4, %cx
, 18		// orw (%rsi), %cx
, 18		// orw %cx, %cx
, 18		// orw %cx, %cx
, 18		// orl $0xc0decafe, %edx
, 18		// orl $0x4, %edx
, 18		// orl (%rsi), %edx
, 18		// orl %edx, %edx
, 18		// orl %edx, %edx
, 18		// orq $0xc0decafe, %rdx
, 18		// orq $0x4, %rdx
, 18		// orq (%rsi), %rdx
, 18		// orq %rdx, %rdx
, 18		// orq %rdx, %rdx
, 18		// orb $0x4, %dl
, 18		// orb (%rsi), %dl
, 18		// orb %dl, %dl
, 18		// orb %dl, %dl
, 43		// orb %ch, %dl
, 43		// orb %ch, %dl
, 18		// orq $0xc0decafe, %rax
, 18		// orb $0x4, %ch
, 18		// orb (%rsi), %ch
, 18		// orb %dl, %ch
, 18		// orb %dl, %ch
, 18		// orb %ch, %ch
, 18		// orb %ch, %ch
, 18		// orpd (%rsi), %xmm0
, 18		// orpd %xmm0, %xmm0
, 18		// orps (%rsi), %xmm0
, 18		// orps %xmm0, %xmm0
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
, 6		// pabsb (%rsi), %mm0
, 18		// pabsb %mm0, %mm0
, 6		// pabsb (%rsi), %xmm0
, 18		// pabsb %xmm0, %xmm0
, 6		// pabsd (%rsi), %mm0
, 18		// pabsd %mm0, %mm0
, 6		// pabsd (%rsi), %xmm0
, 18		// pabsd %xmm0, %xmm0
, 6		// pabsw (%rsi), %mm0
, 18		// pabsw %mm0, %mm0
, 6		// pabsw (%rsi), %xmm0
, 18		// pabsw %xmm0, %xmm0
, 44		// packssdw (%rsi), %mm0
, 44		// packssdw %mm0, %mm0
, 18		// packssdw (%rsi), %xmm0
, 18		// packssdw %xmm0, %xmm0
, 44		// packsswb (%rsi), %mm0
, 44		// packsswb %mm0, %mm0
, 18		// packsswb (%rsi), %xmm0
, 18		// packsswb %xmm0, %xmm0
, 18		// packusdw (%rsi), %xmm0
, 18		// packusdw %xmm0, %xmm0
, 44		// packuswb (%rsi), %mm0
, 44		// packuswb %mm0, %mm0
, 18		// packuswb (%rsi), %xmm0
, 18		// packuswb %xmm0, %xmm0
, 18		// paddb (%rsi), %mm0
, 18		// paddb %mm0, %mm0
, 18		// paddb (%rsi), %xmm0
, 18		// paddb %xmm0, %xmm0
, 18		// paddd (%rsi), %mm0
, 18		// paddd %mm0, %mm0
, 18		// paddd (%rsi), %xmm0
, 18		// paddd %xmm0, %xmm0
, 18		// paddq (%rsi), %mm0
, 18		// paddq %mm0, %mm0
, 18		// paddq (%rsi), %xmm0
, 18		// paddq %xmm0, %xmm0
, 18		// paddsb (%rsi), %mm0
, 18		// paddsb %mm0, %mm0
, 18		// paddsb (%rsi), %xmm0
, 18		// paddsb %xmm0, %xmm0
, 18		// paddsw (%rsi), %mm0
, 18		// paddsw %mm0, %mm0
, 18		// paddsw (%rsi), %xmm0
, 18		// paddsw %xmm0, %xmm0
, 18		// paddusb (%rsi), %mm0
, 18		// paddusb %mm0, %mm0
, 18		// paddusb (%rsi), %xmm0
, 18		// paddusb %xmm0, %xmm0
, 18		// paddusw (%rsi), %mm0
, 18		// paddusw %mm0, %mm0
, 18		// paddusw (%rsi), %xmm0
, 18		// paddusw %xmm0, %xmm0
, 18		// paddw (%rsi), %mm0
, 18		// paddw %mm0, %mm0
, 18		// paddw (%rsi), %xmm0
, 18		// paddw %xmm0, %xmm0
, 18		// palignr $0x4, (%rsi), %mm0
, 18		// palignr $0x4, %mm0, %mm0
, 18		// palignr $0x4, (%rsi), %xmm0
, 18		// palignr $0x4, %xmm0, %xmm0
, 18		// pand (%rsi), %mm0
, 18		// pand %mm0, %mm0
, 18		// pand (%rsi), %xmm0
, 18		// pand %xmm0, %xmm0
, 18		// pandn (%rsi), %mm0
, 18		// pandn %mm0, %mm0
, 18		// pandn (%rsi), %xmm0
, 18		// pandn %xmm0, %xmm0
, 231		// pause 
, 18		// pavgb (%rsi), %mm0
, 18		// pavgb %mm0, %mm0
, 18		// pavgb (%rsi), %xmm0
, 18		// pavgb %xmm0, %xmm0
, 18		// pavgw (%rsi), %mm0
, 18		// pavgw %mm0, %mm0
, 18		// pavgw (%rsi), %xmm0
, 18		// pavgw %xmm0, %xmm0
, 44		// pblendvb %xmm0, (%rsi), %xmm0
, 44		// pblendvb %xmm0, %xmm0, %xmm0
, 18		// pblendw $0x4, (%rsi), %xmm0
, 18		// pblendw $0x4, %xmm0, %xmm0
, 172		// pclmulqdq $0x4, (%rsi), %xmm0
, 172		// pclmulqdq $0x4, %xmm0, %xmm0
, 18		// pcmpeqb (%rsi), %mm0
, 5		// pcmpeqb %mm0, %mm0
, 18		// pcmpeqb (%rsi), %xmm0
, 5		// pcmpeqb %xmm0, %xmm0
, 18		// pcmpeqd (%rsi), %mm0
, 5		// pcmpeqd %mm0, %mm0
, 18		// pcmpeqd (%rsi), %xmm0
, 5		// pcmpeqd %xmm0, %xmm0
, 18		// pcmpeqq (%rsi), %xmm0
, 5		// pcmpeqq %xmm0, %xmm0
, 18		// pcmpeqw (%rsi), %mm0
, 5		// pcmpeqw %mm0, %mm0
, 18		// pcmpeqw (%rsi), %xmm0
, 5		// pcmpeqw %xmm0, %xmm0
, 96		// pcmpestri $0x4, (%rsi), %xmm0
, 96		// pcmpestri $0x4, %xmm0, %xmm0
, 275		// pcmpestrm $0x4, (%rsi), %xmm0
, 275		// pcmpestrm $0x4, %xmm0, %xmm0
, 18		// pcmpgtb (%rsi), %mm0
, 5		// pcmpgtb %mm0, %mm0
, 18		// pcmpgtb (%rsi), %xmm0
, 1		// pcmpgtb %xmm0, %xmm0
, 18		// pcmpgtd (%rsi), %mm0
, 5		// pcmpgtd %mm0, %mm0
, 18		// pcmpgtd (%rsi), %xmm0
, 1		// pcmpgtd %xmm0, %xmm0
, 121		// pcmpgtq (%rsi), %xmm0
, 9		// pcmpgtq %xmm0, %xmm0
, 18		// pcmpgtw (%rsi), %mm0
, 5		// pcmpgtw %mm0, %mm0
, 18		// pcmpgtw (%rsi), %xmm0
, 1		// pcmpgtw %xmm0, %xmm0
, 70		// pcmpistri $0x4, (%rsi), %xmm0
, 70		// pcmpistri $0x4, %xmm0, %xmm0
, 273		// pcmpistrm $0x4, (%rsi), %xmm0
, 268		// pcmpistrm $0x4, %xmm0, %xmm0
, 67		// pdepl (%rsi), %edx, %edx
, 67		// pdepl %edx, %edx, %edx
, 67		// pdepq (%rsi), %rdx, %rdx
, 67		// pdepq %rdx, %rdx, %rdx
, 67		// pextl (%rsi), %edx, %edx
, 67		// pextl %edx, %edx, %edx
, 67		// pextq (%rsi), %rdx, %rdx
, 67		// pextq %rdx, %rdx, %rdx
, 19		// pextrb $0x4, %xmm0, (%rsi)
, 18		// pextrb $0x4, %xmm0, %edx
, 18		// pextrb $0x4, %xmm0, %rdx
, 19		// pextrd $0x4, %xmm0, (%rsi)
, 18		// pextrd $0x4, %xmm0, %edx
, 19		// pextrq $0x4, %xmm0, (%rsi)
, 18		// pextrq $0x4, %xmm0, %rdx
, 19		// pextrw $0x4, %xmm0, (%rsi)
, 18		// pextrw $0x4, %mm0, %edx
, 18		// pextrw $0x4, %xmm0, %edx
, 18		// pextrw $0x4, %xmm0, %edx
, 18		// pextrw $0x4, %mm0, %rdx
, 18		// pextrw $0x4, %xmm0, %rdx
, 18		// pextrw $0x4, %xmm0, %rdx
, 70		// phaddd (%rsi), %mm0
, 70		// phaddd %mm0, %mm0
, 70		// phaddd (%rsi), %xmm0
, 70		// phaddd %xmm0, %xmm0
, 70		// phaddsw (%rsi), %mm0
, 70		// phaddsw %mm0, %mm0
, 70		// phaddsw (%rsi), %xmm0
, 70		// phaddsw %xmm0, %xmm0
, 70		// phaddw (%rsi), %mm0
, 70		// phaddw %mm0, %mm0
, 70		// phaddw (%rsi), %xmm0
, 70		// phaddw %xmm0, %xmm0
, 18		// phminposuw (%rsi), %xmm0
, 121		// phminposuw %xmm0, %xmm0
, 70		// phsubd (%rsi), %mm0
, 70		// phsubd %mm0, %mm0
, 70		// phsubd (%rsi), %xmm0
, 70		// phsubd %xmm0, %xmm0
, 70		// phsubsw (%rsi), %mm0
, 70		// phsubsw %mm0, %mm0
, 70		// phsubsw (%rsi), %xmm0
, 70		// phsubsw %xmm0, %xmm0
, 70		// phsubw (%rsi), %mm0
, 70		// phsubw %mm0, %mm0
, 70		// phsubw (%rsi), %xmm0
, 70		// phsubw %xmm0, %xmm0
, 18		// pinsrb $0x4, (%rsi), %xmm0
, 44		// pinsrb $0x4, %edx, %xmm0
, 18		// pinsrd $0x4, (%rsi), %xmm0
, 44		// pinsrd $0x4, %edx, %xmm0
, 18		// pinsrw $0x4, (%rsi), %mm0
, 44		// pinsrw $0x4, %edx, %mm0
, 18		// pinsrw $0x4, (%rsi), %xmm0
, 44		// pinsrw $0x4, %edx, %xmm0
, 121		// pmaddubsw (%rsi), %mm0
, 121		// pmaddubsw %mm0, %mm0
, 121		// pmaddubsw (%rsi), %xmm0
, 121		// pmaddubsw %xmm0, %xmm0
, 121		// pmaddwd (%rsi), %mm0
, 121		// pmaddwd %mm0, %mm0
, 121		// pmaddwd (%rsi), %xmm0
, 121		// pmaddwd %xmm0, %xmm0
, 18		// pmaxsb (%rsi), %xmm0
, 18		// pmaxsb %xmm0, %xmm0
, 18		// pmaxsd (%rsi), %xmm0
, 18		// pmaxsd %xmm0, %xmm0
, 18		// pmaxsw (%rsi), %mm0
, 18		// pmaxsw %mm0, %mm0
, 18		// pmaxsw (%rsi), %xmm0
, 18		// pmaxsw %xmm0, %xmm0
, 18		// pmaxub (%rsi), %mm0
, 18		// pmaxub %mm0, %mm0
, 18		// pmaxub (%rsi), %xmm0
, 18		// pmaxub %xmm0, %xmm0
, 18		// pmaxud (%rsi), %xmm0
, 18		// pmaxud %xmm0, %xmm0
, 18		// pmaxuw (%rsi), %xmm0
, 18		// pmaxuw %xmm0, %xmm0
, 18		// pminsb (%rsi), %xmm0
, 18		// pminsb %xmm0, %xmm0
, 18		// pminsd (%rsi), %xmm0
, 18		// pminsd %xmm0, %xmm0
, 18		// pminsw (%rsi), %mm0
, 18		// pminsw %mm0, %mm0
, 18		// pminsw (%rsi), %xmm0
, 18		// pminsw %xmm0, %xmm0
, 18		// pminub (%rsi), %mm0
, 18		// pminub %mm0, %mm0
, 18		// pminub (%rsi), %xmm0
, 18		// pminub %xmm0, %xmm0
, 18		// pminud (%rsi), %xmm0
, 18		// pminud %xmm0, %xmm0
, 18		// pminuw (%rsi), %xmm0
, 18		// pminuw %xmm0, %xmm0
, 18		// pmovmskb %mm0, %edx
, 18		// pmovmskb %xmm0, %edx
, 18		// pmovmskb %mm0, %rdx
, 18		// pmovmskb %xmm0, %rdx
, 18		// pmovsxbd (%rsi), %xmm0
, 18		// pmovsxbd %xmm0, %xmm0
, 18		// pmovsxbq (%rsi), %xmm0
, 18		// pmovsxbq %xmm0, %xmm0
, 18		// pmovsxbw (%rsi), %xmm0
, 18		// pmovsxbw %xmm0, %xmm0
, 18		// pmovsxdq (%rsi), %xmm0
, 18		// pmovsxdq %xmm0, %xmm0
, 18		// pmovsxwd (%rsi), %xmm0
, 18		// pmovsxwd %xmm0, %xmm0
, 18		// pmovsxwq (%rsi), %xmm0
, 18		// pmovsxwq %xmm0, %xmm0
, 18		// pmovzxbd (%rsi), %xmm0
, 18		// pmovzxbd %xmm0, %xmm0
, 18		// pmovzxbq (%rsi), %xmm0
, 18		// pmovzxbq %xmm0, %xmm0
, 18		// pmovzxbw (%rsi), %xmm0
, 18		// pmovzxbw %xmm0, %xmm0
, 18		// pmovzxdq (%rsi), %xmm0
, 18		// pmovzxdq %xmm0, %xmm0
, 18		// pmovzxwd (%rsi), %xmm0
, 18		// pmovzxwd %xmm0, %xmm0
, 18		// pmovzxwq (%rsi), %xmm0
, 18		// pmovzxwq %xmm0, %xmm0
, 121		// pmuldq (%rsi), %xmm0
, 121		// pmuldq %xmm0, %xmm0
, 121		// pmulhrsw (%rsi), %mm0
, 121		// pmulhrsw %mm0, %mm0
, 121		// pmulhrsw (%rsi), %xmm0
, 121		// pmulhrsw %xmm0, %xmm0
, 121		// pmulhuw (%rsi), %mm0
, 121		// pmulhuw %mm0, %mm0
, 121		// pmulhuw (%rsi), %xmm0
, 121		// pmulhuw %xmm0, %xmm0
, 121		// pmulhw (%rsi), %mm0
, 121		// pmulhw %mm0, %mm0
, 121		// pmulhw (%rsi), %xmm0
, 121		// pmulhw %xmm0, %xmm0
, 249		// pmulld (%rsi), %xmm0
, 249		// pmulld %xmm0, %xmm0
, 121		// pmullw (%rsi), %mm0
, 121		// pmullw %mm0, %mm0
, 121		// pmullw (%rsi), %xmm0
, 121		// pmullw %xmm0, %xmm0
, 121		// pmuludq (%rsi), %mm0
, 121		// pmuludq %mm0, %mm0
, 121		// pmuludq (%rsi), %xmm0
, 121		// pmuludq %xmm0, %xmm0
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
, 67		// popcntw (%rsi), %cx
, 67		// popcntw %cx, %cx
, 67		// popcntl (%rsi), %edx
, 67		// popcntl %edx, %edx
, 67		// popcntq (%rsi), %rdx
, 67		// popcntq %rdx, %rdx
, 999		// popf 
, 999		// popfq 
, 18		// por (%rsi), %mm0
, 18		// por %mm0, %mm0
, 18		// por (%rsi), %xmm0
, 18		// por %xmm0, %xmm0
, 6		// prefetchnta (%rsi)
, 6		// prefetcht0 (%rsi)
, 6		// prefetcht1 (%rsi)
, 6		// prefetcht2 (%rsi)
, 121		// psadbw (%rsi), %mm0
, 121		// psadbw %mm0, %mm0
, 121		// psadbw (%rsi), %xmm0
, 121		// psadbw %xmm0, %xmm0
, 18		// pshufb (%rsi), %mm0
, 18		// pshufb %mm0, %mm0
, 18		// pshufb (%rsi), %xmm0
, 18		// pshufb %xmm0, %xmm0
, 18		// pshufd $0x4, (%rsi), %xmm0
, 18		// pshufd $0x4, %xmm0, %xmm0
, 18		// pshufhw $0x4, (%rsi), %xmm0
, 18		// pshufhw $0x4, %xmm0, %xmm0
, 18		// pshuflw $0x4, (%rsi), %xmm0
, 18		// pshuflw $0x4, %xmm0, %xmm0
, 18		// pshufw $0x4, (%rsi), %mm0
, 18		// pshufw $0x4, %mm0, %mm0
, 18		// psignb (%rsi), %mm0
, 18		// psignb %mm0, %mm0
, 18		// psignb (%rsi), %xmm0
, 18		// psignb %xmm0, %xmm0
, 18		// psignd (%rsi), %mm0
, 18		// psignd %mm0, %mm0
, 18		// psignd (%rsi), %xmm0
, 18		// psignd %xmm0, %xmm0
, 18		// psignw (%rsi), %mm0
, 18		// psignw %mm0, %mm0
, 18		// psignw (%rsi), %xmm0
, 18		// psignw %xmm0, %xmm0
, 18		// pslld $0x4, %mm0
, 18		// pslld (%rsi), %mm0
, 18		// pslld %mm0, %mm0
, 18		// pslld $0x4, %xmm0
, 18		// pslld (%rsi), %xmm0
, 44		// pslld %xmm0, %xmm0
, 18		// pslldq $0x4, %xmm0
, 18		// psllq $0x4, %mm0
, 18		// psllq (%rsi), %mm0
, 18		// psllq %mm0, %mm0
, 18		// psllq $0x4, %xmm0
, 18		// psllq (%rsi), %xmm0
, 44		// psllq %xmm0, %xmm0
, 18		// psllw $0x4, %mm0
, 18		// psllw (%rsi), %mm0
, 18		// psllw %mm0, %mm0
, 18		// psllw $0x4, %xmm0
, 18		// psllw (%rsi), %xmm0
, 44		// psllw %xmm0, %xmm0
, 18		// psrad $0x4, %mm0
, 18		// psrad (%rsi), %mm0
, 18		// psrad %mm0, %mm0
, 18		// psrad $0x4, %xmm0
, 18		// psrad (%rsi), %xmm0
, 44		// psrad %xmm0, %xmm0
, 18		// psraw $0x4, %mm0
, 18		// psraw (%rsi), %mm0
, 18		// psraw %mm0, %mm0
, 18		// psraw $0x4, %xmm0
, 18		// psraw (%rsi), %xmm0
, 44		// psraw %xmm0, %xmm0
, 18		// psrld $0x4, %mm0
, 18		// psrld (%rsi), %mm0
, 18		// psrld %mm0, %mm0
, 18		// psrld $0x4, %xmm0
, 18		// psrld (%rsi), %xmm0
, 44		// psrld %xmm0, %xmm0
, 18		// psrldq $0x4, %xmm0
, 18		// psrlq $0x4, %mm0
, 18		// psrlq (%rsi), %mm0
, 18		// psrlq %mm0, %mm0
, 18		// psrlq $0x4, %xmm0
, 18		// psrlq (%rsi), %xmm0
, 44		// psrlq %xmm0, %xmm0
, 18		// psrlw $0x4, %mm0
, 18		// psrlw (%rsi), %mm0
, 18		// psrlw %mm0, %mm0
, 18		// psrlw $0x4, %xmm0
, 18		// psrlw (%rsi), %xmm0
, 44		// psrlw %xmm0, %xmm0
, 18		// psubb (%rsi), %mm0
, 5		// psubb %mm0, %mm0
, 18		// psubb (%rsi), %xmm0
, 1		// psubb %xmm0, %xmm0
, 18		// psubd (%rsi), %mm0
, 5		// psubd %mm0, %mm0
, 18		// psubd (%rsi), %xmm0
, 1		// psubd %xmm0, %xmm0
, 18		// psubq (%rsi), %mm0
, 5		// psubq %mm0, %mm0
, 18		// psubq (%rsi), %xmm0
, 9		// psubq %xmm0, %xmm0
, 18		// psubsb (%rsi), %mm0
, 5		// psubsb %mm0, %mm0
, 18		// psubsb (%rsi), %xmm0
, 1		// psubsb %xmm0, %xmm0
, 18		// psubsw (%rsi), %mm0
, 5		// psubsw %mm0, %mm0
, 18		// psubsw (%rsi), %xmm0
, 1		// psubsw %xmm0, %xmm0
, 18		// psubusb (%rsi), %mm0
, 5		// psubusb %mm0, %mm0
, 18		// psubusb (%rsi), %xmm0
, 1		// psubusb %xmm0, %xmm0
, 18		// psubusw (%rsi), %mm0
, 5		// psubusw %mm0, %mm0
, 18		// psubusw (%rsi), %xmm0
, 1		// psubusw %xmm0, %xmm0
, 18		// psubw (%rsi), %mm0
, 5		// psubw %mm0, %mm0
, 18		// psubw (%rsi), %xmm0
, 1		// psubw %xmm0, %xmm0
, 19		// ptest (%rsi), %xmm0
, 18		// ptest %xmm0, %xmm0
, 18		// punpckhbw (%rsi), %mm0
, 18		// punpckhbw %mm0, %mm0
, 18		// punpckhbw (%rsi), %xmm0
, 18		// punpckhbw %xmm0, %xmm0
, 18		// punpckhdq (%rsi), %mm0
, 18		// punpckhdq %mm0, %mm0
, 18		// punpckhdq (%rsi), %xmm0
, 18		// punpckhdq %xmm0, %xmm0
, 18		// punpckhqdq (%rsi), %xmm0
, 18		// punpckhqdq %xmm0, %xmm0
, 18		// punpckhwd (%rsi), %mm0
, 18		// punpckhwd %mm0, %mm0
, 18		// punpckhwd (%rsi), %xmm0
, 18		// punpckhwd %xmm0, %xmm0
, 18		// punpcklbw (%rsi), %mm0
, 18		// punpcklbw %mm0, %mm0
, 18		// punpcklbw (%rsi), %xmm0
, 18		// punpcklbw %xmm0, %xmm0
, 18		// punpckldq (%rsi), %mm0
, 18		// punpckldq %mm0, %mm0
, 18		// punpckldq (%rsi), %xmm0
, 18		// punpckldq %xmm0, %xmm0
, 18		// punpcklqdq (%rsi), %xmm0
, 18		// punpcklqdq %xmm0, %xmm0
, 18		// punpcklwd (%rsi), %mm0
, 18		// punpcklwd %mm0, %mm0
, 18		// punpcklwd (%rsi), %xmm0
, 18		// punpcklwd %xmm0, %xmm0
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
, 18		// pxor (%rsi), %mm0
, 1		// pxor %mm0, %mm0
, 18		// pxor (%rsi), %xmm0
, 1		// pxor %xmm0, %xmm0
, 301		// rclw %cl, (%rsi)
, 301		// rclw $0x4, (%rsi)
, 198		// rclw $0x1, (%rsi)
, 301		// rcll %cl, (%rsi)
, 301		// rcll $0x4, (%rsi)
, 198		// rcll $0x1, (%rsi)
, 301		// rclq %cl, (%rsi)
, 301		// rclq $0x4, (%rsi)
, 198		// rclq $0x1, (%rsi)
, 267		// rclb %cl, (%rsi)
, 267		// rclb $0x4, (%rsi)
, 198		// rclb $0x1, (%rsi)
, 172		// rclw %cl, %cx
, 174		// rclw $0x4, %cx
, 44		// rclw $0x1, %cx
, 174		// rcll %cl, %edx
, 174		// rcll $0x4, %edx
, 44		// rcll $0x1, %edx
, 174		// rclq %cl, %rdx
, 174		// rclq $0x4, %rdx
, 44		// rclq $0x1, %rdx
, 157		// rclb %cl, %dl
, 157		// rclb $0x4, %dl
, 44		// rclb $0x1, %dl
, 157		// rclb %cl, %ch
, 157		// rclb $0x4, %ch
, 44		// rclb $0x1, %ch
, 18		// rcpps (%rsi), %xmm0
, 121		// rcpps %xmm0, %xmm0
, 121		// rcpss (%rsi), %xmm0
, 121		// rcpss %xmm0, %xmm0
, 275		// rcrw %cl, (%rsi)
, 275		// rcrw $0x4, (%rsi)
, 198		// rcrw $0x1, (%rsi)
, 275		// rcrl %cl, (%rsi)
, 275		// rcrl $0x4, (%rsi)
, 198		// rcrl $0x1, (%rsi)
, 275		// rcrq %cl, (%rsi)
, 275		// rcrq $0x4, (%rsi)
, 198		// rcrq $0x1, (%rsi)
, 302		// rcrb %cl, (%rsi)
, 302		// rcrb $0x4, (%rsi)
, 198		// rcrb $0x1, (%rsi)
, 172		// rcrw %cl, %cx
, 148		// rcrw $0x4, %cx
, 44		// rcrw $0x1, %cx
, 148		// rcrl %cl, %edx
, 148		// rcrl $0x4, %edx
, 44		// rcrl $0x1, %edx
, 148		// rcrq %cl, %rdx
, 148		// rcrq $0x4, %rdx
, 44		// rcrq $0x1, %rdx
, 200		// rcrb %cl, %dl
, 200		// rcrb $0x4, %dl
, 44		// rcrb $0x1, %dl
, 200		// rcrb %cl, %ch
, 200		// rcrb $0x4, %ch
, 44		// rcrb $0x1, %ch
, 999		// rdfsbase %eax
, 999		// rdfsbase %rax
, 999		// rdgsbase %eax
, 999		// rdgsbase %rax
, 8728		// rdrand %cx
, 8718		// rdrand %edx
, 8716		// rdrand %rdx
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
, 147		// rolw %cl, (%rsi)
, 156		// rolw $0x4, (%rsi)
, 156		// rolw $0x1, (%rsi)
, 147		// roll %cl, (%rsi)
, 155		// roll $0x4, (%rsi)
, 155		// roll $0x1, (%rsi)
, 147		// rolq %cl, (%rsi)
, 156		// rolq $0x4, (%rsi)
, 155		// rolq $0x1, (%rsi)
, 147		// rolb %cl, (%rsi)
, 155		// rolb $0x4, (%rsi)
, 155		// rolb $0x1, (%rsi)
, 44		// rolw %cl, %cx
, 18		// rolw $0x4, %cx
, 19		// rolw $0x1, %cx
, 44		// roll %cl, %edx
, 18		// roll $0x4, %edx
, 19		// roll $0x1, %edx
, 44		// rolq %cl, %rdx
, 18		// rolq $0x4, %rdx
, 19		// rolq $0x1, %rdx
, 44		// rolb %cl, %dl
, 18		// rolb $0x4, %dl
, 19		// rolb $0x1, %dl
, 44		// rolb %cl, %ch
, 18		// rolb $0x4, %ch
, 19		// rolb $0x1, %ch
, 147		// rorw %cl, (%rsi)
, 155		// rorw $0x4, (%rsi)
, 155		// rorw $0x1, (%rsi)
, 147		// rorl %cl, (%rsi)
, 155		// rorl $0x4, (%rsi)
, 157		// rorl $0x1, (%rsi)
, 147		// rorq %cl, (%rsi)
, 155		// rorq $0x4, (%rsi)
, 155		// rorq $0x1, (%rsi)
, 147		// rorb %cl, (%rsi)
, 156		// rorb $0x4, (%rsi)
, 154		// rorb $0x1, (%rsi)
, 44		// rorw %cl, %cx
, 18		// rorw $0x4, %cx
, 19		// rorw $0x1, %cx
, 44		// rorl %cl, %edx
, 18		// rorl $0x4, %edx
, 19		// rorl $0x1, %edx
, 44		// rorq %cl, %rdx
, 18		// rorq $0x4, %rdx
, 19		// rorq $0x1, %rdx
, 44		// rorb %cl, %dl
, 18		// rorb $0x4, %dl
, 19		// rorb $0x1, %dl
, 44		// rorb %cl, %ch
, 18		// rorb $0x4, %ch
, 19		// rorb $0x1, %ch
, 15		// rorxl $0x4, (%rsi), %edx
, 18		// rorxl $0x4, %edx, %edx
, 15		// rorxq $0x4, (%rsi), %rdx
, 18		// rorxq $0x4, %rdx, %rdx
, 44		// roundpd $0x4, (%rsi), %xmm0
, 147		// roundpd $0x4, %xmm0, %xmm0
, 44		// roundps $0x4, (%rsi), %xmm0
, 147		// roundps $0x4, %xmm0, %xmm0
, 147		// roundsd $0x4, (%rsi), %xmm0
, 147		// roundsd $0x4, %xmm0, %xmm0
, 147		// roundss $0x4, (%rsi), %xmm0
, 147		// roundss $0x4, %xmm0, %xmm0
, 18		// rsqrtps (%rsi), %xmm0
, 121		// rsqrtps %xmm0, %xmm0
, 121		// rsqrtss (%rsi), %xmm0
, 121		// rsqrtss %xmm0, %xmm0
, 42		// sahf 
, 147		// salw %cl, (%rsi)
, 148		// salw $0x4, (%rsi)
, 148		// salw $0x1, (%rsi)
, 147		// sall %cl, (%rsi)
, 148		// sall $0x4, (%rsi)
, 148		// sall $0x1, (%rsi)
, 147		// salq %cl, (%rsi)
, 148		// salq $0x4, (%rsi)
, 148		// salq $0x1, (%rsi)
, 147		// salb %cl, (%rsi)
, 148		// salb $0x4, (%rsi)
, 148		// salb $0x1, (%rsi)
, 44		// salw %cl, %cx
, 18		// salw $0x4, %cx
, 18		// salw $0x1, %cx
, 44		// sall %cl, %edx
, 18		// sall $0x4, %edx
, 18		// sall $0x1, %edx
, 44		// salq %cl, %rdx
, 18		// salq $0x4, %rdx
, 18		// salq $0x1, %rdx
, 44		// salb %cl, %dl
, 18		// salb $0x4, %dl
, 18		// salb $0x1, %dl
, 44		// salb %cl, %ch
, 18		// salb $0x4, %ch
, 18		// salb $0x1, %ch
, 147		// sarw %cl, (%rsi)
, 147		// sarw $0x4, (%rsi)
, 148		// sarw $0x1, (%rsi)
, 147		// sarl %cl, (%rsi)
, 148		// sarl $0x4, (%rsi)
, 148		// sarl $0x1, (%rsi)
, 147		// sarq %cl, (%rsi)
, 148		// sarq $0x4, (%rsi)
, 148		// sarq $0x1, (%rsi)
, 147		// sarb %cl, (%rsi)
, 148		// sarb $0x4, (%rsi)
, 148		// sarb $0x1, (%rsi)
, 44		// sarw %cl, %cx
, 18		// sarw $0x4, %cx
, 18		// sarw $0x1, %cx
, 44		// sarl %cl, %edx
, 18		// sarl $0x4, %edx
, 18		// sarl $0x1, %edx
, 44		// sarq %cl, %rdx
, 18		// sarq $0x4, %rdx
, 18		// sarq $0x1, %rdx
, 44		// sarb %cl, %dl
, 18		// sarb $0x4, %dl
, 18		// sarb $0x1, %dl
, 44		// sarb %cl, %ch
, 18		// sarb $0x4, %ch
, 18		// sarb $0x1, %ch
, 18		// sarxl %edx, (%rsi), %edx
, 18		// sarxl %edx, %edx, %edx
, 18		// sarxq %rdx, (%rsi), %rdx
, 18		// sarxq %rdx, %rdx, %rdx
, 44		// sbbb $0x4, %al
, 67		// sbbw $0x4050, %ax
, 44		// sbbl $0xc0decafe, %eax
, 185		// sbbw $0x4050, (%rsi)
, 182		// sbbw $0x4, (%rsi)
, 185		// sbbw %cx, (%rsi)
, 183		// sbbl $0xc0decafe, (%rsi)
, 180		// sbbl $0x4, (%rsi)
, 186		// sbbl %edx, (%rsi)
, 181		// sbbq $0xc0decafe, (%rsi)
, 183		// sbbq $0x4, (%rsi)
, 184		// sbbq %rdx, (%rsi)
, 183		// sbbb $0x4, (%rsi)
, 188		// sbbb %dl, (%rsi)
, 200		// sbbb %ch, (%rsi)
, 64		// sbbw $0x4050, %cx
, 44		// sbbw $0x4, %cx
, 44		// sbbw (%rsi), %cx
, 44		// sbbw %cx, %cx
, 44		// sbbw %cx, %cx
, 44		// sbbl $0xc0decafe, %edx
, 44		// sbbl $0x4, %edx
, 44		// sbbl (%rsi), %edx
, 44		// sbbl %edx, %edx
, 44		// sbbl %edx, %edx
, 44		// sbbq $0xc0decafe, %rdx
, 44		// sbbq $0x4, %rdx
, 44		// sbbq (%rsi), %rdx
, 44		// sbbq %rdx, %rdx
, 44		// sbbq %rdx, %rdx
, 44		// sbbb $0x4, %dl
, 44		// sbbb (%rsi), %dl
, 44		// sbbb %dl, %dl
, 44		// sbbb %dl, %dl
, 69		// sbbb %ch, %dl
, 69		// sbbb %ch, %dl
, 44		// sbbq $0xc0decafe, %rax
, 44		// sbbb $0x4, %ch
, 44		// sbbb (%rsi), %ch
, 44		// sbbb %dl, %ch
, 44		// sbbb %dl, %ch
, 44		// sbbb %ch, %ch
, 44		// sbbb %ch, %ch
, 999		// scas es:(%rax,%rax,1)
, 999		// scas es:(%rax,%rax,1)
, 999		// scas es:(%rax,%rax,1)
, 999		// scas es:(%rax,%rax,1)
, 999		// scasb 
, 999		// scasl 
, 999		// scasq 
, 999		// scasw 
, 19		// seta (%rsi)
, 19		// seta %dl
, 19		// seta %ch
, 19		// setae (%rsi)
, 18		// setae %dl
, 18		// setae %ch
, 19		// setb (%rsi)
, 18		// setb %dl
, 18		// setb %ch
, 19		// setbe (%rsi)
, 19		// setbe %dl
, 19		// setbe %ch
, 19		// setc (%rsi)
, 18		// setc %dl
, 18		// setc %ch
, 19		// sete (%rsi)
, 18		// sete %dl
, 18		// sete %ch
, 19		// setg (%rsi)
, 18		// setg %dl
, 18		// setg %ch
, 19		// setge (%rsi)
, 18		// setge %dl
, 18		// setge %ch
, 19		// setl (%rsi)
, 18		// setl %dl
, 18		// setl %ch
, 19		// setle (%rsi)
, 18		// setle %dl
, 18		// setle %ch
, 19		// setna (%rsi)
, 19		// setna %dl
, 19		// setna %ch
, 19		// setnae (%rsi)
, 18		// setnae %dl
, 18		// setnae %ch
, 19		// setnb (%rsi)
, 18		// setnb %dl
, 18		// setnb %ch
, 19		// setnbe (%rsi)
, 19		// setnbe %dl
, 19		// setnbe %ch
, 19		// setnc (%rsi)
, 18		// setnc %dl
, 18		// setnc %ch
, 19		// setne (%rsi)
, 18		// setne %dl
, 18		// setne %ch
, 19		// setng (%rsi)
, 18		// setng %dl
, 18		// setng %ch
, 19		// setnge (%rsi)
, 18		// setnge %dl
, 18		// setnge %ch
, 19		// setnl (%rsi)
, 18		// setnl %dl
, 18		// setnl %ch
, 19		// setnle (%rsi)
, 18		// setnle %dl
, 18		// setnle %ch
, 19		// setno (%rsi)
, 18		// setno %dl
, 18		// setno %ch
, 19		// setnp (%rsi)
, 18		// setnp %dl
, 18		// setnp %ch
, 19		// setns (%rsi)
, 18		// setns %dl
, 18		// setns %ch
, 19		// setnz (%rsi)
, 18		// setnz %dl
, 18		// setnz %ch
, 19		// seto (%rsi)
, 18		// seto %dl
, 18		// seto %ch
, 19		// setp (%rsi)
, 18		// setp %dl
, 18		// setp %ch
, 19		// setpe (%rsi)
, 18		// setpe %dl
, 18		// setpe %ch
, 19		// setpo (%rsi)
, 18		// setpo %dl
, 18		// setpo %ch
, 19		// sets (%rsi)
, 18		// sets %dl
, 18		// sets %ch
, 19		// setz (%rsi)
, 18		// setz %dl
, 18		// setz %ch
, 147		// sfence 
, 147		// shlw %cl, (%rsi)
, 148		// shlw $0x4, (%rsi)
, 148		// shlw $0x1, (%rsi)
, 147		// shll %cl, (%rsi)
, 148		// shll $0x4, (%rsi)
, 148		// shll $0x1, (%rsi)
, 147		// shlq %cl, (%rsi)
, 148		// shlq $0x4, (%rsi)
, 148		// shlq $0x1, (%rsi)
, 147		// shlb %cl, (%rsi)
, 148		// shlb $0x4, (%rsi)
, 148		// shlb $0x1, (%rsi)
, 44		// shlw %cl, %cx
, 18		// shlw $0x4, %cx
, 18		// shlw $0x1, %cx
, 44		// shll %cl, %edx
, 18		// shll $0x4, %edx
, 18		// shll $0x1, %edx
, 44		// shlq %cl, %rdx
, 18		// shlq $0x4, %rdx
, 18		// shlq $0x1, %rdx
, 44		// shlb %cl, %dl
, 18		// shlb $0x4, %dl
, 18		// shlb $0x1, %dl
, 44		// shlb %cl, %ch
, 18		// shlb $0x4, %ch
, 18		// shlb $0x1, %ch
, 198		// shldw %cl, %cx, (%rsi)
, 198		// shldw $0x4, %cx, (%rsi)
, 198		// shldl %cl, %edx, (%rsi)
, 198		// shldl $0x4, %edx, (%rsi)
, 198		// shldq %cl, %rdx, (%rsi)
, 198		// shldq $0x4, %rdx, (%rsi)
, 95		// shldw %cl, %cx, %cx
, 18		// shldw $0x4, %cx, %cx
, 95		// shldl %cl, %edx, %edx
, 18		// shldl $0x4, %edx, %edx
, 95		// shldq %cl, %rdx, %rdx
, 18		// shldq $0x4, %rdx, %rdx
, 18		// shlxl %edx, (%rsi), %edx
, 18		// shlxl %edx, %edx, %edx
, 18		// shlxq %rdx, (%rsi), %rdx
, 18		// shlxq %rdx, %rdx, %rdx
, 147		// shrw %cl, (%rsi)
, 148		// shrw $0x4, (%rsi)
, 148		// shrw $0x1, (%rsi)
, 147		// shrl %cl, (%rsi)
, 148		// shrl $0x4, (%rsi)
, 148		// shrl $0x1, (%rsi)
, 147		// shrq %cl, (%rsi)
, 148		// shrq $0x4, (%rsi)
, 148		// shrq $0x1, (%rsi)
, 147		// shrb %cl, (%rsi)
, 148		// shrb $0x4, (%rsi)
, 148		// shrb $0x1, (%rsi)
, 44		// shrw %cl, %cx
, 18		// shrw $0x4, %cx
, 18		// shrw $0x1, %cx
, 44		// shrl %cl, %edx
, 18		// shrl $0x4, %edx
, 18		// shrl $0x1, %edx
, 44		// shrq %cl, %rdx
, 18		// shrq $0x4, %rdx
, 18		// shrq $0x1, %rdx
, 44		// shrb %cl, %dl
, 18		// shrb $0x4, %dl
, 18		// shrb $0x1, %dl
, 44		// shrb %cl, %ch
, 18		// shrb $0x4, %ch
, 18		// shrb $0x1, %ch
, 224		// shrdw %cl, %cx, (%rsi)
, 224		// shrdw $0x4, %cx, (%rsi)
, 224		// shrdl %cl, %edx, (%rsi)
, 224		// shrdl $0x4, %edx, (%rsi)
, 224		// shrdq %cl, %rdx, (%rsi)
, 224		// shrdq $0x4, %rdx, (%rsi)
, 95		// shrdw %cl, %cx, %cx
, 18		// shrdw $0x4, %cx, %cx
, 95		// shrdl %cl, %edx, %edx
, 18		// shrdl $0x4, %edx, %edx
, 95		// shrdq %cl, %rdx, %rdx
, 18		// shrdq $0x4, %rdx, %rdx
, 18		// shrxl %edx, (%rsi), %edx
, 18		// shrxl %edx, %edx, %edx
, 18		// shrxq %rdx, (%rsi), %rdx
, 18		// shrxq %rdx, %rdx, %rdx
, 18		// shufpd $0x4, (%rsi), %xmm0
, 18		// shufpd $0x4, %xmm0, %xmm0
, 18		// shufps $0x4, (%rsi), %xmm0
, 18		// shufps $0x4, %xmm0, %xmm0
, 198		// sqrtpd (%rsi), %xmm0
, 250		// sqrtpd %xmm0, %xmm0
, 172		// sqrtps (%rsi), %xmm0
, 250		// sqrtps %xmm0, %xmm0
, 249		// sqrtsd (%rsi), %xmm0
, 250		// sqrtsd %xmm0, %xmm0
, 249		// sqrtss (%rsi), %xmm0
, 250		// sqrtss %xmm0, %xmm0
, 1		// stc 
, 999		// std 
, 999		// sti 
, 19		// stmxcsr (%rsi)
, 999		// stosw es:(%rax,%rax,1)
, 999		// stosl es:(%rax,%rax,1)
, 999		// stosq es:(%rax,%rax,1)
, 999		// stosb es:(%rax,%rax,1)
, 999		// stosb 
, 999		// stosl 
, 999		// stosq 
, 999		// stosw 
, 18		// subb $0x4, %al
, 76		// subw $0x4050, %ax
, 18		// subl $0xc0decafe, %eax
, 162		// subw $0x4050, (%rsi)
, 167		// subw $0x4, (%rsi)
, 149		// subw %cx, (%rsi)
, 162		// subl $0xc0decafe, (%rsi)
, 167		// subl $0x4, (%rsi)
, 149		// subl %edx, (%rsi)
, 163		// subq $0xc0decafe, (%rsi)
, 162		// subq $0x4, (%rsi)
, 149		// subq %rdx, (%rsi)
, 170		// subb $0x4, (%rsi)
, 149		// subb %dl, (%rsi)
, 180		// subb %ch, (%rsi)
, 78		// subw $0x4050, %cx
, 18		// subw $0x4, %cx
, 18		// subw (%rsi), %cx
, 18		// subw %cx, %cx
, 18		// subw %cx, %cx
, 18		// subl $0xc0decafe, %edx
, 18		// subl $0x4, %edx
, 18		// subl (%rsi), %edx
, 1		// subl %edx, %edx
, 1		// subl %edx, %edx
, 18		// subq $0xc0decafe, %rdx
, 18		// subq $0x4, %rdx
, 18		// subq (%rsi), %rdx
, 1		// subq %rdx, %rdx
, 1		// subq %rdx, %rdx
, 18		// subb $0x4, %dl
, 18		// subb (%rsi), %dl
, 18		// subb %dl, %dl
, 18		// subb %dl, %dl
, 43		// subb %ch, %dl
, 43		// subb %ch, %dl
, 18		// subq $0xc0decafe, %rax
, 18		// subb $0x4, %ch
, 18		// subb (%rsi), %ch
, 18		// subb %dl, %ch
, 18		// subb %dl, %ch
, 18		// subb %ch, %ch
, 18		// subb %ch, %ch
, 70		// subpd (%rsi), %xmm0
, 70		// subpd %xmm0, %xmm0
, 70		// subps (%rsi), %xmm0
, 70		// subps %xmm0, %xmm0
, 70		// subsd (%rsi), %xmm0
, 70		// subsd %xmm0, %xmm0
, 70		// subss (%rsi), %xmm0
, 70		// subss %xmm0, %xmm0
, 999		// swapgs 
, 0		// syscall 
, 999		// sysenter 
, 999		// sysexit 
, 999		// sysexit 
, 0		// sysret 
, 0		// sysret 
, 1		// testb $0x4, %al
, 76		// testw $0x4050, %ax
, 0		// testl $0xc0decafe, %eax
, 40		// testw $0x4050, (%rsi)
, 6		// testw %cx, (%rsi)
, 9		// testl $0xc0decafe, (%rsi)
, 6		// testl %edx, (%rsi)
, 9		// testq $0xc0decafe, (%rsi)
, 6		// testq %rdx, (%rsi)
, 6		// testb $0x4, (%rsi)
, 6		// testb %dl, (%rsi)
, 6		// testb %ch, (%rsi)
, 78		// testw $0x4050, %cx
, 1		// testw %cx, %cx
, 0		// testl $0xc0decafe, %edx
, 1		// testl %edx, %edx
, 2		// testq $0xc0decafe, %rdx
, 1		// testq %rdx, %rdx
, 0		// testb $0x4, %dl
, 1		// testb %dl, %dl
, 0		// testb %ch, %dl
, 1		// testq $0xc0decafe, %rax
, 1		// testb $0x4, %ch
, 0		// testb %dl, %ch
, 1		// testb %ch, %ch
, 67		// tzcntw (%rsi), %cx
, 67		// tzcntw %cx, %cx
, 67		// tzcntl (%rsi), %edx
, 67		// tzcntl %edx, %edx
, 67		// tzcntq (%rsi), %rdx
, 67		// tzcntq %rdx, %rdx
, 18		// ucomisd (%rsi), %xmm0
, 18		// ucomisd %xmm0, %xmm0
, 19		// ucomiss (%rsi), %xmm0
, 18		// ucomiss %xmm0, %xmm0
, 999		// ud2 
, 18		// unpckhpd (%rsi), %xmm0
, 18		// unpckhpd %xmm0, %xmm0
, 18		// unpckhps (%rsi), %xmm0
, 18		// unpckhps %xmm0, %xmm0
, 18		// unpcklpd (%rsi), %xmm0
, 18		// unpcklpd %xmm0, %xmm0
, 18		// unpcklps (%rsi), %xmm0
, 18		// unpcklps %xmm0, %xmm0
, 70		// vaddpd (%rsi), %xmm0, %xmm0
, 70		// vaddpd %xmm0, %xmm0, %xmm0
, 78		// vaddpd (%rsi), %ymm0, %ymm0
, 78		// vaddpd %ymm0, %ymm0, %ymm0
, 70		// vaddps (%rsi), %xmm0, %xmm0
, 70		// vaddps %xmm0, %xmm0, %xmm0
, 78		// vaddps (%rsi), %ymm0, %ymm0
, 78		// vaddps %ymm0, %ymm0, %ymm0
, 70		// vaddsd (%rsi), %xmm0, %xmm0
, 70		// vaddsd %xmm0, %xmm0, %xmm0
, 70		// vaddss (%rsi), %xmm0, %xmm0
, 70		// vaddss %xmm0, %xmm0, %xmm0
, 70		// vaddsubpd (%rsi), %xmm0, %xmm0
, 70		// vaddsubpd %xmm0, %xmm0, %xmm0
, 78		// vaddsubpd (%rsi), %ymm0, %ymm0
, 78		// vaddsubpd %ymm0, %ymm0, %ymm0
, 70		// vaddsubps (%rsi), %xmm0, %xmm0
, 70		// vaddsubps %xmm0, %xmm0, %xmm0
, 78		// vaddsubps (%rsi), %ymm0, %ymm0
, 78		// vaddsubps %ymm0, %ymm0, %ymm0
, 172		// vaesdec (%rsi), %xmm0, %xmm0
, 172		// vaesdec %xmm0, %xmm0, %xmm0
, 172		// vaesdeclast (%rsi), %xmm0, %xmm0
, 172		// vaesdeclast %xmm0, %xmm0, %xmm0
, 172		// vaesenc (%rsi), %xmm0, %xmm0
, 172		// vaesenc %xmm0, %xmm0, %xmm0
, 172		// vaesenclast (%rsi), %xmm0, %xmm0
, 172		// vaesenclast %xmm0, %xmm0, %xmm0
, 44		// vaesimc (%rsi), %xmm0
, 352		// vaesimc %xmm0, %xmm0
, 194		// vaeskeygenassist $0x4, (%rsi), %xmm0
, 249		// vaeskeygenassist $0x4, %xmm0, %xmm0
, 18		// vandnpd (%rsi), %xmm0, %xmm0
, 18		// vandnpd %xmm0, %xmm0, %xmm0
, 18		// vandnpd (%rsi), %ymm0, %ymm0
, 18		// vandnpd %ymm0, %ymm0, %ymm0
, 18		// vandnps (%rsi), %xmm0, %xmm0
, 18		// vandnps %xmm0, %xmm0, %xmm0
, 18		// vandnps (%rsi), %ymm0, %ymm0
, 18		// vandnps %ymm0, %ymm0, %ymm0
, 18		// vandpd (%rsi), %xmm0, %xmm0
, 18		// vandpd %xmm0, %xmm0, %xmm0
, 18		// vandpd (%rsi), %ymm0, %ymm0
, 18		// vandpd %ymm0, %ymm0, %ymm0
, 18		// vandps (%rsi), %xmm0, %xmm0
, 18		// vandps %xmm0, %xmm0, %xmm0
, 18		// vandps (%rsi), %ymm0, %ymm0
, 18		// vandps %ymm0, %ymm0, %ymm0
, 18		// vblendpd $0x4, (%rsi), %xmm0, %xmm0
, 18		// vblendpd $0x4, %xmm0, %xmm0, %xmm0
, 18		// vblendpd $0x4, (%rsi), %ymm0, %ymm0
, 18		// vblendpd $0x4, %ymm0, %ymm0, %ymm0
, 18		// vblendps $0x4, (%rsi), %xmm0, %xmm0
, 18		// vblendps $0x4, %xmm0, %xmm0, %xmm0
, 18		// vblendps $0x4, (%rsi), %ymm0, %ymm0
, 18		// vblendps $0x4, %ymm0, %ymm0, %ymm0
, 44		// vblendvpd %xmm0, (%rsi), %xmm0, %xmm0
, 44		// vblendvpd %xmm0, %xmm0, %xmm0, %xmm0
, 44		// vblendvpd %ymm0, (%rsi), %ymm0, %ymm0
, 44		// vblendvpd %ymm0, %ymm0, %ymm0, %ymm0
, 44		// vblendvps %xmm0, (%rsi), %xmm0, %xmm0
, 44		// vblendvps %xmm0, %xmm0, %xmm0, %xmm0
, 44		// vblendvps %ymm0, (%rsi), %ymm0, %ymm0
, 44		// vblendvps %ymm0, %ymm0, %ymm0, %ymm0
, 6		// vbroadcastf128 (%rsi), %ymm0
, 6		// vpbroadcasti128 (%rsi), %ymm0
, 6		// vbroadcastsd (%rsi), %ymm0
, 70		// vbroadcastsd %xmm0, %ymm0
, 6		// vbroadcastss (%rsi), %xmm0
, 18		// vbroadcastss %xmm0, %xmm0
, 6		// vbroadcastss (%rsi), %ymm0
, 70		// vbroadcastss %xmm0, %ymm0
, 70		// vcmppd $0x4, (%rsi), %xmm0, %xmm0
, 70		// vcmppd $0x4, %xmm0, %xmm0, %xmm0
, 78		// vcmppd $0x4, (%rsi), %ymm0, %ymm0
, 78		// vcmppd $0x4, %ymm0, %ymm0, %ymm0
, 70		// vcmpps $0x4, (%rsi), %xmm0, %xmm0
, 70		// vcmpps $0x4, %xmm0, %xmm0, %xmm0
, 78		// vcmpps $0x4, (%rsi), %ymm0, %ymm0
, 78		// vcmpps $0x4, %ymm0, %ymm0, %ymm0
, 70		// vcmpsd $0x4, (%rsi), %xmm0, %xmm0
, 70		// vcmpsd $0x4, %xmm0, %xmm0, %xmm0
, 70		// vcmpss $0x4, (%rsi), %xmm0, %xmm0
, 70		// vcmpss $0x4, %xmm0, %xmm0, %xmm0
, 18		// vcomisd (%rsi), %xmm0
, 18		// vcomisd %xmm0, %xmm0
, 18		// vcomiss (%rsi), %xmm0
, 18		// vcomiss %xmm0, %xmm0
, 18		// vcvtdq2pd (%rsi), %xmm0
, 95		// vcvtdq2pd %xmm0, %xmm0
, 21		// vcvtdq2pd (%rsi), %ymm0
, 164		// vcvtdq2pd %ymm0, %ymm0
, 18		// vcvtdq2ps (%rsi), %xmm0
, 70		// vcvtdq2ps %xmm0, %xmm0
, 21		// vcvtdq2ps (%rsi), %ymm0
, 78		// vcvtdq2ps %ymm0, %ymm0
, 19		// vcvtpd2dqx (%rsi), %xmm0
, 22		// vcvtpd2dq (%rsi), %xmm0
, 95		// vcvtpd2dqx %xmm0, %xmm0
, 164		// vcvtpd2dq %ymm0, %xmm0
, 19		// vcvtpd2ps (%rsi), %xmm0
, 22		// vcvtpd2ps (%rsi), %xmm0
, 95		// vcvtpd2ps %xmm0, %xmm0
, 164		// vcvtpd2ps %ymm0, %xmm0
, 18		// vcvtph2ps (%rsi), %xmm0
, 95		// vcvtph2ps %xmm0, %xmm0
, 21		// vcvtph2ps (%rsi), %ymm0
, 164		// vcvtph2ps %xmm0, %ymm0
, 18		// vcvtps2dq (%rsi), %xmm0
, 70		// vcvtps2dq %xmm0, %xmm0
, 21		// vcvtps2dq (%rsi), %ymm0
, 78		// vcvtps2dq %ymm0, %ymm0
, 18		// vcvtps2pd (%rsi), %xmm0
, 44		// vcvtps2pd %xmm0, %xmm0
, 21		// vcvtps2pd (%rsi), %ymm0
, 107		// vcvtps2pd %xmm0, %ymm0
, 22		// vcvtps2ph $0x4, %ymm0, (%rsi)
, 19		// vcvtps2ph $0x4, %xmm0, (%rsi)
, 95		// vcvtps2ph $0x4, %xmm0, %xmm0
, 164		// vcvtps2ph $0x4, %ymm0, %xmm0
, 19		// vcvtsd2sil (%rsi), %edx
, 18		// vcvtsd2sil %xmm0, %edx
, 19		// vcvtsd2siq (%rsi), %rdx
, 18		// vcvtsd2siq %xmm0, %rdx
, 19		// vcvtsd2ss (%rsi), %xmm0, %xmm0
, 95		// vcvtsd2ss %xmm0, %xmm0, %xmm0
, 70		// vcvtsi2sdl (%rsi), %xmm0, %xmm0
, 70		// vcvtsi2sdq (%rsi), %xmm0, %xmm0
, 70		// vcvtsi2sdl %edx, %xmm0, %xmm0
, 70		// vcvtsi2sdq %rdx, %xmm0, %xmm0
, 70		// vcvtsi2ssl (%rsi), %xmm0, %xmm0
, 19		// vcvtsi2ssq (%rsi), %xmm0, %xmm0
, 70		// vcvtsi2ssl %edx, %xmm0, %xmm0
, 44		// vcvtsi2ssq %rdx, %xmm0, %xmm0
, 18		// vcvtss2sd (%rsi), %xmm0, %xmm0
, 44		// vcvtss2sd %xmm0, %xmm0, %xmm0
, 19		// vcvtss2sil (%rsi), %edx
, 18		// vcvtss2sil %xmm0, %edx
, 19		// vcvtss2siq (%rsi), %rdx
, 18		// vcvtss2siq %xmm0, %rdx
, 19		// vcvttpd2dq (%rsi), %xmm0
, 22		// vcvttpd2dq (%rsi), %xmm0
, 95		// vcvttpd2dq %xmm0, %xmm0
, 164		// vcvttpd2dq %ymm0, %xmm0
, 18		// vcvttps2dq (%rsi), %xmm0
, 70		// vcvttps2dq %xmm0, %xmm0
, 21		// vcvttps2dq (%rsi), %ymm0
, 78		// vcvttps2dq %ymm0, %ymm0
, 19		// vcvttsd2si (%rsi), %edx
, 18		// vcvttsd2si %xmm0, %edx
, 19		// vcvttsd2siq (%rsi), %rdx
, 18		// vcvttsd2siq %xmm0, %rdx
, 19		// vcvttss2si (%rsi), %edx
, 18		// vcvttss2si %xmm0, %edx
, 19		// vcvttss2siq (%rsi), %rdx
, 18		// vcvttss2siq %xmm0, %rdx
, 249		// vdivpd (%rsi), %xmm0, %xmm0
, 250		// vdivpd %xmm0, %xmm0, %xmm0
, 488		// vdivpd (%rsi), %ymm0, %ymm0
, 482		// vdivpd %ymm0, %ymm0, %ymm0
, 249		// vdivps (%rsi), %xmm0, %xmm0
, 250		// vdivps %xmm0, %xmm0, %xmm0
, 460		// vdivps (%rsi), %ymm0, %ymm0
, 457		// vdivps %ymm0, %ymm0, %ymm0
, 249		// vdivsd (%rsi), %xmm0, %xmm0
, 250		// vdivsd %xmm0, %xmm0, %xmm0
, 249		// vdivss (%rsi), %xmm0, %xmm0
, 250		// vdivss %xmm0, %xmm0, %xmm0
, 224		// vdppd $0x4, (%rsi), %xmm0, %xmm0
, 224		// vdppd $0x4, %xmm0, %xmm0, %xmm0
, 352		// vdpps $0x4, (%rsi), %xmm0, %xmm0
, 352		// vdpps $0x4, %xmm0, %xmm0, %xmm0
, 393		// vdpps $0x4, (%rsi), %ymm0, %ymm0
, 393		// vdpps $0x4, %ymm0, %ymm0, %ymm0
, 1725		// verr (%rsi)
, 3584		// verr %cx
, 1721		// verw (%rsi)
, 3584		// verw %cx
, 19		// vextractf128 $0x4, %ymm0, (%rsi)
, 70		// vextractf128 $0x4, %ymm0, %xmm0
, 70		// vextracti128 $0x4, %ymm0, (%rsi)
, 70		// vextracti128 $0x4, %ymm0, %xmm0
, 19		// vextractps $0x4, %xmm0, (%rsi)
, 18		// vextractps $0x4, %xmm0, %edx
, 136		// vfmadd132pd (%rsi), %xmm0, %xmm0
, 136		// vfmadd132pd %xmm0, %xmm0, %xmm0
, 136		// vfmadd132pd (%rsi), %ymm0, %ymm0
, 136		// vfmadd132pd %ymm0, %ymm0, %ymm0
, 136		// vfmadd132ps (%rsi), %xmm0, %xmm0
, 136		// vfmadd132ps %xmm0, %xmm0, %xmm0
, 136		// vfmadd132ps (%rsi), %ymm0, %ymm0
, 136		// vfmadd132ps %ymm0, %ymm0, %ymm0
, 121		// vfmadd132sd (%rsi), %xmm0, %xmm0
, 121		// vfmadd132sd %xmm0, %xmm0, %xmm0
, 121		// vfmadd132ss (%rsi), %xmm0, %xmm0
, 121		// vfmadd132ss %xmm0, %xmm0, %xmm0
, 136		// vfmadd213pd (%rsi), %xmm0, %xmm0
, 136		// vfmadd213pd %xmm0, %xmm0, %xmm0
, 136		// vfmadd213pd (%rsi), %ymm0, %ymm0
, 136		// vfmadd213pd %ymm0, %ymm0, %ymm0
, 136		// vfmadd213ps (%rsi), %xmm0, %xmm0
, 136		// vfmadd213ps %xmm0, %xmm0, %xmm0
, 136		// vfmadd213ps (%rsi), %ymm0, %ymm0
, 136		// vfmadd213ps %ymm0, %ymm0, %ymm0
, 121		// vfmadd213sd (%rsi), %xmm0, %xmm0
, 121		// vfmadd213sd %xmm0, %xmm0, %xmm0
, 121		// vfmadd213ss (%rsi), %xmm0, %xmm0
, 121		// vfmadd213ss %xmm0, %xmm0, %xmm0
, 136		// vfmadd231pd (%rsi), %xmm0, %xmm0
, 136		// vfmadd231pd %xmm0, %xmm0, %xmm0
, 136		// vfmadd231pd (%rsi), %ymm0, %ymm0
, 136		// vfmadd231pd %ymm0, %ymm0, %ymm0
, 136		// vfmadd231ps (%rsi), %xmm0, %xmm0
, 136		// vfmadd231ps %xmm0, %xmm0, %xmm0
, 136		// vfmadd231ps (%rsi), %ymm0, %ymm0
, 136		// vfmadd231ps %ymm0, %ymm0, %ymm0
, 121		// vfmadd231sd (%rsi), %xmm0, %xmm0
, 121		// vfmadd231sd %xmm0, %xmm0, %xmm0
, 121		// vfmadd231ss (%rsi), %xmm0, %xmm0
, 121		// vfmadd231ss %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub132pd (%rsi), %xmm0, %xmm0
, 136		// vfmaddsub132pd %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub132pd (%rsi), %ymm0, %ymm0
, 136		// vfmaddsub132pd %ymm0, %ymm0, %ymm0
, 136		// vfmaddsub132ps (%rsi), %xmm0, %xmm0
, 136		// vfmaddsub132ps %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub132ps (%rsi), %ymm0, %ymm0
, 136		// vfmaddsub132ps %ymm0, %ymm0, %ymm0
, 136		// vfmaddsub213pd (%rsi), %xmm0, %xmm0
, 136		// vfmaddsub213pd %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub213pd (%rsi), %ymm0, %ymm0
, 136		// vfmaddsub213pd %ymm0, %ymm0, %ymm0
, 136		// vfmaddsub213ps (%rsi), %xmm0, %xmm0
, 136		// vfmaddsub213ps %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub213ps (%rsi), %ymm0, %ymm0
, 136		// vfmaddsub213ps %ymm0, %ymm0, %ymm0
, 136		// vfmaddsub231pd (%rsi), %xmm0, %xmm0
, 136		// vfmaddsub231pd %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub231pd (%rsi), %ymm0, %ymm0
, 136		// vfmaddsub231pd %ymm0, %ymm0, %ymm0
, 136		// vfmaddsub231ps (%rsi), %xmm0, %xmm0
, 136		// vfmaddsub231ps %xmm0, %xmm0, %xmm0
, 136		// vfmaddsub231ps (%rsi), %ymm0, %ymm0
, 136		// vfmaddsub231ps %ymm0, %ymm0, %ymm0
, 136		// vfmsub132pd (%rsi), %xmm0, %xmm0
, 136		// vfmsub132pd %xmm0, %xmm0, %xmm0
, 136		// vfmsub132pd (%rsi), %ymm0, %ymm0
, 136		// vfmsub132pd %ymm0, %ymm0, %ymm0
, 136		// vfmsub132ps (%rsi), %xmm0, %xmm0
, 136		// vfmsub132ps %xmm0, %xmm0, %xmm0
, 136		// vfmsub132ps (%rsi), %ymm0, %ymm0
, 136		// vfmsub132ps %ymm0, %ymm0, %ymm0
, 121		// vfmsub132sd (%rsi), %xmm0, %xmm0
, 121		// vfmsub132sd %xmm0, %xmm0, %xmm0
, 121		// vfmsub132ss (%rsi), %xmm0, %xmm0
, 121		// vfmsub132ss %xmm0, %xmm0, %xmm0
, 136		// vfmsub213pd (%rsi), %xmm0, %xmm0
, 136		// vfmsub213pd %xmm0, %xmm0, %xmm0
, 136		// vfmsub213pd (%rsi), %ymm0, %ymm0
, 136		// vfmsub213pd %ymm0, %ymm0, %ymm0
, 136		// vfmsub213ps (%rsi), %xmm0, %xmm0
, 136		// vfmsub213ps %xmm0, %xmm0, %xmm0
, 136		// vfmsub213ps (%rsi), %ymm0, %ymm0
, 136		// vfmsub213ps %ymm0, %ymm0, %ymm0
, 121		// vfmsub213sd (%rsi), %xmm0, %xmm0
, 121		// vfmsub213sd %xmm0, %xmm0, %xmm0
, 121		// vfmsub213ss (%rsi), %xmm0, %xmm0
, 121		// vfmsub213ss %xmm0, %xmm0, %xmm0
, 136		// vfmsub231pd (%rsi), %xmm0, %xmm0
, 136		// vfmsub231pd %xmm0, %xmm0, %xmm0
, 136		// vfmsub231pd (%rsi), %ymm0, %ymm0
, 136		// vfmsub231pd %ymm0, %ymm0, %ymm0
, 136		// vfmsub231ps (%rsi), %xmm0, %xmm0
, 136		// vfmsub231ps %xmm0, %xmm0, %xmm0
, 136		// vfmsub231ps (%rsi), %ymm0, %ymm0
, 136		// vfmsub231ps %ymm0, %ymm0, %ymm0
, 121		// vfmsub231sd (%rsi), %xmm0, %xmm0
, 121		// vfmsub231sd %xmm0, %xmm0, %xmm0
, 121		// vfmsub231ss (%rsi), %xmm0, %xmm0
, 121		// vfmsub231ss %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd132pd (%rsi), %xmm0, %xmm0
, 136		// vfmsubadd132pd %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd132pd (%rsi), %ymm0, %ymm0
, 136		// vfmsubadd132pd %ymm0, %ymm0, %ymm0
, 136		// vfmsubadd132ps (%rsi), %xmm0, %xmm0
, 136		// vfmsubadd132ps %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd132ps (%rsi), %ymm0, %ymm0
, 136		// vfmsubadd132ps %ymm0, %ymm0, %ymm0
, 136		// vfmsubadd213pd (%rsi), %xmm0, %xmm0
, 136		// vfmsubadd213pd %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd213pd (%rsi), %ymm0, %ymm0
, 136		// vfmsubadd213pd %ymm0, %ymm0, %ymm0
, 136		// vfmsubadd213ps (%rsi), %xmm0, %xmm0
, 136		// vfmsubadd213ps %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd213ps (%rsi), %ymm0, %ymm0
, 136		// vfmsubadd213ps %ymm0, %ymm0, %ymm0
, 136		// vfmsubadd231pd (%rsi), %xmm0, %xmm0
, 136		// vfmsubadd231pd %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd231pd (%rsi), %ymm0, %ymm0
, 136		// vfmsubadd231pd %ymm0, %ymm0, %ymm0
, 136		// vfmsubadd231ps (%rsi), %xmm0, %xmm0
, 136		// vfmsubadd231ps %xmm0, %xmm0, %xmm0
, 136		// vfmsubadd231ps (%rsi), %ymm0, %ymm0
, 136		// vfmsubadd231ps %ymm0, %ymm0, %ymm0
, 136		// vfnmadd132pd (%rsi), %xmm0, %xmm0
, 136		// vfnmadd132pd %xmm0, %xmm0, %xmm0
, 136		// vfnmadd132pd (%rsi), %ymm0, %ymm0
, 136		// vfnmadd132pd %ymm0, %ymm0, %ymm0
, 136		// vfnmadd132ps (%rsi), %xmm0, %xmm0
, 136		// vfnmadd132ps %xmm0, %xmm0, %xmm0
, 136		// vfnmadd132ps (%rsi), %ymm0, %ymm0
, 136		// vfnmadd132ps %ymm0, %ymm0, %ymm0
, 121		// vfnmadd132sd (%rsi), %xmm0, %xmm0
, 121		// vfnmadd132sd %xmm0, %xmm0, %xmm0
, 121		// vfnmadd132ss (%rsi), %xmm0, %xmm0
, 121		// vfnmadd132ss %xmm0, %xmm0, %xmm0
, 136		// vfnmadd213pd (%rsi), %xmm0, %xmm0
, 136		// vfnmadd213pd %xmm0, %xmm0, %xmm0
, 136		// vfnmadd213pd (%rsi), %ymm0, %ymm0
, 136		// vfnmadd213pd %ymm0, %ymm0, %ymm0
, 136		// vfnmadd213ps (%rsi), %xmm0, %xmm0
, 136		// vfnmadd213ps %xmm0, %xmm0, %xmm0
, 136		// vfnmadd213ps (%rsi), %ymm0, %ymm0
, 136		// vfnmadd213ps %ymm0, %ymm0, %ymm0
, 121		// vfnmadd213sd (%rsi), %xmm0, %xmm0
, 121		// vfnmadd213sd %xmm0, %xmm0, %xmm0
, 121		// vfnmadd213ss (%rsi), %xmm0, %xmm0
, 121		// vfnmadd213ss %xmm0, %xmm0, %xmm0
, 136		// vfnmadd231pd (%rsi), %xmm0, %xmm0
, 136		// vfnmadd231pd %xmm0, %xmm0, %xmm0
, 136		// vfnmadd231pd (%rsi), %ymm0, %ymm0
, 136		// vfnmadd231pd %ymm0, %ymm0, %ymm0
, 136		// vfnmadd231ps (%rsi), %xmm0, %xmm0
, 136		// vfnmadd231ps %xmm0, %xmm0, %xmm0
, 136		// vfnmadd231ps (%rsi), %ymm0, %ymm0
, 136		// vfnmadd231ps %ymm0, %ymm0, %ymm0
, 121		// vfnmadd231sd (%rsi), %xmm0, %xmm0
, 121		// vfnmadd231sd %xmm0, %xmm0, %xmm0
, 121		// vfnmadd231ss (%rsi), %xmm0, %xmm0
, 121		// vfnmadd231ss %xmm0, %xmm0, %xmm0
, 136		// vfnmsub132pd (%rsi), %xmm0, %xmm0
, 136		// vfnmsub132pd %xmm0, %xmm0, %xmm0
, 136		// vfnmsub132pd (%rsi), %ymm0, %ymm0
, 136		// vfnmsub132pd %ymm0, %ymm0, %ymm0
, 136		// vfnmsub132ps (%rsi), %xmm0, %xmm0
, 136		// vfnmsub132ps %xmm0, %xmm0, %xmm0
, 136		// vfnmsub132ps (%rsi), %ymm0, %ymm0
, 136		// vfnmsub132ps %ymm0, %ymm0, %ymm0
, 121		// vfnmsub132sd (%rsi), %xmm0, %xmm0
, 121		// vfnmsub132sd %xmm0, %xmm0, %xmm0
, 121		// vfnmsub132ss (%rsi), %xmm0, %xmm0
, 121		// vfnmsub132ss %xmm0, %xmm0, %xmm0
, 136		// vfnmsub213pd (%rsi), %xmm0, %xmm0
, 136		// vfnmsub213pd %xmm0, %xmm0, %xmm0
, 136		// vfnmsub213pd (%rsi), %ymm0, %ymm0
, 136		// vfnmsub213pd %ymm0, %ymm0, %ymm0
, 136		// vfnmsub213ps (%rsi), %xmm0, %xmm0
, 136		// vfnmsub213ps %xmm0, %xmm0, %xmm0
, 136		// vfnmsub213ps (%rsi), %ymm0, %ymm0
, 136		// vfnmsub213ps %ymm0, %ymm0, %ymm0
, 121		// vfnmsub213sd (%rsi), %xmm0, %xmm0
, 121		// vfnmsub213sd %xmm0, %xmm0, %xmm0
, 121		// vfnmsub213ss (%rsi), %xmm0, %xmm0
, 121		// vfnmsub213ss %xmm0, %xmm0, %xmm0
, 136		// vfnmsub231pd (%rsi), %xmm0, %xmm0
, 3751		// vfnmsub231pd %xmm0, %xmm0, %xmm0
, 136		// vfnmsub231pd (%rsi), %ymm0, %ymm0
, 3751		// vfnmsub231pd %ymm0, %ymm0, %ymm0
, 136		// vfnmsub231ps (%rsi), %xmm0, %xmm0
, 136		// vfnmsub231ps %xmm0, %xmm0, %xmm0
, 136		// vfnmsub231ps (%rsi), %ymm0, %ymm0
, 136		// vfnmsub231ps %ymm0, %ymm0, %ymm0
, 121		// vfnmsub231sd (%rsi), %xmm0, %xmm0
, 3657		// vfnmsub231sd %xmm0, %xmm0, %xmm0
, 121		// vfnmsub231ss (%rsi), %xmm0, %xmm0
, 121		// vfnmsub231ss %xmm0, %xmm0, %xmm0
, 999		// vgatherdpd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherdpd %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vgatherdps %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherdps %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vgatherqpd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherqpd %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vgatherqps %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vgatherqps %xmm0, es:(%rax,%rax,1), %xmm0
, 121		// vhaddpd (%rsi), %xmm0, %xmm0
, 121		// vhaddpd %xmm0, %xmm0, %xmm0
, 136		// vhaddpd (%rsi), %ymm0, %ymm0
, 136		// vhaddpd %ymm0, %ymm0, %ymm0
, 121		// vhaddps (%rsi), %xmm0, %xmm0
, 121		// vhaddps %xmm0, %xmm0, %xmm0
, 136		// vhaddps (%rsi), %ymm0, %ymm0
, 136		// vhaddps %ymm0, %ymm0, %ymm0
, 121		// vhsubpd (%rsi), %xmm0, %xmm0
, 121		// vhsubpd %xmm0, %xmm0, %xmm0
, 136		// vhsubpd (%rsi), %ymm0, %ymm0
, 136		// vhsubpd %ymm0, %ymm0, %ymm0
, 121		// vhsubps (%rsi), %xmm0, %xmm0
, 121		// vhsubps %xmm0, %xmm0, %xmm0
, 136		// vhsubps (%rsi), %ymm0, %ymm0
, 136		// vhsubps %ymm0, %ymm0, %ymm0
, 18		// vinsertf128 $0x4, (%rsi), %ymm0, %ymm0
, 70		// vinsertf128 $0x4, %xmm0, %ymm0, %ymm0
, 18		// vinserti128 $0x4, (%rsi), %ymm0, %ymm0
, 70		// vinserti128 $0x4, %xmm0, %ymm0, %ymm0
, 18		// vinsertps $0x4, (%rsi), %xmm0, %xmm0
, 18		// vinsertps $0x4, %xmm0, %xmm0, %xmm0
, 6		// vlddqu (%rsi), %xmm0
, 6		// vlddqu (%rsi), %ymm0
, 52		// vldmxcsr (%rsi)
, 147		// vmaskmovdqu %xmm0, %xmm0
, 19		// vmaskmovpd %xmm0, %xmm0, (%rsi)
, 19		// vmaskmovpd %ymm0, %ymm0, (%rsi)
, 44		// vmaskmovpd (%rsi), %xmm0, %xmm0
, 44		// vmaskmovpd (%rsi), %ymm0, %ymm0
, 19		// vmaskmovps %xmm0, %xmm0, (%rsi)
, 19		// vmaskmovps %ymm0, %ymm0, (%rsi)
, 44		// vmaskmovps (%rsi), %xmm0, %xmm0
, 44		// vmaskmovps (%rsi), %ymm0, %ymm0
, 70		// vmaxpd (%rsi), %xmm0, %xmm0
, 70		// vmaxpd %xmm0, %xmm0, %xmm0
, 78		// vmaxpd (%rsi), %ymm0, %ymm0
, 78		// vmaxpd %ymm0, %ymm0, %ymm0
, 70		// vmaxps (%rsi), %xmm0, %xmm0
, 70		// vmaxps %xmm0, %xmm0, %xmm0
, 78		// vmaxps (%rsi), %ymm0, %ymm0
, 78		// vmaxps %ymm0, %ymm0, %ymm0
, 70		// vmaxsd (%rsi), %xmm0, %xmm0
, 70		// vmaxsd %xmm0, %xmm0, %xmm0
, 70		// vmaxss (%rsi), %xmm0, %xmm0
, 70		// vmaxss %xmm0, %xmm0, %xmm0
, 70		// vminpd (%rsi), %xmm0, %xmm0
, 70		// vminpd %xmm0, %xmm0, %xmm0
, 78		// vminpd (%rsi), %ymm0, %ymm0
, 78		// vminpd %ymm0, %ymm0, %ymm0
, 70		// vminps (%rsi), %xmm0, %xmm0
, 70		// vminps %xmm0, %xmm0, %xmm0
, 78		// vminps (%rsi), %ymm0, %ymm0
, 78		// vminps %ymm0, %ymm0, %ymm0
, 70		// vminsd (%rsi), %xmm0, %xmm0
, 70		// vminsd %xmm0, %xmm0, %xmm0
, 70		// vminss (%rsi), %xmm0, %xmm0
, 70		// vminss %xmm0, %xmm0, %xmm0
, 19		// vmovapd %xmm0, (%rsi)
, 19		// vmovapd %ymm0, (%rsi)
, 6		// vmovapd (%rsi), %xmm0
, 18		// vmovapd %xmm0, %xmm0
, 18		// vmovapd %xmm0, %xmm0
, 6		// vmovapd (%rsi), %ymm0
, 18		// vmovapd %ymm0, %ymm0
, 18		// vmovapd %ymm0, %ymm0
, 19		// vmovaps %xmm0, (%rsi)
, 19		// vmovaps %ymm0, (%rsi)
, 6		// vmovaps (%rsi), %xmm0
, 18		// vmovaps %xmm0, %xmm0
, 18		// vmovaps %xmm0, %xmm0
, 6		// vmovaps (%rsi), %ymm0
, 18		// vmovaps %ymm0, %ymm0
, 18		// vmovaps %ymm0, %ymm0
, 19		// vmovd %xmm0, (%rsi)
, 18		// vmovd %xmm0, %edx
, 6		// vmovd (%rsi), %xmm0
, 18		// vmovd %edx, %xmm0
, 6		// vmovddup (%rsi), %xmm0
, 18		// vmovddup %xmm0, %xmm0
, 6		// vmovddup (%rsi), %ymm0
, 18		// vmovddup %ymm0, %ymm0
, 19		// vmovdqa %xmm0, (%rsi)
, 19		// vmovdqa %ymm0, (%rsi)
, 6		// vmovdqa (%rsi), %xmm0
, 18		// vmovdqa %xmm0, %xmm0
, 18		// vmovdqa %xmm0, %xmm0
, 6		// vmovdqa (%rsi), %ymm0
, 18		// vmovdqa %ymm0, %ymm0
, 18		// vmovdqa %ymm0, %ymm0
, 19		// vmovdqu %xmm0, (%rsi)
, 19		// vmovdqu %ymm0, (%rsi)
, 6		// vmovdqu (%rsi), %xmm0
, 18		// vmovdqu %xmm0, %xmm0
, 18		// vmovdqu %xmm0, %xmm0
, 6		// vmovdqu (%rsi), %ymm0
, 18		// vmovdqu %ymm0, %ymm0
, 18		// vmovdqu %ymm0, %ymm0
, 18		// vmovhlps %xmm0, %xmm0, %xmm0
, 19		// vmovhpd %xmm0, (%rsi)
, 18		// vmovhpd (%rsi), %xmm0, %xmm0
, 19		// vmovhps %xmm0, (%rsi)
, 18		// vmovhps (%rsi), %xmm0, %xmm0
, 18		// vmovlhps %xmm0, %xmm0, %xmm0
, 19		// vmovlpd %xmm0, (%rsi)
, 18		// vmovlpd (%rsi), %xmm0, %xmm0
, 19		// vmovlps %xmm0, (%rsi)
, 18		// vmovlps (%rsi), %xmm0, %xmm0
, 18		// vmovmskpd %xmm0, %edx
, 18		// vmovmskpd %ymm0, %edx
, 18		// vmovmskpd %xmm0, %rdx
, 18		// vmovmskpd %ymm0, %rdx
, 18		// vmovmskps %xmm0, %edx
, 18		// vmovmskps %ymm0, %edx
, 18		// vmovmskps %xmm0, %rdx
, 18		// vmovmskps %ymm0, %rdx
, 19		// vmovntdqa %xmm0, (%rsi)
, 12		// vmovntdqa (%rsi), %xmm0
, 12		// vmovntdqa (%rsi), %ymm0
, 19		// vmovntpd %xmm0, (%rsi)
, 19		// vmovntpd %ymm0, (%rsi)
, 19		// vmovntps %xmm0, (%rsi)
, 19		// vmovntps %ymm0, (%rsi)
, 19		// vmovq %xmm0, (%rsi)
, 19		// vmovq %xmm0, (%rsi)
, 18		// vmovq %xmm0, %rdx
, 6		// vmovq (%rsi), %xmm0
, 6		// vmovq (%rsi), %xmm0
, 18		// vmovq %rdx, %xmm0
, 18		// vmovq %xmm0, %xmm0
, 18		// vmovq %xmm0, %xmm0
, 19		// vmovsd %xmm0, (%rsi)
, 6		// vmovsd (%rsi), %xmm0
, 18		// vmovsd %xmm0, %xmm0, %xmm0
, 18		// vmovsd %xmm0, %xmm0, %xmm0
, 6		// vmovshdup (%rsi), %xmm0
, 18		// vmovshdup %xmm0, %xmm0
, 6		// vmovshdup (%rsi), %ymm0
, 18		// vmovshdup %ymm0, %ymm0
, 6		// vmovsldup (%rsi), %xmm0
, 18		// vmovsldup %xmm0, %xmm0
, 6		// vmovsldup (%rsi), %ymm0
, 18		// vmovsldup %ymm0, %ymm0
, 19		// vmovss %xmm0, (%rsi)
, 6		// vmovss (%rsi), %xmm0
, 18		// vmovss %xmm0, %xmm0, %xmm0
, 18		// vmovss %xmm0, %xmm0, %xmm0
, 19		// vmovupd %xmm0, (%rsi)
, 19		// vmovupd %ymm0, (%rsi)
, 6		// vmovupd (%rsi), %xmm0
, 18		// vmovupd %xmm0, %xmm0
, 18		// vmovupd %xmm0, %xmm0
, 6		// vmovupd (%rsi), %ymm0
, 18		// vmovupd %ymm0, %ymm0
, 18		// vmovupd %ymm0, %ymm0
, 19		// vmovups %xmm0, (%rsi)
, 19		// vmovups %ymm0, (%rsi)
, 6		// vmovups (%rsi), %xmm0
, 18		// vmovups %xmm0, %xmm0
, 18		// vmovups %xmm0, %xmm0
, 6		// vmovups (%rsi), %ymm0
, 18		// vmovups %ymm0, %ymm0
, 18		// vmovups %ymm0, %ymm0
, 147		// vmpsadbw $0x4, (%rsi), %xmm0, %xmm0
, 172		// vmpsadbw $0x4, %xmm0, %xmm0, %xmm0
, 147		// vmpsadbw $0x4, (%rsi), %ymm0, %ymm0
, 172		// vmpsadbw $0x4, %ymm0, %ymm0, %ymm0
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
, 18		// vorpd (%rsi), %xmm0, %xmm0
, 18		// vorpd %xmm0, %xmm0, %xmm0
, 18		// vorpd (%rsi), %ymm0, %ymm0
, 18		// vorpd %ymm0, %ymm0, %ymm0
, 18		// vorps (%rsi), %xmm0, %xmm0
, 18		// vorps %xmm0, %xmm0, %xmm0
, 18		// vorps (%rsi), %ymm0, %ymm0
, 18		// vorps %ymm0, %ymm0, %ymm0
, 6		// vpabsb (%rsi), %xmm0
, 18		// vpabsb %xmm0, %xmm0
, 6		// vpabsb (%rsi), %ymm0
, 18		// vpabsb %ymm0, %ymm0
, 6		// vpabsd (%rsi), %xmm0
, 18		// vpabsd %xmm0, %xmm0
, 6		// vpabsd (%rsi), %ymm0
, 18		// vpabsd %ymm0, %ymm0
, 6		// vpabsw (%rsi), %xmm0
, 18		// vpabsw %xmm0, %xmm0
, 6		// vpabsw (%rsi), %ymm0
, 18		// vpabsw %ymm0, %ymm0
, 18		// vpackssdw (%rsi), %xmm0, %xmm0
, 18		// vpackssdw %xmm0, %xmm0, %xmm0
, 18		// vpackssdw (%rsi), %ymm0, %ymm0
, 18		// vpackssdw %ymm0, %ymm0, %ymm0
, 18		// vpacksswb (%rsi), %xmm0, %xmm0
, 18		// vpacksswb %xmm0, %xmm0, %xmm0
, 18		// vpacksswb (%rsi), %ymm0, %ymm0
, 18		// vpacksswb %ymm0, %ymm0, %ymm0
, 18		// vpackusdw (%rsi), %xmm0, %xmm0
, 18		// vpackusdw %xmm0, %xmm0, %xmm0
, 18		// vpackusdw (%rsi), %ymm0, %ymm0
, 18		// vpackusdw %ymm0, %ymm0, %ymm0
, 18		// vpackuswb (%rsi), %xmm0, %xmm0
, 18		// vpackuswb %xmm0, %xmm0, %xmm0
, 18		// vpackuswb (%rsi), %ymm0, %ymm0
, 18		// vpackuswb %ymm0, %ymm0, %ymm0
, 18		// vpaddb (%rsi), %xmm0, %xmm0
, 18		// vpaddb %xmm0, %xmm0, %xmm0
, 18		// vpaddb (%rsi), %ymm0, %ymm0
, 18		// vpaddb %ymm0, %ymm0, %ymm0
, 18		// vpaddd (%rsi), %xmm0, %xmm0
, 18		// vpaddd %xmm0, %xmm0, %xmm0
, 18		// vpaddd (%rsi), %ymm0, %ymm0
, 18		// vpaddd %ymm0, %ymm0, %ymm0
, 18		// vpaddq (%rsi), %xmm0, %xmm0
, 18		// vpaddq %xmm0, %xmm0, %xmm0
, 18		// vpaddq (%rsi), %ymm0, %ymm0
, 18		// vpaddq %ymm0, %ymm0, %ymm0
, 18		// vpaddsb (%rsi), %xmm0, %xmm0
, 18		// vpaddsb %xmm0, %xmm0, %xmm0
, 18		// vpaddsb (%rsi), %ymm0, %ymm0
, 18		// vpaddsb %ymm0, %ymm0, %ymm0
, 18		// vpaddsw (%rsi), %xmm0, %xmm0
, 18		// vpaddsw %xmm0, %xmm0, %xmm0
, 18		// vpaddsw (%rsi), %ymm0, %ymm0
, 18		// vpaddsw %ymm0, %ymm0, %ymm0
, 18		// vpaddusb (%rsi), %xmm0, %xmm0
, 18		// vpaddusb %xmm0, %xmm0, %xmm0
, 18		// vpaddusb (%rsi), %ymm0, %ymm0
, 18		// vpaddusb %ymm0, %ymm0, %ymm0
, 18		// vpaddusw (%rsi), %xmm0, %xmm0
, 18		// vpaddusw %xmm0, %xmm0, %xmm0
, 18		// vpaddusw (%rsi), %ymm0, %ymm0
, 18		// vpaddusw %ymm0, %ymm0, %ymm0
, 18		// vpaddw (%rsi), %xmm0, %xmm0
, 18		// vpaddw %xmm0, %xmm0, %xmm0
, 18		// vpaddw (%rsi), %ymm0, %ymm0
, 18		// vpaddw %ymm0, %ymm0, %ymm0
, 18		// vpalignr $0x4, (%rsi), %xmm0, %xmm0
, 18		// vpalignr $0x4, %xmm0, %xmm0, %xmm0
, 18		// vpalignr $0x4, (%rsi), %ymm0, %ymm0
, 18		// vpalignr $0x4, %ymm0, %ymm0, %ymm0
, 18		// vpand (%rsi), %xmm0, %xmm0
, 18		// vpand %xmm0, %xmm0, %xmm0
, 18		// vpand (%rsi), %ymm0, %ymm0
, 18		// vpand %ymm0, %ymm0, %ymm0
, 18		// vpandn (%rsi), %xmm0, %xmm0
, 18		// vpandn %xmm0, %xmm0, %xmm0
, 18		// vpandn (%rsi), %ymm0, %ymm0
, 18		// vpandn %ymm0, %ymm0, %ymm0
, 18		// vpavgb (%rsi), %xmm0, %xmm0
, 18		// vpavgb %xmm0, %xmm0, %xmm0
, 18		// vpavgb (%rsi), %ymm0, %ymm0
, 18		// vpavgb %ymm0, %ymm0, %ymm0
, 18		// vpavgw (%rsi), %xmm0, %xmm0
, 18		// vpavgw %xmm0, %xmm0, %xmm0
, 18		// vpavgw (%rsi), %ymm0, %ymm0
, 18		// vpavgw %ymm0, %ymm0, %ymm0
, 18		// vpblendd $0x4, (%rsi), %xmm0, %xmm0
, 18		// vpblendd $0x4, %xmm0, %xmm0, %xmm0
, 18		// vpblendd $0x4, (%rsi), %ymm0, %ymm0
, 18		// vpblendd $0x4, %ymm0, %ymm0, %ymm0
, 44		// vpblendvb %xmm0, (%rsi), %xmm0, %xmm0
, 44		// vpblendvb %xmm0, %xmm0, %xmm0, %xmm0
, 44		// vpblendvb %ymm0, (%rsi), %ymm0, %ymm0
, 44		// vpblendvb %ymm0, %ymm0, %ymm0, %ymm0
, 18		// vpblendw $0x4, (%rsi), %xmm0, %xmm0
, 18		// vpblendw $0x4, %xmm0, %xmm0, %xmm0
, 18		// vpblendw $0x4, (%rsi), %ymm0, %ymm0
, 18		// vpblendw $0x4, %ymm0, %ymm0, %ymm0
, 19		// vpbroadcastb (%rsi), %xmm0
, 18		// vpbroadcastb %xmm0, %xmm0
, 19		// vpbroadcastb (%rsi), %ymm0
, 70		// vpbroadcastb %xmm0, %ymm0
, 6		// vpbroadcastd (%rsi), %xmm0
, 18		// vpbroadcastd %xmm0, %xmm0
, 6		// vpbroadcastd (%rsi), %ymm0
, 70		// vpbroadcastd %xmm0, %ymm0
, 6		// vpbroadcastq (%rsi), %xmm0
, 18		// vpbroadcastq %xmm0, %xmm0
, 6		// vpbroadcastq (%rsi), %ymm0
, 70		// vpbroadcastq %xmm0, %ymm0
, 19		// vpbroadcastw (%rsi), %xmm0
, 18		// vpbroadcastw %xmm0, %xmm0
, 19		// vpbroadcastw (%rsi), %ymm0
, 70		// vpbroadcastw %xmm0, %ymm0
, 172		// vpclmulqdq $0x4, (%rsi), %xmm0, %xmm0
, 172		// vpclmulqdq $0x4, %xmm0, %xmm0, %xmm0
, 18		// vpcmpeqb (%rsi), %xmm0, %xmm0
, 5		// vpcmpeqb %xmm0, %xmm0, %xmm0
, 18		// vpcmpeqb (%rsi), %ymm0, %ymm0
, 5		// vpcmpeqb %ymm0, %ymm0, %ymm0
, 18		// vpcmpeqd (%rsi), %xmm0, %xmm0
, 5		// vpcmpeqd %xmm0, %xmm0, %xmm0
, 18		// vpcmpeqd (%rsi), %ymm0, %ymm0
, 5		// vpcmpeqd %ymm0, %ymm0, %ymm0
, 18		// vpcmpeqq (%rsi), %xmm0, %xmm0
, 5		// vpcmpeqq %xmm0, %xmm0, %xmm0
, 18		// vpcmpeqq (%rsi), %ymm0, %ymm0
, 5		// vpcmpeqq %ymm0, %ymm0, %ymm0
, 18		// vpcmpeqw (%rsi), %xmm0, %xmm0
, 5		// vpcmpeqw %xmm0, %xmm0, %xmm0
, 18		// vpcmpeqw (%rsi), %ymm0, %ymm0
, 5		// vpcmpeqw %ymm0, %ymm0, %ymm0
, 96		// vpcmpestri $0x4, (%rsi), %xmm0
, 96		// vpcmpestri $0x4, %xmm0, %xmm0
, 275		// vpcmpestrm $0x4, (%rsi), %xmm0
, 275		// vpcmpestrm $0x4, %xmm0, %xmm0
, 18		// vpcmpgtb (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtb %xmm0, %xmm0, %xmm0
, 18		// vpcmpgtb (%rsi), %ymm0, %ymm0
, 1		// vpcmpgtb %ymm0, %ymm0, %ymm0
, 18		// vpcmpgtd (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtd %xmm0, %xmm0, %xmm0
, 18		// vpcmpgtd (%rsi), %ymm0, %ymm0
, 1		// vpcmpgtd %ymm0, %ymm0, %ymm0
, 121		// vpcmpgtq (%rsi), %xmm0, %xmm0
, 9		// vpcmpgtq %xmm0, %xmm0, %xmm0
, 121		// vpvmpgtq (%rsi), %ymm0, %ymm0
, 9		// vpvmpgtq %ymm0, %ymm0, %ymm0
, 18		// vpcmpgtw (%rsi), %xmm0, %xmm0
, 1		// vpcmpgtw %xmm0, %xmm0, %xmm0
, 18		// vpcmpgtw (%rsi), %ymm0, %ymm0
, 1		// vpcmpgtw %ymm0, %ymm0, %ymm0
, 70		// vpcmpistri $0x4, (%rsi), %xmm0
, 70		// vpcmpistri $0x4, %xmm0, %xmm0
, 273		// vpcmpistrm $0x4, (%rsi), %xmm0
, 268		// vpcmpistrm $0x4, %xmm0, %xmm0
, 70		// vperm2f128 $0x4, (%rsi), %ymm0, %ymm0
, 70		// vperm2f128 $0x4, %ymm0, %ymm0, %ymm0
, 70		// vperm2i128 $0x4, (%rsi), %ymm0, %ymm0
, 70		// vperm2i128 $0x4, %ymm0, %ymm0, %ymm0
, 70		// vpermd (%rsi), %ymm0, %ymm0
, 70		// vpermd %ymm0, %ymm0, %ymm0
, 18		// vpermilpd $0x4, (%rsi), %xmm0
, 18		// vpermilpd $0x4, %xmm0, %xmm0
, 18		// vpermilpd (%rsi), %xmm0, %xmm0
, 18		// vpermilpd %xmm0, %xmm0, %xmm0
, 18		// vpermilpd $0x4, (%rsi), %ymm0
, 18		// vpermilpd $0x4, %ymm0, %ymm0
, 18		// vpermilpd (%rsi), %ymm0, %ymm0
, 18		// vpermilpd %ymm0, %ymm0, %ymm0
, 18		// vpermilps $0x4, (%rsi), %xmm0
, 18		// vpermilps $0x4, %xmm0, %xmm0
, 18		// vpermilps (%rsi), %xmm0, %xmm0
, 18		// vpermilps %xmm0, %xmm0, %xmm0
, 18		// vpermilps $0x4, (%rsi), %ymm0
, 18		// vpermilps $0x4, %ymm0, %ymm0
, 18		// vpermilps (%rsi), %ymm0, %ymm0
, 18		// vpermilps %ymm0, %ymm0, %ymm0
, 18		// vpermpd $0x4, (%rsi), %ymm0
, 70		// vpermpd $0x4, %ymm0, %ymm0
, 70		// vpermps (%rsi), %ymm0, %ymm0
, 70		// vpermps %ymm0, %ymm0, %ymm0
, 18		// vpermq $0x4, (%rsi), %ymm0
, 70		// vpermq $0x4, %ymm0, %ymm0
, 19		// vpextrb $0x4, %xmm0, (%rsi)
, 18		// vpextrb $0x4, %xmm0, %edx
, 18		// vpextrb $0x4, %xmm0, %rdx
, 19		// vpextrd $0x4, %xmm0, (%rsi)
, 18		// vpextrd $0x4, %xmm0, %edx
, 19		// vpextrq $0x4, %xmm0, (%rsi)
, 18		// vpextrq $0x4, %xmm0, %rdx
, 19		// vpextrw $0x4, %xmm0, (%rsi)
, 18		// vpextrw $0x4, %xmm0, %edx
, 18		// vpextrw $0x4, %xmm0, %edx
, 18		// vpextrw $0x4, %xmm0, %rdx
, 18		// vpextrw $0x4, %xmm0, %rdx
, 999		// vpgatherdd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherdd %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vpgatherdq %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherdq %ymm0, es:(%rax,%rax,1), %ymm0
, 999		// vpgatherqd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqd %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqq %xmm0, es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqq %ymm0, es:(%rax,%rax,1), %ymm0
, 70		// vphaddd (%rsi), %xmm0, %xmm0
, 70		// vphaddd %xmm0, %xmm0, %xmm0
, 70		// vphaddd (%rsi), %ymm0, %ymm0
, 70		// vphaddd %ymm0, %ymm0, %ymm0
, 70		// vphaddsw (%rsi), %xmm0, %xmm0
, 70		// vphaddsw %xmm0, %xmm0, %xmm0
, 70		// vphaddsw (%rsi), %ymm0, %ymm0
, 70		// vphaddsw %ymm0, %ymm0, %ymm0
, 70		// vphaddw (%rsi), %xmm0, %xmm0
, 70		// vphaddw %xmm0, %xmm0, %xmm0
, 70		// vphaddw (%rsi), %ymm0, %ymm0
, 70		// vphaddw %ymm0, %ymm0, %ymm0
, 18		// vphminposuw (%rsi), %xmm0
, 121		// vphminposuw %xmm0, %xmm0
, 70		// vphsubd (%rsi), %xmm0, %xmm0
, 70		// vphsubd %xmm0, %xmm0, %xmm0
, 70		// vphsubd (%rsi), %ymm0, %ymm0
, 70		// vphsubd %ymm0, %ymm0, %ymm0
, 70		// vphsubsw (%rsi), %xmm0, %xmm0
, 70		// vphsubsw %xmm0, %xmm0, %xmm0
, 70		// vphsubsw (%rsi), %ymm0, %ymm0
, 70		// vphsubsw %ymm0, %ymm0, %ymm0
, 70		// vphsubw (%rsi), %xmm0, %xmm0
, 70		// vphsubw %xmm0, %xmm0, %xmm0
, 70		// vphsubw (%rsi), %ymm0, %ymm0
, 70		// vphsubw %ymm0, %ymm0, %ymm0
, 18		// vpinsrb $0x4, (%rsi), %xmm0, %xmm0
, 44		// vpinsrb $0x4, %edx, %xmm0, %xmm0
, 18		// vpinsrd $0x4, (%rsi), %xmm0, %xmm0
, 44		// vpinsrd $0x4, %edx, %xmm0, %xmm0
, 18		// vpinsrq $0x4, (%rsi), %xmm0, %xmm0
, 44		// vpinsrq $0x4, %rdx, %xmm0, %xmm0
, 18		// vpinsrw $0x4, (%rsi), %xmm0, %xmm0
, 44		// vpinsrw $0x4, %edx, %xmm0, %xmm0
, 121		// vpmaddubsw (%rsi), %xmm0, %xmm0
, 121		// vpmaddubsw %xmm0, %xmm0, %xmm0
, 121		// vpmaddubsw (%rsi), %ymm0, %ymm0
, 121		// vpmaddubsw %ymm0, %ymm0, %ymm0
, 121		// vpmaddwd (%rsi), %xmm0, %xmm0
, 121		// vpmaddwd %xmm0, %xmm0, %xmm0
, 121		// vpmaddwd (%rsi), %ymm0, %ymm0
, 121		// vpmaddwd %ymm0, %ymm0, %ymm0
, 19		// vpmaskmovd %xmm0, %xmm0, (%rsi)
, 19		// vpmaskmovd %ymm0, %ymm0, (%rsi)
, 44		// vpmaskmovd (%rsi), %xmm0, %xmm0
, 44		// vpmaskmovd (%rsi), %ymm0, %ymm0
, 19		// vpmaskmovq %xmm0, %xmm0, (%rsi)
, 19		// vpmaskmovq %ymm0, %ymm0, (%rsi)
, 44		// vpmaskmovq (%rsi), %xmm0, %xmm0
, 44		// vpmaskmovq (%rsi), %ymm0, %ymm0
, 18		// vpmaxsb (%rsi), %xmm0, %xmm0
, 18		// vpmaxsb %xmm0, %xmm0, %xmm0
, 18		// vpmaxsb (%rsi), %ymm0, %ymm0
, 18		// vpmaxsb %ymm0, %ymm0, %ymm0
, 18		// vpmaxsd (%rsi), %xmm0, %xmm0
, 18		// vpmaxsd %xmm0, %xmm0, %xmm0
, 18		// vpmaxsd (%rsi), %ymm0, %ymm0
, 18		// vpmaxsd %ymm0, %ymm0, %ymm0
, 18		// vpmaxsw (%rsi), %xmm0, %xmm0
, 18		// vpmaxsw %xmm0, %xmm0, %xmm0
, 18		// vpmaxsw (%rsi), %ymm0, %ymm0
, 18		// vpmaxsw %ymm0, %ymm0, %ymm0
, 18		// vpmaxub (%rsi), %xmm0, %xmm0
, 18		// vpmaxub %xmm0, %xmm0, %xmm0
, 18		// vpmaxub (%rsi), %ymm0, %ymm0
, 18		// vpmaxub %ymm0, %ymm0, %ymm0
, 18		// vpmaxud (%rsi), %xmm0, %xmm0
, 18		// vpmaxud %xmm0, %xmm0, %xmm0
, 18		// vpmaxud (%rsi), %ymm0, %ymm0
, 18		// vpmaxud %ymm0, %ymm0, %ymm0
, 18		// vpmaxuw (%rsi), %xmm0, %xmm0
, 18		// vpmaxuw %xmm0, %xmm0, %xmm0
, 18		// vpmaxuw (%rsi), %ymm0, %ymm0
, 18		// vpmaxuw %ymm0, %ymm0, %ymm0
, 18		// vpminsb (%rsi), %xmm0, %xmm0
, 18		// vpminsb %xmm0, %xmm0, %xmm0
, 18		// vpminsb (%rsi), %ymm0, %ymm0
, 18		// vpminsb %ymm0, %ymm0, %ymm0
, 18		// vpminsd (%rsi), %xmm0, %xmm0
, 18		// vpminsd %xmm0, %xmm0, %xmm0
, 18		// vpminsd (%rsi), %ymm0, %ymm0
, 18		// vpminsd %ymm0, %ymm0, %ymm0
, 18		// vpminsw (%rsi), %xmm0, %xmm0
, 18		// vpminsw %xmm0, %xmm0, %xmm0
, 18		// vpminub (%rsi), %xmm0, %xmm0
, 18		// vpminub %xmm0, %xmm0, %xmm0
, 18		// vpminub (%rsi), %ymm0, %ymm0
, 18		// vpminub %ymm0, %ymm0, %ymm0
, 18		// vpminud (%rsi), %xmm0, %xmm0
, 18		// vpminud %xmm0, %xmm0, %xmm0
, 18		// vpminud (%rsi), %ymm0, %ymm0
, 18		// vpminud %ymm0, %ymm0, %ymm0
, 18		// vpminuw (%rsi), %xmm0, %xmm0
, 18		// vpminuw %xmm0, %xmm0, %xmm0
, 18		// vpminuw (%rsi), %ymm0, %ymm0
, 18		// vpminuw %ymm0, %ymm0, %ymm0
, 18		// vpmovmskb %xmm0, %edx
, 18		// vpmovmskb %ymm0, %edx
, 18		// vpmovmskb %xmm0, %rdx
, 18		// vpmovmskb %ymm0, %rdx
, 18		// vpmovsxbd (%rsi), %xmm0
, 18		// vpmovsxbd %xmm0, %xmm0
, 18		// vpmovsxbd (%rsi), %ymm0
, 70		// vpmovsxbd %xmm0, %ymm0
, 18		// vpmovsxbq (%rsi), %xmm0
, 18		// vpmovsxbq %xmm0, %xmm0
, 18		// vpmovsxbq (%rsi), %ymm0
, 70		// vpmovsxbq %xmm0, %ymm0
, 18		// vpmovsxbw (%rsi), %xmm0
, 18		// vpmovsxbw %xmm0, %xmm0
, 18		// vpmovsxbw (%rsi), %ymm0
, 70		// vpmovsxbw %xmm0, %ymm0
, 18		// vpmovsxdq (%rsi), %xmm0
, 18		// vpmovsxdq %xmm0, %xmm0
, 18		// vpmovsxdq (%rsi), %ymm0
, 70		// vpmovsxdq %xmm0, %ymm0
, 18		// vpmovsxwd (%rsi), %xmm0
, 18		// vpmovsxwd %xmm0, %xmm0
, 18		// vpmovsxwd (%rsi), %ymm0
, 70		// vpmovsxwd %xmm0, %ymm0
, 18		// vpmovsxwq (%rsi), %xmm0
, 18		// vpmovsxwq %xmm0, %xmm0
, 18		// vpmovsxwq (%rsi), %ymm0
, 70		// vpmovsxwq %xmm0, %ymm0
, 18		// vpmovzxbd (%rsi), %xmm0
, 18		// vpmovzxbd %xmm0, %xmm0
, 18		// vpmovzxbd (%rsi), %ymm0
, 70		// vpmovzxbd %xmm0, %ymm0
, 18		// vpmovzxbq (%rsi), %xmm0
, 18		// vpmovzxbq %xmm0, %xmm0
, 18		// vpmovzxbq (%rsi), %ymm0
, 70		// vpmovzxbq %xmm0, %ymm0
, 18		// vpmovzxbw (%rsi), %xmm0
, 18		// vpmovzxbw %xmm0, %xmm0
, 18		// vpmovzxbw (%rsi), %ymm0
, 70		// vpmovzxbw %xmm0, %ymm0
, 18		// vpmovzxdq (%rsi), %xmm0
, 18		// vpmovzxdq %xmm0, %xmm0
, 18		// vpmovzxdq (%rsi), %ymm0
, 70		// vpmovzxdq %xmm0, %ymm0
, 18		// vpmovzxwd (%rsi), %xmm0
, 18		// vpmovzxwd %xmm0, %xmm0
, 18		// vpmovzxwd (%rsi), %ymm0
, 70		// vpmovzxwd %xmm0, %ymm0
, 18		// vpmovzxwq (%rsi), %xmm0
, 18		// vpmovzxwq %xmm0, %xmm0
, 18		// vpmovzxwq (%rsi), %ymm0
, 70		// vpmovzxwq %xmm0, %ymm0
, 121		// vpmuldq (%rsi), %xmm0, %xmm0
, 121		// vpmuldq %xmm0, %xmm0, %xmm0
, 121		// vpmuldq (%rsi), %ymm0, %ymm0
, 121		// vpmuldq %ymm0, %ymm0, %ymm0
, 121		// vpmulhrsw (%rsi), %xmm0, %xmm0
, 121		// vpmulhrsw %xmm0, %xmm0, %xmm0
, 121		// vpmulhrsw (%rsi), %ymm0, %ymm0
, 121		// vpmulhrsw %ymm0, %ymm0, %ymm0
, 121		// vpmulhuw (%rsi), %xmm0, %xmm0
, 121		// vpmulhuw %xmm0, %xmm0, %xmm0
, 121		// vpmulhuw (%rsi), %ymm0, %ymm0
, 121		// vpmulhuw %ymm0, %ymm0, %ymm0
, 121		// vpmulhw (%rsi), %xmm0, %xmm0
, 121		// vpmulhw %xmm0, %xmm0, %xmm0
, 121		// vpmulhw (%rsi), %ymm0, %ymm0
, 121		// vpmulhw %ymm0, %ymm0, %ymm0
, 249		// vpmulld (%rsi), %xmm0, %xmm0
, 249		// vpmulld %xmm0, %xmm0, %xmm0
, 249		// vpmulld (%rsi), %ymm0, %ymm0
, 249		// vpmulld %ymm0, %ymm0, %ymm0
, 121		// vpmullw (%rsi), %xmm0, %xmm0
, 121		// vpmullw %xmm0, %xmm0, %xmm0
, 121		// vpmullw (%rsi), %ymm0, %ymm0
, 121		// vpmullw %ymm0, %ymm0, %ymm0
, 121		// vpmuludq (%rsi), %xmm0, %xmm0
, 121		// vpmuludq %xmm0, %xmm0, %xmm0
, 121		// vpmuludq (%rsi), %ymm0, %ymm0
, 121		// vpmuludq %ymm0, %ymm0, %ymm0
, 18		// vpor (%rsi), %xmm0, %xmm0
, 18		// vpor %xmm0, %xmm0, %xmm0
, 18		// vpor (%rsi), %ymm0, %ymm0
, 18		// vpor %ymm0, %ymm0, %ymm0
, 121		// vpsadbw (%rsi), %xmm0, %xmm0
, 121		// vpsadbw %xmm0, %xmm0, %xmm0
, 121		// vpsadbw (%rsi), %ymm0, %ymm0
, 121		// vpsadbw %ymm0, %ymm0, %ymm0
, 18		// vpshufb (%rsi), %xmm0, %xmm0
, 18		// vpshufb %xmm0, %xmm0, %xmm0
, 18		// vpshufb (%rsi), %ymm0, %ymm0
, 18		// vpshufb %ymm0, %ymm0, %ymm0
, 18		// vpshufd $0x4, (%rsi), %xmm0
, 18		// vpshufd $0x4, %xmm0, %xmm0
, 18		// vpshufd $0x4, (%rsi), %ymm0
, 18		// vpshufd $0x4, %ymm0, %ymm0
, 18		// vpshufhw $0x4, (%rsi), %xmm0
, 18		// vpshufhw $0x4, %xmm0, %xmm0
, 18		// vpshufhw $0x4, (%rsi), %ymm0
, 18		// vpshufhw $0x4, %ymm0, %ymm0
, 18		// vpshuflw $0x4, (%rsi), %xmm0
, 18		// vpshuflw $0x4, %xmm0, %xmm0
, 18		// vpshuflw $0x4, (%rsi), %ymm0
, 18		// vpshuflw $0x4, %ymm0, %ymm0
, 18		// vpsignb (%rsi), %xmm0, %xmm0
, 18		// vpsignb %xmm0, %xmm0, %xmm0
, 18		// vpsignd (%rsi), %xmm0, %xmm0
, 18		// vpsignd %xmm0, %xmm0, %xmm0
, 18		// vpsignw (%rsi), %xmm0, %xmm0
, 18		// vpsignw %xmm0, %xmm0, %xmm0
, 18		// vpslld $0x4, %xmm0, %xmm0
, 18		// vpslld (%rsi), %xmm0, %xmm0
, 44		// vpslld %xmm0, %xmm0, %xmm0
, 18		// vpslld $0x4, %ymm0, %ymm0
, 18		// vpslld (%rsi), %ymm0, %ymm0
, 95		// vpslld %xmm0, %ymm0, %ymm0
, 18		// vpslldq $0x4, %xmm0, %xmm0
, 18		// vpslldq $0x4, %ymm0, %ymm0
, 18		// vpsllq $0x4, %xmm0, %xmm0
, 18		// vpsllq (%rsi), %xmm0, %xmm0
, 44		// vpsllq %xmm0, %xmm0, %xmm0
, 18		// vpsllq $0x4, %ymm0, %ymm0
, 18		// vpsllq (%rsi), %ymm0, %ymm0
, 95		// vpsllq %xmm0, %ymm0, %ymm0
, 44		// vpsllvd (%rsi), %xmm0, %xmm0
, 44		// vpsllvd %xmm0, %xmm0, %xmm0
, 44		// vpsllvd (%rsi), %ymm0, %ymm0
, 44		// vpsllvd %ymm0, %ymm0, %ymm0
, 18		// vpsllvq (%rsi), %xmm0, %xmm0
, 18		// vpsllvq %xmm0, %xmm0, %xmm0
, 18		// vpsllvq (%rsi), %ymm0, %ymm0
, 18		// vpsllvq %ymm0, %ymm0, %ymm0
, 18		// vpsllw $0x4, %xmm0, %xmm0
, 18		// vpsllw (%rsi), %xmm0, %xmm0
, 44		// vpsllw %xmm0, %xmm0, %xmm0
, 18		// vpsllw $0x4, %ymm0, %ymm0
, 18		// vpsllw (%rsi), %ymm0, %ymm0
, 95		// vpsllw %xmm0, %ymm0, %ymm0
, 18		// vpsrad $0x4, %xmm0, %xmm0
, 18		// vpsrad (%rsi), %xmm0, %xmm0
, 44		// vpsrad %xmm0, %xmm0, %xmm0
, 18		// vpsrad $0x4, %ymm0, %ymm0
, 18		// vpsrad (%rsi), %ymm0, %ymm0
, 95		// vpsrad %xmm0, %ymm0, %ymm0
, 44		// vpsravd (%rsi), %xmm0, %xmm0
, 44		// vpsravd %xmm0, %xmm0, %xmm0
, 44		// vpsravd (%rsi), %ymm0, %ymm0
, 44		// vpsravd %ymm0, %ymm0, %ymm0
, 18		// vpsraw $0x4, %xmm0, %xmm0
, 18		// vpsraw (%rsi), %xmm0, %xmm0
, 44		// vpsraw %xmm0, %xmm0, %xmm0
, 18		// vpsraw $0x4, %ymm0, %ymm0
, 18		// vpsraw (%rsi), %ymm0, %ymm0
, 95		// vpsraw %xmm0, %ymm0, %ymm0
, 18		// vpsrld $0x4, %xmm0, %xmm0
, 18		// vpsrld (%rsi), %xmm0, %xmm0
, 44		// vpsrld %xmm0, %xmm0, %xmm0
, 18		// vpsrld $0x4, %ymm0, %ymm0
, 18		// vpsrld (%rsi), %ymm0, %ymm0
, 95		// vpsrld %xmm0, %ymm0, %ymm0
, 18		// vpsrldq $0x4, %xmm0, %xmm0
, 18		// vpsrldq $0x4, %ymm0, %ymm0
, 18		// vpsrlq $0x4, %xmm0, %xmm0
, 18		// vpsrlq (%rsi), %xmm0, %xmm0
, 44		// vpsrlq %xmm0, %xmm0, %xmm0
, 18		// vpsrlq $0x4, %ymm0, %ymm0
, 18		// vpsrlq (%rsi), %ymm0, %ymm0
, 95		// vpsrlq %xmm0, %ymm0, %ymm0
, 44		// vpsrlvd (%rsi), %xmm0, %xmm0
, 44		// vpsrlvd %xmm0, %xmm0, %xmm0
, 44		// vpsrlvd (%rsi), %ymm0, %ymm0
, 44		// vpsrlvd %ymm0, %ymm0, %ymm0
, 18		// vpsrlvq (%rsi), %xmm0, %xmm0
, 18		// vpsrlvq %xmm0, %xmm0, %xmm0
, 18		// vpsrlvq (%rsi), %ymm0, %ymm0
, 18		// vpsrlvq %ymm0, %ymm0, %ymm0
, 18		// vpsrlw $0x4, %xmm0, %xmm0
, 18		// vpsrlw (%rsi), %xmm0, %xmm0
, 44		// vpsrlw %xmm0, %xmm0, %xmm0
, 18		// vpsrlw $0x4, %ymm0, %ymm0
, 18		// vpsrlw (%rsi), %ymm0, %ymm0
, 95		// vpsrlw %xmm0, %ymm0, %ymm0
, 18		// vpsubb (%rsi), %xmm0, %xmm0
, 1		// vpsubb %xmm0, %xmm0, %xmm0
, 18		// vpsubb (%rsi), %ymm0, %ymm0
, 1		// vpsubb %ymm0, %ymm0, %ymm0
, 18		// vpsubd (%rsi), %xmm0, %xmm0
, 1		// vpsubd %xmm0, %xmm0, %xmm0
, 18		// vpsubd (%rsi), %ymm0, %ymm0
, 1		// vpsubd %ymm0, %ymm0, %ymm0
, 18		// vpsubq (%rsi), %xmm0, %xmm0
, 9		// vpsubq %xmm0, %xmm0, %xmm0
, 18		// vpsubq (%rsi), %ymm0, %ymm0
, 9		// vpsubq %ymm0, %ymm0, %ymm0
, 18		// vpsubsb (%rsi), %xmm0, %xmm0
, 1		// vpsubsb %xmm0, %xmm0, %xmm0
, 18		// vpsubsb (%rsi), %ymm0, %ymm0
, 1		// vpsubsb %ymm0, %ymm0, %ymm0
, 18		// vpsubsw (%rsi), %xmm0, %xmm0
, 1		// vpsubsw %xmm0, %xmm0, %xmm0
, 18		// vpsubsw (%rsi), %ymm0, %ymm0
, 1		// vpsubsw %ymm0, %ymm0, %ymm0
, 18		// vpsubusb (%rsi), %xmm0, %xmm0
, 1		// vpsubusb %xmm0, %xmm0, %xmm0
, 18		// vpsubusb (%rsi), %ymm0, %ymm0
, 1		// vpsubusb %ymm0, %ymm0, %ymm0
, 18		// vpsubusw (%rsi), %xmm0, %xmm0
, 1		// vpsubusw %xmm0, %xmm0, %xmm0
, 18		// vpsubusw (%rsi), %ymm0, %ymm0
, 1		// vpsubusw %ymm0, %ymm0, %ymm0
, 18		// vpsubw (%rsi), %xmm0, %xmm0
, 1		// vpsubw %xmm0, %xmm0, %xmm0
, 18		// vpsubw (%rsi), %ymm0, %ymm0
, 1		// vpsubw %ymm0, %ymm0, %ymm0
, 19		// vptest (%rsi), %xmm0
, 18		// vptest %xmm0, %xmm0
, 19		// vptest (%rsi), %ymm0
, 18		// vptest %ymm0, %ymm0
, 18		// vpunpckhbw (%rsi), %xmm0, %xmm0
, 18		// vpunpckhbw %xmm0, %xmm0, %xmm0
, 18		// vpunpckhbw (%rsi), %ymm0, %ymm0
, 18		// vpunpckhbw %ymm0, %ymm0, %ymm0
, 18		// vpunpckhdq (%rsi), %xmm0, %xmm0
, 18		// vpunpckhdq %xmm0, %xmm0, %xmm0
, 18		// vpunpckhdq (%rsi), %ymm0, %ymm0
, 18		// vpunpckhdq %ymm0, %ymm0, %ymm0
, 18		// vpunpckhqdq (%rsi), %xmm0, %xmm0
, 18		// vpunpckhqdq %xmm0, %xmm0, %xmm0
, 18		// vpunpckhqdq (%rsi), %ymm0, %ymm0
, 18		// vpunpckhqdq %ymm0, %ymm0, %ymm0
, 18		// vpunpckhwd (%rsi), %xmm0, %xmm0
, 18		// vpunpckhwd %xmm0, %xmm0, %xmm0
, 18		// vpunpckhwd (%rsi), %ymm0, %ymm0
, 18		// vpunpckhwd %ymm0, %ymm0, %ymm0
, 18		// vpunpcklbw (%rsi), %xmm0, %xmm0
, 18		// vpunpcklbw %xmm0, %xmm0, %xmm0
, 18		// vpunpcklbw (%rsi), %ymm0, %ymm0
, 18		// vpunpcklbw %ymm0, %ymm0, %ymm0
, 18		// vpunpckldq (%rsi), %xmm0, %xmm0
, 18		// vpunpckldq %xmm0, %xmm0, %xmm0
, 18		// vpunpckldq (%rsi), %ymm0, %ymm0
, 18		// vpunpckldq %ymm0, %ymm0, %ymm0
, 18		// vpunpcklqdq (%rsi), %xmm0, %xmm0
, 18		// vpunpcklqdq %xmm0, %xmm0, %xmm0
, 18		// vpunpcklqdq (%rsi), %ymm0, %ymm0
, 18		// vpunpcklqdq %ymm0, %ymm0, %ymm0
, 18		// vpunpcklwd (%rsi), %xmm0, %xmm0
, 18		// vpunpcklwd %xmm0, %xmm0, %xmm0
, 18		// vpunpcklwd (%rsi), %ymm0, %ymm0
, 19		// vpunpcklwd %ymm0, %ymm0, %ymm0
, 18		// vpxor (%rsi), %xmm0, %xmm0
, 1		// vpxor %xmm0, %xmm0, %xmm0
, 18		// vpxor (%rsi), %ymm0, %ymm0
, 1		// vpxor %ymm0, %ymm0, %ymm0
, 18		// vrcpps (%rsi), %xmm0
, 121		// vrcpps %xmm0, %xmm0
, 44		// vrcpps (%rsi), %ymm0
, 172		// vrcpps %ymm0, %ymm0
, 121		// vrcpss (%rsi), %xmm0, %xmm0
, 121		// vrcpss %xmm0, %xmm0, %xmm0
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
, 18		// vrsqrtps (%rsi), %xmm0
, 121		// vrsqrtps %xmm0, %xmm0
, 44		// vrsqrtps (%rsi), %ymm0
, 172		// vrsqrtps %ymm0, %ymm0
, 121		// vrsqrtss (%rsi), %xmm0, %xmm0
, 121		// vrsqrtss %xmm0, %xmm0, %xmm0
, 18		// vshufpd $0x4, (%rsi), %xmm0, %xmm0
, 18		// vshufpd $0x4, %xmm0, %xmm0, %xmm0
, 18		// vshufpd $0x4, (%rsi), %ymm0, %ymm0
, 18		// vshufpd $0x4, %ymm0, %ymm0, %ymm0
, 18		// vshufps $0x4, (%rsi), %xmm0, %xmm0
, 18		// vshufps $0x4, %xmm0, %xmm0, %xmm0
, 18		// vshufps $0x4, (%rsi), %ymm0, %ymm0
, 18		// vshufps $0x4, %ymm0, %ymm0, %ymm0
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
, 19		// vstmxcsr (%rsi)
, 70		// vsubpd (%rsi), %xmm0, %xmm0
, 70		// vsubpd %xmm0, %xmm0, %xmm0
, 78		// vsubpd (%rsi), %ymm0, %ymm0
, 78		// vsubpd %ymm0, %ymm0, %ymm0
, 70		// vsubps (%rsi), %xmm0, %xmm0
, 70		// vsubps %xmm0, %xmm0, %xmm0
, 78		// vsubps (%rsi), %ymm0, %ymm0
, 78		// vsubps %ymm0, %ymm0, %ymm0
, 70		// vsubsd (%rsi), %xmm0, %xmm0
, 70		// vsubsd %xmm0, %xmm0, %xmm0
, 70		// vsubss (%rsi), %xmm0, %xmm0
, 70		// vsubss %xmm0, %xmm0, %xmm0
, 18		// vtestpd (%rsi), %xmm0
, 18		// vtestpd %xmm0, %xmm0
, 18		// vtestpd (%rsi), %ymm0
, 18		// vtestpd %ymm0, %ymm0
, 18		// vtestps (%rsi), %xmm0
, 18		// vtestps %xmm0, %xmm0
, 18		// vtestps (%rsi), %ymm0
, 18		// vtestps %ymm0, %ymm0
, 18		// vucomisd (%rsi), %xmm0
, 18		// vucomisd %xmm0, %xmm0
, 18		// vucomiss (%rsi), %xmm0
, 18		// vucomiss %xmm0, %xmm0
, 18		// vunpckhpd (%rsi), %xmm0, %xmm0
, 19		// vunpckhpd %xmm0, %xmm0, %xmm0
, 18		// vunpckhpd (%rsi), %ymm0, %ymm0
, 18		// vunpckhpd %ymm0, %ymm0, %ymm0
, 18		// vunpckhps (%rsi), %xmm0, %xmm0
, 18		// vunpckhps %xmm0, %xmm0, %xmm0
, 18		// vunpckhps (%rsi), %ymm0, %ymm0
, 18		// vunpckhps %ymm0, %ymm0, %ymm0
, 18		// vunpcklpd (%rsi), %xmm0, %xmm0
, 18		// vunpcklpd %xmm0, %xmm0, %xmm0
, 18		// vunpcklpd (%rsi), %ymm0, %ymm0
, 18		// vunpcklpd %ymm0, %ymm0, %ymm0
, 18		// vunpcklps (%rsi), %xmm0, %xmm0
, 18		// vunpcklps %xmm0, %xmm0, %xmm0
, 18		// vunpcklps (%rsi), %ymm0, %ymm0
, 18		// vunpcklps %ymm0, %ymm0, %ymm0
, 18		// vxorpd (%rsi), %xmm0, %xmm0
, 1		// vxorpd %xmm0, %xmm0, %xmm0
, 18		// vxorpd (%rsi), %ymm0, %ymm0
, 1		// vxorpd %ymm0, %ymm0, %ymm0
, 18		// vxorps (%rsi), %xmm0, %xmm0
, 1		// vxorps %xmm0, %xmm0, %xmm0
, 18		// vxorps (%rsi), %ymm0, %ymm0
, 1		// vxorps %ymm0, %ymm0, %ymm0
, 198		// vzeroall 
, 19		// vzeroupper 
, 19		// wait 
, 999		// wrfsbase %eax
, 999		// wrfsbase %rax
, 999		// wrgsbase %eax
, 999		// wrgsbase %rax
, 999		// xabort $0x0
, 999		// xacquire 
, 179		// xaddw %cx, (%rsi)
, 179		// xaddl %edx, (%rsi)
, 179		// xaddq %rdx, (%rsi)
, 178		// xaddb %dl, (%rsi)
, 178		// xaddb %ch, (%rsi)
, 46		// xaddw %cx, %cx
, 46		// xaddl %edx, %edx
, 46		// xaddq %rdx, %rdx
, 46		// xaddb %dl, %dl
, 44		// xaddb %ch, %dl
, 45		// xaddb %dl, %ch
, 46		// xaddb %ch, %ch
, 999		// xbegin 0
, 999		// xbegin 
, 31		// xchgw %cx, %ax
, 31		// xchgl %edx, %eax
, 481		// xchgw %cx, (%rsi)
, 481		// xchgl %edx, (%rsi)
, 481		// xchgq %rdx, (%rsi)
, 481		// xchgb %dl, (%rsi)
, 481		// xchgb %ch, (%rsi)
, 31		// xchgw %ax, %cx
, 481		// xchgw (%rsi), %cx
, 46		// xchgw %cx, %cx
, 46		// xchgw %cx, %cx
, 31		// xchgl %eax, %edx
, 481		// xchgl (%rsi), %edx
, 46		// xchgl %edx, %edx
, 46		// xchgl %edx, %edx
, 481		// xchgq (%rsi), %rdx
, 46		// xchgq %rdx, %rdx
, 46		// xchgq %rdx, %rdx
, 31		// xchgq %rax, %rdx
, 481		// xchgb (%rsi), %dl
, 46		// xchgb %dl, %dl
, 46		// xchgb %dl, %dl
, 31		// xchgb %ch, %dl
, 31		// xchgb %ch, %dl
, 31		// xchgq %rdx, %rax
, 481		// xchgb (%rsi), %ch
, 31		// xchgb %dl, %ch
, 31		// xchgb %dl, %ch
, 46		// xchgb %ch, %ch
, 46		// xchgb %ch, %ch
, 999		// xend 
, 999		// xgetbv 
, 999		// xlat es:(%rax,%rax,1)
, 999		// xlatb 
, 999		// xlatb 
, 18		// xorb $0x4, %al
, 43		// xorw $0x4050, %ax
, 18		// xorl $0xc0decafe, %eax
, 161		// xorw $0x4050, (%rsi)
, 167		// xorw $0x4, (%rsi)
, 149		// xorw %cx, (%rsi)
, 164		// xorl $0xc0decafe, (%rsi)
, 167		// xorl $0x4, (%rsi)
, 149		// xorl %edx, (%rsi)
, 164		// xorq $0xc0decafe, (%rsi)
, 161		// xorq $0x4, (%rsi)
, 149		// xorq %rdx, (%rsi)
, 167		// xorb $0x4, (%rsi)
, 149		// xorb %dl, (%rsi)
, 181		// xorb %ch, (%rsi)
, 44		// xorw $0x4050, %cx
, 18		// xorw $0x4, %cx
, 18		// xorw (%rsi), %cx
, 18		// xorw %cx, %cx
, 18		// xorw %cx, %cx
, 18		// xorl $0xc0decafe, %edx
, 18		// xorl $0x4, %edx
, 18		// xorl (%rsi), %edx
, 1		// xorl %edx, %edx
, 1		// xorl %edx, %edx
, 18		// xorq $0xc0decafe, %rdx
, 18		// xorq $0x4, %rdx
, 18		// xorq (%rsi), %rdx
, 1		// xorq %rdx, %rdx
, 1		// xorq %rdx, %rdx
, 18		// xorb $0x4, %dl
, 18		// xorb (%rsi), %dl
, 18		// xorb %dl, %dl
, 18		// xorb %dl, %dl
, 43		// xorb %ch, %dl
, 43		// xorb %ch, %dl
, 18		// xorq $0xc0decafe, %rax
, 18		// xorb $0x4, %ch
, 18		// xorb (%rsi), %ch
, 18		// xorb %dl, %ch
, 18		// xorb %dl, %ch
, 18		// xorb %ch, %ch
, 18		// xorb %ch, %ch
, 18		// xorpd (%rsi), %xmm0
, 1		// xorpd %xmm0, %xmm0
, 18		// xorps (%rsi), %xmm0
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
