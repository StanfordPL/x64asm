, 1		// adcb $0x0, %al
, 1		// adcw $0x0, %ax
, 1		// adcl $0x0, %eax
, 999		// adcw $0x0, %es:(%rax,%rax,1)
, 999		// adcw $0x0, %es:(%rax,%rax,1)
, 999		// adcw %ax, %es:(%rax,%rax,1)
, 999		// adcl $0x0, %es:(%rax,%rax,1)
, 999		// adcl $0x0, %es:(%rax,%rax,1)
, 999		// adcl %eax, %es:(%rax,%rax,1)
, 999		// adcq $0x0, %es:(%rax,%rax,1)
, 999		// adcq $0x0, %es:(%rax,%rax,1)
, 999		// adcq %rax, %es:(%rax,%rax,1)
, 999		// adcb $0x0, %es:(%rax,%rax,1)
, 999		// adcb %al, %es:(%rax,%rax,1)
, 999		// adcb  , %es:(%rax,%rax,1)
, 1		// adcw $0x0, %ax
, 1		// adcw $0x0, %ax
, 999		// adcw %es:(%rax,%rax,1), %ax
, 1		// adcw %ax, %ax
, 1		// adcl $0x0, %eax
, 1		// adcl $0x0, %eax
, 999		// adcl %es:(%rax,%rax,1), %eax
, 1		// adcl %eax, %eax
, 1		// adcq $0x0, %rax
, 1		// adcq $0x0, %rax
, 999		// adcq %es:(%rax,%rax,1), %rax
, 1		// adcq %rax, %rax
, 1		// adcb $0x0, %al
, 999		// adcb %es:(%rax,%rax,1), %al
, 1		// adcb %al, %al
, 1		// adcb %ah, %al
, 1		// adcq $0x0, %rax
, 1		// adcb $0x0, %ah
, 999		// adcb %es:(%rax,%rax,1), , 
, 1		// adcb %al, %ah
, 1		// adcb %ah, %ah
, 1		// addb $0x0, %al
, 1		// addw $0x0, %ax
, 1		// addl $0x0, %eax
, 999		// addw $0x0, %es:(%rax,%rax,1)
, 999		// addw $0x0, %es:(%rax,%rax,1)
, 999		// addw %ax, %es:(%rax,%rax,1)
, 999		// addl $0x0, %es:(%rax,%rax,1)
, 999		// addl $0x0, %es:(%rax,%rax,1)
, 999		// addl %eax, %es:(%rax,%rax,1)
, 999		// addq $0x0, %es:(%rax,%rax,1)
, 999		// addq $0x0, %es:(%rax,%rax,1)
, 999		// addq %rax, %es:(%rax,%rax,1)
, 999		// addb $0x0, %es:(%rax,%rax,1)
, 999		// addb %al, %es:(%rax,%rax,1)
, 999		// addb  , %es:(%rax,%rax,1)
, 1		// addw $0x0, %ax
, 1		// addw $0x0, %ax
, 999		// addw %es:(%rax,%rax,1), %ax
, 1		// addw %ax, %ax
, 1		// addl $0x0, %eax
, 1		// addl $0x0, %eax
, 999		// addl %es:(%rax,%rax,1), %eax
, 1		// addl %eax, %eax
, 1		// addq $0x0, %rax
, 1		// addq $0x0, %rax
, 999		// addq %es:(%rax,%rax,1), %rax
, 1		// addq %rax, %rax
, 1		// addb $0x0, %al
, 999		// addb %es:(%rax,%rax,1), %al
, 1		// addb %al, %al
, 1		// addb %ah, %al
, 1		// addq $0x0, %rax
, 1		// addb $0x0, %ah
, 999		// addb %es:(%rax,%rax,1), , 
, 1		// addb %al, %ah
, 1		// addb %ah, %ah
, 999		// addpd %es:(%rax,%rax,1), %xmm0
, 1		// addpd %xmm0, %xmm0
, 999		// addps %es:(%rax,%rax,1), %xmm0
, 1		// addps %xmm0, %xmm0
, 999		// addsd %es:(%rax,%rax,1), %xmm0
, 1		// addsd %xmm0, %xmm0
, 999		// addss %es:(%rax,%rax,1), %xmm0
, 1		// addss %xmm0, %xmm0
, 999		// addsubpd %es:(%rax,%rax,1), %xmm0
, 1		// addsubpd %xmm0, %xmm0
, 999		// addsubps %es:(%rax,%rax,1), %xmm0
, 1		// addsubps %xmm0, %xmm0
, 999		// aesdec %es:(%rax,%rax,1), %xmm0
, 1		// aesdec %xmm0, %xmm0
, 999		// aesdeclast %es:(%rax,%rax,1), %xmm0
, 1		// aesdeclast %xmm0, %xmm0
, 999		// aesenc %es:(%rax,%rax,1), %xmm0
, 1		// aesenc %xmm0, %xmm0
, 999		// aesenclast %es:(%rax,%rax,1), %xmm0
, 1		// aesenclast %xmm0, %xmm0
, 999		// aesimc %es:(%rax,%rax,1), %xmm0
, 14		// aesimc %xmm0, %xmm0
, 999		// aeskeygenassist $0x0, %es:(%rax,%rax,1), %xmm0
, 67		// aeskeygenassist $0x0, %xmm0, %xmm0
, 1		// andb $0x0, %al
, 1		// andw $0x0, %ax
, 1		// andl $0x0, %eax
, 999		// andw $0x0, %es:(%rax,%rax,1)
, 999		// andw $0x0, %es:(%rax,%rax,1)
, 999		// andw %ax, %es:(%rax,%rax,1)
, 999		// andl $0x0, %es:(%rax,%rax,1)
, 999		// andl $0x0, %es:(%rax,%rax,1)
, 999		// andl %eax, %es:(%rax,%rax,1)
, 999		// andq $0x0, %es:(%rax,%rax,1)
, 999		// andq $0x0, %es:(%rax,%rax,1)
, 999		// andq %rax, %es:(%rax,%rax,1)
, 999		// andb $0x0, %es:(%rax,%rax,1)
, 999		// andb %al, %es:(%rax,%rax,1)
, 999		// andb  , %es:(%rax,%rax,1)
, 1		// andw $0x0, %ax
, 1		// andw $0x0, %ax
, 999		// andw %es:(%rax,%rax,1), %ax
, 1		// andw %ax, %ax
, 1		// andl $0x0, %eax
, 1		// andl $0x0, %eax
, 999		// andl %es:(%rax,%rax,1), %eax
, 1		// andl %eax, %eax
, 1		// andq $0x0, %rax
, 1		// andq $0x0, %rax
, 999		// andq %es:(%rax,%rax,1), %rax
, 1		// andq %rax, %rax
, 1		// andb $0x0, %al
, 999		// andb %es:(%rax,%rax,1), %al
, 1		// andb %al, %al
, 1		// andb %ah, %al
, 1		// andq $0x0, %rax
, 1		// andb $0x0, %ah
, 999		// andb %es:(%rax,%rax,1), , 
, 1		// andb %al, %ah
, 1		// andb %ah, %ah
, 999		// andnl %es:(%rax,%rax,1), %eax, %eax
, 1		// andnl %eax, %eax, %eax
, 999		// andnq %es:(%rax,%rax,1), %rax, %rax
, 1		// andnq %rax, %rax, %rax
, 999		// andnpd %es:(%rax,%rax,1), %xmm0
, 1		// andnpd %xmm0, %xmm0
, 999		// andnps %es:(%rax,%rax,1), %xmm0
, 1		// andnps %xmm0, %xmm0
, 999		// andpd %es:(%rax,%rax,1), %xmm0
, 1		// andpd %xmm0, %xmm0
, 999		// andps %es:(%rax,%rax,1), %xmm0
, 1		// andps %xmm0, %xmm0
, 999		// bextrl %eax, %es:(%rax,%rax,1), %eax
, 1		// bextrl %eax, %eax, %eax
, 999		// bextrq %rax, %es:(%rax,%rax,1), %rax
, 1		// bextrq %rax, %rax, %rax
, 999		// blendpd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// blendpd $0x0, %xmm0, %xmm0
, 999		// blendps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// blendps $0x0, %xmm0, %xmm0
, 999		// blendvpd %xmm0, %es:(%rax,%rax,1), %xmm0
, 1		// blendvpd %xmm0, %xmm0, %xmm0
, 999		// blendvps %xmm0, %es:(%rax,%rax,1), %xmm0
, 1		// blendvps %xmm0, %xmm0, %xmm0
, 999		// blsil %es:(%rax,%rax,1), %eax
, 1		// blsil %eax, %eax
, 999		// blsiq %es:(%rax,%rax,1), %rax
, 1		// blsiq %rax, %rax
, 999		// blsmskl %es:(%rax,%rax,1), %eax
, 1		// blsmskl %eax, %eax
, 999		// blsmskq %es:(%rax,%rax,1), %rax
, 1		// blsmskq %rax, %rax
, 999		// blsrl %es:(%rax,%rax,1), %eax
, 1		// blsrl %eax, %eax
, 999		// blsrq %es:(%rax,%rax,1), %rax
, 1		// blsrq %rax, %rax
, 999		// bsfl %es:(%rax,%rax,1), %eax
, 1		// bsfl %eax, %eax
, 999		// bsfq %es:(%rax,%rax,1), %rax
, 1		// bsfq %rax, %rax
, 999		// bsrw %es:(%rax,%rax,1), %ax
, 1		// bsrw %ax, %ax
, 999		// bsrl %es:(%rax,%rax,1), %eax
, 1		// bsrl %eax, %eax
, 999		// bsrq %es:(%rax,%rax,1), %rax
, 1		// bsrq %rax, %rax
, 1		// bswap %eax
, 1		// bswap %rax
, 999		// btw $0x0, %es:(%rax,%rax,1)
, 999		// btw %ax, %es:(%rax,%rax,1)
, 999		// btl $0x0, %es:(%rax,%rax,1)
, 999		// btl %eax, %es:(%rax,%rax,1)
, 999		// btq $0x0, %es:(%rax,%rax,1)
, 999		// btq %rax, %es:(%rax,%rax,1)
, 1		// btw $0x0, %ax
, 1		// btw %ax, %ax
, 1		// btl $0x0, %eax
, 1		// btl %eax, %eax
, 1		// btq $0x0, %rax
, 1		// btq %rax, %rax
, 999		// btcw $0x0, %es:(%rax,%rax,1)
, 999		// btcw %ax, %es:(%rax,%rax,1)
, 999		// btcl $0x0, %es:(%rax,%rax,1)
, 999		// btcl %eax, %es:(%rax,%rax,1)
, 999		// btcq $0x0, %es:(%rax,%rax,1)
, 999		// btcq %rax, %es:(%rax,%rax,1)
, 1		// btcw $0x0, %ax
, 1		// btcw %ax, %ax
, 1		// btcl $0x0, %eax
, 1		// btcl %eax, %eax
, 1		// btcq $0x0, %rax
, 1		// btcq %rax, %rax
, 999		// btrw $0x0, %es:(%rax,%rax,1)
, 999		// btrw %ax, %es:(%rax,%rax,1)
, 999		// btrl $0x0, %es:(%rax,%rax,1)
, 999		// btrl %eax, %es:(%rax,%rax,1)
, 999		// btrq $0x0, %es:(%rax,%rax,1)
, 999		// btrq %rax, %es:(%rax,%rax,1)
, 1		// btrw $0x0, %ax
, 1		// btrw %ax, %ax
, 1		// btrl $0x0, %eax
, 1		// btrl %eax, %eax
, 1		// btrq $0x0, %rax
, 1		// btrq %rax, %rax
, 999		// btsw $0x0, %es:(%rax,%rax,1)
, 999		// btsw %ax, %es:(%rax,%rax,1)
, 999		// btsl $0x0, %es:(%rax,%rax,1)
, 999		// btsl %eax, %es:(%rax,%rax,1)
, 999		// btsq $0x0, %es:(%rax,%rax,1)
, 999		// btsq %rax, %es:(%rax,%rax,1)
, 1		// btsw $0x0, %ax
, 1		// btsw %ax, %ax
, 1		// btsl $0x0, %eax
, 1		// btsl %eax, %eax
, 1		// btsq $0x0, %rax
, 1		// btsq %rax, %rax
, 999		// bzhil %eax, %es:(%rax,%rax,1), %eax
, 1		// bzhil %eax, %eax, %eax
, 999		// bzhiq %rax, %es:(%rax,%rax,1), %rax
, 1		// bzhiq %rax, %rax, %rax
, 0		// callq %es:(%rax,%rax,1)
, 0		// callq %es:(%rax,%rax,1)
, 0		// callq %es:(%rax,%rax,1)
, 0		// callq %es:(%rax,%rax,1)
, 0		// callq %rax
, 0		// callq 0
, 0		// callq 
, 1		// cbtw 
, 1		// cltd 
, 1		// cltq 
, 1		// clc 
, 39		// cld 
, 999		// clflush %es:(%rax,%rax,1)
, 999		// cli 
, 1		// cmc 
, 999		// cmovaw %es:(%rax,%rax,1), %ax
, 1		// cmovaw %ax, %ax
, 999		// cmoval %es:(%rax,%rax,1), %eax
, 1		// cmoval %eax, %eax
, 999		// cmovaq %es:(%rax,%rax,1), %rax
, 1		// cmovaq %rax, %rax
, 999		// cmovaew %es:(%rax,%rax,1), %ax
, 1		// cmovaew %ax, %ax
, 999		// cmovael %es:(%rax,%rax,1), %eax
, 1		// cmovael %eax, %eax
, 999		// cmovaeq %es:(%rax,%rax,1), %rax
, 1		// cmovaeq %rax, %rax
, 999		// cmovbw %es:(%rax,%rax,1), %ax
, 1		// cmovbw %ax, %ax
, 999		// cmovbl %es:(%rax,%rax,1), %eax
, 1		// cmovbl %eax, %eax
, 999		// cmovbq %es:(%rax,%rax,1), %rax
, 1		// cmovbq %rax, %rax
, 999		// cmovbew %es:(%rax,%rax,1), %ax
, 1		// cmovbew %ax, %ax
, 999		// cmovbel %es:(%rax,%rax,1), %eax
, 1		// cmovbel %eax, %eax
, 999		// cmovbeq %es:(%rax,%rax,1), %rax
, 1		// cmovbeq %rax, %rax
, 999		// cmovcw %es:(%rax,%rax,1), %ax
, 1		// cmovcw %ax, %ax
, 999		// cmovcl %es:(%rax,%rax,1), %eax
, 1		// cmovcl %eax, %eax
, 999		// cmovcq %es:(%rax,%rax,1), %rax
, 1		// cmovcq %rax, %rax
, 999		// cmovew %es:(%rax,%rax,1), %ax
, 1		// cmovew %ax, %ax
, 999		// cmovel %es:(%rax,%rax,1), %eax
, 1		// cmovel %eax, %eax
, 999		// cmoveq %es:(%rax,%rax,1), %rax
, 1		// cmoveq %rax, %rax
, 999		// cmovgw %es:(%rax,%rax,1), %ax
, 1		// cmovgw %ax, %ax
, 999		// cmovgl %es:(%rax,%rax,1), %eax
, 1		// cmovgl %eax, %eax
, 999		// cmovgq %es:(%rax,%rax,1), %rax
, 1		// cmovgq %rax, %rax
, 999		// cmovgew %es:(%rax,%rax,1), %ax
, 1		// cmovgew %ax, %ax
, 999		// cmovgel %es:(%rax,%rax,1), %eax
, 1		// cmovgel %eax, %eax
, 999		// cmovgeq %es:(%rax,%rax,1), %rax
, 1		// cmovgeq %rax, %rax
, 999		// cmovlw %es:(%rax,%rax,1), %ax
, 1		// cmovlw %ax, %ax
, 999		// cmovll %es:(%rax,%rax,1), %eax
, 1		// cmovll %eax, %eax
, 999		// cmovlq %es:(%rax,%rax,1), %rax
, 1		// cmovlq %rax, %rax
, 999		// cmovlew %es:(%rax,%rax,1), %ax
, 1		// cmovlew %ax, %ax
, 999		// cmovlel %es:(%rax,%rax,1), %eax
, 1		// cmovlel %eax, %eax
, 999		// cmovleq %es:(%rax,%rax,1), %rax
, 1		// cmovleq %rax, %rax
, 999		// cmovnaw %es:(%rax,%rax,1), %ax
, 1		// cmovnaw %ax, %ax
, 999		// cmovnal %es:(%rax,%rax,1), %eax
, 1		// cmovnal %eax, %eax
, 999		// cmovnaq %es:(%rax,%rax,1), %rax
, 1		// cmovnaq %rax, %rax
, 999		// cmovnaew %es:(%rax,%rax,1), %ax
, 1		// cmovnaew %ax, %ax
, 999		// cmovnael %es:(%rax,%rax,1), %eax
, 1		// cmovnael %eax, %eax
, 999		// cmovnaeq %es:(%rax,%rax,1), %rax
, 1		// cmovnaeq %rax, %rax
, 999		// cmovnbw %es:(%rax,%rax,1), %ax
, 1		// cmovnbw %ax, %ax
, 999		// cmovnbl %es:(%rax,%rax,1), %eax
, 1		// cmovnbl %eax, %eax
, 999		// cmovnbq %es:(%rax,%rax,1), %rax
, 1		// cmovnbq %rax, %rax
, 999		// cmovnbew %es:(%rax,%rax,1), %ax
, 1		// cmovnbew %ax, %ax
, 999		// cmovnbel %es:(%rax,%rax,1), %eax
, 1		// cmovnbel %eax, %eax
, 999		// cmovnbeq %es:(%rax,%rax,1), %rax
, 1		// cmovnbeq %rax, %rax
, 999		// cmovncw %es:(%rax,%rax,1), %ax
, 1		// cmovncw %ax, %ax
, 999		// cmovncl %es:(%rax,%rax,1), %eax
, 1		// cmovncl %eax, %eax
, 999		// cmovncq %es:(%rax,%rax,1), %rax
, 1		// cmovncq %rax, %rax
, 999		// cmovnew %es:(%rax,%rax,1), %ax
, 1		// cmovnew %ax, %ax
, 999		// cmovnel %es:(%rax,%rax,1), %eax
, 1		// cmovnel %eax, %eax
, 999		// cmovneq %es:(%rax,%rax,1), %rax
, 1		// cmovneq %rax, %rax
, 999		// cmovngw %es:(%rax,%rax,1), %ax
, 1		// cmovngw %ax, %ax
, 999		// cmovngl %es:(%rax,%rax,1), %eax
, 1		// cmovngl %eax, %eax
, 999		// cmovngq %es:(%rax,%rax,1), %rax
, 1		// cmovngq %rax, %rax
, 999		// cmovngew %es:(%rax,%rax,1), %ax
, 1		// cmovngew %ax, %ax
, 999		// cmovngel %es:(%rax,%rax,1), %eax
, 1		// cmovngel %eax, %eax
, 999		// cmovngeq %es:(%rax,%rax,1), %rax
, 1		// cmovngeq %rax, %rax
, 999		// cmovnlw %es:(%rax,%rax,1), %ax
, 1		// cmovnlw %ax, %ax
, 999		// cmovnll %es:(%rax,%rax,1), %eax
, 1		// cmovnll %eax, %eax
, 999		// cmovnlq %es:(%rax,%rax,1), %rax
, 1		// cmovnlq %rax, %rax
, 999		// cmovnlew %es:(%rax,%rax,1), %ax
, 1		// cmovnlew %ax, %ax
, 999		// cmovnlel %es:(%rax,%rax,1), %eax
, 1		// cmovnlel %eax, %eax
, 999		// cmovnleq %es:(%rax,%rax,1), %rax
, 1		// cmovnleq %rax, %rax
, 999		// cmovnow %es:(%rax,%rax,1), %ax
, 1		// cmovnow %ax, %ax
, 999		// cmovnol %es:(%rax,%rax,1), %eax
, 1		// cmovnol %eax, %eax
, 999		// cmovnoq %es:(%rax,%rax,1), %rax
, 1		// cmovnoq %rax, %rax
, 999		// cmovnpw %es:(%rax,%rax,1), %ax
, 1		// cmovnpw %ax, %ax
, 999		// cmovnpl %es:(%rax,%rax,1), %eax
, 1		// cmovnpl %eax, %eax
, 999		// cmovnpq %es:(%rax,%rax,1), %rax
, 1		// cmovnpq %rax, %rax
, 999		// cmovnsw %es:(%rax,%rax,1), %ax
, 1		// cmovnsw %ax, %ax
, 999		// cmovnsl %es:(%rax,%rax,1), %eax
, 1		// cmovnsl %eax, %eax
, 999		// cmovnsq %es:(%rax,%rax,1), %rax
, 1		// cmovnsq %rax, %rax
, 999		// cmovnzw %es:(%rax,%rax,1), %ax
, 1		// cmovnzw %ax, %ax
, 999		// cmovnzl %es:(%rax,%rax,1), %eax
, 1		// cmovnzl %eax, %eax
, 999		// cmovnzq %es:(%rax,%rax,1), %rax
, 1		// cmovnzq %rax, %rax
, 999		// cmovow %es:(%rax,%rax,1), %ax
, 1		// cmovow %ax, %ax
, 999		// cmovol %es:(%rax,%rax,1), %eax
, 1		// cmovol %eax, %eax
, 999		// cmovoq %es:(%rax,%rax,1), %rax
, 1		// cmovoq %rax, %rax
, 999		// cmovpw %es:(%rax,%rax,1), %ax
, 1		// cmovpw %ax, %ax
, 999		// cmovpl %es:(%rax,%rax,1), %eax
, 1		// cmovpl %eax, %eax
, 999		// cmovpq %es:(%rax,%rax,1), %rax
, 1		// cmovpq %rax, %rax
, 999		// cmovpew %es:(%rax,%rax,1), %ax
, 1		// cmovpew %ax, %ax
, 999		// cmovpel %es:(%rax,%rax,1), %eax
, 1		// cmovpel %eax, %eax
, 999		// cmovpeq %es:(%rax,%rax,1), %rax
, 1		// cmovpeq %rax, %rax
, 999		// cmovpow %es:(%rax,%rax,1), %ax
, 1		// cmovpow %ax, %ax
, 999		// cmovpol %es:(%rax,%rax,1), %eax
, 1		// cmovpol %eax, %eax
, 999		// cmovpoq %es:(%rax,%rax,1), %rax
, 1		// cmovpoq %rax, %rax
, 999		// cmovsw %es:(%rax,%rax,1), %ax
, 1		// cmovsw %ax, %ax
, 999		// cmovsl %es:(%rax,%rax,1), %eax
, 1		// cmovsl %eax, %eax
, 999		// cmovsq %es:(%rax,%rax,1), %rax
, 1		// cmovsq %rax, %rax
, 999		// cmovzw %es:(%rax,%rax,1), %ax
, 1		// cmovzw %ax, %ax
, 999		// cmovzl %es:(%rax,%rax,1), %eax
, 1		// cmovzl %eax, %eax
, 999		// cmovzq %es:(%rax,%rax,1), %rax
, 1		// cmovzq %rax, %rax
, 1		// cmpb $0x0, %al
, 1		// cmpw $0x0, %ax
, 1		// cmpl $0x0, %eax
, 999		// cmpw $0x0, %es:(%rax,%rax,1)
, 999		// cmpw $0x0, %es:(%rax,%rax,1)
, 999		// cmpw %ax, %es:(%rax,%rax,1)
, 999		// cmpl $0x0, %es:(%rax,%rax,1)
, 999		// cmpl $0x0, %es:(%rax,%rax,1)
, 999		// cmpl %eax, %es:(%rax,%rax,1)
, 999		// cmpq $0x0, %es:(%rax,%rax,1)
, 999		// cmpq $0x0, %es:(%rax,%rax,1)
, 999		// cmpq %rax, %es:(%rax,%rax,1)
, 999		// cmpb $0x0, %es:(%rax,%rax,1)
, 999		// cmpb %al, %es:(%rax,%rax,1)
, 999		// cmpb  , %es:(%rax,%rax,1)
, 1		// cmpw $0x0, %ax
, 1		// cmpw $0x0, %ax
, 999		// cmpw %es:(%rax,%rax,1), %ax
, 1		// cmpw %ax, %ax
, 1		// cmpl $0x0, %eax
, 1		// cmpl $0x0, %eax
, 999		// cmpl %es:(%rax,%rax,1), %eax
, 1		// cmpl %eax, %eax
, 1		// cmpq $0x0, %rax
, 1		// cmpq $0x0, %rax
, 999		// cmpq %es:(%rax,%rax,1), %rax
, 1		// cmpq %rax, %rax
, 1		// cmpb $0x0, %al
, 999		// cmpb %es:(%rax,%rax,1), %al
, 1		// cmpb %al, %al
, 1		// cmpb %ah, %al
, 1		// cmpq $0x0, %rax
, 1		// cmpb $0x0, %ah
, 999		// cmpb %es:(%rax,%rax,1), , 
, 1		// cmpb %al, %ah
, 1		// cmpb %ah, %ah
, 999		// cmppd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// cmppd $0x0, %xmm0, %xmm0
, 999		// cmpps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// cmpps $0x0, %xmm0, %xmm0
, 999		// cmps %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// cmps %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// cmps %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// cmps %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// cmpsb 
, 999		// cmpsl 
, 999		// cmpsd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// cmpsd $0x0, %xmm0, %xmm0
, 999		// cmpsq 
, 999		// cmpss $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// cmpss $0x0, %xmm0, %xmm0
, 999		// cmpsw 
, 999		// cmpxchgw %ax, %es:(%rax,%rax,1)
, 999		// cmpxchgl %eax, %es:(%rax,%rax,1)
, 999		// cmpxchgq %rax, %es:(%rax,%rax,1)
, 999		// cmpxchgb %al, %es:(%rax,%rax,1)
, 999		// cmpxchgb  , %es:(%rax,%rax,1)
, 65		// cmpxchgw %ax, %ax
, 65		// cmpxchgl %eax, %eax
, 65		// cmpxchgq %rax, %rax
, 65		// cmpxchgb %al, %al
, 65		// cmpxchgb %ah, %al
, 65		// cmpxchgb %al, %ah
, 65		// cmpxchgb %ah, %ah
, 999		// cmpxchg16b %es:(%rax,%rax,1)
, 999		// cmpxchg8b %es:(%rax,%rax,1)
, 999		// comisd %es:(%rax,%rax,1), %xmm0
, 1		// comisd %xmm0, %xmm0
, 999		// comiss %es:(%rax,%rax,1), %xmm0
, 1		// comiss %xmm0, %xmm0
, 8154		// cpuid 
, 1		// cqto 
, 999		// crc32w %es:(%rax,%rax,1), %eax
, 999		// crc32l %es:(%rax,%rax,1), %eax
, 999		// crc32b %es:(%rax,%rax,1), %eax
, 1		// crc32w %ax, %eax
, 1		// crc32l %eax, %eax
, 1		// crc32b %al, %eax
, 1		// crc32b %ah, %eax
, 999		// crc32q %es:(%rax,%rax,1), %rax
, 999		// crc32b %es:(%rax,%rax,1), %rax
, 1		// crc32q %rax, %rax
, 1		// crc32b %al, %rax
, 1		// crc32b %ah, %rax
, 999		// cvtdq2pd %es:(%rax,%rax,1), %xmm0
, 1		// cvtdq2pd %xmm0, %xmm0
, 999		// cvtdq2ps %es:(%rax,%rax,1), %xmm0
, 1		// cvtdq2ps %xmm0, %xmm0
, 999		// cvtpd2dq %es:(%rax,%rax,1), %xmm0
, 1		// cvtpd2dq %xmm0, %xmm0
, 999		// cvtpd2pi %es:(%rax,%rax,1), %mm0
, 1		// cvtpd2pi %xmm0, %mm0
, 999		// cvtpd2ps %es:(%rax,%rax,1), %xmm0
, 1		// cvtpd2ps %xmm0, %xmm0
, 999		// cvtpi2pd %es:(%rax,%rax,1), %xmm0
, 1		// cvtpi2pd %mm0, %xmm0
, 999		// cvtpi2ps %es:(%rax,%rax,1), %xmm0
, 1		// cvtpi2ps %mm0, %xmm0
, 999		// cvtps2dq %es:(%rax,%rax,1), %xmm0
, 1		// cvtps2dq %xmm0, %xmm0
, 999		// cvtps2pd %es:(%rax,%rax,1), %xmm0
, 1		// cvtps2pd %xmm0, %xmm0
, 999		// cvtps2pi %es:(%rax,%rax,1), %mm0
, 1		// cvtps2pi %xmm0, %mm0
, 999		// cvtsd2si %es:(%rax,%rax,1), %eax
, 1		// cvtsd2si %xmm0, %eax
, 999		// cvtsd2si %es:(%rax,%rax,1), %rax
, 1		// cvtsd2si %xmm0, %rax
, 999		// cvtsd2ss %es:(%rax,%rax,1), %xmm0
, 1		// cvtsd2ss %xmm0, %xmm0
, 999		// cvtsi2sdl %es:(%rax,%rax,1), %xmm0
, 999		// cvtsi2sdq %es:(%rax,%rax,1), %xmm0
, 1		// cvtsi2sdl %eax, %xmm0
, 1		// cvtsi2sdq %rax, %xmm0
, 999		// cvtsi2ssl %es:(%rax,%rax,1), %xmm0
, 999		// cvtsi2ssq %es:(%rax,%rax,1), %xmm0
, 1		// cvtsi2ssl %eax, %xmm0
, 1		// cvtsi2ssq %rax, %xmm0
, 999		// cvtss2sd %es:(%rax,%rax,1), %xmm0
, 1		// cvtss2sd %xmm0, %xmm0
, 999		// cvtss2sil %es:(%rax,%rax,1), %eax
, 1		// cvtss2sil %xmm0, %eax
, 999		// cvtss2siq %es:(%rax,%rax,1), %rax
, 1		// cvtss2siq %xmm0, %rax
, 999		// cvttpd2dq %es:(%rax,%rax,1), %xmm0
, 1		// cvttpd2dq %xmm0, %xmm0
, 999		// cvttpd2pi %es:(%rax,%rax,1), %mm0
, 1		// cvttpd2pi %xmm0, %mm0
, 999		// cvttps2dq %es:(%rax,%rax,1), %xmm0
, 1		// cvttps2dq %xmm0, %xmm0
, 999		// cvttps2pi %es:(%rax,%rax,1), %mm0
, 1		// cvttps2pi %xmm0, %mm0
, 999		// cvttsd2si %es:(%rax,%rax,1), %eax
, 1		// cvttsd2si %xmm0, %eax
, 999		// cvttsd2siq %es:(%rax,%rax,1), %rax
, 1		// cvttsd2siq %xmm0, %rax
, 999		// cvttss2si %es:(%rax,%rax,1), %eax
, 1		// cvttss2si %xmm0, %eax
, 999		// cvttss2siq %es:(%rax,%rax,1), %rax
, 1		// cvttss2siq %xmm0, %rax
, 1		// cwtd 
, 1		// cwtl 
, 999		// decw %es:(%rax,%rax,1)
, 999		// decl %es:(%rax,%rax,1)
, 999		// decq %es:(%rax,%rax,1)
, 999		// decb %es:(%rax,%rax,1)
, 1		// decw %ax
, 1		// decl %eax
, 1		// decq %rax
, 1		// decb %al
, 1		// decb %ah
, 999		// divw %es:(%rax,%rax,1)
, 999		// divl %es:(%rax,%rax,1)
, 999		// divq %es:(%rax,%rax,1)
, 999		// divb %es:(%rax,%rax,1)
, 999		// divw %ax
, 999		// divl %eax
, 999		// divq %rax
, 999		// divb %al
, 999		// divb  
, 999		// divpd %es:(%rax,%rax,1), %xmm0
, 1		// divpd %xmm0, %xmm0
, 999		// divps %es:(%rax,%rax,1), %xmm0
, 1		// divps %xmm0, %xmm0
, 999		// divsd %es:(%rax,%rax,1), %xmm0
, 1		// divsd %xmm0, %xmm0
, 999		// divss %es:(%rax,%rax,1), %xmm0
, 1		// divss %xmm0, %xmm0
, 999		// dppd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// dppd $0x0, %xmm0, %xmm0
, 999		// dpps $0x0, %es:(%rax,%rax,1), %xmm0
, 14		// dpps $0x0, %xmm0, %xmm0
, 271		// emms 
, 0		// enterq $0x0, $0x0
, 0		// enterq $0x0, $0x0
, 0		// enterq $0x0, $0x0
, 999		// extractps $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// extractps $0x0, %xmm0, %eax
, 1		// extractps $0x0, %xmm0, %rax
, 9150		// f2xm1 
, 1		// fabs 
, 999		// fadds %es:(%rax,%rax,1)
, 999		// faddl %es:(%rax,%rax,1)
, 10022		// fadd %st, %st
, 9185		// fadd %st, %st
, 8478		// faddp 
, 8475		// faddp %st, %st
, 999		// fbld %es:(%rax,%rax,1)
, 999		// fbstp %es:(%rax,%rax,1)
, 1		// fchs 
, 1015		// fclex 
, 1		// fcmovb %st, %st
, 1		// fcmovbe %st, %st
, 1		// fcmove %st, %st
, 1		// fcmovnb %st, %st
, 1		// fcmovnbe %st, %st
, 1		// fcmovne %st, %st
, 1		// fcmovnu %st, %st
, 1		// fcmovu %st, %st
, 7907		// fcom 
, 999		// fcoms %es:(%rax,%rax,1)
, 999		// fcoml %es:(%rax,%rax,1)
, 7858		// fcom %st
, 8613		// fcomi %st, %st
, 8837		// fcomip %st, %st
, 8368		// fcomp 
, 999		// fcomps %es:(%rax,%rax,1)
, 999		// fcompl %es:(%rax,%rax,1)
, 8372		// fcomp %st
, 8788		// fcompp 
, 9173		// fcos 
, 1		// fdecstp 
, 999		// fdivs %es:(%rax,%rax,1)
, 999		// fdivl %es:(%rax,%rax,1)
, 10018		// fdiv %st, %st
, 9186		// fdiv %st, %st
, 8477		// fdivp 
, 8478		// fdivp %st, %st
, 999		// fdivrs %es:(%rax,%rax,1)
, 999		// fdivrl %es:(%rax,%rax,1)
, 10016		// fdivr %st, %st
, 9190		// fdivr %st, %st
, 8476		// fdivrp 
, 8487		// fdivrp %st, %st
, 1		// ffreep %st
, 999		// fiadd %es:(%rax,%rax,1)
, 999		// fiaddl %es:(%rax,%rax,1)
, 999		// ficom %es:(%rax,%rax,1)
, 999		// ficoml %es:(%rax,%rax,1)
, 999		// ficomp %es:(%rax,%rax,1)
, 999		// ficompl %es:(%rax,%rax,1)
, 999		// fidiv %es:(%rax,%rax,1)
, 999		// fidivl %es:(%rax,%rax,1)
, 999		// fidivr %es:(%rax,%rax,1)
, 999		// fidivrl %es:(%rax,%rax,1)
, 999		// fild %es:(%rax,%rax,1)
, 999		// fildl %es:(%rax,%rax,1)
, 999		// fildll %es:(%rax,%rax,1)
, 999		// fimul %es:(%rax,%rax,1)
, 999		// fimull %es:(%rax,%rax,1)
, 1		// fincstp 
, 2349		// finit 
, 999		// fist %es:(%rax,%rax,1)
, 999		// fistl %es:(%rax,%rax,1)
, 999		// fistp %es:(%rax,%rax,1)
, 999		// fistpl %es:(%rax,%rax,1)
, 999		// fistpll %es:(%rax,%rax,1)
, 999		// fisttp %es:(%rax,%rax,1)
, 999		// fisttpl %es:(%rax,%rax,1)
, 999		// fisttpll %es:(%rax,%rax,1)
, 999		// fisub %es:(%rax,%rax,1)
, 999		// fisubl %es:(%rax,%rax,1)
, 999		// fisubr %es:(%rax,%rax,1)
, 999		// fisubrl %es:(%rax,%rax,1)
, 999		// flds %es:(%rax,%rax,1)
, 999		// fldl %es:(%rax,%rax,1)
, 999		// fldt %es:(%rax,%rax,1)
, 8476		// fld %st
, 8841		// fld1 
, 999		// fldcw %es:(%rax,%rax,1)
, 999		// fldenvl %es:(%rax,%rax,1)
, 8854		// fldl2e 
, 8862		// fldl2t 
, 8850		// fldlg2 
, 8849		// fldln2 
, 8838		// fldpi 
, 8474		// fldz 
, 999		// fmuls %es:(%rax,%rax,1)
, 999		// fmull %es:(%rax,%rax,1)
, 10020		// fmul %st, %st
, 9182		// fmul %st, %st
, 8476		// fmulp 
, 8477		// fmulp %st, %st
, 1015		// fnclex 
, 2324		// fninit 
, 1		// fnop 
, 999		// fnsave %es:(%rax,%rax,1)
, 999		// fnstcw %es:(%rax,%rax,1)
, 999		// fnstenvl %es:(%rax,%rax,1)
, 1		// fnstsw %ax
, 999		// fnstsw %es:(%rax,%rax,1)
, 8854		// fpatan 
, 8885		// fprem 
, 8852		// fprem1 
, 8877		// fptan 
, 9172		// frndint 
, 999		// frstor %es:(%rax,%rax,1)
, 999		// fsave %es:(%rax,%rax,1)
, 9586		// fscale 
, 9171		// fsin 
, 8880		// fsincos 
, 8778		// fsqrt 
, 999		// fsts %es:(%rax,%rax,1)
, 999		// fstl %es:(%rax,%rax,1)
, 1		// fst %st
, 999		// fstcw %es:(%rax,%rax,1)
, 999		// fstenvl %es:(%rax,%rax,1)
, 999		// fstps %es:(%rax,%rax,1)
, 999		// fstpl %es:(%rax,%rax,1)
, 999		// fstpt %es:(%rax,%rax,1)
, 8478		// fstp %st
, 1		// fstsw %ax
, 999		// fstsw %es:(%rax,%rax,1)
, 999		// fsubs %es:(%rax,%rax,1)
, 999		// fsubl %es:(%rax,%rax,1)
, 10016		// fsub %st, %st
, 9186		// fsub %st, %st
, 8480		// fsubp 
, 8477		// fsubp %st, %st
, 999		// fsubrs %es:(%rax,%rax,1)
, 999		// fsubrl %es:(%rax,%rax,1)
, 10016		// fsubr %st, %st
, 9187		// fsubr %st, %st
, 8480		// fsubrp 
, 8480		// fsubrp %st, %st
, 7897		// ftst 
, 7894		// fucom 
, 7897		// fucom %st
, 8735		// fucomi %st, %st
, 8834		// fucomip %st, %st
, 8368		// fucomp 
, 8367		// fucomp %st
, 8788		// fucompp 
, 1		// fwait 
, 6850		// fxam 
, 1		// fxch 
, 1		// fxch %st
, 999		// fxrstor %es:(%rax,%rax,1)
, 999		// fxrstor64 %es:(%rax,%rax,1)
, 999		// fxsave %es:(%rax,%rax,1)
, 999		// fxsave64 %es:(%rax,%rax,1)
, 8900		// fxtract 
, 8849		// fyl2x 
, 8850		// fyl2xp1 
, 999		// haddpd %es:(%rax,%rax,1), %xmm0
, 1		// haddpd %xmm0, %xmm0
, 999		// haddps %es:(%rax,%rax,1), %xmm0
, 1		// haddps %xmm0, %xmm0
, 999		// hsubpd %es:(%rax,%rax,1), %xmm0
, 1		// hsubpd %xmm0, %xmm0
, 999		// hsubps %es:(%rax,%rax,1), %xmm0
, 1		// hsubps %xmm0, %xmm0
, 999		// idivw %es:(%rax,%rax,1)
, 999		// idivl %es:(%rax,%rax,1)
, 999		// idivq %es:(%rax,%rax,1)
, 999		// idivb %es:(%rax,%rax,1)
, 999		// idivw %ax
, 999		// idivl %eax
, 999		// idivq %rax
, 999		// idivb %al
, 999		// idivb  
, 999		// imulw %es:(%rax,%rax,1)
, 999		// imull %es:(%rax,%rax,1)
, 999		// imulq %es:(%rax,%rax,1)
, 999		// imulb %es:(%rax,%rax,1)
, 1		// imulw %ax
, 999		// imulw %es:(%rax,%rax,1), %ax
, 999		// imulw $0x0, %es:(%rax,%rax,1), %ax
, 999		// imulw $0x0, %es:(%rax,%rax,1), %ax
, 1		// imulw %ax, %ax
, 1		// imulw $0x0, %ax, %ax
, 1		// imulw $0x0, %ax, %ax
, 1		// imull %eax
, 999		// imull %es:(%rax,%rax,1), %eax
, 999		// imull $0x0, %es:(%rax,%rax,1), %eax
, 999		// imull $0x0, %es:(%rax,%rax,1), %eax
, 1		// imull %eax, %eax
, 1		// imull $0x0, %eax, %eax
, 1		// imull $0x0, %eax, %eax
, 1		// imulq %rax
, 999		// imulq %es:(%rax,%rax,1), %rax
, 999		// imulq $0x0, %es:(%rax,%rax,1), %rax
, 999		// imulq $0x0, %es:(%rax,%rax,1), %rax
, 1		// imulq %rax, %rax
, 1		// imulq $0x0, %rax, %rax
, 1		// imulq $0x0, %rax, %rax
, 1		// imulb %al
, 1		// imulb %ah
, 999		// inb %ax, %al
, 999		// inb $0x0, %al
, 999		// inw %ax, %ax
, 999		// inw $0x0, %ax
, 999		// inl %ax, %eax
, 999		// inl $0x0, %eax
, 999		// incw %es:(%rax,%rax,1)
, 999		// incl %es:(%rax,%rax,1)
, 999		// incq %es:(%rax,%rax,1)
, 999		// incb %es:(%rax,%rax,1)
, 1		// incw %ax
, 1		// incl %eax
, 1		// incq %rax
, 1		// incb %al
, 1		// incb %ah
, 999		// ins %ax, %es:(%rax,%rax,1)
, 999		// ins %ax, %es:(%rax,%rax,1)
, 999		// ins %ax, %es:(%rax,%rax,1)
, 999		// insb 
, 999		// insl 
, 999		// insertps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// insertps $0x0, %xmm0, %xmm0
, 999		// insw 
, 999		// int $0x0
, 999		// int $0x0
, 999		// invpcid %es:(%rax,%rax,1), %rax
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
, 0		// jmpq %es:(%rax,%rax,1)
, 0		// jmpq %es:(%rax,%rax,1)
, 0		// jmpq %es:(%rax,%rax,1)
, 0		// jmpq %es:(%rax,%rax,1)
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
, 1		// lahf 
, 999		// larw %es:(%rax,%rax,1), %ax
, 4010		// larw %ax, %ax
, 999		// lar %es:(%rax,%rax,1), %eax
, 4017		// lar %eax, %eax
, 999		// lar %es:(%rax,%rax,1), %rax
, 4010		// lar %eax, %rax
, 999		// lddqu %es:(%rax,%rax,1), %xmm0
, 999		// ldmxcsr %es:(%rax,%rax,1)
, 1		// leaw (%rax), %ax
, 1		// leaw (%rax), %ax
, 1		// leaw (%rax), %ax
, 1		// leal (%rax), %eax
, 1		// leal (%rax), %eax
, 1		// leal (%rax), %eax
, 1		// leaq (%rax), %rax
, 1		// leaq (%rax), %rax
, 1		// leaq (%rax), %rax
, 0		// leaveq 
, 0		// leavew 
, 707		// lfence 
, 999		// lfsw %es:(%rax,%rax,1), %ax
, 999		// lfsl %es:(%rax,%rax,1), %eax
, 999		// lfs %es:(%rax,%rax,1), %rax
, 999		// lgsw %es:(%rax,%rax,1), %ax
, 999		// lgsl %es:(%rax,%rax,1), %eax
, 999		// lgs %es:(%rax,%rax,1), %rax
, 999		// lock 
, 999		// lods %es:(%rax,%rax,1)
, 999		// lods %es:(%rax,%rax,1)
, 999		// lods %es:(%rax,%rax,1)
, 999		// lods %es:(%rax,%rax,1)
, 999		// lodsb 
, 999		// lodsl 
, 999		// lodsq 
, 999		// lodsw 
, 0		// loop 0
, 0		// loope 0
, 0		// loopne 0
, 999		// lslw %es:(%rax,%rax,1), %ax
, 4010		// lslw %ax, %ax
, 999		// lsl %es:(%rax,%rax,1), %eax
, 4010		// lsl %eax, %eax
, 999		// lsl %es:(%rax,%rax,1), %rax
, 4011		// lsl %eax, %rax
, 999		// lssw %es:(%rax,%rax,1), %ax
, 999		// lssl %es:(%rax,%rax,1), %eax
, 999		// lss %es:(%rax,%rax,1), %rax
, 999		// lzcntw %es:(%rax,%rax,1), %ax
, 1		// lzcntw %ax, %ax
, 999		// lzcntl %es:(%rax,%rax,1), %eax
, 1		// lzcntl %eax, %eax
, 999		// lzcntq %es:(%rax,%rax,1), %rax
, 1		// lzcntq %rax, %rax
, 142		// maskmovdqu %xmm0, %xmm0
, 4135		// maskmovq %mm0, %mm0
, 999		// maxpd %es:(%rax,%rax,1), %xmm0
, 1		// maxpd %xmm0, %xmm0
, 999		// maxps %es:(%rax,%rax,1), %xmm0
, 1		// maxps %xmm0, %xmm0
, 999		// maxsd %es:(%rax,%rax,1), %xmm0
, 1		// maxsd %xmm0, %xmm0
, 999		// maxss %es:(%rax,%rax,1), %xmm0
, 1		// maxss %xmm0, %xmm0
, 1400		// mfence 
, 999		// minpd %es:(%rax,%rax,1), %xmm0
, 1		// minpd %xmm0, %xmm0
, 999		// minps %es:(%rax,%rax,1), %xmm0
, 1		// minps %xmm0, %xmm0
, 999		// minsd %es:(%rax,%rax,1), %xmm0
, 1		// minsd %xmm0, %xmm0
, 999		// minss %es:(%rax,%rax,1), %xmm0
, 1		// minss %xmm0, %xmm0
, 999		// monitor 
, 999		// movabsb %es:0x0, %al
, 999		// movabsb , %es:0x0, %al
, 999		// movabsw %es:0x0, %ax
, 999		// movabsl %es:0x0, %eax
, 999		// movw $0x0, %es:(%rax,%rax,1)
, 999		// movw %ax, %es:(%rax,%rax,1)
, 999		// movw %es, %es:(%rax,%rax,1)
, 999		// movl $0x0, %es:(%rax,%rax,1)
, 999		// movl %eax, %es:(%rax,%rax,1)
, 999		// movq $0x0, %es:(%rax,%rax,1)
, 999		// movq %rax, %es:(%rax,%rax,1)
, 999		// movq %es, %es:(%rax,%rax,1)
, 999		// movb $0x0, %es:(%rax,%rax,1)
, 999		// movb %al, %es:(%rax,%rax,1)
, 999		// movb  , %es:(%rax,%rax,1)
, 999		// movabsw %ax, %es:0x0
, 999		// movabsl %eax, %es:0x0
, 999		// movabsq %rax, %es:0x0
, 999		// movabsb %al, %es:0x0
, 999		// movabsb , %al, %es:0x0
, 1		// movw $0x0, %ax
, 999		// movw %es:(%rax,%rax,1), %ax
, 1		// movw %ax, %ax
, 1		// movw %fs, %ax
, 1		// movl $0x0, %eax
, 999		// movl %es:(%rax,%rax,1), %eax
, 1		// movl %eax, %eax
, 1		// movq $0x0, %rax
, 1		// movq $0x0, %rax
, 999		// movq %es:(%rax,%rax,1), %rax
, 1		// movq %rax, %rax
, 1		// movq %fs, %rax
, 1		// movb $0x0, %al
, 999		// movb %es:(%rax,%rax,1), %al
, 1		// movb %al, %al
, 1		// movb %ah, %al
, 999		// movabsq %es:0x0, %rax
, 1		// movb $0x0, %ah
, 999		// movb %es:(%rax,%rax,1), , 
, 1		// movb %al, %ah
, 1		// movb %ah, %ah
, 999		// mov %es:(%rax,%rax,1), %es
, 999		// mov %es:(%rax,%rax,1), %es
, 999		// mov %ax, %es
, 999		// mov %rax, %es
, 999		// movapd %xmm0, %es:(%rax,%rax,1)
, 999		// movapd %es:(%rax,%rax,1), %xmm0
, 1		// movapd %xmm0, %xmm0
, 999		// movaps %xmm0, %es:(%rax,%rax,1)
, 999		// movaps %es:(%rax,%rax,1), %xmm0
, 1		// movaps %xmm0, %xmm0
, 999		// movbew %ax, %es:(%rax,%rax,1)
, 999		// movbel %eax, %es:(%rax,%rax,1)
, 999		// movbeq %rax, %es:(%rax,%rax,1)
, 999		// movbew %es:(%rax,%rax,1), %ax
, 999		// movbel %es:(%rax,%rax,1), %eax
, 999		// movbeq %es:(%rax,%rax,1), %rax
, 999		// movd %mm0, %es:(%rax,%rax,1)
, 999		// movd %xmm0, %es:(%rax,%rax,1)
, 999		// movd %es:(%rax,%rax,1), %mm0
, 1		// movd %eax, %mm0
, 1		// movd %mm0, %eax
, 1		// movd %xmm0, %eax
, 999		// movd %es:(%rax,%rax,1), %xmm0
, 1		// movd %eax, %xmm0
, 999		// movddup %es:(%rax,%rax,1), %xmm0
, 1		// movddup %xmm0, %xmm0
, 1		// movdq2q %xmm0, %mm0
, 999		// movdqa %xmm0, %es:(%rax,%rax,1)
, 999		// movdqa %es:(%rax,%rax,1), %xmm0
, 1		// movdqa %xmm0, %xmm0
, 999		// movdqu %xmm0, %es:(%rax,%rax,1)
, 999		// movdqu %es:(%rax,%rax,1), %xmm0
, 1		// movdqu %xmm0, %xmm0
, 1		// movhlps %xmm0, %xmm0
, 999		// movhpd %xmm0, %es:(%rax,%rax,1)
, 999		// movhpd %es:(%rax,%rax,1), %xmm0
, 999		// movhps %xmm0, %es:(%rax,%rax,1)
, 999		// movhps %es:(%rax,%rax,1), %xmm0
, 1		// movlhps %xmm0, %xmm0
, 999		// movlpd %xmm0, %es:(%rax,%rax,1)
, 999		// movlpd %es:(%rax,%rax,1), %xmm0
, 999		// movlps %xmm0, %es:(%rax,%rax,1)
, 999		// movlps %es:(%rax,%rax,1), %xmm0
, 1		// movmskpd %xmm0, %eax
, 1		// movmskpd %xmm0, %rax
, 1		// movmskps %xmm0, %eax
, 1		// movmskps %xmm0, %rax
, 999		// movntdq %xmm0, %es:(%rax,%rax,1)
, 999		// movntdq %ymm0, %es:(%rax,%rax,1)
, 999		// movntdqa %es:(%rax,%rax,1), %xmm0
, 999		// movnti %eax, %es:(%rax,%rax,1)
, 999		// movnti %rax, %es:(%rax,%rax,1)
, 999		// movntpd %xmm0, %es:(%rax,%rax,1)
, 999		// movntps %xmm0, %es:(%rax,%rax,1)
, 999		// movntq %mm0, %es:(%rax,%rax,1)
, 999		// movq %mm0, %es:(%rax,%rax,1)
, 999		// movq %xmm0, %es:(%rax,%rax,1)
, 999		// movq %es:(%rax,%rax,1), %mm0
, 1		// movq %mm0, %mm0
, 1		// movq %rax, %mm0
, 1		// movq %mm0, %rax
, 1		// movq %xmm0, %rax
, 999		// movq %es:(%rax,%rax,1), %xmm0
, 1		// movq %rax, %xmm0
, 1		// movq %xmm0, %xmm0
, 1		// movq2dq %mm0, %xmm0
, 999		// movs %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// movs %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// movs %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// movs %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// movsb 
, 999		// movsl 
, 999		// movsd %xmm0, %es:(%rax,%rax,1)
, 999		// movsd %es:(%rax,%rax,1), %xmm0
, 1		// movsd %xmm0, %xmm0
, 999		// movshdup %es:(%rax,%rax,1), %xmm0
, 1		// movshdup %xmm0, %xmm0
, 999		// movsldup %es:(%rax,%rax,1), %xmm0
, 1		// movsldup %xmm0, %xmm0
, 999		// movsq 
, 999		// movss %xmm0, %es:(%rax,%rax,1)
, 999		// movss %es:(%rax,%rax,1), %xmm0
, 1		// movss %xmm0, %xmm0
, 999		// movsw 
, 999		// movsbw %es:(%rax,%rax,1), %ax
, 1		// movsbw %al, %ax
, 1		// movsbw %ah, %ax
, 999		// movswl %es:(%rax,%rax,1), %eax
, 999		// movsbl %es:(%rax,%rax,1), %eax
, 1		// movswl %ax, %eax
, 1		// movsbl %al, %eax
, 1		// movsbl %ah, %eax
, 999		// movswq %es:(%rax,%rax,1), %rax
, 999		// movsbq %es:(%rax,%rax,1), %rax
, 1		// movswq %ax, %rax
, 1		// movsbq %al, %rax
, 1		// movsbq %ah, %rax
, 999		// movslq %es:(%rax,%rax,1), %rax
, 1		// movslq %eax, %rax
, 999		// movupd %xmm0, %es:(%rax,%rax,1)
, 999		// movupd %es:(%rax,%rax,1), %xmm0
, 1		// movupd %xmm0, %xmm0
, 999		// movups %xmm0, %es:(%rax,%rax,1)
, 999		// movups %es:(%rax,%rax,1), %xmm0
, 1		// movups %xmm0, %xmm0
, 999		// movzbw %es:(%rax,%rax,1), %ax
, 1		// movzbw %al, %ax
, 1		// movzbw %ah, %ax
, 999		// movzwl %es:(%rax,%rax,1), %eax
, 999		// movzbl %es:(%rax,%rax,1), %eax
, 1		// movzwl %ax, %eax
, 1		// movzbl %al, %eax
, 1		// movzbl %ah, %eax
, 999		// movzwq %es:(%rax,%rax,1), %rax
, 999		// movzbq %es:(%rax,%rax,1), %rax
, 1		// movzwq %ax, %rax
, 1		// movzbq %al, %rax
, 1		// movzbq %ah, %rax
, 999		// mpsadbw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// mpsadbw $0x0, %xmm0, %xmm0
, 999		// mulw %es:(%rax,%rax,1)
, 999		// mull %es:(%rax,%rax,1)
, 999		// mulq %es:(%rax,%rax,1)
, 999		// mulb %es:(%rax,%rax,1)
, 1		// mulw %ax
, 1		// mull %eax
, 1		// mulq %rax
, 1		// mulb %al
, 1		// mulb %ah
, 999		// mulpd %es:(%rax,%rax,1), %xmm0
, 1		// mulpd %xmm0, %xmm0
, 999		// mulps %es:(%rax,%rax,1), %xmm0
, 1		// mulps %xmm0, %xmm0
, 999		// mulsd %es:(%rax,%rax,1), %xmm0
, 1		// mulsd %xmm0, %xmm0
, 999		// mulss %es:(%rax,%rax,1), %xmm0
, 1		// mulss %xmm0, %xmm0
, 999		// mulxl %es:(%rax,%rax,1), %eax, %eax
, 1		// mulxl %eax, %eax, %eax
, 999		// mulxq %es:(%rax,%rax,1), %rax, %rax
, 1		// mulxq %rax, %rax, %rax
, 999		// mwait 
, 999		// negw %es:(%rax,%rax,1)
, 999		// negl %es:(%rax,%rax,1)
, 999		// negq %es:(%rax,%rax,1)
, 999		// negb %es:(%rax,%rax,1)
, 1		// negw %ax
, 1		// negl %eax
, 1		// negq %rax
, 1		// negb %al
, 1		// negb %ah
, 1		// nop 
, 999		// nopw %es:(%rax,%rax,1)
, 999		// nopl %es:(%rax,%rax,1)
, 1		// nopw %ax
, 1		// nopl %eax
, 999		// notw %es:(%rax,%rax,1)
, 999		// notl %es:(%rax,%rax,1)
, 999		// notq %es:(%rax,%rax,1)
, 999		// notb %es:(%rax,%rax,1)
, 1		// notw %ax
, 1		// notl %eax
, 1		// notq %rax
, 1		// notb %al
, 1		// notb %ah
, 1		// orb $0x0, %al
, 1		// orw $0x0, %ax
, 1		// orl $0x0, %eax
, 999		// orw $0x0, %es:(%rax,%rax,1)
, 999		// orw $0x0, %es:(%rax,%rax,1)
, 999		// orw %ax, %es:(%rax,%rax,1)
, 999		// orl $0x0, %es:(%rax,%rax,1)
, 999		// orl $0x0, %es:(%rax,%rax,1)
, 999		// orl %eax, %es:(%rax,%rax,1)
, 999		// orq $0x0, %es:(%rax,%rax,1)
, 999		// orq $0x0, %es:(%rax,%rax,1)
, 999		// orq %rax, %es:(%rax,%rax,1)
, 999		// orb $0x0, %es:(%rax,%rax,1)
, 999		// orb %al, %es:(%rax,%rax,1)
, 999		// orb  , %es:(%rax,%rax,1)
, 1		// orw $0x0, %ax
, 1		// orw $0x0, %ax
, 999		// orw %es:(%rax,%rax,1), %ax
, 1		// orw %ax, %ax
, 1		// orl $0x0, %eax
, 1		// orl $0x0, %eax
, 999		// orl %es:(%rax,%rax,1), %eax
, 1		// orl %eax, %eax
, 1		// orq $0x0, %rax
, 1		// orq $0x0, %rax
, 999		// orq %es:(%rax,%rax,1), %rax
, 1		// orq %rax, %rax
, 1		// orb $0x0, %al
, 999		// orb %es:(%rax,%rax,1), %al
, 1		// orb %al, %al
, 1		// orb %ah, %al
, 1		// orq $0x0, %rax
, 1		// orb $0x0, %ah
, 999		// orb %es:(%rax,%rax,1), , 
, 1		// orb %al, %ah
, 1		// orb %ah, %ah
, 999		// orpd %es:(%rax,%rax,1), %xmm0
, 1		// orpd %xmm0, %xmm0
, 999		// orps %es:(%rax,%rax,1), %xmm0
, 1		// orps %xmm0, %xmm0
, 999		// outb %al, %ax
, 999		// outw %ax, %ax
, 999		// outl %eax, %ax
, 999		// outb %al, $0x0
, 999		// outw %ax, $0x0
, 999		// outl %eax, $0x0
, 999		// outs %es:(%rax,%rax,1), %ax
, 999		// outs %es:(%rax,%rax,1), %ax
, 999		// outs %es:(%rax,%rax,1), %ax
, 999		// outsb 
, 999		// outsl 
, 999		// outsw 
, 999		// pabsb %es:(%rax,%rax,1), %mm0
, 1		// pabsb %mm0, %mm0
, 999		// pabsb %es:(%rax,%rax,1), %xmm0
, 1		// pabsb %xmm0, %xmm0
, 999		// pabsd %es:(%rax,%rax,1), %mm0
, 1		// pabsd %mm0, %mm0
, 999		// pabsd %es:(%rax,%rax,1), %xmm0
, 1		// pabsd %xmm0, %xmm0
, 999		// pabsw %es:(%rax,%rax,1), %mm0
, 1		// pabsw %mm0, %mm0
, 999		// pabsw %es:(%rax,%rax,1), %xmm0
, 1		// pabsw %xmm0, %xmm0
, 999		// packssdw %es:(%rax,%rax,1), %mm0
, 1		// packssdw %mm0, %mm0
, 999		// packssdw %es:(%rax,%rax,1), %xmm0
, 1		// packssdw %xmm0, %xmm0
, 999		// packsswb %es:(%rax,%rax,1), %mm0
, 1		// packsswb %mm0, %mm0
, 999		// packsswb %es:(%rax,%rax,1), %xmm0
, 1		// packsswb %xmm0, %xmm0
, 999		// packusdw %es:(%rax,%rax,1), %xmm0
, 1		// packusdw %xmm0, %xmm0
, 999		// packuswb %es:(%rax,%rax,1), %mm0
, 1		// packuswb %mm0, %mm0
, 999		// packuswb %es:(%rax,%rax,1), %xmm0
, 1		// packuswb %xmm0, %xmm0
, 999		// paddb %es:(%rax,%rax,1), %mm0
, 1		// paddb %mm0, %mm0
, 999		// paddb %es:(%rax,%rax,1), %xmm0
, 1		// paddb %xmm0, %xmm0
, 999		// paddd %es:(%rax,%rax,1), %mm0
, 1		// paddd %mm0, %mm0
, 999		// paddd %es:(%rax,%rax,1), %xmm0
, 1		// paddd %xmm0, %xmm0
, 999		// paddq %es:(%rax,%rax,1), %mm0
, 1		// paddq %mm0, %mm0
, 999		// paddq %es:(%rax,%rax,1), %xmm0
, 1		// paddq %xmm0, %xmm0
, 999		// paddsb %es:(%rax,%rax,1), %mm0
, 1		// paddsb %mm0, %mm0
, 999		// paddsb %es:(%rax,%rax,1), %xmm0
, 1		// paddsb %xmm0, %xmm0
, 999		// paddsw %es:(%rax,%rax,1), %mm0
, 1		// paddsw %mm0, %mm0
, 999		// paddsw %es:(%rax,%rax,1), %xmm0
, 1		// paddsw %xmm0, %xmm0
, 999		// paddusb %es:(%rax,%rax,1), %mm0
, 1		// paddusb %mm0, %mm0
, 999		// paddusb %es:(%rax,%rax,1), %xmm0
, 1		// paddusb %xmm0, %xmm0
, 999		// paddusw %es:(%rax,%rax,1), %mm0
, 1		// paddusw %mm0, %mm0
, 999		// paddusw %es:(%rax,%rax,1), %xmm0
, 1		// paddusw %xmm0, %xmm0
, 999		// paddw %es:(%rax,%rax,1), %mm0
, 1		// paddw %mm0, %mm0
, 999		// paddw %es:(%rax,%rax,1), %xmm0
, 1		// paddw %xmm0, %xmm0
, 999		// palignr $0x0, %es:(%rax,%rax,1), %mm0
, 1		// palignr $0x0, %mm0, %mm0
, 999		// palignr $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// palignr $0x0, %xmm0, %xmm0
, 999		// pand %es:(%rax,%rax,1), %mm0
, 1		// pand %mm0, %mm0
, 999		// pand %es:(%rax,%rax,1), %xmm0
, 1		// pand %xmm0, %xmm0
, 999		// pandn %es:(%rax,%rax,1), %mm0
, 1		// pandn %mm0, %mm0
, 999		// pandn %es:(%rax,%rax,1), %xmm0
, 1		// pandn %xmm0, %xmm0
, 707		// pause 
, 999		// pavgb %es:(%rax,%rax,1), %mm0
, 1		// pavgb %mm0, %mm0
, 999		// pavgb %es:(%rax,%rax,1), %xmm0
, 1		// pavgb %xmm0, %xmm0
, 999		// pavgw %es:(%rax,%rax,1), %mm0
, 1		// pavgw %mm0, %mm0
, 999		// pavgw %es:(%rax,%rax,1), %xmm0
, 1		// pavgw %xmm0, %xmm0
, 999		// pblendvb %xmm0, %es:(%rax,%rax,1), %xmm0
, 1		// pblendvb %xmm0, %xmm0, %xmm0
, 999		// pblendw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pblendw $0x0, %xmm0, %xmm0
, 999		// pclmulqdq $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pclmulqdq $0x0, %xmm0, %xmm0
, 999		// pcmpeqb %es:(%rax,%rax,1), %mm0
, 1		// pcmpeqb %mm0, %mm0
, 999		// pcmpeqb %es:(%rax,%rax,1), %xmm0
, 1		// pcmpeqb %xmm0, %xmm0
, 999		// pcmpeqd %es:(%rax,%rax,1), %mm0
, 1		// pcmpeqd %mm0, %mm0
, 999		// pcmpeqd %es:(%rax,%rax,1), %xmm0
, 1		// pcmpeqd %xmm0, %xmm0
, 999		// pcmpeqq %es:(%rax,%rax,1), %xmm0
, 1		// pcmpeqq %xmm0, %xmm0
, 999		// pcmpeqw %es:(%rax,%rax,1), %mm0
, 1		// pcmpeqw %mm0, %mm0
, 999		// pcmpeqw %es:(%rax,%rax,1), %xmm0
, 1		// pcmpeqw %xmm0, %xmm0
, 999		// pcmpestri $0x0, %es:(%rax,%rax,1), %xmm0
, 41		// pcmpestri $0x0, %xmm0, %xmm0
, 999		// pcmpestrm $0x0, %es:(%rax,%rax,1), %xmm0
, 65		// pcmpestrm $0x0, %xmm0, %xmm0
, 999		// pcmpgtb %es:(%rax,%rax,1), %mm0
, 1		// pcmpgtb %mm0, %mm0
, 999		// pcmpgtb %es:(%rax,%rax,1), %xmm0
, 1		// pcmpgtb %xmm0, %xmm0
, 999		// pcmpgtd %es:(%rax,%rax,1), %mm0
, 1		// pcmpgtd %mm0, %mm0
, 999		// pcmpgtd %es:(%rax,%rax,1), %xmm0
, 1		// pcmpgtd %xmm0, %xmm0
, 999		// pcmpgtq %es:(%rax,%rax,1), %xmm0
, 1		// pcmpgtq %xmm0, %xmm0
, 999		// pcmpgtw %es:(%rax,%rax,1), %mm0
, 1		// pcmpgtw %mm0, %mm0
, 999		// pcmpgtw %es:(%rax,%rax,1), %xmm0
, 1		// pcmpgtw %xmm0, %xmm0
, 999		// pcmpistri $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pcmpistri $0x0, %xmm0, %xmm0
, 999		// pcmpistrm $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pcmpistrm $0x0, %xmm0, %xmm0
, 999		// pdepl %es:(%rax,%rax,1), %eax, %eax
, 1		// pdepl %eax, %eax, %eax
, 999		// pdepq %es:(%rax,%rax,1), %rax, %rax
, 1		// pdepq %rax, %rax, %rax
, 999		// pextl %es:(%rax,%rax,1), %eax, %eax
, 1		// pextl %eax, %eax, %eax
, 999		// pextq %es:(%rax,%rax,1), %rax, %rax
, 1		// pextq %rax, %rax, %rax
, 999		// pextrb $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// pextrb $0x0, %xmm0, %eax
, 1		// pextrb $0x0, %xmm0, %rax
, 999		// pextrd $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// pextrd $0x0, %xmm0, %eax
, 999		// pextrq $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// pextrq $0x0, %xmm0, %rax
, 999		// pextrw $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// pextrw $0x0, %mm0, %eax
, 1		// pextrw $0x0, %xmm0, %eax
, 1		// pextrw $0x0, %mm0, %rax
, 1		// pextrw $0x0, %xmm0, %rax
, 999		// phaddd %es:(%rax,%rax,1), %mm0
, 1		// phaddd %mm0, %mm0
, 999		// phaddd %es:(%rax,%rax,1), %xmm0
, 1		// phaddd %xmm0, %xmm0
, 999		// phaddsw %es:(%rax,%rax,1), %mm0
, 1		// phaddsw %mm0, %mm0
, 999		// phaddsw %es:(%rax,%rax,1), %xmm0
, 1		// phaddsw %xmm0, %xmm0
, 999		// phaddw %es:(%rax,%rax,1), %mm0
, 1		// phaddw %mm0, %mm0
, 999		// phaddw %es:(%rax,%rax,1), %xmm0
, 1		// phaddw %xmm0, %xmm0
, 999		// phminposuw %es:(%rax,%rax,1), %xmm0
, 1		// phminposuw %xmm0, %xmm0
, 999		// phsubd %es:(%rax,%rax,1), %mm0
, 1		// phsubd %mm0, %mm0
, 999		// phsubd %es:(%rax,%rax,1), %xmm0
, 1		// phsubd %xmm0, %xmm0
, 999		// phsubsw %es:(%rax,%rax,1), %mm0
, 1		// phsubsw %mm0, %mm0
, 999		// phsubsw %es:(%rax,%rax,1), %xmm0
, 1		// phsubsw %xmm0, %xmm0
, 999		// phsubw %es:(%rax,%rax,1), %mm0
, 1		// phsubw %mm0, %mm0
, 999		// phsubw %es:(%rax,%rax,1), %xmm0
, 1		// phsubw %xmm0, %xmm0
, 999		// pinsrb $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pinsrb $0x0, %eax, %xmm0
, 999		// pinsrd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pinsrd $0x0, %eax, %xmm0
, 999		// pinsrw $0x0, %es:(%rax,%rax,1), %mm0
, 1		// pinsrw $0x0, %eax, %mm0
, 999		// pinsrw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pinsrw $0x0, %eax, %xmm0
, 999		// pmaddubsw %es:(%rax,%rax,1), %mm0
, 1		// pmaddubsw %mm0, %mm0
, 999		// pmaddubsw %es:(%rax,%rax,1), %xmm0
, 1		// pmaddubsw %xmm0, %xmm0
, 999		// pmaddwd %es:(%rax,%rax,1), %mm0
, 1		// pmaddwd %mm0, %mm0
, 999		// pmaddwd %es:(%rax,%rax,1), %xmm0
, 1		// pmaddwd %xmm0, %xmm0
, 999		// pmaxsb %es:(%rax,%rax,1), %xmm0
, 1		// pmaxsb %xmm0, %xmm0
, 999		// pmaxsd %es:(%rax,%rax,1), %xmm0
, 1		// pmaxsd %xmm0, %xmm0
, 999		// pmaxsw %es:(%rax,%rax,1), %mm0
, 1		// pmaxsw %mm0, %mm0
, 999		// pmaxsw %es:(%rax,%rax,1), %xmm0
, 1		// pmaxsw %xmm0, %xmm0
, 999		// pmaxub %es:(%rax,%rax,1), %mm0
, 1		// pmaxub %mm0, %mm0
, 999		// pmaxub %es:(%rax,%rax,1), %xmm0
, 1		// pmaxub %xmm0, %xmm0
, 999		// pmaxud %es:(%rax,%rax,1), %xmm0
, 1		// pmaxud %xmm0, %xmm0
, 999		// pmaxuw %es:(%rax,%rax,1), %xmm0
, 1		// pmaxuw %xmm0, %xmm0
, 999		// pminsb %es:(%rax,%rax,1), %xmm0
, 1		// pminsb %xmm0, %xmm0
, 999		// pminsd %es:(%rax,%rax,1), %xmm0
, 1		// pminsd %xmm0, %xmm0
, 999		// pminsw %es:(%rax,%rax,1), %mm0
, 1		// pminsw %mm0, %mm0
, 999		// pminsw %es:(%rax,%rax,1), %xmm0
, 1		// pminsw %xmm0, %xmm0
, 999		// pminub %es:(%rax,%rax,1), %mm0
, 1		// pminub %mm0, %mm0
, 999		// pminub %es:(%rax,%rax,1), %xmm0
, 1		// pminub %xmm0, %xmm0
, 999		// pminud %es:(%rax,%rax,1), %xmm0
, 1		// pminud %xmm0, %xmm0
, 999		// pminuw %es:(%rax,%rax,1), %xmm0
, 1		// pminuw %xmm0, %xmm0
, 1		// pmovmskb %mm0, %eax
, 1		// pmovmskb %xmm0, %eax
, 1		// pmovmskb %mm0, %rax
, 1		// pmovmskb %xmm0, %rax
, 999		// pmovsxbd %es:(%rax,%rax,1), %xmm0
, 1		// pmovsxbd %xmm0, %xmm0
, 999		// pmovsxbq %es:(%rax,%rax,1), %xmm0
, 1		// pmovsxbq %xmm0, %xmm0
, 999		// pmovsxbw %es:(%rax,%rax,1), %xmm0
, 1		// pmovsxbw %xmm0, %xmm0
, 999		// pmovsxdq %es:(%rax,%rax,1), %xmm0
, 1		// pmovsxdq %xmm0, %xmm0
, 999		// pmovsxwd %es:(%rax,%rax,1), %xmm0
, 1		// pmovsxwd %xmm0, %xmm0
, 999		// pmovsxwq %es:(%rax,%rax,1), %xmm0
, 1		// pmovsxwq %xmm0, %xmm0
, 999		// pmovzxbd %es:(%rax,%rax,1), %xmm0
, 1		// pmovzxbd %xmm0, %xmm0
, 999		// pmovzxbq %es:(%rax,%rax,1), %xmm0
, 1		// pmovzxbq %xmm0, %xmm0
, 999		// pmovzxbw %es:(%rax,%rax,1), %xmm0
, 1		// pmovzxbw %xmm0, %xmm0
, 999		// pmovzxdq %es:(%rax,%rax,1), %xmm0
, 1		// pmovzxdq %xmm0, %xmm0
, 999		// pmovzxwd %es:(%rax,%rax,1), %xmm0
, 1		// pmovzxwd %xmm0, %xmm0
, 999		// pmovzxwq %es:(%rax,%rax,1), %xmm0
, 1		// pmovzxwq %xmm0, %xmm0
, 999		// pmuldq %es:(%rax,%rax,1), %xmm0
, 1		// pmuldq %xmm0, %xmm0
, 999		// pmulhrsw %es:(%rax,%rax,1), %mm0
, 1		// pmulhrsw %mm0, %mm0
, 999		// pmulhrsw %es:(%rax,%rax,1), %xmm0
, 1		// pmulhrsw %xmm0, %xmm0
, 999		// pmulhuw %es:(%rax,%rax,1), %mm0
, 1		// pmulhuw %mm0, %mm0
, 999		// pmulhuw %es:(%rax,%rax,1), %xmm0
, 1		// pmulhuw %xmm0, %xmm0
, 999		// pmulhw %es:(%rax,%rax,1), %mm0
, 1		// pmulhw %mm0, %mm0
, 999		// pmulhw %es:(%rax,%rax,1), %xmm0
, 1		// pmulhw %xmm0, %xmm0
, 999		// pmulld %es:(%rax,%rax,1), %xmm0
, 1		// pmulld %xmm0, %xmm0
, 999		// pmullw %es:(%rax,%rax,1), %mm0
, 1		// pmullw %mm0, %mm0
, 999		// pmullw %es:(%rax,%rax,1), %xmm0
, 1		// pmullw %xmm0, %xmm0
, 999		// pmuludq %es:(%rax,%rax,1), %mm0
, 1		// pmuludq %mm0, %mm0
, 999		// pmuludq %es:(%rax,%rax,1), %xmm0
, 1		// pmuludq %xmm0, %xmm0
, 999		// popq %es
, 999		// popq , %es
, 999		// popq %es
, 999		// popq , %es
, 999		// popw %es:(%rax,%rax,1)
, 999		// popq %es:(%rax,%rax,1)
, 999		// popw %ax
, 999		// popq %rax
, 999		// popcntw %es:(%rax,%rax,1), %ax
, 1		// popcntw %ax, %ax
, 999		// popcntl %es:(%rax,%rax,1), %eax
, 1		// popcntl %eax, %eax
, 999		// popcntq %es:(%rax,%rax,1), %rax
, 1		// popcntq %rax, %rax
, 999		// popf 
, 999		// popfq 
, 999		// por %es:(%rax,%rax,1), %mm0
, 1		// por %mm0, %mm0
, 999		// por %es:(%rax,%rax,1), %xmm0
, 1		// por %xmm0, %xmm0
, 999		// prefetchnta %es:(%rax,%rax,1)
, 999		// prefetcht0 %es:(%rax,%rax,1)
, 999		// prefetcht1 %es:(%rax,%rax,1)
, 999		// prefetcht2 %es:(%rax,%rax,1)
, 999		// psadbw %es:(%rax,%rax,1), %mm0
, 1		// psadbw %mm0, %mm0
, 999		// psadbw %es:(%rax,%rax,1), %xmm0
, 1		// psadbw %xmm0, %xmm0
, 999		// pshufb %es:(%rax,%rax,1), %mm0
, 1		// pshufb %mm0, %mm0
, 999		// pshufb %es:(%rax,%rax,1), %xmm0
, 1		// pshufb %xmm0, %xmm0
, 999		// pshufd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pshufd $0x0, %xmm0, %xmm0
, 999		// pshufhw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pshufhw $0x0, %xmm0, %xmm0
, 999		// pshuflw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// pshuflw $0x0, %xmm0, %xmm0
, 999		// pshufw $0x0, %es:(%rax,%rax,1), %mm0
, 1		// pshufw $0x0, %mm0, %mm0
, 999		// psignb %es:(%rax,%rax,1), %mm0
, 1		// psignb %mm0, %mm0
, 999		// psignb %es:(%rax,%rax,1), %xmm0
, 1		// psignb %xmm0, %xmm0
, 999		// psignd %es:(%rax,%rax,1), %mm0
, 1		// psignd %mm0, %mm0
, 999		// psignd %es:(%rax,%rax,1), %xmm0
, 1		// psignd %xmm0, %xmm0
, 999		// psignw %es:(%rax,%rax,1), %mm0
, 1		// psignw %mm0, %mm0
, 999		// psignw %es:(%rax,%rax,1), %xmm0
, 1		// psignw %xmm0, %xmm0
, 1		// pslld $0x0, %mm0
, 999		// pslld %es:(%rax,%rax,1), %mm0
, 1		// pslld %mm0, %mm0
, 1		// pslld $0x0, %xmm0
, 999		// pslld %es:(%rax,%rax,1), %xmm0
, 1		// pslld %xmm0, %xmm0
, 1		// pslldq $0x0, %xmm0
, 1		// psllq $0x0, %mm0
, 999		// psllq %es:(%rax,%rax,1), %mm0
, 1		// psllq %mm0, %mm0
, 1		// psllq $0x0, %xmm0
, 999		// psllq %es:(%rax,%rax,1), %xmm0
, 1		// psllq %xmm0, %xmm0
, 1		// psllw $0x0, %mm0
, 999		// psllw %es:(%rax,%rax,1), %mm0
, 1		// psllw %mm0, %mm0
, 1		// psllw $0x0, %xmm0
, 999		// psllw %es:(%rax,%rax,1), %xmm0
, 1		// psllw %xmm0, %xmm0
, 1		// psrad $0x0, %mm0
, 999		// psrad %es:(%rax,%rax,1), %mm0
, 1		// psrad %mm0, %mm0
, 1		// psrad $0x0, %xmm0
, 999		// psrad %es:(%rax,%rax,1), %xmm0
, 1		// psrad %xmm0, %xmm0
, 1		// psraw $0x0, %mm0
, 999		// psraw %es:(%rax,%rax,1), %mm0
, 1		// psraw %mm0, %mm0
, 1		// psraw $0x0, %xmm0
, 999		// psraw %es:(%rax,%rax,1), %xmm0
, 1		// psraw %xmm0, %xmm0
, 1		// psrld $0x0, %mm0
, 999		// psrld %es:(%rax,%rax,1), %mm0
, 1		// psrld %mm0, %mm0
, 1		// psrld $0x0, %xmm0
, 999		// psrld %es:(%rax,%rax,1), %xmm0
, 1		// psrld %xmm0, %xmm0
, 1		// psrldq $0x0, %xmm0
, 1		// psrlq $0x0, %mm0
, 999		// psrlq %es:(%rax,%rax,1), %mm0
, 1		// psrlq %mm0, %mm0
, 1		// psrlq $0x0, %xmm0
, 999		// psrlq %es:(%rax,%rax,1), %xmm0
, 1		// psrlq %xmm0, %xmm0
, 1		// psrlw $0x0, %mm0
, 999		// psrlw %es:(%rax,%rax,1), %mm0
, 1		// psrlw %mm0, %mm0
, 1		// psrlw $0x0, %xmm0
, 999		// psrlw %es:(%rax,%rax,1), %xmm0
, 1		// psrlw %xmm0, %xmm0
, 999		// psubb %es:(%rax,%rax,1), %mm0
, 1		// psubb %mm0, %mm0
, 999		// psubb %es:(%rax,%rax,1), %xmm0
, 1		// psubb %xmm0, %xmm0
, 999		// psubd %es:(%rax,%rax,1), %mm0
, 1		// psubd %mm0, %mm0
, 999		// psubd %es:(%rax,%rax,1), %xmm0
, 1		// psubd %xmm0, %xmm0
, 999		// psubq %es:(%rax,%rax,1), %mm0
, 1		// psubq %mm0, %mm0
, 999		// psubq %es:(%rax,%rax,1), %xmm0
, 1		// psubq %xmm0, %xmm0
, 999		// psubsb %es:(%rax,%rax,1), %mm0
, 1		// psubsb %mm0, %mm0
, 999		// psubsb %es:(%rax,%rax,1), %xmm0
, 1		// psubsb %xmm0, %xmm0
, 999		// psubsw %es:(%rax,%rax,1), %mm0
, 1		// psubsw %mm0, %mm0
, 999		// psubsw %es:(%rax,%rax,1), %xmm0
, 1		// psubsw %xmm0, %xmm0
, 999		// psubusb %es:(%rax,%rax,1), %mm0
, 1		// psubusb %mm0, %mm0
, 999		// psubusb %es:(%rax,%rax,1), %xmm0
, 1		// psubusb %xmm0, %xmm0
, 999		// psubusw %es:(%rax,%rax,1), %mm0
, 1		// psubusw %mm0, %mm0
, 999		// psubusw %es:(%rax,%rax,1), %xmm0
, 1		// psubusw %xmm0, %xmm0
, 999		// psubw %es:(%rax,%rax,1), %mm0
, 1		// psubw %mm0, %mm0
, 999		// psubw %es:(%rax,%rax,1), %xmm0
, 1		// psubw %xmm0, %xmm0
, 999		// ptest %es:(%rax,%rax,1), %xmm0
, 1		// ptest %xmm0, %xmm0
, 999		// punpckhbw %es:(%rax,%rax,1), %mm0
, 1		// punpckhbw %mm0, %mm0
, 999		// punpckhbw %es:(%rax,%rax,1), %xmm0
, 1		// punpckhbw %xmm0, %xmm0
, 999		// punpckhdq %es:(%rax,%rax,1), %mm0
, 1		// punpckhdq %mm0, %mm0
, 999		// punpckhdq %es:(%rax,%rax,1), %xmm0
, 1		// punpckhdq %xmm0, %xmm0
, 999		// punpckhqdq %es:(%rax,%rax,1), %xmm0
, 1		// punpckhqdq %xmm0, %xmm0
, 999		// punpckhwd %es:(%rax,%rax,1), %mm0
, 1		// punpckhwd %mm0, %mm0
, 999		// punpckhwd %es:(%rax,%rax,1), %xmm0
, 1		// punpckhwd %xmm0, %xmm0
, 999		// punpcklbw %es:(%rax,%rax,1), %mm0
, 1		// punpcklbw %mm0, %mm0
, 999		// punpcklbw %es:(%rax,%rax,1), %xmm0
, 1		// punpcklbw %xmm0, %xmm0
, 999		// punpckldq %es:(%rax,%rax,1), %mm0
, 1		// punpckldq %mm0, %mm0
, 999		// punpckldq %es:(%rax,%rax,1), %xmm0
, 1		// punpckldq %xmm0, %xmm0
, 999		// punpcklqdq %es:(%rax,%rax,1), %xmm0
, 1		// punpcklqdq %xmm0, %xmm0
, 999		// punpcklwd %es:(%rax,%rax,1), %mm0
, 1		// punpcklwd %mm0, %mm0
, 999		// punpcklwd %es:(%rax,%rax,1), %xmm0
, 1		// punpcklwd %xmm0, %xmm0
, 999		// pushq %es
, 999		// pushq %es
, 999		// pushq $0x0
, 999		// pushq $0x0
, 999		// pushq $0x0
, 999		// pushw %es:(%rax,%rax,1)
, 999		// pushq %es:(%rax,%rax,1)
, 999		// pushw %ax
, 999		// pushq %rax
, 999		// pushf 
, 999		// pushfq 
, 999		// pxor %es:(%rax,%rax,1), %mm0
, 1		// pxor %mm0, %mm0
, 999		// pxor %es:(%rax,%rax,1), %xmm0
, 1		// pxor %xmm0, %xmm0
, 999		// rclw %al, %es:(%rax,%rax,1)
, 999		// rclw $0x0, %es:(%rax,%rax,1)
, 999		// rclw $0x0, %es:(%rax,%rax,1)
, 999		// rcll %al, %es:(%rax,%rax,1)
, 999		// rcll $0x0, %es:(%rax,%rax,1)
, 999		// rcll $0x0, %es:(%rax,%rax,1)
, 999		// rclq %al, %es:(%rax,%rax,1)
, 999		// rclq $0x0, %es:(%rax,%rax,1)
, 999		// rclq $0x0, %es:(%rax,%rax,1)
, 999		// rclb %al, %es:(%rax,%rax,1)
, 999		// rclb $0x0, %es:(%rax,%rax,1)
, 999		// rclb $0x0, %es:(%rax,%rax,1)
, 65		// rclw %cl, %ax
, 67		// rclw $0x0, %ax
, 1		// rclw $0x1, %ax
, 65		// rcll %cl, %eax
, 65		// rcll $0x0, %eax
, 1		// rcll $0x1, %eax
, 65		// rclq %cl, %rax
, 65		// rclq $0x0, %rax
, 1		// rclq $0x1, %rax
, 65		// rclb %cl, %al
, 65		// rclb $0x0, %al
, 1		// rclb $0x1, %al
, 65		// rclb %cl, %ah
, 65		// rclb $0x0, %ah
, 1		// rclb $0x1, %ah
, 999		// rcpps %es:(%rax,%rax,1), %xmm0
, 1		// rcpps %xmm0, %xmm0
, 999		// rcpss %es:(%rax,%rax,1), %xmm0
, 1		// rcpss %xmm0, %xmm0
, 999		// rcrw %al, %es:(%rax,%rax,1)
, 999		// rcrw $0x0, %es:(%rax,%rax,1)
, 999		// rcrw $0x0, %es:(%rax,%rax,1)
, 999		// rcrl %al, %es:(%rax,%rax,1)
, 999		// rcrl $0x0, %es:(%rax,%rax,1)
, 999		// rcrl $0x0, %es:(%rax,%rax,1)
, 999		// rcrq %al, %es:(%rax,%rax,1)
, 999		// rcrq $0x0, %es:(%rax,%rax,1)
, 999		// rcrq $0x0, %es:(%rax,%rax,1)
, 999		// rcrb %al, %es:(%rax,%rax,1)
, 999		// rcrb $0x0, %es:(%rax,%rax,1)
, 999		// rcrb $0x0, %es:(%rax,%rax,1)
, 65		// rcrw %cl, %ax
, 65		// rcrw $0x0, %ax
, 1		// rcrw $0x1, %ax
, 65		// rcrl %cl, %eax
, 65		// rcrl $0x0, %eax
, 1		// rcrl $0x1, %eax
, 65		// rcrq %cl, %rax
, 65		// rcrq $0x0, %rax
, 1		// rcrq $0x1, %rax
, 91		// rcrb %cl, %al
, 91		// rcrb $0x0, %al
, 1		// rcrb $0x1, %al
, 91		// rcrb %cl, %ah
, 91		// rcrb $0x0, %ah
, 1		// rcrb $0x1, %ah
, 999		// rdfsbase %eax
, 999		// rdfsbase %rax
, 999		// rdgsbase %eax
, 999		// rdgsbase %rax
, 9066		// rdrand %ax
, 9061		// rdrand %eax
, 9053		// rdrand %rax
, 999		// rep insw %ax, %es:(%rax,%rax,1)
, 999		// rep insl %ax, %es:(%rax,%rax,1)
, 999		// rep insq %ax, %es:(%rax,%rax,1)
, 999		// rep insb %ax, %es:(%rax,%rax,1)
, 1374		// rep lodsb %al
, 1374		// rep lodsw %ax
, 1374		// rep lodsl %eax
, 1376		// rep lodsq %rax
, 999		// rep movsw %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// rep movsl %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// rep movsq %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// rep movsb %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// rep outsw %es:(%rax,%rax,1), %ax
, 999		// rep outsl %es:(%rax,%rax,1), %ax
, 999		// rep outsq %es:(%rax,%rax,1), %ax
, 999		// rep outsb %es:(%rax,%rax,1), %ax
, 999		// rep stosw %es:(%rax,%rax,1)
, 999		// rep stosl %es:(%rax,%rax,1)
, 999		// rep stosq %es:(%rax,%rax,1)
, 999		// rep stosb %es:(%rax,%rax,1)
, 999		// repz cmpsw %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repz cmpsl %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repz cmpsq %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repz cmpsb %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repz scasw %es:(%rax,%rax,1)
, 999		// repz scasl %es:(%rax,%rax,1)
, 999		// repz scasq %es:(%rax,%rax,1)
, 999		// repz scasb %es:(%rax,%rax,1)
, 999		// repnz cmpsw %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repnz cmpsl %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repnz cmpsq %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repnz cmpsb %es:(%rax,%rax,1), %es:(%rax,%rax,1)
, 999		// repnz scasw %es:(%rax,%rax,1)
, 999		// repnz scasl %es:(%rax,%rax,1)
, 999		// repnz scasq %es:(%rax,%rax,1)
, 999		// repnz scasb %es:(%rax,%rax,1)
, 0		// retq 
, 0		// lretl 
, 0		// retq $0x0
, 0		// retq , $0x0
, 999		// rolw %al, %es:(%rax,%rax,1)
, 999		// rolw $0x0, %es:(%rax,%rax,1)
, 999		// rolw $0x0, %es:(%rax,%rax,1)
, 999		// roll %al, %es:(%rax,%rax,1)
, 999		// roll $0x0, %es:(%rax,%rax,1)
, 999		// roll $0x0, %es:(%rax,%rax,1)
, 999		// rolq %al, %es:(%rax,%rax,1)
, 999		// rolq $0x0, %es:(%rax,%rax,1)
, 999		// rolq $0x0, %es:(%rax,%rax,1)
, 999		// rolb %al, %es:(%rax,%rax,1)
, 999		// rolb $0x0, %es:(%rax,%rax,1)
, 999		// rolb $0x0, %es:(%rax,%rax,1)
, 1		// rolw %cl, %ax
, 1		// rolw $0x0, %ax
, 1		// rolw $0x1, %ax
, 1		// roll %cl, %eax
, 1		// roll $0x0, %eax
, 1		// roll $0x1, %eax
, 1		// rolq %cl, %rax
, 1		// rolq $0x0, %rax
, 1		// rolq $0x1, %rax
, 1		// rolb %cl, %al
, 1		// rolb $0x0, %al
, 1		// rolb $0x1, %al
, 1		// rolb %cl, %ah
, 1		// rolb $0x0, %ah
, 1		// rolb $0x1, %ah
, 999		// rorw %al, %es:(%rax,%rax,1)
, 999		// rorw $0x0, %es:(%rax,%rax,1)
, 999		// rorw $0x0, %es:(%rax,%rax,1)
, 999		// rorl %al, %es:(%rax,%rax,1)
, 999		// rorl $0x0, %es:(%rax,%rax,1)
, 999		// rorl $0x0, %es:(%rax,%rax,1)
, 999		// rorq %al, %es:(%rax,%rax,1)
, 999		// rorq $0x0, %es:(%rax,%rax,1)
, 999		// rorq $0x0, %es:(%rax,%rax,1)
, 999		// rorb %al, %es:(%rax,%rax,1)
, 999		// rorb $0x0, %es:(%rax,%rax,1)
, 999		// rorb $0x0, %es:(%rax,%rax,1)
, 1		// rorw %cl, %ax
, 1		// rorw $0x0, %ax
, 1		// rorw $0x1, %ax
, 1		// rorl %cl, %eax
, 1		// rorl $0x0, %eax
, 1		// rorl $0x1, %eax
, 1		// rorq %cl, %rax
, 1		// rorq $0x0, %rax
, 1		// rorq $0x1, %rax
, 1		// rorb %cl, %al
, 1		// rorb $0x0, %al
, 1		// rorb $0x1, %al
, 1		// rorb %cl, %ah
, 1		// rorb $0x0, %ah
, 1		// rorb $0x1, %ah
, 999		// rorxl $0x0, %es:(%rax,%rax,1), %eax
, 1		// rorxl $0x0, %eax, %eax
, 999		// rorxq $0x0, %es:(%rax,%rax,1), %rax
, 1		// rorxq $0x0, %rax, %rax
, 999		// roundpd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// roundpd $0x0, %xmm0, %xmm0
, 999		// roundps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// roundps $0x0, %xmm0, %xmm0
, 999		// roundsd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// roundsd $0x0, %xmm0, %xmm0
, 999		// roundss $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// roundss $0x0, %xmm0, %xmm0
, 999		// rsqrtps %es:(%rax,%rax,1), %xmm0
, 1		// rsqrtps %xmm0, %xmm0
, 999		// rsqrtss %es:(%rax,%rax,1), %xmm0
, 1		// rsqrtss %xmm0, %xmm0
, 1		// sahf 
, 999		// salw %al, %es:(%rax,%rax,1)
, 999		// salw $0x0, %es:(%rax,%rax,1)
, 999		// salw $0x0, %es:(%rax,%rax,1)
, 999		// sall %al, %es:(%rax,%rax,1)
, 999		// sall $0x0, %es:(%rax,%rax,1)
, 999		// sall $0x0, %es:(%rax,%rax,1)
, 999		// salq %al, %es:(%rax,%rax,1)
, 999		// salq $0x0, %es:(%rax,%rax,1)
, 999		// salq $0x0, %es:(%rax,%rax,1)
, 999		// salb %al, %es:(%rax,%rax,1)
, 999		// salb $0x0, %es:(%rax,%rax,1)
, 999		// salb $0x0, %es:(%rax,%rax,1)
, 1		// salw %cl, %ax
, 1		// salw $0x0, %ax
, 1		// salw $0x1, %ax
, 1		// sall %cl, %eax
, 1		// sall $0x0, %eax
, 1		// sall $0x1, %eax
, 1		// salq %cl, %rax
, 1		// salq $0x0, %rax
, 1		// salq $0x1, %rax
, 1		// salb %cl, %al
, 1		// salb $0x0, %al
, 1		// salb $0x1, %al
, 1		// salb %cl, %ah
, 1		// salb $0x0, %ah
, 1		// salb $0x1, %ah
, 999		// sarw %al, %es:(%rax,%rax,1)
, 999		// sarw $0x0, %es:(%rax,%rax,1)
, 999		// sarw $0x0, %es:(%rax,%rax,1)
, 999		// sarl %al, %es:(%rax,%rax,1)
, 999		// sarl $0x0, %es:(%rax,%rax,1)
, 999		// sarl $0x0, %es:(%rax,%rax,1)
, 999		// sarq %al, %es:(%rax,%rax,1)
, 999		// sarq $0x0, %es:(%rax,%rax,1)
, 999		// sarq $0x0, %es:(%rax,%rax,1)
, 999		// sarb %al, %es:(%rax,%rax,1)
, 999		// sarb $0x0, %es:(%rax,%rax,1)
, 999		// sarb $0x0, %es:(%rax,%rax,1)
, 1		// sarw %cl, %ax
, 1		// sarw $0x0, %ax
, 1		// sarw $0x1, %ax
, 1		// sarl %cl, %eax
, 1		// sarl $0x0, %eax
, 1		// sarl $0x1, %eax
, 1		// sarq %cl, %rax
, 1		// sarq $0x0, %rax
, 1		// sarq $0x1, %rax
, 1		// sarb %cl, %al
, 1		// sarb $0x0, %al
, 1		// sarb $0x1, %al
, 1		// sarb %cl, %ah
, 1		// sarb $0x0, %ah
, 1		// sarb $0x1, %ah
, 999		// sarxl %eax, %es:(%rax,%rax,1), %eax
, 1		// sarxl %eax, %eax, %eax
, 999		// sarxq %rax, %es:(%rax,%rax,1), %rax
, 1		// sarxq %rax, %rax, %rax
, 1		// sbbb $0x0, %al
, 1		// sbbw $0x0, %ax
, 1		// sbbl $0x0, %eax
, 999		// sbbw $0x0, %es:(%rax,%rax,1)
, 999		// sbbw $0x0, %es:(%rax,%rax,1)
, 999		// sbbw %ax, %es:(%rax,%rax,1)
, 999		// sbbl $0x0, %es:(%rax,%rax,1)
, 999		// sbbl $0x0, %es:(%rax,%rax,1)
, 999		// sbbl %eax, %es:(%rax,%rax,1)
, 999		// sbbq $0x0, %es:(%rax,%rax,1)
, 999		// sbbq $0x0, %es:(%rax,%rax,1)
, 999		// sbbq %rax, %es:(%rax,%rax,1)
, 999		// sbbb $0x0, %es:(%rax,%rax,1)
, 999		// sbbb %al, %es:(%rax,%rax,1)
, 999		// sbbb  , %es:(%rax,%rax,1)
, 1		// sbbw $0x0, %ax
, 1		// sbbw $0x0, %ax
, 999		// sbbw %es:(%rax,%rax,1), %ax
, 1		// sbbw %ax, %ax
, 1		// sbbl $0x0, %eax
, 1		// sbbl $0x0, %eax
, 999		// sbbl %es:(%rax,%rax,1), %eax
, 1		// sbbl %eax, %eax
, 1		// sbbq $0x0, %rax
, 1		// sbbq $0x0, %rax
, 999		// sbbq %es:(%rax,%rax,1), %rax
, 1		// sbbq %rax, %rax
, 1		// sbbb $0x0, %al
, 999		// sbbb %es:(%rax,%rax,1), %al
, 1		// sbbb %al, %al
, 1		// sbbb %ah, %al
, 1		// sbbq $0x0, %rax
, 1		// sbbb $0x0, %ah
, 999		// sbbb %es:(%rax,%rax,1), , 
, 1		// sbbb %al, %ah
, 1		// sbbb %ah, %ah
, 999		// scas %es:(%rax,%rax,1)
, 999		// scas %es:(%rax,%rax,1)
, 999		// scas %es:(%rax,%rax,1)
, 999		// scas %es:(%rax,%rax,1)
, 999		// scasb 
, 999		// scasl 
, 999		// scasq 
, 999		// scasw 
, 999		// seta %es:(%rax,%rax,1)
, 1		// seta %al
, 1		// seta %ah
, 999		// setae %es:(%rax,%rax,1)
, 1		// setae %al
, 1		// setae %ah
, 999		// setb %es:(%rax,%rax,1)
, 1		// setb %al
, 1		// setb %ah
, 999		// setbe %es:(%rax,%rax,1)
, 1		// setbe %al
, 1		// setbe %ah
, 999		// setc %es:(%rax,%rax,1)
, 1		// setc %al
, 1		// setc %ah
, 999		// sete %es:(%rax,%rax,1)
, 1		// sete %al
, 1		// sete %ah
, 999		// setg %es:(%rax,%rax,1)
, 1		// setg %al
, 1		// setg %ah
, 999		// setge %es:(%rax,%rax,1)
, 1		// setge %al
, 1		// setge %ah
, 999		// setl %es:(%rax,%rax,1)
, 1		// setl %al
, 1		// setl %ah
, 999		// setle %es:(%rax,%rax,1)
, 1		// setle %al
, 1		// setle %ah
, 999		// setna %es:(%rax,%rax,1)
, 1		// setna %al
, 1		// setna %ah
, 999		// setnae %es:(%rax,%rax,1)
, 1		// setnae %al
, 1		// setnae %ah
, 999		// setnb %es:(%rax,%rax,1)
, 1		// setnb %al
, 1		// setnb %ah
, 999		// setnbe %es:(%rax,%rax,1)
, 1		// setnbe %al
, 1		// setnbe %ah
, 999		// setnc %es:(%rax,%rax,1)
, 1		// setnc %al
, 1		// setnc %ah
, 999		// setne %es:(%rax,%rax,1)
, 1		// setne %al
, 1		// setne %ah
, 999		// setng %es:(%rax,%rax,1)
, 1		// setng %al
, 1		// setng %ah
, 999		// setnge %es:(%rax,%rax,1)
, 1		// setnge %al
, 1		// setnge %ah
, 999		// setnl %es:(%rax,%rax,1)
, 1		// setnl %al
, 1		// setnl %ah
, 999		// setnle %es:(%rax,%rax,1)
, 1		// setnle %al
, 1		// setnle %ah
, 999		// setno %es:(%rax,%rax,1)
, 1		// setno %al
, 1		// setno %ah
, 999		// setnp %es:(%rax,%rax,1)
, 1		// setnp %al
, 1		// setnp %ah
, 999		// setns %es:(%rax,%rax,1)
, 1		// setns %al
, 1		// setns %ah
, 999		// setnz %es:(%rax,%rax,1)
, 1		// setnz %al
, 1		// setnz %ah
, 999		// seto %es:(%rax,%rax,1)
, 1		// seto %al
, 1		// seto %ah
, 999		// setp %es:(%rax,%rax,1)
, 1		// setp %al
, 1		// setp %ah
, 999		// setpe %es:(%rax,%rax,1)
, 1		// setpe %al
, 1		// setpe %ah
, 999		// setpo %es:(%rax,%rax,1)
, 1		// setpo %al
, 1		// setpo %ah
, 999		// sets %es:(%rax,%rax,1)
, 1		// sets %al
, 1		// sets %ah
, 999		// setz %es:(%rax,%rax,1)
, 1		// setz %al
, 1		// setz %ah
, 1		// sfence 
, 999		// shlw %al, %es:(%rax,%rax,1)
, 999		// shlw $0x0, %es:(%rax,%rax,1)
, 999		// shlw $0x0, %es:(%rax,%rax,1)
, 999		// shll %al, %es:(%rax,%rax,1)
, 999		// shll $0x0, %es:(%rax,%rax,1)
, 999		// shll $0x0, %es:(%rax,%rax,1)
, 999		// shlq %al, %es:(%rax,%rax,1)
, 999		// shlq $0x0, %es:(%rax,%rax,1)
, 999		// shlq $0x0, %es:(%rax,%rax,1)
, 999		// shlb %al, %es:(%rax,%rax,1)
, 999		// shlb $0x0, %es:(%rax,%rax,1)
, 999		// shlb $0x0, %es:(%rax,%rax,1)
, 1		// shlw %cl, %ax
, 1		// shlw $0x0, %ax
, 1		// shlw $0x1, %ax
, 1		// shll %cl, %eax
, 1		// shll $0x0, %eax
, 1		// shll $0x1, %eax
, 1		// shlq %cl, %rax
, 1		// shlq $0x0, %rax
, 1		// shlq $0x1, %rax
, 1		// shlb %cl, %al
, 1		// shlb $0x0, %al
, 1		// shlb $0x1, %al
, 1		// shlb %cl, %ah
, 1		// shlb $0x0, %ah
, 1		// shlb $0x1, %ah
, 999		// shldw %al, %ax, %es:(%rax,%rax,1)
, 999		// shldw $0x0, %ax, %es:(%rax,%rax,1)
, 999		// shldl %al, %eax, %es:(%rax,%rax,1)
, 999		// shldl $0x0, %eax, %es:(%rax,%rax,1)
, 999		// shldq %al, %rax, %es:(%rax,%rax,1)
, 999		// shldq $0x0, %rax, %es:(%rax,%rax,1)
, 1		// shldw %cl, %ax, %ax
, 1		// shldw $0x0, %ax, %ax
, 1		// shldl %cl, %eax, %eax
, 1		// shldl $0x0, %eax, %eax
, 1		// shldq %cl, %rax, %rax
, 1		// shldq $0x0, %rax, %rax
, 999		// shlxl %eax, %es:(%rax,%rax,1), %eax
, 1		// shlxl %eax, %eax, %eax
, 999		// shlxq %rax, %es:(%rax,%rax,1), %rax
, 1		// shlxq %rax, %rax, %rax
, 999		// shrw %al, %es:(%rax,%rax,1)
, 999		// shrw $0x0, %es:(%rax,%rax,1)
, 999		// shrw $0x0, %es:(%rax,%rax,1)
, 999		// shrl %al, %es:(%rax,%rax,1)
, 999		// shrl $0x0, %es:(%rax,%rax,1)
, 999		// shrl $0x0, %es:(%rax,%rax,1)
, 999		// shrq %al, %es:(%rax,%rax,1)
, 999		// shrq $0x0, %es:(%rax,%rax,1)
, 999		// shrq $0x0, %es:(%rax,%rax,1)
, 999		// shrb %al, %es:(%rax,%rax,1)
, 999		// shrb $0x0, %es:(%rax,%rax,1)
, 999		// shrb $0x0, %es:(%rax,%rax,1)
, 1		// shrw %cl, %ax
, 1		// shrw $0x0, %ax
, 1		// shrw $0x1, %ax
, 1		// shrl %cl, %eax
, 1		// shrl $0x0, %eax
, 1		// shrl $0x1, %eax
, 1		// shrq %cl, %rax
, 1		// shrq $0x0, %rax
, 1		// shrq $0x1, %rax
, 1		// shrb %cl, %al
, 1		// shrb $0x0, %al
, 1		// shrb $0x1, %al
, 1		// shrb %cl, %ah
, 1		// shrb $0x0, %ah
, 1		// shrb $0x1, %ah
, 999		// shrdw %al, %ax, %es:(%rax,%rax,1)
, 999		// shrdw $0x0, %ax, %es:(%rax,%rax,1)
, 999		// shrdl %al, %eax, %es:(%rax,%rax,1)
, 999		// shrdl $0x0, %eax, %es:(%rax,%rax,1)
, 999		// shrdq %al, %rax, %es:(%rax,%rax,1)
, 999		// shrdq $0x0, %rax, %es:(%rax,%rax,1)
, 1		// shrdw %cl, %ax, %ax
, 1		// shrdw $0x0, %ax, %ax
, 1		// shrdl %cl, %eax, %eax
, 1		// shrdl $0x0, %eax, %eax
, 1		// shrdq %cl, %rax, %rax
, 1		// shrdq $0x0, %rax, %rax
, 999		// shrxl %eax, %es:(%rax,%rax,1), %eax
, 1		// shrxl %eax, %eax, %eax
, 999		// shrxq %rax, %es:(%rax,%rax,1), %rax
, 1		// shrxq %rax, %rax, %rax
, 999		// shufpd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// shufpd $0x0, %xmm0, %xmm0
, 999		// shufps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// shufps $0x0, %xmm0, %xmm0
, 999		// sqrtpd %es:(%rax,%rax,1), %xmm0
, 1		// sqrtpd %xmm0, %xmm0
, 999		// sqrtps %es:(%rax,%rax,1), %xmm0
, 1		// sqrtps %xmm0, %xmm0
, 999		// sqrtsd %es:(%rax,%rax,1), %xmm0
, 1		// sqrtsd %xmm0, %xmm0
, 999		// sqrtss %es:(%rax,%rax,1), %xmm0
, 1		// sqrtss %xmm0, %xmm0
, 1		// stc 
, 999		// std 
, 999		// sti 
, 999		// stmxcsr %es:(%rax,%rax,1)
, 999		// stosw %es:(%rax,%rax,1)
, 999		// stosl %es:(%rax,%rax,1)
, 999		// stosq %es:(%rax,%rax,1)
, 999		// stosb %es:(%rax,%rax,1)
, 999		// stosb 
, 999		// stosl 
, 999		// stosq 
, 999		// stosw 
, 1		// subb $0x0, %al
, 1		// subw $0x0, %ax
, 1		// subl $0x0, %eax
, 999		// subw $0x0, %es:(%rax,%rax,1)
, 999		// subw $0x0, %es:(%rax,%rax,1)
, 999		// subw %ax, %es:(%rax,%rax,1)
, 999		// subl $0x0, %es:(%rax,%rax,1)
, 999		// subl $0x0, %es:(%rax,%rax,1)
, 999		// subl %eax, %es:(%rax,%rax,1)
, 999		// subq $0x0, %es:(%rax,%rax,1)
, 999		// subq $0x0, %es:(%rax,%rax,1)
, 999		// subq %rax, %es:(%rax,%rax,1)
, 999		// subb $0x0, %es:(%rax,%rax,1)
, 999		// subb %al, %es:(%rax,%rax,1)
, 999		// subb  , %es:(%rax,%rax,1)
, 1		// subw $0x0, %ax
, 1		// subw $0x0, %ax
, 999		// subw %es:(%rax,%rax,1), %ax
, 1		// subw %ax, %ax
, 1		// subl $0x0, %eax
, 1		// subl $0x0, %eax
, 999		// subl %es:(%rax,%rax,1), %eax
, 1		// subl %eax, %eax
, 1		// subq $0x0, %rax
, 1		// subq $0x0, %rax
, 999		// subq %es:(%rax,%rax,1), %rax
, 1		// subq %rax, %rax
, 1		// subb $0x0, %al
, 999		// subb %es:(%rax,%rax,1), %al
, 1		// subb %al, %al
, 1		// subb %ah, %al
, 1		// subq $0x0, %rax
, 1		// subb $0x0, %ah
, 999		// subb %es:(%rax,%rax,1), , 
, 1		// subb %al, %ah
, 1		// subb %ah, %ah
, 999		// subpd %es:(%rax,%rax,1), %xmm0
, 1		// subpd %xmm0, %xmm0
, 999		// subps %es:(%rax,%rax,1), %xmm0
, 1		// subps %xmm0, %xmm0
, 999		// subsd %es:(%rax,%rax,1), %xmm0
, 1		// subsd %xmm0, %xmm0
, 999		// subss %es:(%rax,%rax,1), %xmm0
, 1		// subss %xmm0, %xmm0
, 999		// swapgs 
, 0		// syscall 
, 999		// sysenter 
, 999		// sysexit 
, 999		// sysexit 
, 0		// sysret 
, 0		// sysret 
, 1		// testb $0x0, %al
, 1		// testw $0x0, %ax
, 1		// testl $0x0, %eax
, 999		// testw $0x0, %es:(%rax,%rax,1)
, 999		// testw %ax, %es:(%rax,%rax,1)
, 999		// testl $0x0, %es:(%rax,%rax,1)
, 999		// testl %eax, %es:(%rax,%rax,1)
, 999		// testq $0x0, %es:(%rax,%rax,1)
, 999		// testq %rax, %es:(%rax,%rax,1)
, 999		// testb $0x0, %es:(%rax,%rax,1)
, 999		// testb %al, %es:(%rax,%rax,1)
, 999		// testb  , %es:(%rax,%rax,1)
, 1		// testw $0x0, %ax
, 1		// testw %ax, %ax
, 1		// testl $0x0, %eax
, 1		// testl %eax, %eax
, 1		// testq $0x0, %rax
, 1		// testq %rax, %rax
, 1		// testb $0x0, %al
, 1		// testb %al, %al
, 1		// testb %ah, %al
, 1		// testq $0x0, %rax
, 1		// testb $0x0, %ah
, 1		// testb %al, %ah
, 1		// testb %ah, %ah
, 999		// tzcntw %es:(%rax,%rax,1), %ax
, 1		// tzcntw %ax, %ax
, 999		// tzcntl %es:(%rax,%rax,1), %eax
, 1		// tzcntl %eax, %eax
, 999		// tzcntq %es:(%rax,%rax,1), %rax
, 1		// tzcntq %rax, %rax
, 999		// ucomisd %es:(%rax,%rax,1), %xmm0
, 1		// ucomisd %xmm0, %xmm0
, 999		// ucomiss %es:(%rax,%rax,1), %xmm0
, 1		// ucomiss %xmm0, %xmm0
, 999		// ud2 
, 999		// unpckhpd %es:(%rax,%rax,1), %xmm0
, 1		// unpckhpd %xmm0, %xmm0
, 999		// unpckhps %es:(%rax,%rax,1), %xmm0
, 1		// unpckhps %xmm0, %xmm0
, 999		// unpcklpd %es:(%rax,%rax,1), %xmm0
, 1		// unpcklpd %xmm0, %xmm0
, 999		// unpcklps %es:(%rax,%rax,1), %xmm0
, 1		// unpcklps %xmm0, %xmm0
, 999		// vaddpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaddpd %xmm0, %xmm0, %xmm0
, 999		// vaddpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vaddpd %ymm0, %ymm0, %ymm0
, 999		// vaddps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaddps %xmm0, %xmm0, %xmm0
, 999		// vaddps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vaddps %ymm0, %ymm0, %ymm0
, 999		// vaddsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaddsd %xmm0, %xmm0, %xmm0
, 999		// vaddss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaddss %xmm0, %xmm0, %xmm0
, 999		// vaddsubpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaddsubpd %xmm0, %xmm0, %xmm0
, 999		// vaddsubpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vaddsubpd %ymm0, %ymm0, %ymm0
, 999		// vaddsubps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaddsubps %xmm0, %xmm0, %xmm0
, 999		// vaddsubps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vaddsubps %ymm0, %ymm0, %ymm0
, 999		// vaesdec %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaesdec %xmm0, %xmm0, %xmm0
, 999		// vaesdeclast %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaesdeclast %xmm0, %xmm0, %xmm0
, 999		// vaesenc %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaesenc %xmm0, %xmm0, %xmm0
, 999		// vaesenclast %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vaesenclast %xmm0, %xmm0, %xmm0
, 999		// vaesimc %es:(%rax,%rax,1), %xmm0
, 14		// vaesimc %xmm0, %xmm0
, 999		// vaeskeygenassist $0x0, %es:(%rax,%rax,1), %xmm0
, 65		// vaeskeygenassist $0x0, %xmm0, %xmm0
, 999		// vandnpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vandnpd %xmm0, %xmm0, %xmm0
, 999		// vandnpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vandnpd %ymm0, %ymm0, %ymm0
, 999		// vandnps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vandnps %xmm0, %xmm0, %xmm0
, 999		// vandnps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vandnps %ymm0, %ymm0, %ymm0
, 999		// vandpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vandpd %xmm0, %xmm0, %xmm0
, 999		// vandpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vandpd %ymm0, %ymm0, %ymm0
, 999		// vandps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vandps %xmm0, %xmm0, %xmm0
, 999		// vandps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vandps %ymm0, %ymm0, %ymm0
, 999		// vblendpd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vblendpd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vblendpd $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vblendpd $0x0, %ymm0, %ymm0, %ymm0
, 999		// vblendps $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vblendps $0x0, %xmm0, %xmm0, %xmm0
, 999		// vblendps $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vblendps $0x0, %ymm0, %ymm0, %ymm0
, 999		// vblendvpd %xmm0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vblendvpd %xmm0, %xmm0, %xmm0, %xmm0
, 999		// vblendvpd %ymm0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vblendvpd %ymm0, %ymm0, %ymm0, %ymm0
, 999		// vblendvps %xmm0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vblendvps %xmm0, %xmm0, %xmm0, %xmm0
, 999		// vblendvps %ymm0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vblendvps %ymm0, %ymm0, %ymm0, %ymm0
, 999		// vbroadcastf128 %es:(%rax,%rax,1), %ymm0
, 999		// vpbroadcasti128 %es:(%rax,%rax,1), %ymm0
, 999		// vbroadcastsd %es:(%rax,%rax,1), %ymm0
, 1		// vbroadcastsd %xmm0, %ymm0
, 999		// vbroadcastss %es:(%rax,%rax,1), %xmm0
, 1		// vbroadcastss %xmm0, %xmm0
, 999		// vbroadcastss %es:(%rax,%rax,1), %ymm0
, 1		// vbroadcastss %xmm0, %ymm0
, 999		// vcmppd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcmppd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vcmppd $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vcmppd $0x0, %ymm0, %ymm0, %ymm0
, 999		// vcmpps $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcmpps $0x0, %xmm0, %xmm0, %xmm0
, 999		// vcmpps $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vcmpps $0x0, %ymm0, %ymm0, %ymm0
, 999		// vcmpsd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcmpsd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vcmpss $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcmpss $0x0, %xmm0, %xmm0, %xmm0
, 999		// vcomisd %es:(%rax,%rax,1), %xmm0
, 1		// vcomisd %xmm0, %xmm0
, 999		// vcomiss %es:(%rax,%rax,1), %xmm0
, 1		// vcomiss %xmm0, %xmm0
, 999		// vcvtdq2pd %es:(%rax,%rax,1), %xmm0
, 1		// vcvtdq2pd %xmm0, %xmm0
, 999		// vcvtdq2pd %es:(%rax,%rax,1), %ymm0
, 27		// vcvtdq2pd %ymm0, %ymm0
, 999		// vcvtdq2ps %es:(%rax,%rax,1), %xmm0
, 1		// vcvtdq2ps %xmm0, %xmm0
, 999		// vcvtdq2ps %es:(%rax,%rax,1), %ymm0
, 28		// vcvtdq2ps %ymm0, %ymm0
, 999		// vcvtpd2dqx %es:(%rax,%rax,1), %xmm0
, 999		// vcvtpd2dq %es:(%rax,%rax,1), %xmm0
, 1		// vcvtpd2dqx %xmm0, %xmm0
, 27		// vcvtpd2dq %ymm0, %xmm0
, 999		// vcvtpd2ps %es:(%rax,%rax,1), %xmm0
, 999		// vcvtpd2ps %es:(%rax,%rax,1), %xmm0
, 1		// vcvtpd2ps %xmm0, %xmm0
, 27		// vcvtpd2ps %ymm0, %xmm0
, 999		// vcvtph2ps %es:(%rax,%rax,1), %xmm0
, 1		// vcvtph2ps %xmm0, %xmm0
, 999		// vcvtph2ps %es:(%rax,%rax,1), %ymm0
, 27		// vcvtph2ps %xmm0, %ymm0
, 999		// vcvtps2dq %es:(%rax,%rax,1), %xmm0
, 1		// vcvtps2dq %xmm0, %xmm0
, 999		// vcvtps2dq %es:(%rax,%rax,1), %ymm0
, 27		// vcvtps2dq %ymm0, %ymm0
, 999		// vcvtps2pd %es:(%rax,%rax,1), %xmm0
, 1		// vcvtps2pd %xmm0, %xmm0
, 999		// vcvtps2pd %es:(%rax,%rax,1), %ymm0
, 27		// vcvtps2pd %xmm0, %ymm0
, 999		// vcvtps2ph $0x0, %ymm0, %es:(%rax,%rax,1)
, 999		// vcvtps2ph $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// vcvtps2ph $0x0, %xmm0, %xmm0
, 27		// vcvtps2ph $0x0, %ymm0, %xmm0
, 999		// vcvtsd2sil %es:(%rax,%rax,1), %eax
, 1		// vcvtsd2sil %xmm0, %eax
, 999		// vcvtsd2siq %es:(%rax,%rax,1), %rax
, 1		// vcvtsd2siq %xmm0, %rax
, 999		// vcvtsd2ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcvtsd2ss %xmm0, %xmm0, %xmm0
, 999		// vcvtsi2sdl %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vcvtsi2sdq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcvtsi2sdl %eax, %xmm0, %xmm0
, 1		// vcvtsi2sdq %rax, %xmm0, %xmm0
, 999		// vcvtsi2ssl %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vcvtsi2ssq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcvtsi2ssl %eax, %xmm0, %xmm0
, 1		// vcvtsi2ssq %rax, %xmm0, %xmm0
, 999		// vcvtss2sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vcvtss2sd %xmm0, %xmm0, %xmm0
, 999		// vcvtss2sil %es:(%rax,%rax,1), %eax
, 1		// vcvtss2sil %xmm0, %eax
, 999		// vcvtss2siq %es:(%rax,%rax,1), %rax
, 1		// vcvtss2siq %xmm0, %rax
, 999		// vcvttpd2dq %es:(%rax,%rax,1), %xmm0
, 999		// vcvttpd2dq %es:(%rax,%rax,1), %xmm0
, 1		// vcvttpd2dq %xmm0, %xmm0
, 27		// vcvttpd2dq %ymm0, %xmm0
, 999		// vcvttps2dq %es:(%rax,%rax,1), %xmm0
, 1		// vcvttps2dq %xmm0, %xmm0
, 999		// vcvttps2dq %es:(%rax,%rax,1), %ymm0
, 27		// vcvttps2dq %ymm0, %ymm0
, 999		// vcvttsd2si %es:(%rax,%rax,1), %eax
, 1		// vcvttsd2si %xmm0, %eax
, 999		// vcvttsd2siq %es:(%rax,%rax,1), %rax
, 1		// vcvttsd2siq %xmm0, %rax
, 999		// vcvttss2si %es:(%rax,%rax,1), %eax
, 1		// vcvttss2si %xmm0, %eax
, 999		// vcvttss2siq %es:(%rax,%rax,1), %rax
, 1		// vcvttss2siq %xmm0, %rax
, 999		// vdivpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vdivpd %xmm0, %xmm0, %xmm0
, 999		// vdivpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 159		// vdivpd %ymm0, %ymm0, %ymm0
, 999		// vdivps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vdivps %xmm0, %xmm0, %xmm0
, 999		// vdivps %es:(%rax,%rax,1), %ymm0, %ymm0
, 117		// vdivps %ymm0, %ymm0, %ymm0
, 999		// vdivsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vdivsd %xmm0, %xmm0, %xmm0
, 999		// vdivss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vdivss %xmm0, %xmm0, %xmm0
, 999		// vdppd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vdppd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vdpps $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 14		// vdpps $0x0, %xmm0, %xmm0, %xmm0
, 999		// vdpps $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 56		// vdpps $0x0, %ymm0, %ymm0, %ymm0
, 999		// verr %es:(%rax,%rax,1)
, 4022		// verr %ax
, 999		// verw %es:(%rax,%rax,1)
, 4016		// verw %ax
, 999		// vextractf128 $0x0, %ymm0, %es:(%rax,%rax,1)
, 1		// vextractf128 $0x0, %ymm0, %xmm0
, 999		// vextracti128 $0x0, %ymm0, %es:(%rax,%rax,1)
, 1		// vextracti128 $0x0, %ymm0, %xmm0
, 999		// vextractps $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// vextractps $0x0, %xmm0, %eax
, 999		// vfmadd132pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmadd132pd %xmm0, %xmm0, %xmm0
, 999		// vfmadd132pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmadd132pd %ymm0, %ymm0, %ymm0
, 999		// vfmadd132ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmadd132ps %xmm0, %xmm0, %xmm0
, 999		// vfmadd132ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmadd132ps %ymm0, %ymm0, %ymm0
, 999		// vfmadd132sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmadd132sd %xmm0, %xmm0, %xmm0
, 999		// vfmadd132ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmadd132ss %xmm0, %xmm0, %xmm0
, 999		// vfmadd213pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmadd213pd %xmm0, %xmm0, %xmm0
, 999		// vfmadd213pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmadd213pd %ymm0, %ymm0, %ymm0
, 999		// vfmadd213ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmadd213ps %xmm0, %xmm0, %xmm0
, 999		// vfmadd213ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmadd213ps %ymm0, %ymm0, %ymm0
, 999		// vfmadd213sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmadd213sd %xmm0, %xmm0, %xmm0
, 999		// vfmadd213ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmadd213ss %xmm0, %xmm0, %xmm0
, 999		// vfmadd231pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmadd231pd %xmm0, %xmm0, %xmm0
, 999		// vfmadd231pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmadd231pd %ymm0, %ymm0, %ymm0
, 999		// vfmadd231ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmadd231ps %xmm0, %xmm0, %xmm0
, 999		// vfmadd231ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmadd231ps %ymm0, %ymm0, %ymm0
, 999		// vfmadd231sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmadd231sd %xmm0, %xmm0, %xmm0
, 999		// vfmadd231ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmadd231ss %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub132pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmaddsub132pd %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub132pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmaddsub132pd %ymm0, %ymm0, %ymm0
, 999		// vfmaddsub132ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmaddsub132ps %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub132ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmaddsub132ps %ymm0, %ymm0, %ymm0
, 999		// vfmaddsub213pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmaddsub213pd %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub213pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmaddsub213pd %ymm0, %ymm0, %ymm0
, 999		// vfmaddsub213ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmaddsub213ps %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub213ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmaddsub213ps %ymm0, %ymm0, %ymm0
, 999		// vfmaddsub231pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmaddsub231pd %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub231pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmaddsub231pd %ymm0, %ymm0, %ymm0
, 999		// vfmaddsub231ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmaddsub231ps %xmm0, %xmm0, %xmm0
, 999		// vfmaddsub231ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmaddsub231ps %ymm0, %ymm0, %ymm0
, 999		// vfmsub132pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsub132pd %xmm0, %xmm0, %xmm0
, 999		// vfmsub132pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsub132pd %ymm0, %ymm0, %ymm0
, 999		// vfmsub132ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsub132ps %xmm0, %xmm0, %xmm0
, 999		// vfmsub132ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsub132ps %ymm0, %ymm0, %ymm0
, 999		// vfmsub132sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmsub132sd %xmm0, %xmm0, %xmm0
, 999		// vfmsub132ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmsub132ss %xmm0, %xmm0, %xmm0
, 999		// vfmsub213pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmsub213pd %xmm0, %xmm0, %xmm0
, 999		// vfmsub213pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsub213pd %ymm0, %ymm0, %ymm0
, 999		// vfmsub213ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsub213ps %xmm0, %xmm0, %xmm0
, 999		// vfmsub213ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsub213ps %ymm0, %ymm0, %ymm0
, 999		// vfmsub213sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmsub213sd %xmm0, %xmm0, %xmm0
, 999		// vfmsub213ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmsub213ss %xmm0, %xmm0, %xmm0
, 999		// vfmsub231pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmsub231pd %xmm0, %xmm0, %xmm0
, 999		// vfmsub231pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsub231pd %ymm0, %ymm0, %ymm0
, 999		// vfmsub231ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsub231ps %xmm0, %xmm0, %xmm0
, 999		// vfmsub231ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsub231ps %ymm0, %ymm0, %ymm0
, 999		// vfmsub231sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmsub231sd %xmm0, %xmm0, %xmm0
, 999		// vfmsub231ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfmsub231ss %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd132pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfmsubadd132pd %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd132pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsubadd132pd %ymm0, %ymm0, %ymm0
, 999		// vfmsubadd132ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsubadd132ps %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd132ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsubadd132ps %ymm0, %ymm0, %ymm0
, 999		// vfmsubadd213pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsubadd213pd %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd213pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsubadd213pd %ymm0, %ymm0, %ymm0
, 999		// vfmsubadd213ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsubadd213ps %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd213ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsubadd213ps %ymm0, %ymm0, %ymm0
, 999		// vfmsubadd231pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsubadd231pd %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd231pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsubadd231pd %ymm0, %ymm0, %ymm0
, 999		// vfmsubadd231ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfmsubadd231ps %xmm0, %xmm0, %xmm0
, 999		// vfmsubadd231ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfmsubadd231ps %ymm0, %ymm0, %ymm0
, 999		// vfnmadd132pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmadd132pd %xmm0, %xmm0, %xmm0
, 999		// vfnmadd132pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmadd132pd %ymm0, %ymm0, %ymm0
, 999		// vfnmadd132ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmadd132ps %xmm0, %xmm0, %xmm0
, 999		// vfnmadd132ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmadd132ps %ymm0, %ymm0, %ymm0
, 999		// vfnmadd132sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmadd132sd %xmm0, %xmm0, %xmm0
, 999		// vfnmadd132ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmadd132ss %xmm0, %xmm0, %xmm0
, 999		// vfnmadd213pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfnmadd213pd %xmm0, %xmm0, %xmm0
, 999		// vfnmadd213pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmadd213pd %ymm0, %ymm0, %ymm0
, 999		// vfnmadd213ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmadd213ps %xmm0, %xmm0, %xmm0
, 999		// vfnmadd213ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vfnmadd213ps %ymm0, %ymm0, %ymm0
, 999		// vfnmadd213sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmadd213sd %xmm0, %xmm0, %xmm0
, 999		// vfnmadd213ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmadd213ss %xmm0, %xmm0, %xmm0
, 999		// vfnmadd231pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfnmadd231pd %xmm0, %xmm0, %xmm0
, 999		// vfnmadd231pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmadd231pd %ymm0, %ymm0, %ymm0
, 999		// vfnmadd231ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmadd231ps %xmm0, %xmm0, %xmm0
, 999		// vfnmadd231ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmadd231ps %ymm0, %ymm0, %ymm0
, 999		// vfnmadd231sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmadd231sd %xmm0, %xmm0, %xmm0
, 999		// vfnmadd231ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmadd231ss %xmm0, %xmm0, %xmm0
, 999		// vfnmsub132pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfnmsub132pd %xmm0, %xmm0, %xmm0
, 999		// vfnmsub132pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmsub132pd %ymm0, %ymm0, %ymm0
, 999		// vfnmsub132ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmsub132ps %xmm0, %xmm0, %xmm0
, 999		// vfnmsub132ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmsub132ps %ymm0, %ymm0, %ymm0
, 999		// vfnmsub132sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmsub132sd %xmm0, %xmm0, %xmm0
, 999		// vfnmsub132ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmsub132ss %xmm0, %xmm0, %xmm0
, 999		// vfnmsub213pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfnmsub213pd %xmm0, %xmm0, %xmm0
, 999		// vfnmsub213pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmsub213pd %ymm0, %ymm0, %ymm0
, 999		// vfnmsub213ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmsub213ps %xmm0, %xmm0, %xmm0
, 999		// vfnmsub213ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmsub213ps %ymm0, %ymm0, %ymm0
, 999		// vfnmsub213sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmsub213sd %xmm0, %xmm0, %xmm0
, 999		// vfnmsub213ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmsub213ss %xmm0, %xmm0, %xmm0
, 999		// vfnmsub231pd %es:(%rax,%rax,1), %xmm0, %xmm0
, 27		// vfnmsub231pd %xmm0, %xmm0, %xmm0
, 999		// vfnmsub231pd %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmsub231pd %ymm0, %ymm0, %ymm0
, 999		// vfnmsub231ps %es:(%rax,%rax,1), %xmm0, %xmm0
, 26		// vfnmsub231ps %xmm0, %xmm0, %xmm0
, 999		// vfnmsub231ps %es:(%rax,%rax,1), %ymm0, %ymm0
, 26		// vfnmsub231ps %ymm0, %ymm0, %ymm0
, 999		// vfnmsub231sd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmsub231sd %xmm0, %xmm0, %xmm0
, 999		// vfnmsub231ss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vfnmsub231ss %xmm0, %xmm0, %xmm0
, 999		// vgatherdpd %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vgatherdpd %ymm0, %es:(%rax,%rax,1), %ymm0
, 999		// vgatherdps %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vgatherdps %ymm0, %es:(%rax,%rax,1), %ymm0
, 999		// vgatherqpd %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vgatherqpd %ymm0, %es:(%rax,%rax,1), %ymm0
, 999		// vgatherqps %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vhaddpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vhaddpd %xmm0, %xmm0, %xmm0
, 999		// vhaddpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vhaddpd %ymm0, %ymm0, %ymm0
, 999		// vhaddps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vhaddps %xmm0, %xmm0, %xmm0
, 999		// vhaddps %es:(%rax,%rax,1), %ymm0, %ymm0
, 28		// vhaddps %ymm0, %ymm0, %ymm0
, 999		// vhsubpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vhsubpd %xmm0, %xmm0, %xmm0
, 999		// vhsubpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 28		// vhsubpd %ymm0, %ymm0, %ymm0
, 999		// vhsubps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vhsubps %xmm0, %xmm0, %xmm0
, 999		// vhsubps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vhsubps %ymm0, %ymm0, %ymm0
, 999		// vinsertf128 $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vinsertf128 $0x0, %xmm0, %ymm0, %ymm0
, 999		// vinserti128 $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vinserti128 $0x0, %xmm0, %ymm0, %ymm0
, 999		// vinsertps $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vinsertps $0x0, %xmm0, %xmm0, %xmm0
, 999		// vlddqu %es:(%rax,%rax,1), %xmm0
, 999		// vlddqu %es:(%rax,%rax,1), %ymm0
, 999		// vldmxcsr %es:(%rax,%rax,1)
, 142		// vmaskmovdqu %xmm0, %xmm0
, 999		// vmaskmovpd %xmm0, %xmm0, %es:(%rax,%rax,1)
, 999		// vmaskmovpd %ymm0, %ymm0, %es:(%rax,%rax,1)
, 999		// vmaskmovpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmaskmovpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 999		// vmaskmovps %xmm0, %xmm0, %es:(%rax,%rax,1)
, 999		// vmaskmovps %ymm0, %ymm0, %es:(%rax,%rax,1)
, 999		// vmaskmovps %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmaskmovps %es:(%rax,%rax,1), %ymm0, %ymm0
, 999		// vmaxpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmaxpd %xmm0, %xmm0, %xmm0
, 999		// vmaxpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vmaxpd %ymm0, %ymm0, %ymm0
, 999		// vmaxps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmaxps %xmm0, %xmm0, %xmm0
, 999		// vmaxps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vmaxps %ymm0, %ymm0, %ymm0
, 999		// vmaxsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmaxsd %xmm0, %xmm0, %xmm0
, 999		// vmaxss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmaxss %xmm0, %xmm0, %xmm0
, 999		// vminpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vminpd %xmm0, %xmm0, %xmm0
, 999		// vminpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vminpd %ymm0, %ymm0, %ymm0
, 999		// vminps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vminps %xmm0, %xmm0, %xmm0
, 999		// vminps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vminps %ymm0, %ymm0, %ymm0
, 999		// vminsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vminsd %xmm0, %xmm0, %xmm0
, 999		// vminss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vminss %xmm0, %xmm0, %xmm0
, 999		// vmovapd %xmm0, %es:(%rax,%rax,1)
, 999		// vmovapd %ymm0, %es:(%rax,%rax,1)
, 999		// vmovapd %es:(%rax,%rax,1), %xmm0
, 1		// vmovapd %xmm0, %xmm0
, 999		// vmovapd %es:(%rax,%rax,1), %ymm0
, 1		// vmovapd %ymm0, %ymm0
, 999		// vmovaps %xmm0, %es:(%rax,%rax,1)
, 999		// vmovaps %ymm0, %es:(%rax,%rax,1)
, 999		// vmovaps %es:(%rax,%rax,1), %xmm0
, 1		// vmovaps %xmm0, %xmm0
, 999		// vmovaps %es:(%rax,%rax,1), %ymm0
, 1		// vmovaps %ymm0, %ymm0
, 999		// vmovd %xmm0, %es:(%rax,%rax,1)
, 1		// vmovd %xmm0, %eax
, 999		// vmovd %es:(%rax,%rax,1), %xmm0
, 1		// vmovd %eax, %xmm0
, 999		// vmovddup %es:(%rax,%rax,1), %xmm0
, 1		// vmovddup %xmm0, %xmm0
, 999		// vmovddup %es:(%rax,%rax,1), %ymm0
, 1		// vmovddup %ymm0, %ymm0
, 999		// vmovdqa %xmm0, %es:(%rax,%rax,1)
, 999		// vmovdqa %ymm0, %es:(%rax,%rax,1)
, 999		// vmovdqa %es:(%rax,%rax,1), %xmm0
, 1		// vmovdqa %xmm0, %xmm0
, 999		// vmovdqa %es:(%rax,%rax,1), %ymm0
, 1		// vmovdqa %ymm0, %ymm0
, 999		// vmovdqu %xmm0, %es:(%rax,%rax,1)
, 999		// vmovdqu %ymm0, %es:(%rax,%rax,1)
, 999		// vmovdqu %es:(%rax,%rax,1), %xmm0
, 1		// vmovdqu %xmm0, %xmm0
, 999		// vmovdqu %es:(%rax,%rax,1), %ymm0
, 1		// vmovdqu %ymm0, %ymm0
, 1		// vmovhlps %xmm0, %xmm0, %xmm0
, 999		// vmovhpd %xmm0, %es:(%rax,%rax,1)
, 999		// vmovhpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmovhps %xmm0, %es:(%rax,%rax,1)
, 999		// vmovhps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmovlhps %xmm0, %xmm0, %xmm0
, 999		// vmovlpd %xmm0, %es:(%rax,%rax,1)
, 999		// vmovlpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vmovlps %xmm0, %es:(%rax,%rax,1)
, 999		// vmovlps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmovmskpd %xmm0, %eax
, 1		// vmovmskpd %ymm0, %eax
, 1		// vmovmskpd %xmm0, %rax
, 1		// vmovmskpd %ymm0, %rax
, 1		// vmovmskps %xmm0, %eax
, 1		// vmovmskps %ymm0, %eax
, 1		// vmovmskps %xmm0, %rax
, 1		// vmovmskps %ymm0, %rax
, 999		// vmovntdqa %xmm0, %es:(%rax,%rax,1)
, 999		// vmovntdqa %es:(%rax,%rax,1), %xmm0
, 999		// vmovntdqa %es:(%rax,%rax,1), %ymm0
, 999		// vmovntpd %xmm0, %es:(%rax,%rax,1)
, 999		// vmovntpd %ymm0, %es:(%rax,%rax,1)
, 999		// vmovntps %xmm0, %es:(%rax,%rax,1)
, 999		// vmovntps %ymm0, %es:(%rax,%rax,1)
, 999		// vmovq %xmm0, %es:(%rax,%rax,1)
, 1		// vmovq %xmm0, %rax
, 999		// vmovq %es:(%rax,%rax,1), %xmm0
, 1		// vmovq %rax, %xmm0
, 1		// vmovq %xmm0, %xmm0
, 999		// vmovsd %xmm0, %es:(%rax,%rax,1)
, 999		// vmovsd %es:(%rax,%rax,1), %xmm0
, 1		// vmovsd %xmm0, %xmm0, %xmm0
, 999		// vmovshdup %es:(%rax,%rax,1), %xmm0
, 1		// vmovshdup %xmm0, %xmm0
, 999		// vmovshdup %es:(%rax,%rax,1), %ymm0
, 1		// vmovshdup %ymm0, %ymm0
, 999		// vmovsldup %es:(%rax,%rax,1), %xmm0
, 1		// vmovsldup %xmm0, %xmm0
, 999		// vmovsldup %es:(%rax,%rax,1), %ymm0
, 1		// vmovsldup %ymm0, %ymm0
, 999		// vmovss %xmm0, %es:(%rax,%rax,1)
, 999		// vmovss %es:(%rax,%rax,1), %xmm0
, 1		// vmovss %xmm0, %xmm0, %xmm0
, 999		// vmovupd %xmm0, %es:(%rax,%rax,1)
, 999		// vmovupd %ymm0, %es:(%rax,%rax,1)
, 999		// vmovupd %es:(%rax,%rax,1), %xmm0
, 1		// vmovupd %xmm0, %xmm0
, 999		// vmovupd %es:(%rax,%rax,1), %ymm0
, 1		// vmovupd %ymm0, %ymm0
, 999		// vmovups %xmm0, %es:(%rax,%rax,1)
, 999		// vmovups %ymm0, %es:(%rax,%rax,1)
, 999		// vmovups %es:(%rax,%rax,1), %xmm0
, 1		// vmovups %xmm0, %xmm0
, 999		// vmovups %es:(%rax,%rax,1), %ymm0
, 1		// vmovups %ymm0, %ymm0
, 999		// vmpsadbw $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmpsadbw $0x0, %xmm0, %xmm0, %xmm0
, 999		// vmpsadbw $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vmpsadbw $0x0, %ymm0, %ymm0, %ymm0
, 999		// vmulpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmulpd %xmm0, %xmm0, %xmm0
, 999		// vmulpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vmulpd %ymm0, %ymm0, %ymm0
, 999		// vmulps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmulps %xmm0, %xmm0, %xmm0
, 999		// vmulps %es:(%rax,%rax,1), %ymm0, %ymm0
, 28		// vmulps %ymm0, %ymm0, %ymm0
, 999		// vmulsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmulsd %xmm0, %xmm0, %xmm0
, 999		// vmulss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vmulss %xmm0, %xmm0, %xmm0
, 999		// vorpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vorpd %xmm0, %xmm0, %xmm0
, 999		// vorpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vorpd %ymm0, %ymm0, %ymm0
, 999		// vorps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vorps %xmm0, %xmm0, %xmm0
, 999		// vorps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vorps %ymm0, %ymm0, %ymm0
, 999		// vpabsb %es:(%rax,%rax,1), %xmm0
, 1		// vpabsb %xmm0, %xmm0
, 999		// vpabsb %es:(%rax,%rax,1), %ymm0
, 1		// vpabsb %ymm0, %ymm0
, 999		// vpabsd %es:(%rax,%rax,1), %xmm0
, 1		// vpabsd %xmm0, %xmm0
, 999		// vpabsd %es:(%rax,%rax,1), %ymm0
, 1		// vpabsd %ymm0, %ymm0
, 999		// vpabsw %es:(%rax,%rax,1), %xmm0
, 1		// vpabsw %xmm0, %xmm0
, 999		// vpabsw %es:(%rax,%rax,1), %ymm0
, 1		// vpabsw %ymm0, %ymm0
, 999		// vpackssdw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpackssdw %xmm0, %xmm0, %xmm0
, 999		// vpackssdw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpackssdw %ymm0, %ymm0, %ymm0
, 999		// vpacksswb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpacksswb %xmm0, %xmm0, %xmm0
, 999		// vpacksswb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpacksswb %ymm0, %ymm0, %ymm0
, 999		// vpackusdw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpackusdw %xmm0, %xmm0, %xmm0
, 999		// vpackusdw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpackusdw %ymm0, %ymm0, %ymm0
, 999		// vpackuswb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpackuswb %xmm0, %xmm0, %xmm0
, 999		// vpackuswb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpackuswb %ymm0, %ymm0, %ymm0
, 999		// vpaddb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddb %xmm0, %xmm0, %xmm0
, 999		// vpaddb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddb %ymm0, %ymm0, %ymm0
, 999		// vpaddd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddd %xmm0, %xmm0, %xmm0
, 999		// vpaddd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddd %ymm0, %ymm0, %ymm0
, 999		// vpaddq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddq %xmm0, %xmm0, %xmm0
, 999		// vpaddq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddq %ymm0, %ymm0, %ymm0
, 999		// vpaddsb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddsb %xmm0, %xmm0, %xmm0
, 999		// vpaddsb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddsb %ymm0, %ymm0, %ymm0
, 999		// vpaddsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddsw %xmm0, %xmm0, %xmm0
, 999		// vpaddsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddsw %ymm0, %ymm0, %ymm0
, 999		// vpaddusb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddusb %xmm0, %xmm0, %xmm0
, 999		// vpaddusb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddusb %ymm0, %ymm0, %ymm0
, 999		// vpaddusw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddusw %xmm0, %xmm0, %xmm0
, 999		// vpaddusw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddusw %ymm0, %ymm0, %ymm0
, 999		// vpaddw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpaddw %xmm0, %xmm0, %xmm0
, 999		// vpaddw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpaddw %ymm0, %ymm0, %ymm0
, 999		// vpalignr $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpalignr $0x0, %xmm0, %xmm0, %xmm0
, 999		// vpalignr $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpalignr $0x0, %ymm0, %ymm0, %ymm0
, 999		// vpand %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpand %xmm0, %xmm0, %xmm0
, 999		// vpand %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpand %ymm0, %ymm0, %ymm0
, 999		// vpandn %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpandn %xmm0, %xmm0, %xmm0
, 999		// vpandn %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpandn %ymm0, %ymm0, %ymm0
, 999		// vpavgb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpavgb %xmm0, %xmm0, %xmm0
, 999		// vpavgb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpavgb %ymm0, %ymm0, %ymm0
, 999		// vpavgw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpavgw %xmm0, %xmm0, %xmm0
, 999		// vpavgw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpavgw %ymm0, %ymm0, %ymm0
, 999		// vpblendd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpblendd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vpblendd $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpblendd $0x0, %ymm0, %ymm0, %ymm0
, 999		// vpblendvb %xmm0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpblendvb %xmm0, %xmm0, %xmm0, %xmm0
, 999		// vpblendvb %ymm0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpblendvb %ymm0, %ymm0, %ymm0, %ymm0
, 999		// vpblendw $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpblendw $0x0, %xmm0, %xmm0, %xmm0
, 999		// vpblendw $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpblendw $0x0, %ymm0, %ymm0, %ymm0
, 999		// vpbroadcastb %es:(%rax,%rax,1), %xmm0
, 1		// vpbroadcastb %xmm0, %xmm0
, 999		// vpbroadcastb %es:(%rax,%rax,1), %ymm0
, 1		// vpbroadcastb %xmm0, %ymm0
, 999		// vpbroadcastd %es:(%rax,%rax,1), %xmm0
, 1		// vpbroadcastd %xmm0, %xmm0
, 999		// vpbroadcastd %es:(%rax,%rax,1), %ymm0
, 1		// vpbroadcastd %xmm0, %ymm0
, 999		// vpbroadcastq %es:(%rax,%rax,1), %xmm0
, 1		// vpbroadcastq %xmm0, %xmm0
, 999		// vpbroadcastq %es:(%rax,%rax,1), %ymm0
, 1		// vpbroadcastq %xmm0, %ymm0
, 999		// vpbroadcastw %es:(%rax,%rax,1), %xmm0
, 1		// vpbroadcastw %xmm0, %xmm0
, 999		// vpbroadcastw %es:(%rax,%rax,1), %ymm0
, 1		// vpbroadcastw %xmm0, %ymm0
, 999		// vpclmulqdq $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpclmulqdq $0x0, %xmm0, %xmm0, %xmm0
, 999		// vpcmpeqb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpeqb %xmm0, %xmm0, %xmm0
, 999		// vpcmpeqb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpeqb %ymm0, %ymm0, %ymm0
, 999		// vpcmpeqd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpeqd %xmm0, %xmm0, %xmm0
, 999		// vpcmpeqd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpeqd %ymm0, %ymm0, %ymm0
, 999		// vpcmpeqq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpeqq %xmm0, %xmm0, %xmm0
, 999		// vpcmpeqq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpeqq %ymm0, %ymm0, %ymm0
, 999		// vpcmpeqw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpeqw %xmm0, %xmm0, %xmm0
, 999		// vpcmpeqw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpeqw %ymm0, %ymm0, %ymm0
, 999		// vpcmpestri $0x0, %es:(%rax,%rax,1), %xmm0
, 40		// vpcmpestri $0x0, %xmm0, %xmm0
, 999		// vpcmpestrm $0x0, %es:(%rax,%rax,1), %xmm0
, 65		// vpcmpestrm $0x0, %xmm0, %xmm0
, 999		// vpcmpgtb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpgtb %xmm0, %xmm0, %xmm0
, 999		// vpcmpgtb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpgtb %ymm0, %ymm0, %ymm0
, 999		// vpcmpgtd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpgtd %xmm0, %xmm0, %xmm0
, 999		// vpcmpgtd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpgtd %ymm0, %ymm0, %ymm0
, 999		// vpcmpgtq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpgtq %xmm0, %xmm0, %xmm0
, 999		// vpvmpgtq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpvmpgtq %ymm0, %ymm0, %ymm0
, 999		// vpcmpgtw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpcmpgtw %xmm0, %xmm0, %xmm0
, 999		// vpcmpgtw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpcmpgtw %ymm0, %ymm0, %ymm0
, 999		// vpcmpistri $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpcmpistri $0x0, %xmm0, %xmm0
, 999		// vpcmpistrm $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpcmpistrm $0x0, %xmm0, %xmm0
, 999		// vperm2f128 $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vperm2f128 $0x0, %ymm0, %ymm0, %ymm0
, 999		// vperm2i128 $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vperm2i128 $0x0, %ymm0, %ymm0, %ymm0
, 999		// vpermd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpermd %ymm0, %ymm0, %ymm0
, 999		// vpermilpd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpermilpd $0x0, %xmm0, %xmm0
, 999		// vpermilpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpermilpd %xmm0, %xmm0, %xmm0
, 999		// vpermilpd $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpermilpd $0x0, %ymm0, %ymm0
, 999		// vpermilpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpermilpd %ymm0, %ymm0, %ymm0
, 999		// vpermilps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpermilps $0x0, %xmm0, %xmm0
, 999		// vpermilps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpermilps %xmm0, %xmm0, %xmm0
, 999		// vpermilps $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpermilps $0x0, %ymm0, %ymm0
, 999		// vpermilps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpermilps %ymm0, %ymm0, %ymm0
, 999		// vpermpd $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpermpd $0x0, %ymm0, %ymm0
, 999		// vpermps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpermps %ymm0, %ymm0, %ymm0
, 999		// vpermq $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpermq $0x0, %ymm0, %ymm0
, 999		// vpextrb $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// vpextrb $0x0, %xmm0, %eax
, 1		// vpextrb $0x0, %xmm0, %rax
, 999		// vpextrd $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// vpextrd $0x0, %xmm0, %eax
, 999		// vpextrq $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// vpextrq $0x0, %xmm0, %rax
, 999		// vpextrw $0x0, %xmm0, %es:(%rax,%rax,1)
, 1		// vpextrw $0x0, %xmm0, %eax
, 1		// vpextrw $0x0, %xmm0, %rax
, 999		// vpgatherdd %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vpgatherdd %ymm0, %es:(%rax,%rax,1), %ymm0
, 999		// vpgatherdq %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vpgatherdq %ymm0, %es:(%rax,%rax,1), %ymm0
, 999		// vpgatherqd %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqq %xmm0, %es:(%rax,%rax,1), %xmm0
, 999		// vpgatherqq %ymm0, %es:(%rax,%rax,1), %ymm0
, 999		// vphaddd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vphaddd %xmm0, %xmm0, %xmm0
, 999		// vphaddd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vphaddd %ymm0, %ymm0, %ymm0
, 999		// vphaddsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vphaddsw %xmm0, %xmm0, %xmm0
, 999		// vphaddsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vphaddsw %ymm0, %ymm0, %ymm0
, 999		// vphaddw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vphaddw %xmm0, %xmm0, %xmm0
, 999		// vphaddw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vphaddw %ymm0, %ymm0, %ymm0
, 999		// vphminposuw %es:(%rax,%rax,1), %xmm0
, 1		// vphminposuw %xmm0, %xmm0
, 999		// vphsubd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vphsubd %xmm0, %xmm0, %xmm0
, 999		// vphsubd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vphsubd %ymm0, %ymm0, %ymm0
, 999		// vphsubsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vphsubsw %xmm0, %xmm0, %xmm0
, 999		// vphsubsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vphsubsw %ymm0, %ymm0, %ymm0
, 999		// vphsubw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vphsubw %xmm0, %xmm0, %xmm0
, 999		// vphsubw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vphsubw %ymm0, %ymm0, %ymm0
, 999		// vpinsrb $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpinsrb $0x0, %eax, %xmm0, %xmm0
, 999		// vpinsrd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpinsrd $0x0, %eax, %xmm0, %xmm0
, 999		// vpinsrq $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpinsrq $0x0, %rax, %xmm0, %xmm0
, 999		// vpinsrw $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpinsrw $0x0, %eax, %xmm0, %xmm0
, 999		// vpmaddubsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaddubsw %xmm0, %xmm0, %xmm0
, 999		// vpmaddubsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaddubsw %ymm0, %ymm0, %ymm0
, 999		// vpmaddwd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaddwd %xmm0, %xmm0, %xmm0
, 999		// vpmaddwd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaddwd %ymm0, %ymm0, %ymm0
, 999		// vpmaskmovd %xmm0, %xmm0, %es:(%rax,%rax,1)
, 999		// vpmaskmovd %ymm0, %ymm0, %es:(%rax,%rax,1)
, 999		// vpmaskmovd %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vpmaskmovd %es:(%rax,%rax,1), %ymm0, %ymm0
, 999		// vpmaskmovq %xmm0, %xmm0, %es:(%rax,%rax,1)
, 999		// vpmaskmovq %ymm0, %ymm0, %es:(%rax,%rax,1)
, 999		// vpmaskmovq %es:(%rax,%rax,1), %xmm0, %xmm0
, 999		// vpmaskmovq %es:(%rax,%rax,1), %ymm0, %ymm0
, 999		// vpmaxsb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaxsb %xmm0, %xmm0, %xmm0
, 999		// vpmaxsb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaxsb %ymm0, %ymm0, %ymm0
, 999		// vpmaxsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaxsd %xmm0, %xmm0, %xmm0
, 999		// vpmaxsd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaxsd %ymm0, %ymm0, %ymm0
, 999		// vpmaxsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaxsw %xmm0, %xmm0, %xmm0
, 999		// vpmaxsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaxsw %ymm0, %ymm0, %ymm0
, 999		// vpmaxub %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaxub %xmm0, %xmm0, %xmm0
, 999		// vpmaxub %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaxub %ymm0, %ymm0, %ymm0
, 999		// vpmaxud %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaxud %xmm0, %xmm0, %xmm0
, 999		// vpmaxud %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaxud %ymm0, %ymm0, %ymm0
, 999		// vpmaxuw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmaxuw %xmm0, %xmm0, %xmm0
, 999		// vpmaxuw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmaxuw %ymm0, %ymm0, %ymm0
, 999		// vpminsb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpminsb %xmm0, %xmm0, %xmm0
, 999		// vpminsb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpminsb %ymm0, %ymm0, %ymm0
, 999		// vpminsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpminsd %xmm0, %xmm0, %xmm0
, 999		// vpminsd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpminsd %ymm0, %ymm0, %ymm0
, 999		// vpminsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpminsw %xmm0, %xmm0, %xmm0
, 999		// vpminub %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpminub %xmm0, %xmm0, %xmm0
, 999		// vpminub %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpminub %ymm0, %ymm0, %ymm0
, 999		// vpminud %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpminud %xmm0, %xmm0, %xmm0
, 999		// vpminud %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpminud %ymm0, %ymm0, %ymm0
, 999		// vpminuw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpminuw %xmm0, %xmm0, %xmm0
, 999		// vpminuw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpminuw %ymm0, %ymm0, %ymm0
, 1		// vpmovmskb %xmm0, %eax
, 1		// vpmovmskb %ymm0, %eax
, 1		// vpmovmskb %xmm0, %rax
, 1		// vpmovmskb %ymm0, %rax
, 999		// vpmovsxbd %es:(%rax,%rax,1), %xmm0
, 1		// vpmovsxbd %xmm0, %xmm0
, 999		// vpmovsxbd %es:(%rax,%rax,1), %ymm0
, 1		// vpmovsxbd %xmm0, %ymm0
, 999		// vpmovsxbq %es:(%rax,%rax,1), %xmm0
, 1		// vpmovsxbq %xmm0, %xmm0
, 999		// vpmovsxbq %es:(%rax,%rax,1), %ymm0
, 1		// vpmovsxbq %xmm0, %ymm0
, 999		// vpmovsxbw %es:(%rax,%rax,1), %xmm0
, 1		// vpmovsxbw %xmm0, %xmm0
, 999		// vpmovsxbw %es:(%rax,%rax,1), %ymm0
, 1		// vpmovsxbw %xmm0, %ymm0
, 999		// vpmovsxdq %es:(%rax,%rax,1), %xmm0
, 1		// vpmovsxdq %xmm0, %xmm0
, 999		// vpmovsxdq %es:(%rax,%rax,1), %ymm0
, 1		// vpmovsxdq %xmm0, %ymm0
, 999		// vpmovsxwd %es:(%rax,%rax,1), %xmm0
, 1		// vpmovsxwd %xmm0, %xmm0
, 999		// vpmovsxwd %es:(%rax,%rax,1), %ymm0
, 1		// vpmovsxwd %xmm0, %ymm0
, 999		// vpmovsxwq %es:(%rax,%rax,1), %xmm0
, 1		// vpmovsxwq %xmm0, %xmm0
, 999		// vpmovsxwq %es:(%rax,%rax,1), %ymm0
, 1		// vpmovsxwq %xmm0, %ymm0
, 999		// vpmovzxbd %es:(%rax,%rax,1), %xmm0
, 1		// vpmovzxbd %xmm0, %xmm0
, 999		// vpmovzxbd %es:(%rax,%rax,1), %ymm0
, 1		// vpmovzxbd %xmm0, %ymm0
, 999		// vpmovzxbq %es:(%rax,%rax,1), %xmm0
, 1		// vpmovzxbq %xmm0, %xmm0
, 999		// vpmovzxbq %es:(%rax,%rax,1), %ymm0
, 1		// vpmovzxbq %xmm0, %ymm0
, 999		// vpmovzxbw %es:(%rax,%rax,1), %xmm0
, 1		// vpmovzxbw %xmm0, %xmm0
, 999		// vpmovzxbw %es:(%rax,%rax,1), %ymm0
, 1		// vpmovzxbw %xmm0, %ymm0
, 999		// vpmovzxdq %es:(%rax,%rax,1), %xmm0
, 1		// vpmovzxdq %xmm0, %xmm0
, 999		// vpmovzxdq %es:(%rax,%rax,1), %ymm0
, 1		// vpmovzxdq %xmm0, %ymm0
, 999		// vpmovzxwd %es:(%rax,%rax,1), %xmm0
, 1		// vpmovzxwd %xmm0, %xmm0
, 999		// vpmovzxwd %es:(%rax,%rax,1), %ymm0
, 1		// vpmovzxwd %xmm0, %ymm0
, 999		// vpmovzxwq %es:(%rax,%rax,1), %xmm0
, 1		// vpmovzxwq %xmm0, %xmm0
, 999		// vpmovzxwq %es:(%rax,%rax,1), %ymm0
, 1		// vpmovzxwq %xmm0, %ymm0
, 999		// vpmuldq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmuldq %xmm0, %xmm0, %xmm0
, 999		// vpmuldq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmuldq %ymm0, %ymm0, %ymm0
, 999		// vpmulhrsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmulhrsw %xmm0, %xmm0, %xmm0
, 999		// vpmulhrsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmulhrsw %ymm0, %ymm0, %ymm0
, 999		// vpmulhuw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmulhuw %xmm0, %xmm0, %xmm0
, 999		// vpmulhuw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmulhuw %ymm0, %ymm0, %ymm0
, 999		// vpmulhw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmulhw %xmm0, %xmm0, %xmm0
, 999		// vpmulhw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmulhw %ymm0, %ymm0, %ymm0
, 999		// vpmulld %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmulld %xmm0, %xmm0, %xmm0
, 999		// vpmulld %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmulld %ymm0, %ymm0, %ymm0
, 999		// vpmullw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmullw %xmm0, %xmm0, %xmm0
, 999		// vpmullw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmullw %ymm0, %ymm0, %ymm0
, 999		// vpmuludq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpmuludq %xmm0, %xmm0, %xmm0
, 999		// vpmuludq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpmuludq %ymm0, %ymm0, %ymm0
, 999		// vpor %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpor %xmm0, %xmm0, %xmm0
, 999		// vpor %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpor %ymm0, %ymm0, %ymm0
, 999		// vpsadbw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsadbw %xmm0, %xmm0, %xmm0
, 999		// vpsadbw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsadbw %ymm0, %ymm0, %ymm0
, 999		// vpshufb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpshufb %xmm0, %xmm0, %xmm0
, 999		// vpshufb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpshufb %ymm0, %ymm0, %ymm0
, 999		// vpshufd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpshufd $0x0, %xmm0, %xmm0
, 999		// vpshufd $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpshufd $0x0, %ymm0, %ymm0
, 999		// vpshufhw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpshufhw $0x0, %xmm0, %xmm0
, 999		// vpshufhw $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpshufhw $0x0, %ymm0, %ymm0
, 999		// vpshuflw $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vpshuflw $0x0, %xmm0, %xmm0
, 999		// vpshuflw $0x0, %es:(%rax,%rax,1), %ymm0
, 1		// vpshuflw $0x0, %ymm0, %ymm0
, 999		// vpsignb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsignb %xmm0, %xmm0, %xmm0
, 999		// vpsignd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsignd %xmm0, %xmm0, %xmm0
, 999		// vpsignw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsignw %xmm0, %xmm0, %xmm0
, 1		// vpslld $0x0, %xmm0, %xmm0
, 999		// vpslld %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpslld %xmm0, %xmm0, %xmm0
, 1		// vpslld $0x0, %ymm0, %ymm0
, 999		// vpslld %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpslld %xmm0, %ymm0, %ymm0
, 1		// vpslldq $0x0, %xmm0, %xmm0
, 1		// vpslldq $0x0, %ymm0, %ymm0
, 1		// vpsllq $0x0, %xmm0, %xmm0
, 999		// vpsllq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsllq %xmm0, %xmm0, %xmm0
, 1		// vpsllq $0x0, %ymm0, %ymm0
, 999		// vpsllq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsllq %xmm0, %ymm0, %ymm0
, 999		// vpsllvd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsllvd %xmm0, %xmm0, %xmm0
, 999		// vpsllvd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsllvd %ymm0, %ymm0, %ymm0
, 999		// vpsllvq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsllvq %xmm0, %xmm0, %xmm0
, 999		// vpsllvq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsllvq %ymm0, %ymm0, %ymm0
, 1		// vpsllw $0x0, %xmm0, %xmm0
, 999		// vpsllw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsllw %xmm0, %xmm0, %xmm0
, 1		// vpsllw $0x0, %ymm0, %ymm0
, 999		// vpsllw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsllw %xmm0, %ymm0, %ymm0
, 1		// vpsrad $0x0, %xmm0, %xmm0
, 999		// vpsrad %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsrad %xmm0, %xmm0, %xmm0
, 1		// vpsrad $0x0, %ymm0, %ymm0
, 999		// vpsrad %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsrad %xmm0, %ymm0, %ymm0
, 999		// vpsravd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsravd %xmm0, %xmm0, %xmm0
, 999		// vpsravd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsravd %ymm0, %ymm0, %ymm0
, 1		// vpsraw $0x0, %xmm0, %xmm0
, 999		// vpsraw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsraw %xmm0, %xmm0, %xmm0
, 1		// vpsraw $0x0, %ymm0, %ymm0
, 999		// vpsraw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsraw %xmm0, %ymm0, %ymm0
, 1		// vpsrld $0x0, %xmm0, %xmm0
, 999		// vpsrld %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsrld %xmm0, %xmm0, %xmm0
, 1		// vpsrld $0x0, %ymm0, %ymm0
, 999		// vpsrld %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsrld %xmm0, %ymm0, %ymm0
, 1		// vpsrldq $0x0, %xmm0, %xmm0
, 1		// vpsrldq $0x0, %ymm0, %ymm0
, 1		// vpsrlq $0x0, %xmm0, %xmm0
, 999		// vpsrlq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsrlq %xmm0, %xmm0, %xmm0
, 1		// vpsrlq $0x0, %ymm0, %ymm0
, 999		// vpsrlq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsrlq %xmm0, %ymm0, %ymm0
, 999		// vpsrlvd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsrlvd %xmm0, %xmm0, %xmm0
, 999		// vpsrlvd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsrlvd %ymm0, %ymm0, %ymm0
, 999		// vpsrlvq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsrlvq %xmm0, %xmm0, %xmm0
, 999		// vpsrlvq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsrlvq %ymm0, %ymm0, %ymm0
, 1		// vpsrlw $0x0, %xmm0, %xmm0
, 999		// vpsrlw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsrlw %xmm0, %xmm0, %xmm0
, 1		// vpsrlw $0x0, %ymm0, %ymm0
, 999		// vpsrlw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsrlw %xmm0, %ymm0, %ymm0
, 999		// vpsubb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubb %xmm0, %xmm0, %xmm0
, 999		// vpsubb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubb %ymm0, %ymm0, %ymm0
, 999		// vpsubd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubd %xmm0, %xmm0, %xmm0
, 999		// vpsubd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubd %ymm0, %ymm0, %ymm0
, 999		// vpsubq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubq %xmm0, %xmm0, %xmm0
, 999		// vpsubq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubq %ymm0, %ymm0, %ymm0
, 999		// vpsubsb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubsb %xmm0, %xmm0, %xmm0
, 999		// vpsubsb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubsb %ymm0, %ymm0, %ymm0
, 999		// vpsubsw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubsw %xmm0, %xmm0, %xmm0
, 999		// vpsubsw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubsw %ymm0, %ymm0, %ymm0
, 999		// vpsubusb %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubusb %xmm0, %xmm0, %xmm0
, 999		// vpsubusb %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubusb %ymm0, %ymm0, %ymm0
, 999		// vpsubusw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubusw %xmm0, %xmm0, %xmm0
, 999		// vpsubusw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubusw %ymm0, %ymm0, %ymm0
, 999		// vpsubw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpsubw %xmm0, %xmm0, %xmm0
, 999		// vpsubw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpsubw %ymm0, %ymm0, %ymm0
, 999		// vptest %es:(%rax,%rax,1), %xmm0
, 1		// vptest %xmm0, %xmm0
, 999		// vptest %es:(%rax,%rax,1), %ymm0
, 1		// vptest %ymm0, %ymm0
, 999		// vpunpckhbw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpckhbw %xmm0, %xmm0, %xmm0
, 999		// vpunpckhbw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpckhbw %ymm0, %ymm0, %ymm0
, 999		// vpunpckhdq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpckhdq %xmm0, %xmm0, %xmm0
, 999		// vpunpckhdq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpckhdq %ymm0, %ymm0, %ymm0
, 999		// vpunpckhqdq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpckhqdq %xmm0, %xmm0, %xmm0
, 999		// vpunpckhqdq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpckhqdq %ymm0, %ymm0, %ymm0
, 999		// vpunpckhwd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpckhwd %xmm0, %xmm0, %xmm0
, 999		// vpunpckhwd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpckhwd %ymm0, %ymm0, %ymm0
, 999		// vpunpcklbw %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpcklbw %xmm0, %xmm0, %xmm0
, 999		// vpunpcklbw %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpcklbw %ymm0, %ymm0, %ymm0
, 999		// vpunpckldq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpckldq %xmm0, %xmm0, %xmm0
, 999		// vpunpckldq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpckldq %ymm0, %ymm0, %ymm0
, 999		// vpunpcklqdq %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpcklqdq %xmm0, %xmm0, %xmm0
, 999		// vpunpcklqdq %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpcklqdq %ymm0, %ymm0, %ymm0
, 999		// vpunpcklwd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpunpcklwd %xmm0, %xmm0, %xmm0
, 999		// vpunpcklwd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpunpcklwd %ymm0, %ymm0, %ymm0
, 999		// vpxor %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vpxor %xmm0, %xmm0, %xmm0
, 999		// vpxor %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vpxor %ymm0, %ymm0, %ymm0
, 999		// vrcpps %es:(%rax,%rax,1), %xmm0
, 1		// vrcpps %xmm0, %xmm0
, 999		// vrcpps %es:(%rax,%rax,1), %ymm0
, 1		// vrcpps %ymm0, %ymm0
, 999		// vrcpss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vrcpss %xmm0, %xmm0, %xmm0
, 999		// vroundpd $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vroundpd $0x0, %xmm0, %xmm0
, 999		// vroundpd $0x0, %es:(%rax,%rax,1), %ymm0
, 27		// vroundpd $0x0, %ymm0, %ymm0
, 999		// vroundps $0x0, %es:(%rax,%rax,1), %xmm0
, 1		// vroundps $0x0, %xmm0, %xmm0
, 999		// vroundps $0x0, %es:(%rax,%rax,1), %ymm0
, 27		// vroundps $0x0, %ymm0, %ymm0
, 999		// vroundsd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vroundsd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vroundss $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vroundss $0x0, %xmm0, %xmm0, %xmm0
, 999		// vrsqrtps %es:(%rax,%rax,1), %xmm0
, 1		// vrsqrtps %xmm0, %xmm0
, 999		// vrsqrtps %es:(%rax,%rax,1), %ymm0
, 1		// vrsqrtps %ymm0, %ymm0
, 999		// vrsqrtss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vrsqrtss %xmm0, %xmm0, %xmm0
, 999		// vshufpd $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vshufpd $0x0, %xmm0, %xmm0, %xmm0
, 999		// vshufpd $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vshufpd $0x0, %ymm0, %ymm0, %ymm0
, 999		// vshufps $0x0, %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vshufps $0x0, %xmm0, %xmm0, %xmm0
, 999		// vshufps $0x0, %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vshufps $0x0, %ymm0, %ymm0, %ymm0
, 999		// vsqrtpd %es:(%rax,%rax,1), %xmm0
, 1		// vsqrtpd %xmm0, %xmm0
, 999		// vsqrtpd %es:(%rax,%rax,1), %ymm0
, 159		// vsqrtpd %ymm0, %ymm0
, 999		// vsqrtps %es:(%rax,%rax,1), %xmm0
, 1		// vsqrtps %xmm0, %xmm0
, 999		// vsqrtps %es:(%rax,%rax,1), %ymm0
, 125		// vsqrtps %ymm0, %ymm0
, 999		// vsqrtsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vsqrtsd %xmm0, %xmm0, %xmm0
, 999		// vsqrtss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vsqrtss %xmm0, %xmm0, %xmm0
, 999		// vstmxcsr %es:(%rax,%rax,1)
, 999		// vsubpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vsubpd %xmm0, %xmm0, %xmm0
, 999		// vsubpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 28		// vsubpd %ymm0, %ymm0, %ymm0
, 999		// vsubps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vsubps %xmm0, %xmm0, %xmm0
, 999		// vsubps %es:(%rax,%rax,1), %ymm0, %ymm0
, 27		// vsubps %ymm0, %ymm0, %ymm0
, 999		// vsubsd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vsubsd %xmm0, %xmm0, %xmm0
, 999		// vsubss %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vsubss %xmm0, %xmm0, %xmm0
, 999		// vtestpd %es:(%rax,%rax,1), %xmm0
, 1		// vtestpd %xmm0, %xmm0
, 999		// vtestpd %es:(%rax,%rax,1), %ymm0
, 1		// vtestpd %ymm0, %ymm0
, 999		// vtestps %es:(%rax,%rax,1), %xmm0
, 1		// vtestps %xmm0, %xmm0
, 999		// vtestps %es:(%rax,%rax,1), %ymm0
, 1		// vtestps %ymm0, %ymm0
, 999		// vucomisd %es:(%rax,%rax,1), %xmm0
, 1		// vucomisd %xmm0, %xmm0
, 999		// vucomiss %es:(%rax,%rax,1), %xmm0
, 1		// vucomiss %xmm0, %xmm0
, 999		// vunpckhpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vunpckhpd %xmm0, %xmm0, %xmm0
, 999		// vunpckhpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vunpckhpd %ymm0, %ymm0, %ymm0
, 999		// vunpckhps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vunpckhps %xmm0, %xmm0, %xmm0
, 999		// vunpckhps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vunpckhps %ymm0, %ymm0, %ymm0
, 999		// vunpcklpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vunpcklpd %xmm0, %xmm0, %xmm0
, 999		// vunpcklpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vunpcklpd %ymm0, %ymm0, %ymm0
, 999		// vunpcklps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vunpcklps %xmm0, %xmm0, %xmm0
, 999		// vunpcklps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vunpcklps %ymm0, %ymm0, %ymm0
, 999		// vxorpd %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vxorpd %xmm0, %xmm0, %xmm0
, 999		// vxorpd %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vxorpd %ymm0, %ymm0, %ymm0
, 999		// vxorps %es:(%rax,%rax,1), %xmm0, %xmm0
, 1		// vxorps %xmm0, %xmm0, %xmm0
, 999		// vxorps %es:(%rax,%rax,1), %ymm0, %ymm0
, 1		// vxorps %ymm0, %ymm0, %ymm0
, 142		// vzeroall 
, 1		// vzeroupper 
, 1		// wait 
, 999		// wrfsbase %eax
, 999		// wrfsbase %rax
, 999		// wrgsbase %eax
, 999		// wrgsbase %rax
, 999		// xabort $0x0
, 1		// xacquire 
, 999		// xaddw %ax, %es:(%rax,%rax,1)
, 999		// xaddl %eax, %es:(%rax,%rax,1)
, 999		// xaddq %rax, %es:(%rax,%rax,1)
, 999		// xaddb %al, %es:(%rax,%rax,1)
, 999		// xaddb  , %es:(%rax,%rax,1)
, 1		// xaddw %ax, %ax
, 1		// xaddl %eax, %eax
, 1		// xaddq %rax, %rax
, 1		// xaddb %al, %al
, 1		// xaddb %ah, %al
, 1		// xaddb %al, %ah
, 1		// xaddb %ah, %ah
, 999		// xbegin 0
, 999		// xbegin 
, 1		// xchgw %ax, %ax
, 1		// xchgl %eax, %eax
, 999		// xchgw %ax, %es:(%rax,%rax,1)
, 999		// xchgl %eax, %es:(%rax,%rax,1)
, 999		// xchgq %rax, %es:(%rax,%rax,1)
, 999		// xchgb %al, %es:(%rax,%rax,1)
, 999		// xchgb  , %es:(%rax,%rax,1)
, 1		// xchgw %ax, %ax
, 999		// xchgw %es:(%rax,%rax,1), %ax
, 1		// xchgw %ax, %ax
, 1		// xchgl %eax, %eax
, 999		// xchgl %es:(%rax,%rax,1), %eax
, 1		// xchgl %eax, %eax
, 999		// xchgq %es:(%rax,%rax,1), %rax
, 1		// xchgq %rax, %rax
, 1		// xchgq %rax, %rax
, 999		// xchgb %es:(%rax,%rax,1), %al
, 1		// xchgb %al, %al
, 1		// xchgb %ah, %al
, 1		// xchgq %rax, %rax
, 999		// xchgb %es:(%rax,%rax,1), , 
, 1		// xchgb %al, %ah
, 1		// xchgb %ah, %ah
, 999		// xend 
, 999		// xgetbv 
, 999		// xlat %es:(%rax,%rax,1)
, 999		// xlatb 
, 1		// xorb $0x0, %al
, 1		// xorw $0x0, %ax
, 1		// xorl $0x0, %eax
, 999		// xorw $0x0, %es:(%rax,%rax,1)
, 999		// xorw $0x0, %es:(%rax,%rax,1)
, 999		// xorw %ax, %es:(%rax,%rax,1)
, 999		// xorl $0x0, %es:(%rax,%rax,1)
, 999		// xorl $0x0, %es:(%rax,%rax,1)
, 999		// xorl %eax, %es:(%rax,%rax,1)
, 999		// xorq $0x0, %es:(%rax,%rax,1)
, 999		// xorq $0x0, %es:(%rax,%rax,1)
, 999		// xorq %rax, %es:(%rax,%rax,1)
, 999		// xorb $0x0, %es:(%rax,%rax,1)
, 999		// xorb %al, %es:(%rax,%rax,1)
, 999		// xorb  , %es:(%rax,%rax,1)
, 1		// xorw $0x0, %ax
, 1		// xorw $0x0, %ax
, 999		// xorw %es:(%rax,%rax,1), %ax
, 1		// xorw %ax, %ax
, 1		// xorl $0x0, %eax
, 1		// xorl $0x0, %eax
, 999		// xorl %es:(%rax,%rax,1), %eax
, 1		// xorl %eax, %eax
, 1		// xorq $0x0, %rax
, 1		// xorq $0x0, %rax
, 999		// xorq %es:(%rax,%rax,1), %rax
, 1		// xorq %rax, %rax
, 1		// xorb $0x0, %al
, 999		// xorb %es:(%rax,%rax,1), %al
, 1		// xorb %al, %al
, 1		// xorb %ah, %al
, 1		// xorq $0x0, %rax
, 1		// xorb $0x0, %ah
, 999		// xorb %es:(%rax,%rax,1), , 
, 1		// xorb %al, %ah
, 1		// xorb %ah, %ah
, 999		// xorpd %es:(%rax,%rax,1), %xmm0
, 1		// xorpd %xmm0, %xmm0
, 999		// xorps %es:(%rax,%rax,1), %xmm0
, 1		// xorps %xmm0, %xmm0
, 1		// xrelease 
, 999		// xrstor %es:(%rax,%rax,1)
, 999		// xrstor %es:(%rax,%rax,1)
, 999		// xrstor %es:(%rax,%rax,1)
, 999		// xrstor64 %es:(%rax,%rax,1)
, 999		// xrstor64 %es:(%rax,%rax,1)
, 999		// xrstor64 %es:(%rax,%rax,1)
, 999		// xsave %es:(%rax,%rax,1)
, 999		// xsave %es:(%rax,%rax,1)
, 999		// xsave %es:(%rax,%rax,1)
, 999		// xsave64 %es:(%rax,%rax,1)
, 999		// xsave64 %es:(%rax,%rax,1)
, 999		// xsave64 %es:(%rax,%rax,1)
, 999		// xsaveopt %es:(%rax,%rax,1)
, 999		// xsaveopt %es:(%rax,%rax,1)
, 999		// xsaveopt %es:(%rax,%rax,1)
, 999		// xsaveopt64 %es:(%rax,%rax,1)
, 999		// xsaveopt64 %es:(%rax,%rax,1)
, 999		// xsaveopt64 %es:(%rax,%rax,1)
, 999		// xtest 
