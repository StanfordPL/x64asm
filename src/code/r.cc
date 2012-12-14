#include "src/code/r.h"

using namespace std;

namespace x64 {

const Rh ah{4};
const Rh ch{5};
const Rh dh{6};
const Rh bh{7};

const Rhs rhs {{
	ah, ch, dh, bh
}};

const Al al;
const Cl cl;
const Rl dl{2};
const Rl bl{3};

const Rls rls {{
	al, cl, dl, bl
}};

const Rb spl{4};
const Rb bpl{5};
const Rb sil{6};
const Rb dil{7};
const Rb r8b{8};
const Rb r9b{9};
const Rb r10b{10};
const Rb r11b{11};
const Rb r12b{12};
const Rb r13b{13};
const Rb r14b{14};
const Rb r15b{15};

const Rbs rbs {{
	spl, bpl, sil,  dil,
	r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b
}};

const NoRexR8s no_rex_r8s {{
	al, cl, dl, bl, ah, ch, dh, bh
}};

const RexR8s rex_r8s {{
	al,  cl,  dl,   bl,   spl,  bpl,  sil,  dil,
	r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b
}};

const Ax ax;
const R16 cx{1};
const R16 dx{2};
const R16 bx{3};
const R16 sp{4};
const R16 bp{5};
const R16 si{6};
const R16 di{7};
const R16 r8w{8};
const R16 r9w{9};
const R16 r10w{10};
const R16 r11w{11};
const R16 r12w{12};
const R16 r13w{13};
const R16 r14w{14};
const R16 r15w{15};

const R16s r16s {{
	ax,  cx,  dx,   bx,   sp,   bp,   si,   di, 
	r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w
}};

const Eax eax;
const R32 ecx{1};
const R32 edx{2};
const R32 ebx{3};
const R32 esp{4};
const R32 ebp{5};
const R32 esi{6};
const R32 edi{7};
const R32 r8d{8};
const R32 r9d{9};
const R32 r10d{10};
const R32 r11d{11};
const R32 r12d{12};
const R32 r13d{13};
const R32 r14d{14};
const R32 r15d{15};

const R32s r32s {{
	eax, ecx, edx,  ebx,  esp,  ebp,  esi,  edi,
	r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d
}};

const Rax rax;
const R64 rcx{1};
const R64 rdx{2};
const R64 rbx{3};
const R64 rsp{4};
const R64 rbp{5};
const R64 rsi{6};
const R64 rdi{7};
const R64 r8{8};
const R64 r9{9};
const R64 r10{10};
const R64 r11{11};
const R64 r12{12};
const R64 r13{13};
const R64 r14{14};
const R64 r15{15};

const R64s r64s {{
	rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
	r8,  r9,  r10, r11, r12, r13, r14, r15
}};

} // namespace x64
