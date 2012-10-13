#include "src/code/gp_reg.h"

using namespace std;

namespace x64 {

const vector<R64> R64::domain_ {{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
}};

const vector<R32> R32::domain_ {{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
}};

const vector<R16> R16::domain_ {{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
}};

const vector<R8> R8::domain_ {{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
}};

const vector<RH> RH::domain_ {{
	0, 1, 2, 3
}};

const Rax rax = 0;
const R64 rcx = 1;
const R64 rdx = 2;
const R64 rbx = 3;
const R64 rsp = 4;
const R64 rbp = 5;
const R64 rsi = 6;
const R64 rdi = 7;
const R64 r8  = 8;
const R64 r9  = 9;
const R64 r10 = 10;
const R64 r11 = 11;
const R64 r12 = 12;
const R64 r13 = 13;
const R64 r14 = 14;
const R64 r15 = 15;

const Eax eax = 0;
const R32 ecx = 1;
const R32 edx = 2;
const R32 ebx = 3;
const R32 esp = 4;
const R32 ebp = 5;
const R32 esi = 6;
const R32 edi = 7;
const R32 r8d  = 8;
const R32 r9d  = 9;
const R32 r10d = 10;
const R32 r11d = 11;
const R32 r12d = 12;
const R32 r13d = 13;
const R32 r14d = 14;
const R32 r15d = 15;

const Ax ax = 0;
const R16 cx = 1;
const R16 dx = 2;
const R16 bx = 3;
const R16 sp = 4;
const R16 bp = 5;
const R16 si = 6;
const R16 di = 7;
const R16 r8w  = 8;
const R16 r9w  = 9;
const R16 r10w = 10;
const R16 r11w = 11;
const R16 r12w = 12;
const R16 r13w = 13;
const R16 r14w = 14;
const R16 r15w = 15;

const Al al = 0;
const Cl cl = 1;
const R8 dl = 2;
const R8 bl = 3;
const R8 spl = 4;
const R8 bpl = 5;
const R8 sil = 6;
const R8 dil = 7;
const R8 r8b  = 8;
const R8 r9b  = 9;
const R8 r10b = 10;
const R8 r11b = 11;
const R8 r12b = 12;
const R8 r13b = 13;
const R8 r14b = 14;
const R8 r15b = 15;

const R8 ah = 0;
const R8 ch = 1;
const R8 dh = 2;
const R8 bh = 3;

const R r_null = 16;

} // namespace x64
