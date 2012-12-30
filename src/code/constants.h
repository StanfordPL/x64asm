#ifndef X64_SRC_CODE_CONSTANTS_H
#define X64_SRC_CODE_CONSTANTS_H

#include "src/code/cr.h"
#include "src/code/dr.h"
#include "src/code/eflag.h"
#include "src/code/imm.h"
#include "src/code/mm.h"
#include "src/code/modifier.h"
#include "src/code/r.h"
#include "src/code/sreg.h"
#include "src/code/st.h"
#include "src/code/xmm.h"
#include "src/code/ymm.h"

namespace x64 {

// Condition Registers (cr.h)
extern const Cr0234 cr0;
extern const Cr0234 cr2;
extern const Cr0234 cr3;
extern const Cr0234 cr4;
extern const Cr8 cr8;

// Debug Registers (dr.h)
extern const Dr dr0;
extern const Dr dr1;
extern const Dr dr2;
extern const Dr dr3;
extern const Dr dr4;
extern const Dr dr5;
extern const Dr dr6;
extern const Dr dr7;

// Eflags (eflag.h)
extern const Eflag cf;
extern const Eflag pf;
extern const Eflag af;
extern const Eflag zf;
extern const Eflag sf;
extern const Eflag tf;
extern const Eflag if_;
extern const Eflag df;
extern const Eflag of;
extern const Eflag iopl0;
extern const Eflag iopl1;
extern const Eflag nt;
extern const Eflag rf;
extern const Eflag vm;
extern const Eflag ac;
extern const Eflag vif;
extern const Eflag vip;
extern const Eflag id;

// Immediates (imm.h)
extern const Zero zero;
extern const One one;
extern const Three three;

// MMX Registers (mm.h)
extern const Mm mm0;
extern const Mm mm1;
extern const Mm mm2;
extern const Mm mm3;
extern const Mm mm4;
extern const Mm mm5;
extern const Mm mm6;
extern const Mm mm7;

// Modifiers (modifier.h)
extern const Pref66 pref_66;
extern const PrefRexW pref_rexw;
extern const Far far;

// General Purpose Registers (r.h)
extern const Al al;
extern const Cl cl;
extern const Rl dl;
extern const Rl bl;

extern const Rh ah;
extern const Rh ch;
extern const Rh dh;
extern const Rh bh;

extern const Rb spl;
extern const Rb bpl;
extern const Rb sil;
extern const Rb dil;
extern const Rb r8b;
extern const Rb r9b;
extern const Rb r10b;
extern const Rb r11b;
extern const Rb r12b;
extern const Rb r13b;
extern const Rb r14b;
extern const Rb r15b;

extern const Ax ax;
extern const R16 cx;
extern const Dx dx;
extern const R16 bx;
extern const R16 sp;
extern const R16 bp;
extern const R16 si;
extern const R16 di;
extern const R16 r8w;
extern const R16 r9w;
extern const R16 r10w;
extern const R16 r11w;
extern const R16 r12w;
extern const R16 r13w;
extern const R16 r14w;
extern const R16 r15w;

extern const Eax eax;
extern const R32 ecx;
extern const R32 edx;
extern const R32 ebx;
extern const R32 esp;
extern const R32 ebp;
extern const R32 esi;
extern const R32 edi;
extern const R32 r8d;
extern const R32 r9d;
extern const R32 r10d;
extern const R32 r11d;
extern const R32 r12d;
extern const R32 r13d;
extern const R32 r14d;
extern const R32 r15d;

extern const Rax rax;
extern const R64 rcx;
extern const R64 rdx;
extern const R64 rbx;
extern const R64 rsp;
extern const R64 rbp;
extern const R64 rsi;
extern const R64 rdi;
extern const R64 r8;
extern const R64 r9;
extern const R64 r10;
extern const R64 r11;
extern const R64 r12;
extern const R64 r13;
extern const R64 r14;
extern const R64 r15;

// Segment Registers (sreg.h)
extern const Sreg es;
extern const Sreg cs;
extern const Sreg ss;
extern const Sreg ds;
extern const Fs fs;
extern const Gs gs;

// Floating Point Registers (st.h)
extern const St0 st0;
extern const St st1;
extern const St st2;
extern const St st3;
extern const St st4;
extern const St st5;
extern const St st6;
extern const St st7;

// Xmm Registers (xmm.h)
extern const Xmm0 xmm0;
extern const Xmm xmm1;
extern const Xmm xmm2;
extern const Xmm xmm3;
extern const Xmm xmm4;
extern const Xmm xmm5;
extern const Xmm xmm6;
extern const Xmm xmm7;
extern const Xmm xmm8;
extern const Xmm xmm9;
extern const Xmm xmm10;
extern const Xmm xmm11;
extern const Xmm xmm12;
extern const Xmm xmm13;
extern const Xmm xmm14;
extern const Xmm xmm15;

// Ymm Registers (ymm.h)
extern const Ymm ymm0;
extern const Ymm ymm1;
extern const Ymm ymm2;
extern const Ymm ymm3;
extern const Ymm ymm4;
extern const Ymm ymm5;
extern const Ymm ymm6;
extern const Ymm ymm7;
extern const Ymm ymm8;
extern const Ymm ymm9;
extern const Ymm ymm10;
extern const Ymm ymm11;
extern const Ymm ymm12;
extern const Ymm ymm13;
extern const Ymm ymm14;
extern const Ymm ymm15;

} // namespace x64

#endif
