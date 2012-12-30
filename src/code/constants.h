#ifndef X64_SRC_OPERANDS_CONSTANTS_H
#define X64_SRC_OPERANDS_CONSTANTS_H

#include "src/operands/cr.h"
#include "src/operands/dr.h"
#include "src/operands/eflag.h"
#include "src/operands/imm.h"
#include "src/operands/mm.h"
#include "src/operands/modifier.h"
#include "src/operands/r.h"
#include "src/operands/sreg.h"
#include "src/operands/st.h"
#include "src/operands/xmm.h"
#include "src/operands/ymm.h"

namespace x64 {

class Constants {
	public:
		// Condition Registers (cr.h)
		static inline Cr0234 cr0() { return Cr0234{0}; }
		static inline Cr0234 cr2() { return Cr0234{2}; }
		static inline Cr0234 cr3() { return Cr0234{3}; }
		static inline Cr0234 cr4() { return Cr0234{4}; }
		static inline Cr8 cr8() { return Cr8{}; }

		// Debug Registers (dr.h)
		static inline Dr dr0() { return Dr{0}; }
		static inline Dr dr1() { return Dr{1}; }
		static inline Dr dr2() { return Dr{2}; }
		static inline Dr dr3() { return Dr{3}; }
		static inline Dr dr4() { return Dr{4}; }
		static inline Dr dr5() { return Dr{5}; }
		static inline Dr dr6() { return Dr{6}; }
		static inline Dr dr7() { return Dr{7}; }

		// EFlags (eflag.h)
		static inline Eflag cf() { return Eflag{0}; }
		static inline Eflag pf() { return Eflag{2}; }
		static inline Eflag af() { return Eflag{4}; }
		static inline Eflag zf() { return Eflag{6}; }
		static inline Eflag sf() { return Eflag{7}; }
		static inline Eflag tf() { return Eflag{8}; }
		static inline Eflag if_() { return Eflag{9}; }
		static inline Eflag df() { return Eflag{10}; }
		static inline Eflag of() { return Eflag{11}; }
		static inline Eflag iopl0() { return Eflag{12}; }
		static inline Eflag iopl1() { return Eflag{13}; }
		static inline Eflag nt() { return Eflag{14}; }
		static inline Eflag rf() { return Eflag{16}; }
		static inline Eflag vm() { return Eflag{17}; }
		static inline Eflag ac() { return Eflag{18}; }
		static inline Eflag vif() { return Eflag{19}; }
		static inline Eflag vip() { return Eflag{20}; }
		static inline Eflag id() { return Eflag{21}; }

		// Immediates (imm.h)
		static inline Zero zero() { return Zero(); }
		static inline One one() { return One(); }
		static inline Three three() { return Three(); }

		// MMX Registers (mm.h)
		static inline Mm mm0() { return Mm{0}; }
		static inline Mm mm1() { return Mm{1}; }
		static inline Mm mm2() { return Mm{2}; }
		static inline Mm mm3() { return Mm{3}; }
		static inline Mm mm4() { return Mm{4}; }
		static inline Mm mm5() { return Mm{5}; }
		static inline Mm mm6() { return Mm{6}; }
		static inline Mm mm7() { return Mm{7}; }

		// Modifiers (modifier.h)
		static inline Pref66 pref_66() { return Pref66(); }
		static inline PrefRexW pref_rex_w() { return PrefRexW(); }
		static inline Far far() { return Far(); }

		// General Purpose Registers (r.h)
		static inline Al al() { return Al{}; }
		static inline Cl cl() { return Cl{}; }
		static inline Rl dl() { return Rl{2}; }
		static inline Rl bl() { return Rl{3}; }

		static inline Rh ah() { return Rh{4}; }
		static inline Rh ch() { return Rh{5}; }
		static inline Rh dh() { return Rh{6}; }
		static inline Rh bh() { return Rh{7}; }

		static inline Rb spl() { return Rb{4}; }
		static inline Rb bpl() { return Rb{5}; }
		static inline Rb sil() { return Rb{6}; }
		static inline Rb dil() { return Rb{7}; }
		static inline Rb r8b() { return Rb{8}; }
		static inline Rb r9b() { return Rb{9}; }
		static inline Rb r10b() { return Rb{10}; }
		static inline Rb r11b() { return Rb{11}; }
		static inline Rb r12b() { return Rb{12}; }
		static inline Rb r13b() { return Rb{13}; }
		static inline Rb r14b() { return Rb{14}; }
		static inline Rb r15b() { return Rb{15}; }

		static inline Ax ax() { return Ax{}; }
		static inline R16 cx() { return R16{1}; }
		static inline Dx dx() { return Dx{}; }
		static inline R16 bx() { return R16{3}; }
		static inline R16 sp() { return R16{4}; }
		static inline R16 bp() { return R16{5}; }
		static inline R16 si() { return R16{6}; }
		static inline R16 di() { return R16{7}; }
		static inline R16 r8w() { return R16{8}; }
		static inline R16 r9w() { return R16{9}; }
		static inline R16 r10w() { return R16{10}; }
		static inline R16 r11w() { return R16{11}; }
		static inline R16 r12w() { return R16{12}; }
		static inline R16 r13w() { return R16{13}; }
		static inline R16 r14w() { return R16{14}; }
		static inline R16 r15w() { return R16{15}; }

		static inline Eax eax() { return Eax{}; }
		static inline R32 ecx() { return R32{1}; }
		static inline R32 edx() { return R32{2}; }
		static inline R32 ebx() { return R32{3}; }
		static inline R32 esp() { return R32{4}; }
		static inline R32 ebp() { return R32{5}; }
		static inline R32 esi() { return R32{6}; }
		static inline R32 edi() { return R32{7}; }
		static inline R32 r8d() { return R32{8}; }
		static inline R32 r9d() { return R32{9}; }
		static inline R32 r10d() { return R32{10}; }
		static inline R32 r11d() { return R32{11}; }
		static inline R32 r12d() { return R32{12}; }
		static inline R32 r13d() { return R32{13}; }
		static inline R32 r14d() { return R32{14}; }
		static inline R32 r15d() { return R32{15}; }

		static inline Rax rax() { return Rax{}; }
		static inline R64 rcx() { return R64{1}; }
		static inline R64 rdx() { return R64{2}; }
		static inline R64 rbx() { return R64{3}; }
		static inline R64 rsp() { return R64{4}; }
		static inline R64 rbp() { return R64{5}; }
		static inline R64 rsi() { return R64{6}; }
		static inline R64 rdi() { return R64{7}; }
		static inline R64 r8() { return R64{8}; }
		static inline R64 r9() { return R64{9}; }
		static inline R64 r10() { return R64{10}; }
		static inline R64 r11() { return R64{11}; }
		static inline R64 r12() { return R64{12}; }
		static inline R64 r13() { return R64{13}; }
		static inline R64 r14() { return R64{14}; }
		static inline R64 r15() { return R64{15}; }

		// Segment Registers (sreg.h)
		static inline Sreg es() { return Sreg{0}; }
		static inline Sreg cs() { return Sreg{1}; }
		static inline Sreg ss() { return Sreg{2}; }
		static inline Sreg ds() { return Sreg{3}; }
		static inline Fs fs() { return Fs{}; }
		static inline Gs gs() { return Gs{}; }

		// Floating Point Registers (st.h)
		static inline St0 st0() { return St0{}; }
		static inline St st1() { return St{1}; }
		static inline St st2() { return St{2}; }
		static inline St st3() { return St{3}; }
		static inline St st4() { return St{4}; }
		static inline St st5() { return St{5}; }
		static inline St st6() { return St{6}; }
		static inline St st7() { return St{7}; }

		// XMM Registers (xmm.h)
		static inline Xmm0 xmm0() { return Xmm0{}; }
		static inline Xmm xmm1() { return Xmm{1}; }
		static inline Xmm xmm2() { return Xmm{2}; }
		static inline Xmm xmm3() { return Xmm{3}; }
		static inline Xmm xmm4() { return Xmm{4}; }
		static inline Xmm xmm5() { return Xmm{5}; }
		static inline Xmm xmm6() { return Xmm{6}; }
		static inline Xmm xmm7() { return Xmm{7}; }
		static inline Xmm xmm8() { return Xmm{8}; }
		static inline Xmm xmm9() { return Xmm{9}; }
		static inline Xmm xmm10() { return Xmm{10}; }
		static inline Xmm xmm11() { return Xmm{11}; }
		static inline Xmm xmm12() { return Xmm{12}; }
		static inline Xmm xmm13() { return Xmm{13}; }
		static inline Xmm xmm14() { return Xmm{14}; }
		static inline Xmm xmm15() { return Xmm{15}; }

		// YMM Registers (ymm.h)
		static inline Ymm ymm0() { return Ymm{0}; }
		static inline Ymm ymm1() { return Ymm{1}; }
		static inline Ymm ymm2() { return Ymm{2}; }
		static inline Ymm ymm3() { return Ymm{3}; }
		static inline Ymm ymm4() { return Ymm{4}; }
		static inline Ymm ymm5() { return Ymm{5}; }
		static inline Ymm ymm6() { return Ymm{6}; }
		static inline Ymm ymm7() { return Ymm{7}; }
		static inline Ymm ymm8() { return Ymm{8}; }
		static inline Ymm ymm9() { return Ymm{9}; }
		static inline Ymm ymm10() { return Ymm{10}; }
		static inline Ymm ymm11() { return Ymm{11}; }
		static inline Ymm ymm12() { return Ymm{12}; }
		static inline Ymm ymm13() { return Ymm{13}; }
		static inline Ymm ymm14() { return Ymm{14}; }
		static inline Ymm ymm15() { return Ymm{15}; }
};

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
