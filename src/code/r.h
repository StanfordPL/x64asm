#ifndef X64_SRC_CODE_R_H
#define X64_SRC_CODE_R_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A general-purpose register. */
class R {
	public:
		inline R() 
				: g_{0} { 
		}
		
		inline R(Operand g) 
				: g_{g} { 
		}

		inline operator Operand() const {
			return g_;
		}

	private:
		Operand g_;
};

/** One of the byte general-purpose registers: AH, CH, DH, BH available when 
	  not using a REX prefix. 
*/
struct NoRexR8 : public virtual R {
	inline NoRexR8() : R{} { /* inlined away */ }
};

/** One of the byte general-purpose registers: AH, CH, DH, BH, AL, CL, DL, BL,
   	available when using a REX prefix. 
*/
struct RexR8 : public virtual R {
	inline RexR8() : R{} { /* inlined away */ }
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
struct Rh : public NoRexR8 {
	inline Rh() : R{4} { }
	inline Rh(Operand o) : R{o} { }
};

/** One of the byte general-purpose registers: AL, CL, DL, BL. */
struct Rl : public NoRexR8, public RexR8 {
	inline Rl() : R{} { }
	inline Rl(Operand o) : R{o} { }
};

/** One of the byte general-purpose registers: BPL, SPL, DIL and SIL; or one of 
	  the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
struct Rb : public RexR8 {
	inline Rb() : R{4} { }
	inline Rb(Operand o) : R{o} { }
};

/** The byte general-purpose register AL. */
struct Al : public Rl {
	inline Al() : Rl{0} { }
};

/** The byte general-purpose register CL. */
struct Cl : public Rl {
	inline Cl() : Rl{1} { }
};

/** One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI; 
	  or one of the word registers (R8W - R15W) available when using REX.R and 
		64-bit mode.
*/
struct R16 : public R {
	inline R16() : R{} { }
	inline R16(Operand o) : R{o} { }
};

/** The word general-purpose register AX. */
struct Ax : public R16 {
	inline Ax() : R16{0} { }
};

/** The word general-purpose register DX. */
struct Dx : public R16 {
	inline Dx() : R16{2} { }
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP, 
	  EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available 
		when using REX.R in 64-bit mode.
*/
struct R32 : public R {
	inline R32() : R{} { }
	inline R32(Operand o) : R{o} { }
};

/** The doubleword general-purpose register EAX. */
struct Eax : public R32 {
	inline Eax() : R32{0} { }
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
	  RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
struct R64 : public R {
	inline R64() : R{} { }
	inline R64(Operand o) : R{o} { }
};

/** The quadword general-purpose register RAX. */
struct Rax : public R64 {
	inline Rax() : R64{0} { }
};

extern const Rh ah;
extern const Rh ch;
extern const Rh dh;
extern const Rh bh;

typedef std::vector<Rh> Rhs;
extern const Rhs rhs;

extern const Al al;
extern const Cl cl;
extern const Rl dl;
extern const Rl bl;

typedef std::vector<Rl> Rls;
extern const Rls rls;

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

typedef std::vector<Rb> Rbs;
extern const Rbs rbs;

typedef std::vector<NoRexR8> NoRexR8s;
extern const NoRexR8s no_rex_r8s;

typedef std::vector<RexR8> RexR8s;
extern const RexR8s rex_r8s;

extern const Ax ax;
extern const R16 cx;
extern const R16 dx;
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

typedef std::vector<R16> R16s;
extern const R16s r16s;

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

typedef std::vector<R32> R32s;
extern const R32s r32s;

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

typedef std::vector<R64> R64s;
extern const R64s r64s;

} // namespace x64

#endif
