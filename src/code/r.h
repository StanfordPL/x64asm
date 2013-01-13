#ifndef X64_SRC_CODE_R_H
#define X64_SRC_CODE_R_H

#include "src/code/operand.h"

namespace x64 {

/** One of the byte general-purpose registers: AL, CL, DL, BL. */
class Rl : public Operand {
	public:
		inline Rl(uint64_t val) : Operand{val} { }

		inline bool check() const {
			return val_ < 4;
		}
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
class Rh : public Operand {
	public:
		inline Rh(uint64_t val) : Operand{val} { }

		inline bool check() const {
			return val_ >= 4 && val_ < 8;
		}
};

/** One of the byte general-purpose registers: BPL, SPL, DIL and SIL; or one of 
	  the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
class Rb : public Operand {
	public:
		inline Rb(uint64_t val) : Operand{val} { }

		inline bool check() const {
			return val_ >= 4 && val_ < 16;
		}
};

/** The byte general-purpose register AL. */
class Al : public Rl {
	public:
		inline Al() : Rl{0} { }
		inline Al(uint64_t ignore) : Rl{0} { }

		inline bool check() const {
			return val_ == 0;
		}
};

/** The byte general-purpose register CL. */
class Cl : public Rl {
	public:
		inline Cl() : Rl{1} { }
		inline Cl(uint64_t ignore) : Rl{1} { }

		inline bool check() const {
			return val_ == 1;
		}
};

/** One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI; 
	  or one of the word registers (R8W - R15W) available when using REX.R and 
		64-bit mode.
*/
class R16 : public Operand {
	public:
		inline R16(uint64_t val) : Operand{val} { }

		inline bool check() const {
			return val_ < 16;
		}
};

/** The word general-purpose register AX. */
class Ax : public R16 {
	public:
		inline Ax() : R16{0} { }
		inline Ax(uint64_t ignore) : R16{0} { }

		inline bool check() const {
			return val_ == 0;
		}
};

/** The word general-purpose register DX. */
class Dx : public R16 {
	public:
		inline Dx() : R16{2} { }
		inline Dx(uint64_t ignore) : R16{2} { }

		inline bool check() const {
			return val_ == 2;
		}
};

/** One of the double or quadword general-purpose register which may
	  be used to form an address in memory.
*/
class AddrR : public Operand {
	public:
		AddrR(uint64_t val) : Operand{val} { }

		inline bool check() const {
			return val_ < 16;
		}
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP, 
	  EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available 
		when using REX.R in 64-bit mode.
*/
class R32 : public AddrR {
	public:
		inline R32(uint64_t val) : AddrR{val} { }

		inline bool check() const {
			return val_ < 16;
		}
};

/** The doubleword general-purpose register EAX. */
class Eax : public R32 {
	public:
		inline Eax() : R32{0} { }
		inline Eax(uint64_t ignore) : R32{0} { }

		inline bool check() const {
			return val_ == 0;
		}
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
	  RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
class R64 : public AddrR {
	public:
		inline R64(uint64_t val) : AddrR{val} { }

		inline bool check() const {
			return val_ < 16;
		}
};

/** The quadword general-purpose register RAX. */
class Rax : public R64 {
	public:
		inline Rax() : R64{0} { }
		inline Rax(uint64_t ignore) : R64{0} { }

		inline bool check() const {
			return val_ == 0;
		}
};

} // namespace x64

#endif
