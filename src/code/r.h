#ifndef X64_SRC_CODE_R_H
#define X64_SRC_CODE_R_H

#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** One of the byte general-purpose registers: AL, CL, DL, BL. */
class Rl : public AtomicOperand {
	friend class Constants;
	protected:
		inline Rl(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** One of the byte general-purpose registers: AH, CH, DH, BH. */
class Rh : public AtomicOperand {
	friend class Constants;
	private:
		inline Rh(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** One of the byte general-purpose registers: BPL, SPL, DIL and SIL; or one of 
	  the byte registers (R8B - R15B) available when using REX.R and 64-bit mode.
*/
class Rb : public AtomicOperand {
	friend class Constants;
	private:
		inline Rb(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The byte general-purpose register AL. */
class Al : public Rl {
	friend class Constants;
	private:
		inline Al() : Rl{0} { }
};

/** The byte general-purpose register CL. */
class Cl : public Rl {
	friend class Constants;
	private:
		inline Cl() : Rl{1} { }
};

/** One of the word general-purpose registers: AX, CX, DX, BX, SP, BP, SI, DI; 
	  or one of the word registers (R8W - R15W) available when using REX.R and 
		64-bit mode.
*/
class R16 : public AtomicOperand {
	friend class Constants;
	protected:
		inline R16(uint64_t val) : AtomicOperand{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The word general-purpose register AX. */
class Ax : public R16 {
	friend class Constants;
	private:
		inline Ax() : R16{0} { }
};

/** The word general-purpose register DX. */
class Dx : public R16 {
	friend class Constants;
	private:
		inline Dx() : R16{2} { }
};

/** One of the double or quadword general-purpose register which may
	  be used to form an address in memory.
*/
class AddrR : public AtomicOperand {
	public:
		AddrR(uint64_t val) : AtomicOperand{val} { }
		virtual ~AddrR() = 0;

		virtual void write_att(std::ostream& os) const = 0;
		virtual void write_intel(std::ostream& os) const = 0;
};

/** One of the doubleword general-purpose registers: EAX, ECX, EDX, EBX, ESP, 
	  EBP, ESI, EDI; or one of the doubleword registers (R8D - R15D) available 
		when using REX.R in 64-bit mode.
*/
class R32 : public AddrR {
	friend class Constants;
	protected:
		inline R32(uint64_t val) : AddrR{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The doubleword general-purpose register EAX. */
class Eax : public R32 {
	friend class Constants;
	private:
		inline Eax() : R32{0} { }
};

/** One of the quadword general-purpose registers: RAX, RBX, RCX, RDX, RDI, RSI,
	  RBP, RSP, R8–R15. These are available when using REX.R and 64-bit mode.
*/
class R64 : public AddrR {
	friend class Constants;
	protected:
		inline R64(uint64_t val) : AddrR{val} { }

	public:
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** The quadword general-purpose register RAX. */
class Rax : public R64 {
	friend class Constants;
	private:
		inline Rax() : R64{0} { }
};

} // namespace x64

#endif
