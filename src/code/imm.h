#ifndef X64_SRC_CODE_IMM_H
#define X64_SRC_CODE_IMM_H

#include "src/code/operand.h"

namespace x64 {

/** A 64-bit immediate.
*/
class Imm {
	public:
		inline Imm() 
				: i_(0) { 
		}

		inline Imm(Operand o) 
				: i_(o) {
		}
		
		inline operator Operand() const { 
			return i_;
		}

		inline bool is_null() const {
			return false;
		}

    friend std::ostream& operator<<(std::ostream& os, const Imm i) {
      os << "[I " << i.i_ << "]";
      return os;
    }

	private:
		Operand i_;
};

class Imm8 : public Imm {
	public:
		inline Imm8() : Imm() { }
		inline Imm8(Operand o) : Imm(o) { }
};

class Imm16 : public Imm {
	public:
		inline Imm16() : Imm() { }
		inline Imm16(Operand o) : Imm(o) { }
};

class Imm32 : public Imm {
	public:
		inline Imm32() : Imm() { }
		inline Imm32(Operand o) : Imm(o) { }
};

class Imm64 : public Imm {
	public:
		inline Imm64() : Imm() { }
		inline Imm64(Operand o) : Imm(o) { }

		template <typename T>
		inline Imm64(T* t) : Imm((Operand) t) { }
};

} // namespace x64

#endif
