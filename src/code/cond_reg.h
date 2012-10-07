#ifndef X64_SRC_CODE_COND_REG_H
#define X64_SRC_CODE_COND_REG_H

#include <cassert>
#include <iostream>

#include "src/code/operand.h"

namespace x64 {

/** A Condition register: AF, CF, OF, PF, SF, or ZF
*/
class CondReg {
	public:
		inline CondReg()
				: c_(6) {
		}

		inline CondReg(Operand c)
				: c_(c) {
			assert(is_valid());
		}		

		inline operator Operand() const {
			return c_;
		}

		inline bool is_null() const {
			return c_ == 6;
		}

		inline bool is_valid() const {
			return c_ <= 6;
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		inline void write_att(std::ostream& os) const {
			os.setstate(std::ios::failbit);
		}

	private:
		Operand c_;
};

extern const CondReg af;
extern const CondReg cf;
extern const CondReg of;
extern const CondReg pf;
extern const CondReg sf;
extern const CondReg zf;
extern const CondReg cond_null;

} // namespace x64

#endif
