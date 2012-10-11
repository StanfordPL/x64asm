#ifndef X64_SRC_CODE_COND_REG_H
#define X64_SRC_CODE_COND_REG_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A Condition register: af, cf, of, pf, sf, or zf
*/
class CondReg {
	public:
		inline CondReg()
				: c_(6) {
		}

		inline CondReg(Operand c)
				: c_(c) {
		}		

		inline operator Operand() const {
			return c_;
		}

		inline bool is_null() const {
			return c_ >= 6;
		}

		typedef const std::vector<CondReg>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand c_;

		static const std::vector<CondReg> domain_;
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
