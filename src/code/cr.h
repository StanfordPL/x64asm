#ifndef X64_SRC_CODE_CR_H
#define X64_SRC_CODE_CR_H

#include "src/code/operand.h"

namespace x64 {

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
class Cr0234 : public Operand {
	public:
		inline Cr0234(uint64_t val) : Operand{val} { }

		inline bool check() const {
			return val_ == 0 ||
				     val_ == 2 ||
						 val_ == 3 ||
						 val_ == 4;
		}
};

/** The control register CR8 */
class Cr8 : public Operand {
	public:
		inline Cr8() : Operand{8} { }
		inline Cr8(uint64_t ignore) : Operand{8} { }

		inline bool check() const {
			return val_ == 8;
		}
};

} // namespace x64

#endif
