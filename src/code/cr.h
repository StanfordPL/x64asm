#ifndef X64_SRC_CODE_CR_H
#define X64_SRC_CODE_CR_H

#include "src/code/operand.h"

namespace x64 {

/** A control register. */
class Cr : public Operand {
	public:
		inline Cr(uint64_t val) : Operand{val} { }
};

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
class Cr0234 : public Cr {
	public:
		inline Cr0234(uint64_t val) : Cr{val} { }
};

/** The control register CR8 */
class Cr8 : public Cr {
	public:
		inline Cr8() : Cr{8} { }
		inline Cr8(uint64_t ignore) : Cr{8} { }
};

} // namespace x64

#endif
