#ifndef X64_SRC_CODE_CR_H
#define X64_SRC_CODE_CR_H

#include "src/code/operand.h"

namespace x64 {

/** A control register. */
class Cr : public Operand {
	protected:
		inline Cr(uint64_t val) : Operand{val} { }
};

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
class Cr0234 : public Cr {
	friend class Constants;
	private:
		inline Cr0234(uint64_t val) : Cr{val} { }
	public:	
		inline Cr0234(const Cr0234& c) = default;
		inline Cr0234& operator=(const Cr0234& c) = default;
};

/** The control register CR8 */
class Cr8 : public Cr {
	friend class Constants;
	private:
		inline Cr8() : Cr{8} { }
	public:	
		inline Cr8(const Cr8& c) = default;
		inline Cr8& operator=(const Cr8& c) = default;
};

} // namespace x64

#endif
