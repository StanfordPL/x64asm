#ifndef X64_SRC_CODE_CR_H
#define X64_SRC_CODE_CR_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A control register. */
class Cr {
	public:
		inline Cr()
				: c_{0} {
		}

		inline Cr(Operand o)
				: c_{o} {
		}

		inline operator Operand() const {
			return c_;
		}

	private:
		Operand c_;	
};

/** One of the control reigsters: CR0, CR2, CR3, CR4. */
struct Cr0234 : public Cr {
	inline Cr0234() : Cr{} { }
	inline Cr0234(Operand o) : Cr{o} { }
};

/** The control register CR8 */
struct Cr8 : public Cr {
	inline Cr8() : Cr{8} { }
};

extern const Cr0234 cr0;
extern const Cr0234 cr2;
extern const Cr0234 cr3;
extern const Cr0234 cr4;
extern const Cr8 cr8;

typedef std::vector<Cr> Crs;
extern const Crs crs;

} // namespace x64

#endif
