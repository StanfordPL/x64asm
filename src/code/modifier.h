#ifndef X64_SRC_CODE_MODIFIER_H
#define X64_SRC_CODE_MODIFIER_H

#include "src/code/operand.h"

namespace x64 {

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Operand {
	public:
		inline Pref66() : Operand{0} { }
		inline Pref66(uint64_t ignore) : Operand{0} { }

		inline virtual bool check() const {
			return val_ == 0;
		}
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Operand {
	public:
		inline PrefRexW() : Operand{0} { }
		inline PrefRexW(uint64_t ignore) : Operand{0} { }

		inline virtual bool check() const {
			return val_ == 0;
		}
};

/** Far instruction variant. */
class Far : public Operand {
	public:
		inline Far() : Operand{0} { }
		inline Far(uint64_t ignore) : Operand{0} { }

		inline virtual bool check() const {
			return val_ == 0;
		}
};

} // namespace x64

#endif
