#ifndef X64_SRC_CODE_MODIFIER_H
#define X64_SRC_CODE_MODIFIER_H

#include "src/code/operand.h"

namespace x64 {

/** An instruction modifier. Used to disambiguate otherwise identical
	  instruction signatures.
*/
class Modifier : public Operand {
	protected:
		inline Modifier(uint64_t val) : Operand{val} { }
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
	friend class Constants;
	private:
		Pref66() : Modifier{0} { }
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
	friend class Constants;
	private:
		PrefRexW() : Modifier{0} { }
};

/** Far instruction variant. */
class Far : public Modifier {
	friend class Constants;
	private:
		Far() : Modifier{0} { }
};

} // namespace x64

#endif
