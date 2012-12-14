#ifndef X64_SRC_CODE_LABEL_H
#define X64_SRC_CODE_LABEL_H

#include "src/code/operand.h"

namespace x64 {

/** A symbolic representation of a Rel.
*/
class Label {
	public:
		inline Label() 
				: l_{0} { 
		}

		inline Label(Operand o) 
				: l_{o} {
		}

		inline operator Operand() const { 
			return l_;
		}

	private:
		Operand l_;
};

/** A symbolic representation of a Rel8. */
struct Label8 : public Label {
	inline Label8() : Label{} { }
	inline Label8(Operand o) : Label{o} { }
};

/** A symbolic representation of a Rel32. */
struct Label32 : public Label {
	inline Label32() : Label{} { }
	inline Label32(Operand o) : Label{o} { }
};

} // namespace x64

#endif
