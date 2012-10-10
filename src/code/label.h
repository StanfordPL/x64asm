#ifndef X64_SRC_CODE_LABEL_H
#define X64_SRC_CODE_LABEL_H

#include "src/code/operand.h"

namespace x64 {

/** A code label, represented as an integer for simplicity.
*/
class Label {
	public:
		inline Label() 
				: l_(0) { 
		}

		inline Label(Operand o) 
				: l_(o) {
		}

		inline operator Operand() const { 
			return l_;
		}

	private:
		Operand l_;
};

} // namespace x64

#endif
