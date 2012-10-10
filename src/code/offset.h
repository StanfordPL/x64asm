#ifndef X64_SRC_CODE_OFFSET_H
#define X64_SRC_CODE_OFFSET_H

#include "src/code/operand.h"

namespace x64 {

/** An absolute memory offset, used by some variants of MOV.
*/
class Offset {
	public:
		inline Offset()
				: o_(0) {
		}

		inline Offset(Operand o)
				: o_(o) {
		}

		inline operator Operand() const {
			return o_;
		}

	private:
		Operand o_;
};

}

#endif
