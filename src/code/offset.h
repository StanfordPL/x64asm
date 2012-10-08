#ifndef X64_SRC_CODE_OFFSET_H
#define X64_SRC_CODE_OFFSET_H

#include <iostream>

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

		inline bool is_null() const {
			return false;
		}

		inline bool is_valid() const {
			return true;
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		inline void write_att(std::ostream& os) {
			os << std::hex << std::showbase << o_;
		}

	private:
		Operand o_;
};

}

#endif
