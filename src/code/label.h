#ifndef X64_SRC_CODE_LABEL_H
#define X64_SRC_CODE_LABEL_H

#include <iostream>

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

		inline bool is_null() const {
			return false;
		}

		inline bool is_valid() const {
			return true;
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		inline void write_att(std::ostream& os) const {
			os << std::dec << ".L" << l_;
		}

	private:
		Operand l_;
};

} // namespace x64

#endif
