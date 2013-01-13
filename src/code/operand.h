#ifndef X64_SRC_CODE_OPERAND_H
#define X64_SRC_CODE_OPERAND_H

#include <stdint.h>

namespace x64 {

/** Base operand type. */
class Operand {
	public:
		inline Operand(uint64_t val) : val_{val} { }
		virtual inline ~Operand() { }

		virtual inline bool check() const = 0;

		uint64_t val_;	


};

} // namespace x64

#endif
