#ifndef X64_SRC_CODE_DR_H
#define X64_SRC_CODE_DR_H

#include "src/code/operand.h"

namespace x64 {

/** A debug register. */
class Dr : public Operand {
	friend class Constants;
	private:
		inline Dr(uint64_t val) : Operand{val} { }
	public:
		inline Dr(const Dr& d) = default;
		inline Dr& operator=(const Dr& d) = default;	
};

} // namespace x64

#endif
