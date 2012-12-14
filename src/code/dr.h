#ifndef X64_SRC_CODE_DR_H
#define X64_SRC_CODE_DR_H

#include <vector>

#include "src/code/operand.h"

namespace x64 {

/** A debug register. */
class Dr {
	public:
		inline Dr() 
				: d_{0} {
		}

		inline Dr(Operand o)
				: d_{o} {
		}

		inline operator Operand() const {
			return d_;
		}

	private:
		Operand d_;
};

extern const Dr dr0;
extern const Dr dr1;
extern const Dr dr2;
extern const Dr dr3;
extern const Dr dr4;
extern const Dr dr5;
extern const Dr dr6;
extern const Dr dr7;

typedef std::vector<Dr> Drs;

extern const Drs drs;

} // namespace x64

#endif
