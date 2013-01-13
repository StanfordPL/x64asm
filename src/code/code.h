#ifndef X64_SRC_CODE_CODE_H
#define X64_SRC_CODE_CODE_H

#include <vector>

#include "src/code/instruction.h"

namespace x64 {

/** A sequence of Instructions. */
class Code {
	public:
		// Put some constructors here...
		// Put some vector stuff here...

		inline bool check() const {
			for ( const auto& i : code_ )
				if ( !i.check() )
					return false;
			return true;
		}

	private:
		std::vector<Instruction> code_;
};

} // namespace x64

#endif
