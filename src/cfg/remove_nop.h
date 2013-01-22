#ifndef SRC_CFG_REMOVE_NOP_H
#define SRC_CFG_REMOVE_NOP_H

#include "src/cfg/cfg.h"
#include "src/code/code.h"

namespace x64 {

class RemoveNop {
	public:
		static inline void remove(Code& c) {
			remove(c, Cfg(c));
		}

		static inline void remove(Code& c, const Cfg& cfg) {
			Code code;
			for ( const auto& instr : cfg.get_code() )
				if ( !instr.is_nop() )
					code.push_back(instr);
			c = code;
		}
};

} // namespace x64

#endif
