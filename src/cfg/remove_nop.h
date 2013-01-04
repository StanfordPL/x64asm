#ifndef SRC_CFG_REMOVE_NOP_H
#define SRC_CFG_REMOVE_NOP_H

#include "src/cfg/cfg.h"
#include "src/code/attributes.h"
#include "src/code/code.h"

namespace x64 {

class RemoveNop {
	public:
		static inline void remove(Code& c) {
			remove(Cfg(&c));
		}

		static inline void remove(const Cfg& cfg) {
			Code code;
			for ( const auto& instr : cfg.get_code() )
				if ( !Attributes::is_nop(instr) )
					code.push_back(instr);

			cfg.get_code() = code;
		}
};

} // namespace x64

#endif
