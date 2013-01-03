#ifndef SRC_CFG_REMOVE_NOP_H
#define SRC_CFG_REMOVE_NOP_H

#include "src/cfg/cfg.h"
#include "src/code/attributes.h"
#include "src/code/code.h"

namespace x64 {

class RemoveNop {
	public:
		static inline Code run(const Cfg& cfg) {
			return run(cfg.get_code());
		}

		static inline Code run(const Code& c) {
			Code code;
			for ( const auto& instr : c )
				if ( !Attributes::is_nop(instr) )
					code.push_back(instr);

			return code;
		}
};

} // namespace x64

#endif
