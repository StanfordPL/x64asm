#ifndef SRC_CFG_REMOVE_UNREACHABLE_H
#define SRC_CFG_REMOVE_UNREACHABLE_H

#include "src/cfg/cfg.h"
#include "src/cfg/reachable.h"
#include "src/code/code.h"

namespace x64 {

class RemoveUnreachable {
	public:
		static inline void remove(Code& c) {
			Cfg cfg(&c);
			Reachable reachable(cfg);
			remove(cfg, reachable);
		}

		static inline void remove(const Cfg& cfg, const Reachable& reachable) {
			Code code;
			for ( auto r = reachable.begin(), re = reachable.end(); r != re; ++r )
				for ( auto i = cfg.instr_begin(*r), ie = cfg.instr_end(*r); i != ie; ++i )
					code.push_back(*i);

			cfg.get_code() = code;
		}
};

} // namespace x64

#endif
