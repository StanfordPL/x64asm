#ifndef SRC_CFG_REMOVE_UNREACHABLE_H
#define SRC_CFG_REMOVE_UNREACHABLE_H

#include "src/cfg/cfg.h"
#include "src/code/code.h"

namespace x64 {

class RemoveUnreachable {
	public:
		static inline Code run(const Cfg& cfg) {
			return run(cfg.get_code());
		}

		static inline Code run(const Code& c) {
			// TODO...
			return c;
		}
};

} // namespace x64

#endif
