#ifndef X64_SRC_CFG_REACHABLE_H
#define X64_SRC_CFG_REACHABLE_H

#include <unordered_set>

#include "src/cfg/cfg.h"

namespace x64 {

class Reachable {
	public:
		Reachable(const Cfg& cfg) : cfg_{cfg} {
			recompute();
		}

		void recompute();

	private:
		const Cfg& cfg_;	

};

} // namesapce x64:

#endif
