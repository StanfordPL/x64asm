#ifndef X64_SRC_CFG_DOMINATORS_H
#define X64_SRC_CFG_DOMINATORS_H

#include <cassert>
#include <unordered_set>

#include "src/cfg/cfg.h"

namespace x64 {

class Dominators {
	public:
		Dominators(const Cfg& cfg) : cfg_{cfg} { 
			recompute();
		}

	private:
		const Cfg& cfg_;	

};

} // namespace x64

#endif
