#ifndef X64_SRC_CFG_LOOPS_H
#define X64_SRC_CFG_LOOPS_H

#include <cassert>
#include <map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "src/cfg/cfg.h"
#include "src/cfg/dominators.h"
#include "src/cfg/reachable.h"

namespace x64 {

class Loops {
	public:
		Loops(const Cfg& cfg, const Dominators& doms, const Reachable& reachable) 
				: cfg_{cfg}, doms_{doms}, reachable_{reachable} {
			recompute();
		}

		void recompute();

	private:
		const Cfg& cfg_;
		const Dominators& doms_;
		const Reachable& reachable_;
		
};

} // namespace x64

#endif
