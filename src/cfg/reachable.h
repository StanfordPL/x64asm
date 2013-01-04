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

		typedef std::unordered_set<Cfg::id_type>::const_iterator block_iterator;

		inline block_iterator begin() const {
			return reachable_.begin();
		}

		inline block_iterator end() const {
			return reachable_.end();
		}

		inline bool contains(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			assert(!cfg_.is_entry(id));
			assert(!cfg_.is_exit(id));
			return reachable_.find(id) != reachable_.end();
		}

	private:
		const Cfg& cfg_;	

		// Does not include ENTRY or EXIT
		std::unordered_set<Cfg::id_type> reachable_;
};

} // namesapce x64:

#endif
