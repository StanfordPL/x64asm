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

    void recompute();
		
		typedef std::unordered_set<Cfg::id_type>::const_iterator const_iterator;

		inline const_iterator in_begin(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
		 	assert(!cfg_.is_entry(id));
			return ins_[id].begin();
		}

		inline const_iterator in_end(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
		 	assert(!cfg_.is_entry(id));
			return ins_[id].end();
		}

		inline const_iterator out_begin(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			return outs_[id].begin();
		}

		inline const_iterator out_end(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			return outs_[id].end();
		}

		inline bool dom(Cfg::id_type x, Cfg::id_type y) const {
			assert(x < cfg_.num_blocks());
			assert(y < cfg_.num_blocks());
			const auto& dominators = outs_[y];
			return dominators.find(x) != dominators.end();
		}

	private:
		const Cfg& cfg_;	

		std::vector<std::unordered_set<Cfg::id_type>> ins_;
		std::vector<std::unordered_set<Cfg::id_type>> outs_;
};

} // namespace x64

#endif
