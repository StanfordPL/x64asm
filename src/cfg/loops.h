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
		typedef std::pair<Cfg::id_type, Cfg::id_type> edge_type;
		typedef std::unordered_set<Cfg::id_type> loop_type;

		Loops(const Cfg& cfg, const Dominators& doms, const Reachable& reachable) 
				: cfg_{cfg}, doms_{doms}, reachable_{reachable} {
			recompute();
		}

		void recompute();

		typedef std::vector<edge_type>::const_iterator back_edge_iterator;

		inline back_edge_iterator back_edge_begin() const {
			return back_edges_.begin();
		}

		inline back_edge_iterator back_edge_end() const {
			return back_edges_.end();
		}

		typedef loop_type::const_iterator loop_iterator;

		inline loop_iterator loop_begin(const edge_type& be) const {
			const auto itr = loops_.find(be);
			assert(itr != loops_.end());
			return itr->second.begin();
		}

		inline loop_iterator loop_end(const edge_type& be) const {
			const auto itr = loops_.find(be);
			assert(itr != loops_.end());
			return itr->second.end();
		}

		inline size_t nesting_depth(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			return nesting_depth_[id];
		}

	private:
		const Cfg& cfg_;
		const Dominators& doms_;
		const Reachable& reachable_;
		
		std::vector<edge_type> back_edges_;
		std::map<edge_type, loop_type> loops_;
		std::vector<size_t> nesting_depth_;

};

} // namespace x64

#endif
