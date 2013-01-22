#ifndef X64_SRC_CFG_LIVE_REGISTER_H
#define X64_SRC_CFG_LIVE_REGISTER_H

#include <cassert>
#include <vector>

#include "src/cfg/cfg.h"
#include "src/code/op_set.h"

namespace x64 {

class LiveRegister {
	public:
		LiveRegister(const Cfg& cfg, OpSet boundary)
				: cfg_{cfg}, boundary_{boundary} {
			recompute();
		}

		void recompute();

		inline OpSet live_ins(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			return ins_[id];
		}

		inline OpSet live_ins(const Cfg::location_type& loc) const {
			auto os = live_outs(loc.first);
			// TODO...
			return os;
		}

		inline OpSet live_outs(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			assert(!cfg_.is_exit(id));
			return outs_[id];
		}

		inline OpSet live_outs(const Cfg::location_type& loc) const {
			auto os = live_outs(loc.first);
			// TODO...
			return os;
		}

	private:
		const Cfg& cfg_;
		const OpSet boundary_;

		std::vector<OpSet> ins_;
		std::vector<OpSet> outs_;
};

} // namespace x64

#endif
