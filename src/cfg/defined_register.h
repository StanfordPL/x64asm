#ifndef X64_SRC_CFG_DEFINED_REGISTER_H
#define X64_SRC_CFG_DEFINED_REGISTER_H

#include <cassert>
#include <vector>

#include "src/cfg/cfg.h"
#include "src/code/op_set.h"

namespace x64 {

class DefinedRegister {
	public:
		DefinedRegister(const Cfg& cfg, OpSet boundary)
				: cfg_{cfg}, boundary_{boundary} {
			recompute();
		}

		void recompute();

		inline OpSet def_ins(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			assert(!cfg_.is_entry(id));
			return ins_[id];
		}

		inline OpSet def_ins(const Cfg::location_type& loc) const {
			auto os = def_ins(loc.first);
			// TODO...
			return os;
		}

		inline OpSet def_outs(Cfg::id_type id) const {
			assert(id < cfg_.num_blocks());
			return outs_[id];
		}

		inline OpSet get_def_outs(const Cfg::location_type& loc) const {
			auto os = def_ins(loc.first);
			// TODO...
			return os;
		}

	private:
		const Cfg& cfg_;
		const OpSet boundary_;

};

} // namespace x64

#endif
