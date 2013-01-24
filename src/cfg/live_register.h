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

	private:
		const Cfg& cfg_;
		const OpSet boundary_;

};

} // namespace x64

#endif
