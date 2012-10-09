#ifndef X64_SRC_TRACE_STATE_H
#define X64_SRC_TRACE_STATE_H

#include "src/code/gp_reg.h"

#include <cassert>
#include <stdint.h>

namespace x64 {

class State {
	friend class Tracer;

	public:
		typedef uint64_t gp_reg_val_type;

		inline size_t line() const {
			return line_;
		}

		inline gp_reg_val_type gp_before(GpReg gp) const {
			assert(gp.is_valid() && !gp.is_null());
			return gp_before_[gp];
		}

		inline gp_reg_val_type gp_after(GpReg gp) const {
			assert(gp.is_valid() && !gp.is_null());
			return gp_after_[gp];
		}

	private:
		size_t line_;

		gp_reg_val_type gp_before_[16];
		gp_reg_val_type gp_after_[16];
};	

} // namespace x64

#endif
