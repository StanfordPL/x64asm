#ifndef X64_SRC_TRACE_STATE_H
#define X64_SRC_TRACE_STATE_H

#include "src/code/gp_reg.h"
#include "src/tracer/tracer.h"

#include <array>
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

		inline gp_reg_val_type gp_before(GpReg gp) {
			assert(gp.is_valid() && !gp.is_null());
			return gp_before_[gp];
		}

		inline gp_reg_val_type gp_after(GpReg gp) {
			assert(gp.is_valid() && !gp.is_null());
			return gp_after_[gp];
		}

	private:
		size_t line_;

		std::array<gp_reg_val_type, 16> gp_before_;
		std::array<gp_reg_val_type, 16> gp_after_;
};	

} // namespace x64

#endif
