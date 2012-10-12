#ifndef X64_SRC_TRACE_STATE_H
#define X64_SRC_TRACE_STATE_H

#include "src/code/cond_reg.h"
#include "src/code/gp_reg.h"
#include "src/code/xmm_reg.h"

#include <cassert>
#include <stdint.h>

namespace x64 {

class State {
	friend class Tracer;

	public:
		typedef uint64_t gp_reg_val_type;
		typedef bool cond_reg_val_type;
		typedef __uint128_t xmm_reg_val_type;

		inline size_t line() const {
			return line_;
		}

		inline gp_reg_val_type gp_before(GpReg gp) const {
			assert(!gp.is_null());
			return gp_before_[gp];
		}

		inline gp_reg_val_type gp_after(GpReg gp) const {
			assert(!gp.is_null());
			return gp_after_[gp];
		}

		inline cond_reg_val_type cond_before(CondReg cond) const {
			switch ( cond ) {
				case 0: return (cond_before_ >> (8+4))  & 0x1;
				case 1: return (cond_before_ >> (8+0))  & 0x1;
				case 2: return (cond_before_ >> (8+11)) & 0x1;
				case 3: return (cond_before_ >> (8+2))  & 0x1;
				case 4: return (cond_before_ >> (8+7))  & 0x1;
				case 5: return (cond_before_ >> (8+6))  & 0x1;
				default:
					return false;
			}
		}

		inline cond_reg_val_type cond_after(CondReg cond) const {
			switch ( cond ) {
				case 0: return (cond_after_ >> (8+4))  & 0x1;
				case 1: return (cond_after_ >> (8+0))  & 0x1;
				case 2: return (cond_after_ >> (8+11)) & 0x1;
				case 3: return (cond_after_ >> (8+2))  & 0x1;
				case 4: return (cond_after_ >> (8+7))  & 0x1;
				case 5: return (cond_after_ >> (8+6))  & 0x1;
				default:
					return false;
			}
		}

		inline xmm_reg_val_type xmm_before(XmmReg xmm) const {
			assert(!xmm.is_null());
			return xmm_before_[xmm];
		}

		inline xmm_reg_val_type xmm_after(XmmReg xmm) const {
			assert(!xmm.is_null());
			return xmm_after_[xmm];
		}

	private:
		size_t line_;

		gp_reg_val_type gp_before_[16];
		gp_reg_val_type gp_after_[16];

		uint64_t cond_before_;
		uint64_t cond_after_;

		xmm_reg_val_type xmm_before_[16];
		xmm_reg_val_type xmm_after_[16];
};	

} // namespace x64

#endif
