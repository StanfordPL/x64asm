#ifndef X64_SRC_TRACE_STATE_H
#define X64_SRC_TRACE_STATE_H

#include "src/code/cond_reg.h"
#include "src/code/r.h"
#include "src/code/xmm.h"

#include <cassert>
#include <stdint.h>

namespace x64 {

class State {
	friend class Tracer;

	public:
		typedef uint64_t mem_addr_type;
		typedef size_t   mem_size_type;
		typedef uint8_t  mem_val_type;

		typedef uint64_t r_val_type;
		typedef bool cond_reg_val_type;
		typedef __uint128_t xmm_val_type;

		State() 
				: addr_(0), size_(0) {
		}

		inline size_t line() const {
			return line_;
		}

		inline r_val_type r_before(R64 r) const {
			assert(!r.is_null());
			return r_before_[r];
		}

		inline r_val_type r_after(R64 r) const {
			assert(!r.is_null());
			return r_after_[r];
		}

		inline cond_reg_val_type cond_before(CondReg cond) const {
			switch ( cond ) {
				case 0: return (cond_before_ >> 4)  & 0x1;
				case 1: return (cond_before_ >> 0)  & 0x1;
				case 2: return (cond_before_ >> 11) & 0x1;
				case 3: return (cond_before_ >> 2)  & 0x1;
				case 4: return (cond_before_ >> 7)  & 0x1;
				case 5: return (cond_before_ >> 6)  & 0x1;
				default:
					return false;
			}
		}

		inline cond_reg_val_type cond_after(CondReg cond) const {
			switch ( cond ) {
				case 0: return (cond_after_ >> 4)  & 0x1;
				case 1: return (cond_after_ >> 0)  & 0x1;
				case 2: return (cond_after_ >> 11) & 0x1;
				case 3: return (cond_after_ >> 2)  & 0x1;
				case 4: return (cond_after_ >> 7)  & 0x1;
				case 5: return (cond_after_ >> 6)  & 0x1;
				default:
					return false;
			}
		}

		inline xmm_val_type xmm_before(Xmm xmm) const {
			assert(!xmm.is_null());
			return xmm_before_[xmm];
		}

		inline xmm_val_type xmm_after(Xmm xmm) const {
			assert(!xmm.is_null());
			return xmm_after_[xmm];
		}

		inline bool mem_access() const {
			return size_ > 0;
		}

		inline mem_addr_type mem_addr() const {
			return addr_;
		}

		inline mem_size_type mem_size() const {
			return size_;
		}

		inline mem_val_type mem_before(size_t index) const {
			assert(index < mem_size());
			return mem_before_[index];
		}

		inline mem_val_type mem_after(size_t index) const {
			assert(index < mem_size());
			return mem_after_[index];
		}
			
	private:
		size_t line_;

		r_val_type r_before_[16];
		r_val_type r_after_[16];

		uint64_t cond_before_;
		uint64_t cond_after_;

		xmm_val_type xmm_before_[16];
		xmm_val_type xmm_after_[16];

		mem_addr_type addr_;
		mem_size_type size_;
		mem_val_type mem_before_[32];
		mem_val_type mem_after_[32];
};	

} // namespace x64

#endif
