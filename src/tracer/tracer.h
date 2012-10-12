#ifndef X64_SRC_TRACER_TRACE_H
#define X64_SRC_TRACER_TRACE_H

#include <cassert>
#include <set>

#include "src/assembler/assembler.h"
#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/gp_reg.h"
#include "src/code/xmm_reg.h"
#include "src/tracer/trace.h"

namespace x64 {

class Trace;

class Tracer {
	public:
		Tracer() 
				: conds_(false) {
		}

		inline void set(GpReg gp) {
			assert(!gp.is_null());
			gps_.insert(gp);
		}

		inline void set(CondReg cond) {
			assert(!cond.is_null());
			conds_ = true;
		}

		inline void set(XmmReg xmm) {
			assert(!xmm.is_null());
			xmms_.insert(xmm);
		}

		inline void set_before(size_t line) {
			befores_.insert(line);
		}

		inline void set_after(size_t line) {
			afters_.insert(line);
		}

		inline Function trace(Trace& t, const Code& code) {
			Function fxn;
			trace(fxn, t, code);
			return fxn;
		}

		Function& trace(Function& fxn, Trace& trace, const Code& code);

	private:
		Assembler assm_;

		std::set<size_t> befores_;
		std::set<size_t> afters_;

		std::set<GpReg> gps_;
		bool conds_;
		std::set<XmmReg> xmms_;

		template <bool is_before>
		void trace(Trace& t);
		void finish_state(Trace& t, size_t line);
};

} // namespace x64

#endif
