#ifndef X64_SRC_TRACER_TRACE_H
#define X64_SRC_TRACER_TRACE_H

#include <cassert>
#include <set>

#include "src/assembler/assembler.h"
#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/r.h"
#include "src/code/xmm.h"
#include "src/tracer/trace.h"

namespace x64 {

class Trace;

class Tracer {
	public:
		Tracer() 
				: conds_(false) {
		}

		inline void set(R64 r) {
			assert(!r.is_null());
			r64_.insert(r);
		}

		inline void set(CondReg cond) {
			assert(!cond.is_null());
			conds_ = true;
		}

		inline void set(Xmm xmm) {
			assert(!xmm.is_null());
			xmm_.insert(xmm);
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

		std::set<R64> r64_;
		bool conds_;
		std::set<Xmm> xmm_;

		template <bool is_before>
		void trace(Trace& t);
		void finish_state(Trace& t, size_t line);
};

} // namespace x64

#endif
