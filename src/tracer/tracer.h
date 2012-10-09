#ifndef X64_SRC_TRACER_TRACE_H
#define X64_SRC_TRACER_TRACE_H

#include <cassert>
#include <set>

#include "src/assembler/assembler.h"
#include "src/assembler/function.h"
#include "src/code/code.h"
#include "src/code/gp_reg.h"
#include "src/tracer/trace.h"

namespace x64 {

class State;
class Trace;

class Tracer {
	public:
		inline void set(GpReg gp) {
			assert(gp.is_valid() && !gp.is_null());
			gp_regs_.insert(gp);
		}

		inline void set_before(size_t line) {
			befores_.insert(line);
		}

		inline void set_after(size_t line) {
			afters_.insert(line);
		}

		inline Trace trace(const Code& code) {
			Trace t;
			trace(t, code);
			return t;
		}

		Trace& trace(Trace& trace, const Code& code);

	private:
		Assembler assm_;
		Function fxn_;

		std::set<size_t> befores_;
		std::set<size_t> afters_;

		std::set<GpReg> gp_regs_;
};

} // namespace x64

#endif
