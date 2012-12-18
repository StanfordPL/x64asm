#ifndef X64_SRC_TRACER_TRACE_H
#define X64_SRC_TRACER_TRACE_H

#include <cassert>
#include <set>

#include "src/assembler/assembler.h"
#include "src/code/code.h"
#include "src/tracer/trace.h"

namespace x64 {

class Tracer {
	public:
		inline Trace trace(const Code& code) {
			Trace t;
			return trace(t, code);	
		}

		Trace& trace(Trace& t, const Code& code);

	private:
		Assembler assm_;

		void trace(Trace& t, const Instruction& instr, size_t line,
				       bool is_before);
};

} // namespace x64

#endif
