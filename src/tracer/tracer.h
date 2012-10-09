#ifndef X64_SRC_TRACER_TRACE_H
#define X64_SRC_TRACER_TRACE_H

#include <array>
#include <vector>

#include "src/code/code.h"
#include "src/code/gp_reg.h"

namespace x64 {

class Trace;

class Tracer {
	public:
		Tracer(const Code& code)
				: code_(code) {
			befores_.resize(code_.size());
			afters_.resize(code_.size());

			for ( size_t i = 0, ie = code_.size(); i < ie; ++i )
				befores_[i] = afters_[i] = false;
		}

		inline bool set(GpReg gp) {
			if( !gp.is_valid() || gp.is_null() )
				return false;
			if ( find(gp_regs_.begin(), gp_regs_.end(), gp) == gp_regs_.end() )
				gp_regs_.push_back(gp);
			return true;
		}

		inline bool set_before(size_t line) {
			if ( line >= code_.size() )
				return false;
			befores_[line] = true;
			return true;
		}

		inline bool set_after(size_t line) {
			if ( line >= code_.size() )
				return false;
			if ( code_.get(line).is_jump() )
				return false;
			afters_[line] = true;
			return true;
		}

		void trace(Trace& trace) const;

	private:
		const Code& code_;

		std::vector<bool> befores_;
		std::vector<bool> afters_;

		std::vector<GpReg> gp_regs_;
};

} // namespace x64

#endif
