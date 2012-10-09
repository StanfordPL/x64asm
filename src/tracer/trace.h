#ifndef X64_SRC_TRACE_TRACE_H
#define X64_SRC_TRACE_TRACE_H

#include <array>

#include "src/tracer/state.h"

namespace x64 {

class Trace {
	friend class Tracer;

	public:
		inline Trace()
				: next_elem_(0) {
		}

		inline size_t size() const {
			return next_elem_;
		}

		inline void clear() {
			next_elem_ = 0;
		}

		inline State& get(size_t index) {
			assert(index < size());
			return trace_[index];
		}

		typedef std::array<State, 1024>::const_iterator iterator;

		inline iterator begin() const {
			return trace_.begin();
		}

		inline iterator end() const {
			return trace_.begin() + next_elem_;
		}

	private:
		size_t next_elem_;	
		std::array<State, 1024> trace_;
};

} // namespace x64

#endif
