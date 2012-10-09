#ifndef X64_SRC_TRACE_TRACE_H
#define X64_SRC_TRACE_TRACE_H

#include <vector>

#include "src/tracer/state.h"
#include "src/tracer/tracer.h"

namespace x64 {

class Trace {
	friend class Tracer;

	public:
		inline Trace(size_t capacity) 
				: next_elem_(0), trace_(capacity) {
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

		typedef std::vector<State>::const_iterator iterator;

		inline iterator begin() const {
			return trace_.begin();
		}

		inline iterator end() const {
			return trace_.begin() + next_elem_;
		}

	private:
		size_t next_elem_;	
		std::vector<State> trace_;
};

} // namespace x64

#endif
