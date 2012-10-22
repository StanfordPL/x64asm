#ifndef X64_SRC_TRACE_TRACE_H
#define X64_SRC_TRACE_TRACE_H

#include <vector>

#include "src/assembler/function.h"
#include "src/tracer/state.h"

namespace x64 {

class Trace {
	friend class Tracer;

	public:
		inline Trace()
				: fxn_(1024*1024), next_elem_(0), trace_(1024) {
		}

		inline uint64_t operator()() {
			return fxn_();
		}

		inline uint64_t operator()(uint64_t rdi) {
			return fxn_(rdi);
		}

		inline uint64_t operator()(uint64_t rdi, uint64_t rsi) {
			return fxn_(rdi, rsi);
		}

		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx) {
			return fxn_(rdi, rsi, rdx);
		}

		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx,
				                       uint64_t rcx) {
			return fxn_(rdi, rsi, rdx, rcx);
		}

		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx,
				                       uint64_t rcx, uint64_t r8) {
			return fxn_(rdi, rsi, rdx, rcx, r8);
		}

		inline uint64_t operator()(uint64_t rdi, uint64_t rsi, uint64_t rdx,
				                       uint64_t rcx, uint64_t r8,  uint64_t r9) {
			return fxn_(rdi, rsi, rdx, rcx, r8, r9);
		}

		inline size_t size() const {
			return next_elem_;
		}

		inline void clear() {
			next_elem_ = 0;
		}

		inline const State& operator[](size_t index) const {
			assert(index < size());
			return trace_[index];
		}

		typedef std::vector<State>::const_iterator const_iterator;

		inline const_iterator begin() const {
			return trace_.begin();
		}

		inline const_iterator end() const {
			return trace_.begin() + next_elem_;
		}

	private:
		Function fxn_;

		size_t next_elem_;	
		std::vector<State> trace_;
};

} // namespace x64

#endif
