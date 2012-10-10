#ifndef X64_SRC_SANDBOXER_SANDBOX_H
#define X64_SRC_SANDBOXER_SANDBOX_H

#include <climits>

namespace x64 {

class Sandbox {
	friend class Sandboxer;

	public:
		inline Sandbox() {
			clear();
		}
	
		inline void clear() {
			runaway_ = 0;
			max_jumps_ = UINT_MAX;
		}

		inline void set_max_jumps(uint64_t max) {
			max_jumps_ = max;
		}

		inline bool runaway() const {
			return runaway_;
		}

		inline bool max_jumps_exceeded() const {
			return max_jumps_ == 0;
		}

		inline bool error() const {
			return runaway() || max_jumps_exceeded();
		}

	private:
		uint64_t runaway_;
		uint64_t max_jumps_;
};

} // namespace x64

#endif
