#ifndef X64_SRC_SANDBOXER_SANDBOX_H
#define X64_SRC_SANDBOXER_SANDBOX_H

namespace x64 {

class Sandbox {
	friend class Sandboxer;

	public:
		inline Sandbox()
				: runaway_(0) {
		}
	
		inline void clear() {
			runaway_ = 0;
		}

		inline bool runaway() const {
			return runaway_;
		}

		inline bool error() const {
			return runaway();
		}

	private:
		uint64_t runaway_;
};

} // namespace x64

#endif
