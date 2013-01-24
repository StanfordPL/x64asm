#ifndef X64_SRC_HINT_H
#define X64_SRC_HINT_H

namespace x64 {

/** A taken/not-taken hint for conditional jumps. */
enum class Hint {
	TAKEN = 0,
	NOT_TAKEN
};

} // namespace x64

#endif
