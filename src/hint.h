#ifndef X64ASM_SRC_HINT_H
#define X64ASM_SRC_HINT_H

namespace x64asm {

/** A taken/not-taken hint for conditional jumps. */
enum class Hint {
	TAKEN = 0,
	NOT_TAKEN
};

} // namespace x64asm

#endif
