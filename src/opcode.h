#ifndef X64ASM_SRC_OPCODE_H
#define X64ASM_SRC_OPCODE_H

namespace x64asm {

/** An x64 mnemonic. */
enum Opcode : int32_t {
	// Internal mnemonics
	LABEL_DEFN = 0
	// Auto-generated mnemonics
	#include "src/opcode.enum"
};

} // namespace x64asm

#endif
