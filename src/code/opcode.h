#ifndef X64_SRC_CODE_OPCODE_H
#define X64_SRC_CODE_OPCODE_H

namespace x64 {

/** An x64 mnemonic. */
enum Opcode : int32_t {
	// Internal mnemonics
	LABEL_DEFN = 0
	// Auto-generated mnemonics
	#include "src/code/opcode.enum"
};

} // namespace x64

#endif
