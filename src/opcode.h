#ifndef X64_SRC_OPCODE_H
#define X64_SRC_OPCODE_H

namespace x64 {

/** An x64 mnemonic. */
enum Opcode : int32_t {
	// Internal mnemonics
	LABEL_DEFN = 0
	// Auto-generated mnemonics
	#include "src/opcode.enum"
};

} // namespace x64

#endif
