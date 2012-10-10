#ifndef X64_SRC_CODE_OPERAND_H
#define X64_SRC_CODE_OPERAND_H

#include <stdint.h>

namespace x64 {

/** Base operand type.
*/
typedef uint64_t Operand;

/** Operand types.
*/
enum Type { 
	GP_REG = 0,
	RAX_ONLY,
	RCX_ONLY,
	XMM_REG,
	MMX_REG,
	FP_REG,
	ST0_ONLY,
	IMM,
	LABEL,
	SCALE,
	ADDR,
	OFFSET,

	TYPE_NULL
};

/** Bit field widths.
*/
enum BitWidth {
	LOW = 0,
	HIGH,
	WORD,
	DOUBLE,
	QUAD,
	OCT,
	FIXED,

	BIT_WIDTH_NULL
};

} // namespace x64

#endif
