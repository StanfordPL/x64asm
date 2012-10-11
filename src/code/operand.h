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
	ADDR = 0,
	FP_REG,
	GP_REG,
	IMM,
	LABEL,
	MMX_REG,
	OFFSET,
	SCALE,
	XMM_REG,

	RAX_ONLY,
	RCX_ONLY,
	ST0_ONLY,

	TYPE_NULL
};

/** Operand widths.
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

/** Operand modifiers.
*/
enum Modifier {
	READ = 0,
	WRITE,
	READ_WRITE,
	NONE,

	MODIFIER_NULL
};

} // namespace x64

#endif
