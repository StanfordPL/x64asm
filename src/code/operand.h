#ifndef X64_SRC_CODE_OPERAND_H
#define X64_SRC_CODE_OPERAND_H

#include <stdint.h>

namespace x64 {

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

	NUM_TYPES,
	TYPE_NULL = NUM_TYPES
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

	NUM_BIT_WIDTHS,
	BIT_WIDTH_NULL = NUM_BIT_WIDTHS
};

/** Operand values.
*/
enum OperandVal {
	NUM_OPERAND_VALS = 0,
	OPERAND_VAL_NULL = NUM_OPERAND_VALS
};

/** Base operand type.
*/
typedef uint64_t Operand;

} // namespace x64

#endif
