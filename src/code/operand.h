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
	REL_8,
	REL_16,
	REL_32,
	AL,
	AX,
	EAX,
	RAX,
	CL,
	R_H,
	R_8,
	R_16,
	R_32,
	R_64,
	IMM_8,
	IMM_16,
	IMM_32,
	IMM_64,
	LABEL,
	M_8,
	M_16,
	M_32,
	M_64,
	M_80,
	M_128,
	M_256,
	MM,
	MOFFS_8,
	MOFFS_16,
	MOFFS_32,
	MOFFS_64,
	SCALE,
	SREG,
	ST0,
	ST,
	XMM,
	XMM0,
	YMM,

	// DEPRECATED -- But required by parser
	ADDR,
	IMM,
	GP_REG,
	MMX_REG,
	XMM_REG,
	FP_REG,
	SEG_REG,
	OFFSET,
	RAX_ONLY,
	RCX_ONLY,
	ST0_ONLY,

	TYPE_NULL
};

// DEPRECATED -- But required by parser
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
/*
enum Modifier {
	READ = 0,
	WRITE,
	READ_WRITE,
	NONE,

	MODIFIER_NULL
};
*/

} // namespace x64

#endif
