#ifndef X64_SRC_OPERANDS_IMM_H
#define X64_SRC_OPERANDS_IMM_H

#include "src/operands/operand.h"

namespace x64 {

/** An immediate value. */
class Imm : public Operand {
	protected:
		inline Imm(uint64_t val) : Operand{val} { }
};

/** An immediate byte value. The imm8 symbol is a signed number between –128 
	  and +127 inclusive. For instructions in which imm8 is combined with a 
		word or doubleword operand, the immediate value is sign-extended to form 
		a word or doubleword. The upper byte of the word is filled with the topmost 
		bit of the immediate value. 
*/
class Imm8 : public Imm {
	public:
		inline Imm8(uint8_t i) : Imm{i} { }
};

/** An immediate word value used for instructions whose operand-size attribute 
	  is 16 bits. This is a number between -32,768 and +32,767 inclusive.
*/
class Imm16 : public Imm {
	public:
		inline Imm16(uint16_t i) : Imm{i} { }
};

/** An immediate doubleword value used for instructions whose operand-size 
	  attribute is 32 bits. It allows the use of a number between 
		+2,147,483,647 and -2,147,483,648 inclusive.
*/
class Imm32 : public Imm {
	public:
		inline Imm32(uint32_t i) : Imm{i} { }
};

/** An immediate quadword value used for instructions whose operand-size 
	  attribute is 64 bits. The value allows the use of a number between 
		+9,223,372,036,854,775,807 and -9,223,372,036,854,775,808 inclusive.
*/
class Imm64 : public Imm {
	public:
		inline Imm64(uint64_t i) : Imm{i} { }

		template <typename T>
		inline Imm64(T* t) : Imm{(Operand) t} { }
};

/** The immediate constant value zero */
class Zero : public Imm8 {
	friend class Constants;
	private:
		inline Zero() : Imm8{0} { }
};

/** The immediate constant value one */
class One : public Imm8 {
	friend class Constants;
	private:
		inline One() : Imm8{1} { }
};

/** The immediate constant value three */
class Three : public Imm8 {
	friend class Constants;
	private:
		inline Three() : Imm8{3} { }
};

} // namespace x64

#endif
