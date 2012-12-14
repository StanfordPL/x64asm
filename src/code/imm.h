#ifndef X64_SRC_CODE_IMM_H
#define X64_SRC_CODE_IMM_H

#include "src/code/operand.h"

namespace x64 {

/** An immediate value. */
class Imm {
	public:
		inline Imm() 
				: i_{0} { 
		}

		inline Imm(Operand o) 
				: i_{o} {
		}
		
		inline operator Operand() const { 
			return i_;
		}

	private:
		Operand i_;
};

/** An immediate byte value. The imm8 symbol is a signed number between –128 
	  and +127 inclusive. For instructions in which imm8 is combined with a 
		word or doubleword operand, the immediate value is sign-extended to form 
		a word or doubleword. The upper byte of the word is filled with the topmost 
		bit of the immediate value. 
*/
class Imm8 : public Imm {
	public:
	inline Imm8() : Imm{} { }
	inline Imm8(Operand o) : Imm{o} { }
};

/** An immediate word value used for instructions whose operand-size attribute 
	  is 16 bits. This is a number between -32,768 and +32,767 inclusive.
*/
struct Imm16 : public Imm {
	inline Imm16() : Imm{} { }
	inline Imm16(Operand o) : Imm{o} { }
};

/** An immediate doubleword value used for instructions whose operand-size 
	  attribute is 32 bits. It allows the use of a number between 
		+2,147,483,647 and -2,147,483,648 inclusive.
*/
struct Imm32 : public Imm {
	inline Imm32() : Imm{} { }
	inline Imm32(Operand o) : Imm{o} { }
};

/** An immediate quadword value used for instructions whose operand-size 
	  attribute is 64 bits. The value allows the use of a number between 
		+9,223,372,036,854,775,807 and -9,223,372,036,854,775,808 inclusive.
*/
struct Imm64 : public Imm {
	inline Imm64() : Imm{} { }
	inline Imm64(Operand o) : Imm{o} { }

	template <typename T>
	inline Imm64(T* t) : Imm{(Operand) t} { }
};

/** The immediate constant value zero */
struct Zero : public Imm {
	inline Zero() : Imm{0} { }
};

/** The immediate constant value one */
struct One : public Imm {
	inline One() : Imm{1} { }
};

/** The immediate constant value three */
struct Three : public Imm {
	inline Three() : Imm{3} { }
};

extern const Zero zero;
extern const One one;
extern const Three three;

} // namespace x64

#endif
