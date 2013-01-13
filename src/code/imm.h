#ifndef X64_SRC_CODE_IMM_H
#define X64_SRC_CODE_IMM_H

#include "src/code/operand.h"

namespace x64 {

/** An immediate byte value. The imm8 symbol is a signed number between –128 
	  and +127 inclusive. For instructions in which imm8 is combined with a 
		word or doubleword operand, the immediate value is sign-extended to form 
		a word or doubleword. The upper byte of the word is filled with the topmost 
		bit of the immediate value. 
*/
class Imm8 : public Operand {
	public:
		inline Imm8(uint8_t i) : Operand{i} { }

		inline virtual bool check() const {
			return (int64_t)val_ >= -128 && (int64_t)val_ <= 127;
		}
};

/** An immediate word value used for instructions whose operand-size attribute 
	  is 16 bits. This is a number between -32,768 and +32,767 inclusive.
*/
class Imm16 : public Operand {
	public:
		inline Imm16(uint16_t i) : Operand{i} { }

		inline virtual bool check() const {
			return (int64_t)val_ >= -32768 && (int64_t)val_ <= 32767;
		}
};

/** An immediate doubleword value used for instructions whose operand-size 
	  attribute is 32 bits. It allows the use of a number between 
		+2,147,483,647 and -2,147,483,648 inclusive.
*/
class Imm32 : public Operand {
	public:
		inline Imm32(uint32_t i) : Operand{i} { }

		inline virtual bool check() const {
			return (int64_t)val_ >= -2147483648 && (int64_t)val_ <= 2147483647;
		}
};

/** An immediate quadword value used for instructions whose operand-size 
	  attribute is 64 bits. The value allows the use of a number between 
		+9,223,372,036,854,775,807 and -9,223,372,036,854,775,808 inclusive.
*/
class Imm64 : public Operand {
	public:
		inline Imm64(uint64_t i) : Operand{i} { }

		template <typename T>
		inline Imm64(T* t) : Operand{(Operand) t} { }

		inline virtual bool check() const {
			return true;
		}
};

/** The immediate constant value zero */
class Zero : public Imm8 {
	public:
		inline Zero() : Imm8{0} { }
		inline Zero(uint64_t ignore) : Imm8{0} { }

		inline virtual bool check() const {
			return val_ == 0;
		}
};

/** The immediate constant value one */
class One : public Imm8 {
	public:
		inline One() : Imm8{1} { }
		inline One(uint64_t ignore) : Imm8{0} { }

		inline virtual bool check() const {
			return val_ == 1;
		}
};

/** The immediate constant value three */
class Three : public Imm8 {
	public:
		inline Three() : Imm8{3} { }
		inline Three(uint64_t ignore) : Imm8{0} { }

		inline virtual bool check() const {
			return val_ == 3;
		}
};

} // namespace x64

#endif
