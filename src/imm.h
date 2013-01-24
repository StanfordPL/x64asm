#ifndef X64_SRC_IMM_H
#define X64_SRC_IMM_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64 {

/** An immediate value. */
class Imm : public AtomicOperand {
	public:
		inline Imm(uint64_t val) : AtomicOperand{val} { }
		virtual ~Imm() = 0;

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** An immediate byte value. The imm8 symbol is a signed number between –128 
	  and +127 inclusive. For instructions in which imm8 is combined with a 
		word or doubleword operand, the immediate value is sign-extended to form 
		a word or doubleword. The upper byte of the word is filled with the topmost 
		bit of the immediate value. 
*/
class Imm8 : public Imm {
	public:
		inline Imm8(int8_t i) : Imm{(uint64_t)i} { }

		virtual OpType type() const;
		virtual bool check() const;
};

/** An immediate word value used for instructions whose operand-size attribute 
	  is 16 bits. This is a number between -32,768 and +32,767 inclusive.
*/
class Imm16 : public Imm {
	public:
		inline Imm16(int16_t i) : Imm{(uint64_t)i} { }

		virtual OpType type() const;
		virtual bool check() const;
};

/** An immediate doubleword value used for instructions whose operand-size 
	  attribute is 32 bits. It allows the use of a number between 
		+2,147,483,647 and -2,147,483,648 inclusive.
*/
class Imm32 : public Imm {
	public:
		inline Imm32(int32_t i) : Imm{(uint64_t)i} { }

		virtual OpType type() const;
		virtual bool check() const;
};

/** An immediate quadword value used for instructions whose operand-size 
	  attribute is 64 bits. The value allows the use of a number between 
		+9,223,372,036,854,775,807 and -9,223,372,036,854,775,808 inclusive.
*/
class Imm64 : public Imm {
	public:
		inline Imm64(int64_t i) : Imm{(uint64_t)i} { }
		template <typename T>
		inline Imm64(T* t) : Imm{(uint64_t)t} { }

		virtual OpType type() const;
};

/** The immediate constant value zero */
class Zero : public Imm8 {
	friend class Constants;
	private:
		inline Zero() : Imm8{0} { }

	public:	
		virtual OpType type() const;
		virtual bool check() const;
};

/** The immediate constant value one */
class One : public Imm8 {
	friend class Constants;
	private:
		inline One() : Imm8{1} { }

	public:
		virtual OpType type() const;
		virtual bool check() const;
};

/** The immediate constant value three */
class Three : public Imm8 {
	friend class Constants;
	private:
		inline Three() : Imm8{3} { }

	public:
		virtual OpType type() const;
		virtual bool check() const;
};

} // namespace x64

#endif
