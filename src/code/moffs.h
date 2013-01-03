#ifndef X64_SRC_CODE_MOFFS_H
#define X64_SRC_CODE_MOFFS_H

#include "src/code/operand.h"

namespace x64 {

/** A simple memory variable (memory offset) of type byte, word, or doubleword 
	  or quadword used by some variants of the MOV instruction. The actual 
		address is given by a simple offset relative to the segment base. No ModR/M 
		byte is used in the instruction. The number shown with moffs indicates its 
		size, which is determined by the address-size attribute of the instruction.
*/
class Moffs : public Operand {
	public:
		inline Moffs(uint64_t val) : Operand{val} { }
		template <typename T>
		inline Moffs(T* t) : Operand{(uint64_t)t} { }
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
	public:
		inline Moffs8(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs8(T* t) : Moffs{(uint64_t)t} { }
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
	public:
		inline Moffs16(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs16(T* t) : Moffs{(uint64_t)t} { }
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
	public:
		inline Moffs32(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs32(T* t) : Moffs{(uint64_t)t} { }
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
	public:
		inline Moffs64(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs64(T* t) : Moffs{(uint64_t)t} { }
};

}

#endif
