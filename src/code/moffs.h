#ifndef X64_SRC_CODE_MOFFS_H
#define X64_SRC_CODE_MOFFS_H

#include "src/code/operand.h"

namespace x64 {

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Operand {
	public:
		inline Moffs8(uint64_t o) : Operand{o} { }
		template <typename T>
		inline Moffs8(T* t) : Operand{(uint64_t)t} { }

		inline bool check() const {
			return true;
		}
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Operand {
	public:
		inline Moffs16(uint64_t o) : Operand{o} { }
		template <typename T>
		inline Moffs16(T* t) : Operand{(uint64_t)t} { }

		inline bool check() const {
			return true;
		}
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Operand {
	public:
		inline Moffs32(uint64_t o) : Operand{o} { }
		template <typename T>
		inline Moffs32(T* t) : Operand{(uint64_t)t} { }

		inline bool check() const {
			return true;
		}
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Operand {
	public:
		inline Moffs64(uint64_t o) : Operand{o} { }
		template <typename T>
		inline Moffs64(T* t) : Operand{(uint64_t)t} { }

		inline bool check() const {
			return true;
		}
};

}

#endif
