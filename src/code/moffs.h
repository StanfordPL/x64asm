#ifndef X64_SRC_CODE_MOFFS_H
#define X64_SRC_CODE_MOFFS_H

#include "src/code/operand.h"

namespace x64 {

/** An absolute memory offset, used by some variants of MOV.
*/
class Moffs {
	public:
		inline Moffs()
				: o_(0) {
		}

		inline Moffs(Operand o)
				: o_(o) {
		}

		template <typename T>
		inline Moffs(T* t)
				: o_((Operand) t) {
		}

		inline operator Operand() const {
			return o_;
		}

		inline bool is_null() const {
			return false;
		}

	private:
		Operand o_;
};

class Moffs8 : public Moffs {
	public:
		inline Moffs8() : Moffs() { }
		inline Moffs8(Operand o) : Moffs(o) { }

		template <typename T>
		inline Moffs8(T* t) : Moffs(t) { }
};

class Moffs16 : public Moffs {
	public:
		inline Moffs16() : Moffs() { }
		inline Moffs16(Operand o) : Moffs(o) { }

		template <typename T>
		inline Moffs16(T* t) : Moffs(t) { }
};

class Moffs32 : public Moffs {
	public:
		inline Moffs32() : Moffs() { }
		inline Moffs32(Operand o) : Moffs(o) { }

		template <typename T>
		inline Moffs32(T* t) : Moffs(t) { }
};

class Moffs64 : public Moffs {
	public:
		inline Moffs64() : Moffs() { }
		inline Moffs64(Operand o) : Moffs(o) { }

		template <typename T>
		inline Moffs64(T* t) : Moffs(t) { }
};

}

#endif
