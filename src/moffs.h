#ifndef X64_SRC_MOFFS_H
#define X64_SRC_MOFFS_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64 {

/** A simple memory variable. */
class Moffs : public AtomicOperand {
	public:
		inline Moffs(uint64_t val) : AtomicOperand{val} { }
		virtual ~Moffs() = 0;

		virtual OpType type() const;

		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
	public:
		inline Moffs8(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs8(T* t) : Moffs{(uint64_t)t} { }

		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
	public:
		inline Moffs16(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs16(T* t) : Moffs{(uint64_t)t} { }

		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
	public:
		inline Moffs32(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs32(T* t) : Moffs{(uint64_t)t} { }

		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
	public:
		inline Moffs64(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs64(T* t) : Moffs{(uint64_t)t} { }

		virtual OpType type() const;
};

}

#endif
