/*
Copyright 2103 eric schkufza

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifndef X64ASM_SRC_MOFFS_H
#define X64ASM_SRC_MOFFS_H

#include <iostream>

#include "src/op_type.h"
#include "src/operand.h"

namespace x64asm {

/** A simple memory variable. */
class Moffs : public AtomicOperand {
	public:
		inline Moffs(uint64_t val) : AtomicOperand{val} { }
		virtual ~Moffs() = 0;
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	private:
		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
	public:
		inline Moffs8(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs8(T* t) : Moffs{(uint64_t)t} { }
	private:
		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
	public:
		inline Moffs16(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs16(T* t) : Moffs{(uint64_t)t} { }
	private:
		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
	public:
		inline Moffs32(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs32(T* t) : Moffs{(uint64_t)t} { }
	private:
		virtual OpType type() const;
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
	public:
		inline Moffs64(uint64_t o) : Moffs{o} { }
		template <typename T>
		inline Moffs64(T* t) : Moffs{(uint64_t)t} { }
	private:
		virtual OpType type() const;
};

} // namespace x64asm

#endif
