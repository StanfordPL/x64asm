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

#ifndef X64ASM_SRC_OPERAND_H
#define X64ASM_SRC_OPERAND_H

#include <cassert>
#include <iostream>
#include <stdint.h>

#include "src/op_type.h"

namespace x64asm {

class RegSet;

/** Base operand type. */
class Operand {
	friend class Instruction;
	public:
		virtual constexpr bool check() {
			return true;
		}
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		constexpr Operand() { }	
	private:
		virtual constexpr OpType type() {
			return OpType::HINT;
		}
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

/** Atomic Operand Type. */
class AtomicOperand : public Operand {
	friend class Assembler;
	friend class M;
	friend class RegSet;
	protected:
		constexpr AtomicOperand(uint64_t val) : val_{val} { }
		const uint64_t val_;	
};

/** Aggregate Operand Type. */
class CompoundOperand : public Operand {
	protected:
		constexpr CompoundOperand() { }
};

} // namespace x64asm

#endif
