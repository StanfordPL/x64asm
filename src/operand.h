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

#include <array>
#include <cassert>
#include <iostream>
#include <stdint.h>

#include "src/op_type.h"

namespace x64asm {

class RegSet;

/** Base operand type. */
class Operand {
	friend class std::array<Operand, 4>;
	friend class Assembler;
	friend class Instruction;
	friend class M;
	friend class Moffs;
	friend class RegSet;
	public:
		virtual constexpr bool check() {
			return true;
		}
		virtual constexpr OpType type() {
			return OpType::OPERAND;
		}
		virtual void write_att(std::ostream& os) const;
		virtual void write_intel(std::ostream& os) const;
	protected:
		constexpr Operand() : val_{0}, val2_{0} { }	
		constexpr Operand(uint64_t val) : val_{val}, val2_{0} { }	
		constexpr Operand(uint64_t val, uint64_t val2) : val_{val}, val2_{val2} { }
		uint64_t val_;
		uint64_t val2_;
	private:
		virtual void insert_in(RegSet& os, bool promote = false) const;
};

} // namespace x64asm

#endif
