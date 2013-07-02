/*
Copyright 2013 eric schkufza

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
#include <stdint.h>

namespace x64asm {

class RegSet;

/** Base operand type. This class is provisioned with enough storage space
    for an operand of any type. This prevents object slicing from losing
    information if an object is cast back and form to/from an Operand.
		The only operand type which requires more than 64-bits to store its
		internal representation is the Moffs type.
 */
class Operand {
    // Needs access to default constructor.
    friend class std::array<Operand, 4>;
    // Needs access to underlying value.
    friend class Assembler;
    // Needs access to non-default constructor.
    friend class Instruction;
    // Needs access to non-default constructor.
    friend class M;
    // Needs access to non-default constructor.
    friend class Moffs;
    // Needs access to underlying value.
    friend class RegSet;

	public:
		/** Copy constructor. */
		Operand(const Operand& rhs);
		/** Move constructor. */
		Operand(Operand&& rhs);
		/** Copy assignment operator. */
		Operand& operator=(const Operand& rhs);
		/** Move assignment operator. */
		Operand& operator=(Operand&& rhs);

		/** STL-compliant swap. */
		void swap(Operand& rhs);

  protected:
    /** Creates an operand with no underlying value. */
    constexpr Operand();
    /** Creates an operand with one underlying value. */
    constexpr Operand(uint64_t val);
    /** Creates an operand with two underlying values. */
    constexpr Operand(uint64_t val, uint64_t val2);

    /** Underlying value. */
    uint64_t val_;
    /** Extended storage space for underlying value. */
    uint64_t val2_;
};

inline Operand::Operand(const Operand& rhs) {
	val_ = rhs.val_;
	val2_ = rhs.val2_;
}

inline Operand::Operand(Operand&& rhs) {
	val_ = rhs.val_;
	val2_ = rhs.val2_;
}

inline Operand& Operand::operator=(const Operand& rhs) {
	Operand(rhs).swap(*this);
	return *this;
}

inline Operand& Operand::operator=(Operand&& rhs) {
	Operand(std::move(rhs)).swap(*this);
	return *this;
}

inline void Operand::swap(Operand& rhs) {
	std::swap(val_, rhs.val_);
	std::swap(val2_, rhs.val2_);
}

inline constexpr Operand::Operand() : 
		val_ {0}, val2_ {0} { 
}

inline constexpr Operand::Operand(uint64_t val) : 
		val_ {val}, val2_ {0} { 
}

inline constexpr Operand::Operand(uint64_t val, uint64_t val2) : 
		val_ {val}, val2_ {val2} { 
}

} // namespace x64asm

#endif
