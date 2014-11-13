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

#include "src/type.h"

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

  public:
    /** Copy constructor. */
    Operand(const Operand& rhs) {
			val_ = rhs.val_;
			val2_ = rhs.val2_;
		}
    /** Move constructor. */
    Operand(Operand&& rhs) {
			val_ = rhs.val_;
			val2_ = rhs.val2_;
		}
    /** Copy assignment operator. */
    Operand& operator=(const Operand& rhs) {
			Operand(rhs).swap(*this);
			return *this;
		}
    /** Move assignment operator. */
    Operand& operator=(Operand&& rhs) {
			Operand(std::move(rhs)).swap(*this);
			return *this;
		}

    /** STL-compliant swap. */
    void swap(Operand& rhs) {
			std::swap(val_, rhs.val_);
			std::swap(val2_, rhs.val2_);
		}

    /** Return the type of this operand */
    constexpr Type type() { return (Type)(val2_ >> 3); }
    /** Return the size of this operand */
    uint16_t size() const;
    /** Is this a general purpose register? */
    bool is_gp_register() const;
    /** Is this an SSE register? */
    bool is_sse_register() const;
    /** Is this a normal 8/16/32/64/128/256-bit memory operand? */
    bool is_typical_memory() const;
    /** Is this an immediate? */
    bool is_immediate() const;

  protected:
    /** Creates an operand with a type and no underlying value. */
    constexpr Operand(Type t) : val_(0), val2_((uint64_t)t << 3) {}
    /** Creates an operand with a type and one underlying value. */
    constexpr Operand(Type t, uint64_t val) : val_(val), val2_((uint64_t)t << 3) {}
    /** Creates an operand with a type and two underlying values. */
    constexpr Operand(Type t, uint64_t val, uint64_t val2) : val_(val), val2_(val2 | ((uint64_t)t << 3)) {}
    /** Creates an operand with no type and no underlying value. */
    constexpr Operand() : val_(0), val2_(0) {}

    /** Underlying value. */
    uint64_t val_;
    /** Extended storage space for underlying value. */
    uint64_t val2_;

  private:  
    /** Comparison based on underlying values. */
    bool operator<(const Operand& rhs) const {
			return std::make_pair(val_, val2_) < std::make_pair(rhs.val_, rhs.val2_);
		}
    /** Comparison based on underlying values. */
    bool operator==(const Operand& rhs) const {
			return std::make_pair(val_, val2_) == std::make_pair(rhs.val_, rhs.val2_);
		}
    /** Comparison based on underlying values. */
    bool operator!=(const Operand& rhs) const {
  		return !(*this == rhs);
		}
};

} // namespace x64asm

#endif
