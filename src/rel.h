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

#ifndef X64ASM_SRC_REL_H
#define X64ASM_SRC_REL_H

#include <iostream>

#include "src/imm.h"
#include "src/operand.h"

namespace x64asm {

/** A relative address. */
class Rel : public Operand {
  public:
    /** Comparison based on relative value. */
    constexpr bool operator<(const Rel& rhs) {
      return val_ < rhs.val_;
    }

    /** Comparison based on relative value. */
    constexpr bool operator==(const Rel& rhs) {
      return val_ == rhs.val_;
    }

    /** Conversion based on relative value. */
    constexpr operator uint64_t() {
      return val_;
    }

    /** Writes this relative address to an ostream in at&t syntax. */
    void write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Rel(uint64_t val)
      : Operand {val} {
    }
};

/** A relative address in the range from 128 bytes before the end of the
	  instruction to 127 bytes after the end of the instruction.
*/
class Rel8 : public Rel {
  public:
    /** Creates an 8-bit relative offset. */
    constexpr Rel8(int8_t val)
      : Rel {(uint64_t)val} {
    }

    /** Checks that this offset fits in 8 bits. */
    constexpr bool check() {
			return ((val_>>8) == 0x0ul) || ((val_>>8) == 0xfffffffffffffful);
    }
};

/** A relative address within the same code segment as the instruction
	  assembled. The rel32 symbol applies to instructions with an
		operand-size attribute of 32 bits.
*/
class Rel32 : public Rel {
  public:
    /** Creates a 32-bit relative offset. */
    constexpr Rel32(int64_t val)
      : Rel {(uint64_t)val} {
    }

    /** Checks that this offset value fits in 32-bits. */
    constexpr bool check() {
			return ((val_>>32) == 0x0ul) || ((val_>>32) == 0xfffffffful);
    }
};

} // namespace x64asm

#endif
