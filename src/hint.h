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

#ifndef X64ASM_SRC_HINT_H
#define X64ASM_SRC_HINT_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A taken/not-taken hint for conditional jumps. */
class Hint : public Operand {
    // Needs access to internal value for emitting prefixes.
    friend class Assembler;
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Checks that this hint is well-formed. */
    constexpr bool check() {
      return val_ < 2;
    }

    /** Writes this hint to an ostream using (something like) at&t syntax. */
    void write_att(std::ostream& os) const;
    /** Writes this hint to an ostream using (something like) intel syntax. */
    void write_intel(std::ostream& os) const;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Hint(uint64_t val) : Operand {val} { }
};

} // namespace x64asm

#endif
