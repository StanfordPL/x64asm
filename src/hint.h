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

#ifndef X64ASM_SRC_HINT_H
#define X64ASM_SRC_HINT_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A taken/not-taken hint for conditional jumps. */
class Hint : public Operand {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Checks that this hint is well-formed. */
  constexpr bool check() {
    return val() < 2;
  }

  /** Conversion based on val. */
  constexpr operator uint64_t() {
    return val();
  }

  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Does nothing. AT&T syntax is undefined for hints. */
  std::ostream& write_att(std::ostream& os) const {
    return os;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Hint(uint64_t val) : Operand(Type::HINT, val) {}
};

} // namespace x64asm

namespace std {

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Hint& h) {
  return h.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Hint& h) {
  return h.write_att(os);
}

} // namespace std

#endif
