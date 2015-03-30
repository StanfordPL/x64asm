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

#ifndef X64ASM_SRC_MM_H
#define X64ASM_SRC_MM_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public Operand {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this xmm register is well-formed. */
  constexpr bool check() {
    return val() < 8;
  }

  /** Conversion based on underlying value. */
  constexpr operator uint64_t() {
    return val();
  }

  /** Reads this mm register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this mm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Mm(uint64_t val) : Operand(Type::MM, val) {}
};

} // namespace x64asm

namespace std {

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Mm& m) {
  return m.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Mm& m) {
  return m.write_att(os);
}

} // namespace std

#endif
