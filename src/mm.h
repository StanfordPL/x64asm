/*
Copyright 2013-2015 Stanford University

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

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public Operand {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this xmm register is well-formed. */
  constexpr bool check() const {
    return val_ < 8;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const Mm& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Mm& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Mm& rhs) const {
    return !(*this == rhs);
  }

  /** Conversion based on underlying value. */
  constexpr operator uint64_t() const {
    return val_;
  }

  /** STL-compliant hash. */
  constexpr size_t hash() const {
    return val_;
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

/** STL hash specialization. */
template <>
struct hash<x64asm::Mm> {
  size_t operator()(const x64asm::Mm& m) const {
    return m.hash();
  }
};

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
