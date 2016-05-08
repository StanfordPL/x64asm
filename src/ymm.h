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

#ifndef X64ASM_SRC_YMM_H
#define X64ASM_SRC_YMM_H

#include <cassert>
#include <iostream>

#include "src/sse.h"

namespace x64asm {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8
    through YMM15 are available using REX.R in 64-bit mode.
*/
class Ymm : public Sse {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this xmm register is well-formed. */
  constexpr bool check() const {
    return val_ < 16;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const Ymm& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Ymm& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Ymm& rhs) const {
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

  /** Reads this ymm register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this ymm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Ymm(uint64_t val) : Sse(Type::YMM, val) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Ymm> {
  size_t operator()(const x64asm::Ymm& y) const {
    return y.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Ymm& y) {
  return y.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Ymm& y) {
  return y.write_att(os);
}

} // namespace std

#endif
