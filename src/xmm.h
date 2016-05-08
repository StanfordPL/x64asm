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

#ifndef X64ASM_SRC_XMM_H
#define X64ASM_SRC_XMM_H

#include <iostream>

#include "src/sse.h"

namespace x64asm {

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8
    through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm : public Sse {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this xmm register is well-formed. */
  constexpr bool check() const {
    return val_ < 16;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const Xmm& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Xmm& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Xmm& rhs) const {
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
  /** STL-compliant swap. */
  void swap(Xmm& rhs) {
    std::swap(val_, rhs.val_);
  }

  /** Reads this xmm register from an ostream using at&t syntax. */
  std::istream& read_att(std::istream& is);
  /** Writes this xmm register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Xmm(uint64_t val) : Sse(Type::XMM, val) {}
  constexpr Xmm(Type t, uint64_t val) : Sse(t, val) {}
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this xmm register is %xmm0. */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Xmm0() : Xmm(Type::XMM_0, 0) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Xmm> {
  size_t operator()(const x64asm::Xmm& x) const {
    return x.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Xmm& x) {
  return x.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Xmm& x) {
  return x.write_att(os);
}

} // namespace std

#endif
