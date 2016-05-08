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

#ifndef X64ASM_SRC_REL_H
#define X64ASM_SRC_REL_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A relative address. */
class Rel : public Operand {
public:
  /** Comparison based on on val_. */
  constexpr bool operator<(const Rel& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Rel& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Rel& rhs) const {
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

  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this rel to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const {
    const auto fmt = os.flags();
    os << std::hex << std::showbase << val_;
    os.flags(fmt);
    return os;
  }

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Rel(Type t, uint64_t val) : Operand(t, val) {}
};

/** A relative address in the range from 128 bytes before the end of the
    instruction to 127 bytes after the end of the instruction.
*/
class Rel8 : public Rel {
public:
  /** Creates an 8-bit relative offset. */
  constexpr Rel8(int8_t val) : Rel(Type::REL_8, (uint64_t)val) {}

  /** Checks that this offset fits in 8 bits. */
  constexpr bool check() const {
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
  constexpr Rel32(int64_t val) : Rel(Type::REL_32, (uint64_t)val) {}

  /** Checks that this offset value fits in 32-bits. */
  constexpr bool check() const {
    return ((val_>>32) == 0x0ul) || ((val_>>32) == 0xfffffffful);
  }
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Rel> {
  size_t operator()(const x64asm::Rel& r) const {
    return r.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Rel& r) {
  return r.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Rel& r) {
  return r.write_att(os);
}

} // namespace std

#endif
