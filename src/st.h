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

#ifndef X64ASM_SRC_ST_H
#define X64ASM_SRC_ST_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** The ith element from the top of the FPU register stack
    (i = 0 through 7).
*/
class St : public Operand {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this stack register is well-formed. */
  constexpr bool check() const {
    return val_ < 8;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const St& rhs) const {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const St& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const St& rhs) const {
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
  /** Writes this stack register to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const {
    assert(check());
    os << "%";
    if (val_ == 0) {
      os << "st";
    } else {
      os << "st(" << std::dec << val_ << ")";
    }
    return os;
  }

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr St(uint64_t val) : Operand(Type::ST, val) {}
  constexpr St(Type t, uint64_t val) : Operand(t, val) {}
};

/** The top element of the FPU register stack. */
class St0 : public St {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Returns true if this stack register is %st(0). */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr St0() : St(Type::ST_0, 0) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::St> {
  size_t operator()(const x64asm::St& s) const {
    return s.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::St& s) {
  return s.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::St& s) {
  return s.write_att(os);
}

} // namespace std

#endif
