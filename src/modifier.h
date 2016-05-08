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

#ifndef X64ASM_SRC_MODIFIER_H
#define X64ASM_SRC_MODIFIER_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A modifier used to distinguish between mnemonics. These are non-standard
    operands which we have introduced to disambiguate parts of the intel x86_64
    specification.
*/
class Modifier : public Operand {
public:
  /** Returns true if this modifier is well-formed. */
  constexpr bool check() {
    return val_ == 0;
  }

  /** Comparison based on on val_. */
  constexpr bool operator<(const Modifier& rhs) {
    return val_ < rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator==(const Modifier& rhs) {
    return val_ == rhs.val_;
  }
  /** Comparison based on on val_. */
  constexpr bool operator!=(const Modifier& rhs) {
    return !(*this == rhs);
  }

  /** Conversion based on val_. */
  constexpr operator uint64_t() {
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
  /** Does nothing. AT&T syntax is undefined for modifiers. */
  std::ostream& write_att(std::ostream& os) const {
    return os;
  }

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Modifier(Type t) : Operand(t) {}
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
  // Needs access to constructor.
  friend class Constants;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Pref66() : Modifier(Type::PREF_66) {}
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
  // Needs access to constructor.
  friend class Constants;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr PrefRexW() : Modifier(Type::PREF_REX_W) {}
};

/** Far instruction variant. */
class Far : public Modifier {
  // Needs access to constructor.
  friend class Constants;

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Far() : Modifier(Type::FAR) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Modifier> {
  size_t operator()(const x64asm::Modifier& m) const {
    return m.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Modifier& m) {
  return m.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Modifier& m) {
  return m.write_att(os);
}

} // namespace std

#endif
