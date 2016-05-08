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

#ifndef X64ASM_SRC_IMM_H
#define X64ASM_SRC_IMM_H

#include <iostream>

#include "src/function.h"
#include "src/operand.h"

namespace x64asm {

/** An immediate value. */
class Imm : public Operand {
public:
  /** Comparison based on immediate value. */
  constexpr bool operator==(const Imm& rhs) const {
    return val_ == rhs.val_;
  }
  /** Comparison based on immediate value. */
  constexpr bool operator!=(const Imm& rhs) const {
    return !(*this == rhs);
  }
  /** Comparison based on immediate value. */
  constexpr bool operator<(const Imm& rhs) const {
    return val_ < rhs.val_;
  }

  /** Conversion based on underlying value. */
  constexpr operator uint64_t() const {
    return val_;
  }

  /** STL-compliant hash. */
  constexpr size_t hash() const {
    return val_;
  }

  /** @todo This method is undefined */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this immediate to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const {
    const auto fmt = os.flags();
    os << "$0x" << std::noshowbase << std::hex << val_;
    os.flags(fmt);
    return os;
  }

protected:
  /** Direct access to this constructor is disallowed. */
  constexpr Imm(Type t, uint64_t value) : Operand(t, value) {}
};

/** An immediate byte value. The imm8 symbol is a signed number between –128
    and +127 inclusive. For instructions in which imm8 is combined with a
    word or doubleword operand, the immediate value is sign-extended to form
    a word or doubleword. The upper byte of the word is filled with the topmost
    bit of the immediate value.
*/
class Imm8 : public Imm {
public:
  /** Creates a 8-bit immediate. */
  constexpr Imm8(uint8_t i) : Imm(Type::IMM_8, i) {}

  /** Checks that this immediate value fits in 8 bits. */
  constexpr bool check() const {
    return ((val_>>8) == 0x0ul) || ((val_>>8) == 0xfffffffffffffful);
  }
};

/** An immediate word value used for instructions whose operand-size attribute
    is 16 bits. This is a number between -32,768 and +32,767 inclusive.
*/
class Imm16 : public Imm {
public:
  /** Creates a 16-bit immediate. */
  constexpr Imm16(uint16_t i) : Imm(Type::IMM_16, i) {}

  /** Checks that this immediate value fits in 16 bits. */
  constexpr bool check() const {
    return ((val_>>16) == 0x0ul) || ((val_>>16) == 0xfffffffffffful);
  }
};

/** An immediate doubleword value used for instructions whose operand-size
    attribute is 32 bits. It allows the use of a number between
    +2,147,483,647 and -2,147,483,648 inclusive.
*/
class Imm32 : public Imm {
public:
  /** Creates a 32-bit immediate. */
  constexpr Imm32(uint32_t i) : Imm(Type::IMM_32, i) {}

  /** Check that this immediate value fits in 32 bits. */
  constexpr bool check() const {
    return ((val_>>32) == 0x0ul) || ((val_>>32) == 0xfffffffful);
  }
};

/** An immediate quadword value used for instructions whose operand-size
    attribute is 64 bits. The value allows the use of a number between
    +9,223,372,036,854,775,807 and -9,223,372,036,854,775,808 inclusive.
*/
class Imm64 : public Imm {
public:
  /** Creates a 64-bit immediate. */
  constexpr Imm64(uint64_t i) : Imm(Type::IMM_64, i) {}
  /** Creates a 64-bit immediate from a 64-bit pointer. */
  template <typename T>
  constexpr Imm64(T* t) : Imm(Type::IMM_64, (uint64_t)t) {}
  /** Creates a 64-bit immediate from the address of a function. */
  Imm64(const Function& f) : Imm(Type::IMM_64, (uint64_t)f.data()) {}

  /** Checks that this immediate value fits in 64-bits. */
  constexpr bool check() const {
    return true;
  }
};

/** The immediate constant value zero */
class Zero : public Imm8 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Checks that this immediate value equals zero. */
  constexpr bool check() const {
    return val_ == 0;
  }

private:
  /** Direct access to this constructor is disallowed. */
  constexpr Zero() : Imm8(0) {}
};

/** The immediate constant value one */
class One : public Imm8 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Checks that this immediate value equals one. */
  constexpr bool check() const {
    return val_ == 1;
  }

private:
  /** Direct access to this construcotr is disallowed. */
  constexpr One() : Imm8(1) {}
};

/** The immediate constant value three */
class Three : public Imm8 {
  // Needs access to constructor.
  friend class Constants;

public:
  /** Checks that this immediate value equals three. */
  constexpr bool check() const {
    return val_ == 3;
  }

private:
  /** Direct access to this constructor is disallosed. */
  constexpr Three() : Imm8(3) {}
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Imm> {
  size_t operator()(const x64asm::Imm& i) const {
    return i.hash();
  }
};

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Imm& i) {
  return i.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Imm& i) {
  return i.write_att(os);
}

} // namespace std

#endif
