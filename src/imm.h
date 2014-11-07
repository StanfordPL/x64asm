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

#ifndef X64ASM_SRC_IMM_H
#define X64ASM_SRC_IMM_H

#include <iostream>

#include "src/function.h"
#include "src/operand.h"

namespace x64asm {

/** An immediate value. */
class Imm : public Operand {
  public:
    /** Copy constructor. */
    Imm(const Imm& rhs);
    /** Move constructor. */
    Imm(Imm&& rhs);
    /** Copy assignment operator. */
    Imm& operator=(const Imm& rhs);
    /** Move assignment operator. */
    Imm& operator=(Imm&& rhs);

    /** Comparison based on immediate value. */
    constexpr bool operator==(const Imm& rhs);
    /** Comparison based on immediate value. */
    constexpr bool operator!=(const Imm& rhs);
    /** Comparison based on immediate value. */
    constexpr bool operator<(const Imm& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Imm& rhs);

    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Imm(uint64_t val) : Operand {val} { }
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
    constexpr Imm8(uint8_t i);

    /** Checks that this immediate value fits in 8 bits. */
    constexpr bool check();
};

/** An immediate word value used for instructions whose operand-size attribute
    is 16 bits. This is a number between -32,768 and +32,767 inclusive.
*/
class Imm16 : public Imm {
  public:
    /** Creates a 16-bit immediate. */
    constexpr Imm16(uint16_t i);

    /** Checks that this immediate value fits in 16 bits. */
    constexpr bool check();
};

/** An immediate doubleword value used for instructions whose operand-size
    attribute is 32 bits. It allows the use of a number between
    +2,147,483,647 and -2,147,483,648 inclusive.
*/
class Imm32 : public Imm {
  public:
    /** Creates a 32-bit immediate. */
    constexpr Imm32(uint32_t i);

    /** Check that this immediate value fits in 32 bits. */
    constexpr bool check();
};

/** An immediate quadword value used for instructions whose operand-size
    attribute is 64 bits. The value allows the use of a number between
    +9,223,372,036,854,775,807 and -9,223,372,036,854,775,808 inclusive.
*/
class Imm64 : public Imm {
  public:
    /** Creates a 64-bit immediate. */
    constexpr Imm64(uint64_t i);
    /** Creates a 64-bit immediate from a 64-bit pointer. */
    template <typename T>
    constexpr Imm64(T* t);
    /** Creates a 64-bit immediate from the address of a function. */
    Imm64(const Function& f);

    /** Checks that this immediate value fits in 64-bits. */
    constexpr bool check();
};

/** The immediate constant value zero */
class Zero : public Imm8 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Checks that this immediate value equals zero. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Zero();
};

/** The immediate constant value one */
class One : public Imm8 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Checks that this immediate value equals one. */
    constexpr bool check();

  private:
    /** Direct access to this construcotr is disallowed. */
    constexpr One();
};

/** The immediate constant value three */
class Three : public Imm8 {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Checks that this immediate value equals three. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallosed. */
    constexpr Three();
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Imm> {
  size_t operator()(const x64asm::Imm& i) const;
};

/** STL swap overload. */
void swap(x64asm::Imm& lhs, x64asm::Imm& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Imm& i);

} // namespace std

namespace x64asm {

inline Imm::Imm(const Imm& rhs) : Operand{0,0} {
  val_ = rhs.val_;
}

inline Imm::Imm(Imm&& rhs) {
  val_ = rhs.val_;
}

inline Imm& Imm::operator=(const Imm& rhs) {
  Imm(rhs).swap(*this);
  return *this;
}

inline Imm& Imm::operator=(Imm&& rhs) {
  Imm(std::move(rhs)).swap(*this);
  return *this;
}

constexpr bool Imm::operator==(const Imm& rhs) {
  return val_ == rhs.val_;
}

constexpr bool Imm::operator!=(const Imm& rhs) {
  return !(*this == rhs);
}

constexpr bool Imm::operator<(const Imm& rhs) {
  return val_ < rhs.val_;
}

inline constexpr Imm::operator uint64_t() {
  return val_;
}

inline constexpr size_t Imm::hash() {
  return val_;
}

inline void Imm::swap(Imm& rhs) {
  std::swap(val_, rhs.val_);
}

inline std::ostream& Imm::write_att(std::ostream& os) const {
	const auto fmt = os.flags();
  os << "$0x" << std::noshowbase << std::hex << val_;
	os.flags(fmt);
	return os;
}

inline constexpr Imm8::Imm8(uint8_t i) : 
    Imm {i} {
}

inline constexpr bool Imm8::check() {
  return ((val_>>8) == 0x0ul) || ((val_>>8) == 0xfffffffffffffful);
}

inline constexpr Imm16::Imm16(uint16_t i) : 
    Imm {i} {
}

inline constexpr bool Imm16::check() {
  return ((val_>>16) == 0x0ul) || ((val_>>16) == 0xfffffffffffful);
}

inline constexpr Imm32::Imm32(uint32_t i) : 
    Imm {i} {
}

inline constexpr bool Imm32::check() {
  return ((val_>>32) == 0x0ul) || ((val_>>32) == 0xfffffffful);
}

inline constexpr Imm64::Imm64(uint64_t i) : 
    Imm {i} {
}

template <typename T>
inline constexpr Imm64::Imm64(T* t) :
    Imm {(uint64_t)t} {
}

inline Imm64::Imm64(const Function& f) : 
    Imm {(uint64_t)f.buffer_} {
}

inline constexpr bool Imm64::check() {
  return true;
}

inline constexpr bool Zero::check() {
  return val_ == 0;
}

inline constexpr Zero::Zero() : 
    Imm8 {0} {
}

inline constexpr bool One::check() {
  return val_ == 1;
}

inline constexpr One::One() : 
    Imm8 {1} {
}

inline constexpr bool Three::check() {
  return val_ == 3;
}

inline constexpr Three::Three() : 
    Imm8 {3} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Imm>::operator()(const x64asm::Imm& i) const {
  return i.hash();
}

inline void swap(x64asm::Imm& lhs, x64asm::Imm& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Imm& i) {
  return i.write_att(os);
}

} // namespace std

#endif
