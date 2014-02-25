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

#ifndef X64ASM_SRC_REL_H
#define X64ASM_SRC_REL_H

#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A relative address. */
class Rel : public Operand {
  public:
    /** Copy constructor. */
    Rel(const Rel& rhs);
    /** Move constructor. */
    Rel(Rel&& rhs);
    /** Copy assignment operator. */
    Rel& operator=(const Rel& rhs);
    /** Move assignment operator. */
    Rel& operator=(Rel&& rhs);

    /** Comparison based on on val_. */
    constexpr bool operator<(const Rel& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Rel& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Rel& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Rel& rhs);

    /** Writes this rel to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Rel(uint64_t val);
};

/** A relative address in the range from 128 bytes before the end of the
    instruction to 127 bytes after the end of the instruction.
*/
class Rel8 : public Rel {
  public:
    /** Creates an 8-bit relative offset. */
    constexpr Rel8(int8_t val);

    /** Checks that this offset fits in 8 bits. */
    constexpr bool check();
};

/** A relative address within the same code segment as the instruction
    assembled. The rel32 symbol applies to instructions with an
    operand-size attribute of 32 bits.
*/
class Rel32 : public Rel {
  public:
    /** Creates a 32-bit relative offset. */
    constexpr Rel32(int64_t val);

    /** Checks that this offset value fits in 32-bits. */
    constexpr bool check();
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Rel> {
  size_t operator()(const x64asm::Rel& r) const;
};

/** STL swap overload. */
void swap(x64asm::Rel& lhs, x64asm::Rel& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Rel& r);

} // namespace std

namespace x64asm {

inline Rel::Rel(const Rel& rhs) : Operand{0,0} {
  val_ = rhs.val_;
}

inline Rel::Rel(Rel&& rhs) {
  val_ = rhs.val_;
}

inline Rel& Rel::operator=(const Rel& rhs) {
  Rel(rhs).swap(*this);
  return *this;
}

inline Rel& Rel::operator=(Rel&& rhs) {
  Rel(std::move(rhs)).swap(*this);
  return *this;
}

inline constexpr bool Rel::operator<(const Rel& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Rel::operator==(const Rel& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Rel::operator!=(const Rel& rhs) {
  return val_ != rhs.val_;
}

inline constexpr Rel::operator uint64_t() {
  return val_;
}

inline std::ostream& Rel::write_att(std::ostream& os) const {
  return (os << std::hex << std::showbase << val_);
}

inline constexpr size_t Rel::hash() {
  return val_;
}

inline void Rel::swap(Rel& rhs) {
  std::swap(val_, rhs.val_);
}

inline constexpr Rel::Rel(uint64_t val) : Operand{val} {
}

inline constexpr Rel8::Rel8(int8_t val) :
    Rel {(uint64_t)val} {
}

inline constexpr bool Rel8::check() {
  return ((val_>>8) == 0x0ul) || ((val_>>8) == 0xfffffffffffffful);
}

inline constexpr Rel32::Rel32(int64_t val) :
    Rel {(uint64_t)val} {
}
    
inline constexpr bool Rel32::check() {
  return ((val_>>32) == 0x0ul) || ((val_>>32) == 0xfffffffful);
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Rel>::operator()(const x64asm::Rel& r) const {
  return r.hash();
}

inline void swap(x64asm::Rel& lhs, x64asm::Rel& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Rel& r) {
  return r.write_att(os);
}

} // namespace std

#endif
