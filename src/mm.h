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

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** An MMX register. The 64-bit MMX registers are: MM0 through MM7. */
class Mm : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Mm(const Mm& rhs);
    /** Move constructor. */
    Mm(Mm&& rhs);
    /** Copy assignment operator. */
    Mm& operator=(const Mm& rhs);
    /** Move assignment operator. */
    Mm& operator=(Mm&& rhs);

    /** Returns true if this xmm register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Mm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Mm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Mm& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Mm& rhs);

    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Mm(uint64_t val);
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Mm> {
  size_t operator()(const x64asm::Mm& m) const;
};

/** STL swap overload. */
void swap(x64asm::Mm& lhs, x64asm::Mm& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Mm& m);

} // namespace std

namespace x64asm {

inline Mm::Mm(const Mm& rhs) : Operand{0,0} {
  val_ = rhs.val_;
}

inline Mm::Mm(Mm&& rhs) {
  val_ = rhs.val_;
}

inline Mm& Mm::operator=(const Mm& rhs) {
  Mm(rhs).swap(*this);
  return *this;
}

inline Mm& Mm::operator=(Mm&& rhs) {
  Mm(std::move(rhs)).swap(*this);
  return *this;
}

inline constexpr bool Mm::check() {
  return val_ < 8;
}

inline constexpr bool Mm::operator<(const Mm& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Mm::operator==(const Mm& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Mm::operator!=(const Mm& rhs) {
  return val_ != rhs.val_;
}

inline constexpr Mm::operator uint64_t() {
  return val_;
}

inline std::ostream& Mm::write_att(std::ostream& os) const {
  assert(check());
  return (os << "%mm" << std::dec << val_);
}

inline constexpr size_t Mm::hash() {
  return val_;
}

inline void Mm::swap(Mm& rhs) {
  std::swap(val_, rhs.val_);
}

inline constexpr Mm::Mm(uint64_t val) : Operand{val} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Mm>::operator()(const x64asm::Mm& m) const {
  return m.hash();
}

inline void swap(x64asm::Mm& lhs, x64asm::Mm& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Mm& m) {
  return m.write_att(os);
}

} // namespace std

#endif

