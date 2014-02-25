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

#ifndef X64ASM_SRC_ZMM_H
#define X64ASM_SRC_ZMM_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A ZMM register. The 512-bit ZMM registers are: ZMM0 through ZMM7; ZMM8
    through ZMM15 are available using REX.R in 64-bit mode.
*/
class Zmm : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Zmm(const Zmm& rhs);
    /** Move constructor. */
    Zmm(Zmm&& rhs);
    /** Copy assignment operator. */
    Zmm& operator=(const Zmm& rhs);
    /** Move assignment operator. */
    Zmm& operator=(Zmm&& rhs);

    /** Returns true if this xmm register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Zmm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Zmm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Zmm& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Zmm& rhs);

    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Zmm(uint64_t val);
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Zmm> {
  size_t operator()(const x64asm::Zmm& z) const;
};

/** STL swap overload. */
void swap(x64asm::Zmm& lhs, x64asm::Zmm& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Zmm& z);

} // namespace std

namespace x64asm {

inline Zmm::Zmm(const Zmm& rhs) : Operand{0,0} {
  val_ = rhs.val_;
}

inline Zmm::Zmm(Zmm&& rhs) {
  val_ = rhs.val_;
}

inline Zmm& Zmm::operator=(const Zmm& rhs) {
  Zmm(rhs).swap(*this);
  return *this;
}

inline Zmm& Zmm::operator=(Zmm&& rhs) {
  Zmm(std::move(rhs)).swap(*this);
  return *this;
}

inline constexpr bool Zmm::check() {
  return val_ < 16;
}

inline constexpr bool Zmm::operator<(const Zmm& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Zmm::operator==(const Zmm& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Zmm::operator!=(const Zmm& rhs) {
  return val_ != rhs.val_;
}

inline constexpr Zmm::operator uint64_t() {
  return val_;
}

inline constexpr size_t Zmm::hash() {
  return val_;
}

inline void Zmm::swap(Zmm& rhs) {
  std::swap(val_, rhs.val_);
}

inline std::ostream& Zmm::write_att(std::ostream& os) const {
  assert(check());
  return (os << "%zmm" << std::dec << val_);
}

inline constexpr Zmm::Zmm(uint64_t val) : Operand{val} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Zmm>::operator()(const x64asm::Zmm& z) const {
  return z.hash();
}

inline void swap(x64asm::Zmm& lhs, x64asm::Zmm& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Zmm& z) {
  return z.write_att(os);
}

} // namespace std

#endif
