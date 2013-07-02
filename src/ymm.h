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

#ifndef X64ASM_SRC_YMM_H
#define X64ASM_SRC_YMM_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A YMM register. The 256-bit YMM registers are: YMM0 through YMM7; YMM8
    through YMM15 are available using REX.R in 64-bit mode.
*/
class Ymm : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Ymm(const Ymm& rhs);
    /** Move constructor. */
    Ymm(Ymm&& rhs);
    /** Copy assignment operator. */
    Ymm& operator=(const Ymm& rhs);
    /** Move assignment operator. */
    Ymm& operator=(Ymm&& rhs);

    /** Returns true if this xmm register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Ymm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Ymm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Ymm& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Ymm& rhs);

    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Ymm(uint64_t val);
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Ymm> {
  size_t operator()(const x64asm::Ymm& y) const;
};

/** STL swap overload. */
void swap(x64asm::Ymm& lhs, x64asm::Ymm& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Ymm& y);

} // namespace std

namespace x64asm {

inline Ymm::Ymm(const Ymm& rhs) {
  val_ = rhs.val_;
}

inline Ymm::Ymm(Ymm&& rhs) {
  val_ = rhs.val_;
}

inline Ymm& Ymm::operator=(const Ymm& rhs) {
  Ymm(rhs).swap(*this);
}

inline Ymm& Ymm::operator=(Ymm&& rhs) {
  Ymm(std::move(rhs)).swap(*this);
}

inline constexpr bool Ymm::check() {
  return val_ < 16;
}

inline constexpr bool Ymm::operator<(const Ymm& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Ymm::operator==(const Ymm& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Ymm::operator!=(const Ymm& rhs) {
  return val_ != rhs.val_;
}

inline constexpr Ymm::operator uint64_t() {
  return val_;
}

inline std::ostream& Ymm::write_att(std::ostream& os) const {
  assert(check());
  return (os << "%ymm" << std::dec << val_);
}

inline constexpr size_t Ymm::hash() {
  return val_;
}

inline void Ymm::swap(Ymm& rhs) {
  std::swap(val_, rhs.val_);
}

inline constexpr Ymm::Ymm(uint64_t val) : Operand{val} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Ymm>::operator()(const x64asm::Ymm& y) const {
  return y.hash();
}

inline void swap(x64asm::Ymm& lhs, x64asm::Ymm& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Ymm& y) {
  return y.write_att(os);
}

} // namespace std

#endif
