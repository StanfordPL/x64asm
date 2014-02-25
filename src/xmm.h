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

#ifndef X64ASM_SRC_XMM_H
#define X64ASM_SRC_XMM_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** An XMM register. The 128-bit XMM registers are: XMM0 through XMM7; XMM8
    through XMM15 are available using REX.R in 64-bit mode.
*/
class Xmm : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Xmm(const Xmm& rhs);
    /** Move constructor. */
    Xmm(Xmm&& rhs);
    /** Copy assignment operator. */
    Xmm& operator=(const Xmm& rhs);
    /** Move assignment operator. */
    Xmm& operator=(Xmm&& rhs);

    /** Returns true if this xmm register is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Xmm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Xmm& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Xmm& rhs);

    /** Conversion based on underlying value. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Xmm& rhs);

    /** Writes this xmm register to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Xmm(uint64_t val);
};

/** The XMM register XMM0. */
class Xmm0 : public Xmm {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Returns true if this xmm register is %xmm0. */
    constexpr bool check();

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Xmm0();
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Xmm> {
  size_t operator()(const x64asm::Xmm& x) const;
};

/** STL swap overload. */
void swap(x64asm::Xmm& lhs, x64asm::Xmm& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Xmm& x);

} // namespace std

namespace x64asm {

inline Xmm::Xmm(const Xmm& rhs) : Operand{0,0} {
  val_ = rhs.val_;
}

inline Xmm::Xmm(Xmm&& rhs) {
  val_ = rhs.val_;
}

inline Xmm& Xmm::operator=(const Xmm& rhs) {
  Xmm(rhs).swap(*this);
  return *this;
}

inline Xmm& Xmm::operator=(Xmm&& rhs) {
  Xmm(std::move(rhs)).swap(*this);
  return *this;
}

inline constexpr bool Xmm::check() {
  return val_ < 16;
}

inline constexpr bool Xmm::operator<(const Xmm& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Xmm::operator==(const Xmm& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Xmm::operator!=(const Xmm& rhs) {
  return val_ != rhs.val_;
}

inline constexpr Xmm::operator uint64_t() {
  return val_;
}

inline std::ostream& Xmm::write_att(std::ostream& os) const {
  assert(check());
  return (os << "%xmm" << std::dec << val_);
}

inline constexpr size_t Xmm::hash() {
  return val_;
}

inline void Xmm::swap(Xmm& rhs) {
  std::swap(val_, rhs.val_);
}

inline constexpr Xmm::Xmm(uint64_t val) : Operand{val} {
}

inline constexpr bool Xmm0::check() {
  return val_ == 0;
}

inline constexpr Xmm0::Xmm0() : 
    Xmm {0} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Xmm>::operator()(const x64asm::Xmm& x) const {
  return x.hash();
}

inline void swap(x64asm::Xmm& lhs, x64asm::Xmm& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Xmm& x) {
  return x.write_att(os);
}

} // namespace std

#endif
