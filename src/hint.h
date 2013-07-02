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

#ifndef X64ASM_SRC_HINT_H
#define X64ASM_SRC_HINT_H

#include <cassert>
#include <iostream>

#include "src/operand.h"

namespace x64asm {

/** A taken/not-taken hint for conditional jumps. */
class Hint : public Operand {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Copy constructor. */
    Hint(const Hint& rhs);
    /** Move constructor. */
    Hint(Hint&& rhs);
    /** Copy assignment operator. */
    Hint& operator=(const Hint& rhs);
    /** Move assignment operator. */
    Hint& operator=(Hint&& rhs);

    /** Checks that this hint is well-formed. */
    constexpr bool check();

    /** Comparison based on val_. */
    constexpr bool operator==(const Hint& rhs);
    /** Comparison based on val_. */
    constexpr bool operator!=(const Hint& rhs);
    /** Comparison based on val_. */
    constexpr bool operator<(const Hint& rhs);

    /** Conversion based on val_. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Hint& rhs);

    /** Writes this hint to an ostream using (something like) at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Hint(uint64_t val);
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Hint> {
  size_t operator()(const x64asm::Hint& h) const;
};

/** STL swap overload. */
void swap(x64asm::Hint& lhs, x64asm::Hint& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Hint& h);

} // namespace std

namespace x64asm {

inline Hint::Hint(const Hint& rhs) {
  val_ = rhs.val_;
}

inline Hint::Hint(Hint&& rhs) {
  val_ = rhs.val_;
}

inline Hint& Hint::operator=(const Hint& rhs) {
  Hint(rhs).swap(*this);
}

inline Hint& Hint::operator=(Hint&& rhs) {
  Hint(std::move(rhs)).swap(*this);
}

inline constexpr bool Hint::check() {
  return val_ < 2;
}

inline constexpr bool Hint::operator==(const Hint& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Hint::operator!=(const Hint& rhs) {
  return !(*this == rhs);
}

inline constexpr bool Hint::operator<(const Hint& rhs) {
  return val_ < rhs.val_;
}

inline constexpr Hint::operator uint64_t() {
  return val_;
}

inline constexpr size_t Hint::hash() {
  return val_;
}

inline void Hint::swap(Hint& rhs) {
  std::swap(val_, rhs.val_);
}

inline std::ostream& Hint::write_att(std::ostream& os) const {
  assert(check());
  return (os << (val_ == 0 ? "<taken>" : "<not taken>"));
}

inline constexpr Hint::Hint(uint64_t val) : Operand {val} { 
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Hint>::operator()(const x64asm::Hint& h) const {
  return h.hash();
}

inline void swap(x64asm::Hint& lhs, x64asm::Hint& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Hint& h) {
  return h.write_att(os);
}

} // namespace std

#endif
