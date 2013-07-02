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
    /** Copy constructor. */
    Modifier(const Modifier& rhs);
    /** Move constructor. */
    Modifier(Modifier&& rhs);
    /** Copy assignment operator. */
    Modifier& operator=(const Modifier& rhs);
    /** Move assignment operator. */
    Modifier& operator=(Modifier&& rhs);

    /** Returns true if this modifier is well-formed. */
    constexpr bool check();

    /** Comparison based on on val_. */
    constexpr bool operator<(const Modifier& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator==(const Modifier& rhs);
    /** Comparison based on on val_. */
    constexpr bool operator!=(const Modifier& rhs);

    /** Conversion based on val_. */
    constexpr operator uint64_t();

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Modifier& rhs);

  protected:
    /** Direct access to this constructor is disallowed. */
    constexpr Modifier(uint64_t val);
};

/** The 32-bit memory address override prefix: 0x66. */
class Pref66 : public Modifier {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Writes this modifier to an ostream using (something like) at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Pref66();
};

/** The REX.w prefix: 0x48. */
class PrefRexW : public Modifier {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Writes this modifier to an ostream using (something like) at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr PrefRexW();
};

/** Far instruction variant. */
class Far : public Modifier {
    // Needs access to constructor.
    friend class Constants;

  public:
    /** Writes this modifier to an ostream using (something like) at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  private:
    /** Direct access to this constructor is disallowed. */
    constexpr Far();
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Modifier> {
  size_t operator()(const x64asm::Modifier& m) const;
};

/** STL swap overload. */
void swap(x64asm::Modifier& lhs, x64asm::Modifier& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Pref66& p);
/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::PrefRexW& p);
/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Far& f);

} // namespace std

namespace x64asm {

inline Modifier::Modifier(const Modifier& rhs) {
  val_ = rhs.val_;
}

inline Modifier::Modifier(Modifier&& rhs) {
  val_ = rhs.val_;
}

inline Modifier& Modifier::operator=(const Modifier& rhs) {
  Modifier(rhs).swap(*this);
}

inline Modifier& Modifier::operator=(Modifier&& rhs) {
  Modifier(std::move(rhs)).swap(*this);
}

inline constexpr bool Modifier::check() {
  return val_ == 0;
}

inline constexpr bool Modifier::operator<(const Modifier& rhs) {
  return val_ < rhs.val_;
}

inline constexpr bool Modifier::operator==(const Modifier& rhs) {
  return val_ == rhs.val_;
}

inline constexpr bool Modifier::operator!=(const Modifier& rhs) {
  return val_ != rhs.val_;
}

inline constexpr Modifier::operator uint64_t() {
  return val_;
}

inline constexpr size_t Modifier::hash() {
  return val_;
}

inline void Modifier::swap(Modifier& rhs) {
  std::swap(val_, rhs.val_);
}

inline constexpr Modifier::Modifier(uint64_t val) : 
    Operand {val} {
}

inline std::ostream& Pref66::write_att(std::ostream& os) const {
  return (os << "<66>");
}

inline constexpr Pref66::Pref66() :
    Modifier {0} {
}

inline std::ostream& PrefRexW::write_att(std::ostream& os) const {
  return (os << "<rexw>");
}

inline constexpr PrefRexW::PrefRexW() :
    Modifier {0} {
}

inline std::ostream& Far::write_att(std::ostream& os) const {
  return (os << "<far>");
}

inline constexpr Far::Far() :
    Modifier {0} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Modifier>::operator()(const x64asm::Modifier& m) const {
  return m.hash();
}

inline void swap(x64asm::Modifier& lhs, x64asm::Modifier& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Pref66& p) {
  return p.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::PrefRexW& p) {
  return p.write_att(os);
}

inline ostream& operator<<(ostream& os, const x64asm::Far& f) {
  return f.write_att(os);
}

} // namespace std

#endif
