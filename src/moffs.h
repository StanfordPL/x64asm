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

#ifndef X64ASM_SRC_MOFFS_H
#define X64ASM_SRC_MOFFS_H

#include <iostream>

#include "src/operand.h"
#include "src/sreg.h"
#include "src/imm.h"

namespace x64asm {

/** A simple memory variable. This is the only operand type to use both
    underlying value variables.
*/
class Moffs : public Operand {
  private:
    /** Constant bit masks used to represent absent operands. */
    enum class Null : uint64_t {
      SEG = 0x7
    };

    /** Bit mask representing where operand are stored in underlying value. */
    enum class Mask : uint64_t {
      SEG = 0xf
    };

  public:
    /** Copy constructor. */
    Moffs(const Moffs& rhs);
    /** Move constructor. */
    Moffs(Moffs&& rhs);
    /** Copy assignment operator. */
    Moffs& operator=(const Moffs& rhs);
    /** Move assignment operator. */
    Moffs& operator=(Moffs&& rhs);

    /** Returns true if this moffs contains a segment register. */
    constexpr bool contains_seg();

    /** Returns this moffs' segment register; undefined if absent. */
    constexpr Sreg get_seg();
    /** Returns this moffs' offset. */
    constexpr Imm64 get_offset();

    /** Sets this moffs' segment register. */
    void set_seg(const Sreg& seg);
    /** Sets this moffs' offset. */
    void set_offset(const Imm64& offset);

    /** Removes the segment register from this moffs. */
    void clear_seg();

    /** Returns true if this moffs contains a well-formed segment register. */
    constexpr bool check();

    /** Comparison based on on val_. */
    bool operator<(const Moffs& rhs) const;
    /** Comparison based on on val_. */
    bool operator==(const Moffs& rhs) const;
    /** Comparison based on on val_. */
    bool operator!=(const Moffs& rhs) const;

    /** STL-compliant hash. */
    constexpr size_t hash();
    /** STL-compliant swap. */
    void swap(Moffs& rhs);

    /** Writes this moffs to an ostream using at&t syntax. */
    std::ostream& write_att(std::ostream& os) const;

  protected:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs(const Sreg& seg, const Imm64& offset);

    /** Create a moffs using offset form. */
    constexpr Moffs(const Imm64& offset);
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs8(const Sreg& seg, const Imm64& offset);
    /** Create a moffs using offset form. */
    constexpr Moffs8(const Imm64& offset);
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs16(const Sreg& seg, const Imm64& offset);
    /** Create a moffs using offset form. */
    constexpr Moffs16(const Imm64& offset);
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs32(const Sreg& seg, const Imm64& offset);
    /** Create a moffs using offset form. */
    constexpr Moffs32(const Imm64& offset);
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs64(const Sreg& seg, const Imm64& offset);
    /** Create a moffs using offset form. */
    constexpr Moffs64(const Imm64& offset);
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Moffs> {
  size_t operator()(const x64asm::Moffs& m) const;
};

/** STL swap overload. */
void swap(x64asm::Moffs& lhs, x64asm::Moffs& rhs);

/** I/O overload. */
ostream& operator<<(ostream& os, const x64asm::Moffs& m);

} // namespace std

namespace x64asm {

inline Moffs::Moffs(const Moffs& rhs) {
  val_ = rhs.val_;
  val2_ = rhs.val2_;
}

inline Moffs::Moffs(Moffs&& rhs) {
  val_ = rhs.val_;
  val2_ = rhs.val2_;
}

inline Moffs& Moffs::operator=(const Moffs& rhs) {
  Moffs(rhs).swap(*this);
  return *this;
}

inline Moffs& Moffs::operator=(Moffs&& rhs) {
  Moffs(std::move(rhs)).swap(*this);
  return *this;
}

inline constexpr bool Moffs::contains_seg() {
  return (val2_ & (uint64_t)Mask::SEG) != (uint64_t)Null::SEG;
}

inline constexpr Sreg Moffs::get_seg() {
  return Sreg {val2_};
}

inline constexpr Imm64 Moffs::get_offset() {
  return Imm64 {val_};
}

inline void Moffs::set_seg(const Sreg& seg) {
  val2_ = seg.val_;
}

inline void Moffs::set_offset(const Imm64& offset) {
  val_ = (uint64_t)offset;
}

inline void Moffs::clear_seg() {
  set_seg(Sreg {(uint64_t)Null::SEG});
}

inline constexpr bool Moffs::check() {
  return (!contains_seg() || get_seg().check()) && get_offset().check();
}

inline bool Moffs::operator<(const Moffs& rhs) const {
  return std::make_pair(val_, val2_) < std::make_pair(rhs.val_, rhs.val2_);
}

inline bool Moffs::operator==(const Moffs& rhs) const {
  return std::make_pair(val_, val2_) == std::make_pair(rhs.val_, rhs.val2_);
}

inline bool Moffs::operator!=(const Moffs& rhs) const {
  return !(*this == rhs);
}

inline constexpr size_t Moffs::hash() {
  return val_ ^ val2_;
}

inline void Moffs::swap(Moffs& rhs) {
  std::swap(val_, rhs.val_);
  std::swap(val2_, rhs.val2_);
}

inline std::ostream& Moffs::write_att(std::ostream& os) const {
  if (contains_seg()) {
    get_seg().write_att(os);
    os << ":";
  }
  os << "0x" << std::noshowbase << std::hex << (uint64_t)get_offset();
  return os;
}

inline constexpr Moffs::Moffs(const Sreg& seg, const Imm64& offset) :
    Operand {(uint64_t)offset, seg.val_} {
}

inline constexpr Moffs::Moffs(const Imm64& offset) :
    Operand {(uint64_t)offset, (uint64_t)Null::SEG} {
}

inline constexpr Moffs8::Moffs8(const Sreg& seg, const Imm64& offset) : 
    Moffs {seg, offset} {
}

inline constexpr Moffs8::Moffs8(const Imm64& offset) :
    Moffs {offset} {
}

inline constexpr Moffs16::Moffs16(const Sreg& seg, const Imm64& offset) : 
    Moffs {seg, offset} {
}

inline constexpr Moffs16::Moffs16(const Imm64& offset) :
    Moffs {offset} {
}

inline constexpr Moffs32::Moffs32(const Sreg& seg, const Imm64& offset) : 
    Moffs {seg, offset} {
}

inline constexpr Moffs32::Moffs32(const Imm64& offset) :
    Moffs {offset} {
}

inline constexpr Moffs64::Moffs64(const Sreg& seg, const Imm64& offset) : 
    Moffs {seg, offset} {
}

inline constexpr Moffs64::Moffs64(const Imm64& offset) :
    Moffs {offset} {
}

} // namespace x64asm

namespace std {

inline size_t hash<x64asm::Moffs>::operator()(const x64asm::Moffs& m) const {
  return m.hash();
}

inline void swap(x64asm::Moffs& lhs, x64asm::Moffs& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::Moffs& m) {
  return m.write_att(os);
}

} // namespace std

#endif
