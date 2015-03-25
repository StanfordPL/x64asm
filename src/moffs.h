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
  /** Value constants */
  static constexpr uint64_t seg_null_ = 0x7;
  /** Mask constants */
  static constexpr uint64_t seg_mask_ = 0x7;

public:
  /** Returns true if this moffs contains a segment register. */
  constexpr bool contains_seg() {
    return val2(seg_mask_) != seg_null_;
  }

  /** Returns this moffs' segment register; undefined if absent. */
  constexpr Sreg get_seg() {
    return {val2(seg_mask_)};
  }
  /** Returns this moffs' offset. */
  constexpr Imm64 get_offset() {
    return {val()};
  }

  /** Sets this moffs' segment register. */
  void set_seg(const Sreg& seg) {
    set_val2(seg, seg_mask_);
  }
  /** Sets this moffs' offset. */
  void set_offset(const Imm64& offset) {
    set_val(offset);
  }

  /** Removes the segment register from this moffs. */
  void clear_seg() {
    set_val2(seg_null_, seg_mask_);
  }

  /** Returns true if this moffs contains a well-formed segment register. */
  constexpr bool check() {
    return (!contains_seg() || get_seg().check()) && get_offset().check();
  }

  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this moffs to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const {
    const auto fmt = os.flags();
    if (contains_seg()) {
      get_seg().write_att(os);
      os << ":";
    }
    os << "0x" << std::noshowbase << std::hex << (uint64_t)get_offset();
    os.flags(fmt);
    return os;
  }

protected:
  /** Create a moffs using seg:offset form. */
  constexpr Moffs(Type t, const Sreg& seg, const Imm64& offset) :
    Operand(t, offset, seg) {
  }
  /** Create a moffs using offset form. */
  constexpr Moffs(Type t, const Imm64& offset) :
    Operand(t, offset, seg_null_) {
  }
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
public:
  /** Create a moffs using seg:offset form. */
  constexpr Moffs8(const Sreg& seg, const Imm64& offset) : Moffs(Type::MOFFS_8, seg, offset) {}
  /** Create a moffs using offset form. */
  constexpr Moffs8(const Imm64& offset) : Moffs(Type::MOFFS_8, offset) {}
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
public:
  /** Create a moffs using seg:offset form. */
  constexpr Moffs16(const Sreg& seg, const Imm64& offset) : Moffs(Type::MOFFS_16, seg, offset) {}
  /** Create a moffs using offset form. */
  constexpr Moffs16(const Imm64& offset) : Moffs(Type::MOFFS_16, offset) {}
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
public:
  /** Create a moffs using seg:offset form. */
  constexpr Moffs32(const Sreg& seg, const Imm64& offset) : Moffs(Type::MOFFS_32, seg, offset) {}
  /** Create a moffs using offset form. */
  constexpr Moffs32(const Imm64& offset) : Moffs(Type::MOFFS_32, offset) {}
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
public:
  /** Create a moffs using seg:offset form. */
  constexpr Moffs64(const Sreg& seg, const Imm64& offset) : Moffs(Type::MOFFS_64, seg, offset) {}
  /** Create a moffs using offset form. */
  constexpr Moffs64(const Imm64& offset) : Moffs(Type::MOFFS_64, offset) {}
};

} // namespace x64asm

namespace std {

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Moffs& m) {
  return m.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Moffs& m) {
  return m.write_att(os);
}

} // namespace std

#endif
