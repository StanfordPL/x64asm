/*
Copyright 2103 eric schkufza

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
    /** Returns true if this moffs contains a segment register. */
    constexpr bool contains_seg() {
      return (val2_ & (uint64_t)Mask::SEG) != (uint64_t)Null::SEG;
    }

    /** Returns this moffs' segment register; undefined if absent. */
    constexpr Sreg get_seg() {
      return Sreg {val2_};
    }

    /** Returns this moffs' offset. */
    constexpr Imm64 get_offset() {
      return Imm64 {val_};
    }

    /** Sets this moffs' segment register. */
    void set_seg(const Sreg& seg) {
      val2_ = seg.val_;
    }

    /** Sets this moffs' offset. */
    void set_offset(const Imm64& offset) {
      val_ = offset.val_;
    }

    /** Removes the segment register from this moffs. */
    void clear_seg() {
      set_seg(Sreg {(uint64_t)Null::SEG});
    }

    /** Returns true if this moffs contains a well-formed segment register. */
    constexpr bool check() {
      return (!contains_seg() || get_seg().check()) && get_offset().check();
    }

    /** Comparison based on underlying value. */
    constexpr bool operator<(const Moffs& rhs) {
      return val_ == rhs.val_ ? val2_ < rhs.val2_ : val_ < rhs.val_;
    }

    /** Comparison based on underlying value. */
    constexpr bool operator==(const Moffs& rhs) {
      return val_ == rhs.val_ && val2_ == rhs.val2_;
    }

    /** Writes this moffs to an ostream using at&t syntax. */
    void write_att(std::ostream& os) const;
    /** Writes this moffs to an ostream using intel syntax. */
    void write_intel(std::ostream& os) const;

  protected:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs(const Sreg& seg, const Imm64& offset)
      : Operand {offset.val_, seg.val_} {
    }

    /** Create a moffs using offset form. */
    constexpr Moffs(const Imm64& offset)
      : Operand {offset.val_, (uint64_t)Null::SEG} {
    }
};

/** A simple memory variable (memory offset) of type byte. */
class Moffs8 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs8(const Sreg& seg, const Imm64& offset)
      : Moffs {seg, offset} {
    }

    /** Create a moffs using offset form. */
    constexpr Moffs8(const Imm64& offset)
      : Moffs {offset} {
    }
};

/** A simple memory variable (memory offset) of type word. */
class Moffs16 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs16(const Sreg& seg, const Imm64& offset)
      : Moffs {seg, offset} {
    }

    /** Create a moffs using offset form. */
    constexpr Moffs16(const Imm64& offset)
      : Moffs {offset} {
    }
};

/** A simple memory variable (memory offset) of type doubleword. */
class Moffs32 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs32(const Sreg& seg, const Imm64& offset)
      : Moffs {seg, offset} {
    }

    /** Create a moffs using offset form. */
    constexpr Moffs32(const Imm64& offset)
      : Moffs {offset} {
    }
};

/** A simple memory variable (memory offset) of type quadword. */
class Moffs64 : public Moffs {
  public:
    /** Create a moffs using seg:offset form. */
    constexpr Moffs64(const Sreg& seg, const Imm64& offset)
      : Moffs {seg, offset} {
    }

    /** Create a moffs using offset form. */
    constexpr Moffs64(const Imm64& offset)
      : Moffs {offset} {
    }
};

} // namespace x64asm

#endif
