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

#ifndef X64ASM_SRC_REG_SET_H
#define X64ASM_SRC_REG_SET_H

#include <algorithm>
#include <iostream>

#include "src/env_bits.h"
#include "src/env_reg.h"
#include "src/m.h"
#include "src/mm.h"
#include "src/moffs.h"
#include "src/r.h"
#include "src/sreg.h"
#include "src/st.h"
#include "src/xmm.h"
#include "src/ymm.h"

namespace x64asm {

/** A compact implementation of a bit set for registers. */
class RegSet {
    friend class Instruction;
  private:
    /** Per register type position masks. */
    enum class Mask : uint64_t {
      // Group 1
      LOW     = 0x0000000000000001,
      HIGH    = 0x0000000000010000,
      WORD    = 0x0000000000010001,
      DOUBLE  = 0x0000000100010001,
      QUAD    = 0x0001000100010001,

      // Group 2
      XMM     = 0x0000000000000001,
      YMM     = 0x0000000000010001,
      MM      = 0x0000000100000000,
      ST      = 0x0000010000000000,
      SREG    = 0x0001000000000000,
      DATA    = 0x0100000000000000,
      INSTR   = 0x0200000000000000,
      OPCODE  = 0x0400000000000000,
      RIP     = 0x0800000000000000,

      // Group 3
      EFLAG   = 0x0000000000000001,
      CONTROL = 0x0000000100000000,
      STATUS  = 0x0001000000000000,

      // Group 4
      TAG     = 0x0000000000000001,
      MXCSR   = 0x0000000000010000,

      // Top and Bottom
      EMPTY   = 0x0000000000000000,
      UNIV1   = 0xffffffffffffffff,
      UNIV2   = 0x0f3fffffffffffff,
      UNIV3   = 0xe7ff1a3f003f6fd5,
      UNIV4   = 0x00000000dfffaaaa,
    };

    /** Creates a register set from four bit masks. */
    constexpr RegSet(uint64_t g1, uint64_t g2, uint64_t g3, uint64_t g4)
      : group1_ {g1}, group2_ {g2}, group3_ {g3}, group4_ {g4} {
    }

    /** Creates a register set from four position masks. */
    constexpr RegSet(Mask g1, Mask g2, Mask g3, Mask g4)
      : group1_ {(uint64_t)g1}, group2_ {(uint64_t)g2}, group3_ {(uint64_t)g3},
    group4_ {(uint64_t)g4}	{
    }

  public:
    /** Creates an empty register set. */
    constexpr RegSet()
      : group1_ {(uint64_t)Mask::EMPTY}, group2_ {(uint64_t)Mask::EMPTY},
    group3_ {(uint64_t)Mask::EMPTY}, group4_ {(uint64_t)Mask::EMPTY} {
    }

    /** Creates an empty regiter set. */
    static constexpr RegSet empty() {
      return RegSet {Mask::EMPTY, Mask::EMPTY, Mask::EMPTY, Mask::EMPTY};
    }

    /** Creates a full register set. */
    static constexpr RegSet universe() {
      return RegSet {Mask::UNIV1, Mask::UNIV2, Mask::UNIV3, Mask::UNIV4};
    }

    /** Set inversion. */
    constexpr RegSet operator~() {
      return RegSet {~group1_, ~group2_, ~group3_, ~group4_};
    }

    /** Set intersection. */
    constexpr RegSet operator&(const RegSet& rhs) {
      return RegSet {group1_ & rhs.group1_, group2_ & rhs.group2_,
                     group3_ & rhs.group3_, group4_ & rhs.group4_
                    };
    }

    /** Set union. */
    constexpr RegSet operator|(const RegSet& rhs) {
      return RegSet {group1_ | rhs.group1_, group2_ | rhs.group2_,
                     group3_ | rhs.group3_, group4_ | rhs.group4_
                    };
    }

    /** Set difference. */
    constexpr RegSet operator-(const RegSet& rhs) {
      return RegSet {group1_& ~rhs.group1_, group2_& ~rhs.group2_,
                     group3_& ~rhs.group3_, group4_& ~rhs.group4_
                    };
    }

    /** Set intersection. */
    RegSet& operator&=(const RegSet& rhs) {
      group1_ &= rhs.group1_;
      group2_ &= rhs.group2_;
      group3_ &= rhs.group3_;
      group4_ &= rhs.group4_;
      return *this;
    }

    /** Set union. */
    RegSet& operator|=(const RegSet& rhs) {
      group1_ |= rhs.group1_;
      group2_ |= rhs.group2_;
      group3_ |= rhs.group3_;
      group4_ |= rhs.group4_;
      return *this;
    }

    /** Set difference. */
    RegSet& operator-=(const RegSet& rhs) {
      group1_ &= ~rhs.group1_;
      group2_ &= ~rhs.group2_;
      group3_ &= ~rhs.group3_;
      group4_ &= ~rhs.group4_;
      return *this;
    }

    /** Set equality. */
    constexpr bool operator==(const RegSet& rhs) {
      return group1_ == rhs.group1_ && group2_ == rhs.group2_ &&
             group3_ == rhs.group3_ && group4_ == rhs.group4_;
    }

    /** Set inequality. */
    constexpr bool operator!=(const RegSet& rhs) {
      return !(*this == rhs);
    }

    /** Insert a low register. */
    constexpr RegSet operator+(Rl rhs) {
      return RegSet {group1_ | ((uint64_t) Mask::LOW << rhs.val_), group2_,
                     group3_, group4_
                    };
    }

    /** Insert a high register. */
    constexpr RegSet operator+(Rh rhs) {
      return RegSet {group1_ | ((uint64_t) Mask::HIGH << (rhs.val_ - 4)), group2_,
                     group3_, group4_
                    };
    }

    /** Insert a byte register. */
    constexpr RegSet operator+(Rb rhs) {
      return RegSet {group1_ | ((uint64_t) Mask::LOW << rhs.val_), group2_,
                     group3_, group4_
                    };
    }

    /** Insert a word register. */
    constexpr RegSet operator+(R16 rhs) {
      return RegSet {group1_ | ((uint64_t) Mask::WORD << rhs.val_), group2_,
                     group3_, group4_
                    };
    }

    /** Insert a double register. */
    constexpr RegSet operator+(R32 rhs) {
      return RegSet {group1_ | ((uint64_t) Mask::DOUBLE << rhs.val_), group2_,
                     group3_, group4_
                    };
    }

    /** Insert a quad register. */
    constexpr RegSet operator+(R64 rhs) {
      return RegSet {group1_ | ((uint64_t) Mask::QUAD << rhs.val_), group2_,
                     group3_, group4_
                    };
    }

    /** Insert an xmm register. */
    constexpr RegSet operator+(Xmm rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::XMM << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert a ymm register. */
    constexpr RegSet operator+(Ymm rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::YMM << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert an mmx register. */
    constexpr RegSet operator+(Mm rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::MM << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert a floating point stack register. */
    constexpr RegSet operator+(St rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::ST << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert a segment register. */
    constexpr RegSet operator+(Sreg rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::SREG << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert an environment register. */
    constexpr RegSet operator+(FpuData rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::DATA << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert an environment register. */
    constexpr RegSet operator+(FpuInstruction rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::INSTR << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert an environment register. */
    constexpr RegSet operator+(FpuOpcode rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::OPCODE << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert an environment register. */
    constexpr RegSet operator+(Rip rhs) {
      return RegSet {group1_, group2_ | ((uint64_t) Mask::RIP << rhs.val_),
                     group3_, group4_
                    };
    }

    /** Insert environment bits. */
    constexpr RegSet operator+(Eflags rhs) {
      return RegSet {group1_, group2_,
                     group3_	| ((uint64_t) Mask::EFLAG << rhs.index()), group4_
                    };
    }

    /** Insert environment bits. */
    constexpr RegSet operator+(FpuControl rhs) {
      return RegSet {group1_, group2_,
                     group3_	| ((uint64_t) Mask::CONTROL << rhs.index()), group4_
                    };
    }

    /** Insert environment bits. */
    constexpr RegSet operator+(FpuStatus rhs) {
      return RegSet {group1_, group2_,
                     group3_	| ((uint64_t) Mask::STATUS << rhs.index()), group4_
                    };
    }

    /** Insert environment bits. */
    constexpr RegSet operator+(FpuTag rhs) {
      return RegSet {group1_, group2_,
                     group3_, group4_ | ((uint64_t) Mask::TAG << rhs.index())
                    };
    }

    /** Insert environment bits. */
    constexpr RegSet operator+(Mxcsr rhs) {
      return RegSet {group1_, group2_,
                     group3_, group4_ | ((uint64_t) Mask::MXCSR << rhs.index())
                    };
    }

    /** Insert a memory operand. */
    RegSet operator+(const M& rhs) const {
      auto ret = *this;
      return ret += rhs;
    }

    /** Insert a moffs operand. */
    RegSet operator+(const Moffs& rhs) const {
      auto ret = *this;
      return ret += rhs;
    }

    /** Insert a low register. */
    RegSet& operator+=(Rl rhs) {
      group1_ |= ((uint64_t) Mask::LOW << rhs.val_);
      return *this;
    }

    /** Insert a high register. */
    RegSet& operator+=(Rh rhs) {
      group1_ |= ((uint64_t) Mask::HIGH << (rhs.val_ - 4));
      return *this;
    }

    /** Insert a byte register. */
    RegSet& operator+=(Rb rhs) {
      group1_ |= ((uint64_t) Mask::LOW << rhs.val_);
      return *this;
    }

    /** Insert a word register. */
    RegSet& operator+=(R16 rhs) {
      group1_ |= ((uint64_t) Mask::WORD << rhs.val_);
      return *this;
    }

    /** Insert a double register. */
    RegSet& operator+=(R32 rhs) {
      group1_ |= ((uint64_t) Mask::DOUBLE << rhs.val_);
      return *this;
    }

    /** Insert a quad register. */
    RegSet& operator+=(R64 rhs) {
      group1_ |= ((uint64_t) Mask::QUAD << rhs.val_);
      return *this;
    }

    /** Insert an xmm register. */
    RegSet& operator+=(Xmm rhs) {
      group2_ |= ((uint64_t) Mask::XMM << rhs.val_);
      return *this;
    }

    /** Insert a ymm register. */
    RegSet& operator+=(Ymm rhs) {
      group2_ |= ((uint64_t) Mask::YMM << rhs.val_);
      return *this;
    }

    /** Insert an mmx register. */
    RegSet& operator+=(Mm rhs) {
      group2_ |= ((uint64_t) Mask::MM << rhs.val_);
      return *this;
    }

    /** Insert a floating point stack register. */
    RegSet& operator+=(St rhs) {
      group2_ |= ((uint64_t) Mask::ST << rhs.val_);
      return *this;
    }

    /** Insert a segment register. */
    RegSet& operator+=(Sreg rhs) {
      group2_ |= ((uint64_t) Mask::SREG << rhs.val_);
      return *this;
    }

    /** Insert an environment register. */
    RegSet& operator+=(FpuData rhs) {
      group2_ |= ((uint64_t) Mask::DATA << rhs.val_);
      return *this;
    }

    /** Insert an environment register. */
    RegSet& operator+=(FpuInstruction rhs) {
      group2_ |= ((uint64_t) Mask::INSTR << rhs.val_);
      return *this;
    }

    /** Insert an environment register. */
    RegSet& operator+=(FpuOpcode rhs) {
      group2_ |= ((uint64_t) Mask::OPCODE << rhs.val_);
      return *this;
    }

    /** Insert an environment register. */
    RegSet& operator+=(Rip rhs) {
      group2_ |= ((uint64_t) Mask::RIP << rhs.val_);
      return *this;
    }

    /** Insert environment bits. */
    RegSet& operator+=(Eflags rhs) {
      group3_ |= ((uint64_t) Mask::EFLAG << rhs.index());
      return *this;
    }

    /** Insert environment bits. */
    RegSet& operator+=(FpuControl rhs) {
      group3_ |= ((uint64_t) Mask::CONTROL << rhs.index());
      return *this;
    }

    /** Insert environment bits. */
    RegSet& operator+=(FpuStatus rhs) {
      group3_ |= ((uint64_t) Mask::STATUS << rhs.index());
      return *this;
    }

    /** Insert environment bits. */
    RegSet& operator+=(FpuTag rhs) {
      group4_ |= ((uint64_t) Mask::TAG << rhs.index());
      return *this;
    }

    /** Insert environment bits. */
    RegSet& operator+=(Mxcsr rhs) {
      group4_ |= ((uint64_t) Mask::MXCSR << rhs.index());
      return *this;
    }

    /** Insert a memory operand. */
    RegSet& operator+=(const M& rhs);

    /** Insert a moffs operand. */
    RegSet& operator+=(const Moffs& rhs) {
      if (rhs.contains_seg()) {
        *this += rhs.get_seg();
      }
      return *this;
    }

    /** Returns true if this set contains a low register. */
    constexpr bool contains(Rl rhs) {
      return ((group1_ >> rhs.val_) & (uint64_t)Mask::LOW) ==
             (uint64_t)Mask::LOW;
    }

    /** Returns true if this set contains a high register. */
    constexpr bool contains(Rh rhs) {
      return ((group1_ >> (rhs.val_ - 4)) & (uint64_t)Mask::HIGH) ==
             (uint64_t)Mask::HIGH;
    }

    /** Returns true if this set contains a byte register. */
    constexpr bool contains(Rb rhs) {
      return ((group1_ >> rhs.val_) & (uint64_t)Mask::LOW) ==
             (uint64_t)Mask::LOW;
    }

    /** Returns true if this set contains a word register. */
    constexpr bool contains(R16 rhs) {
      return ((group1_ >> rhs.val_) & (uint64_t)Mask::WORD) ==
             (uint64_t)Mask::WORD;
    }

    /** Returns true if this set contains a double register. */
    constexpr bool contains(R32 rhs) {
      return ((group1_ >> rhs.val_) & (uint64_t)Mask::DOUBLE) ==
             (uint64_t)Mask::DOUBLE;
    }

    /** Returns true if this set contains a quad register. */
    constexpr bool contains(R64 rhs) {
      return ((group1_ >> rhs.val_) & (uint64_t)Mask::QUAD) ==
             (uint64_t)Mask::QUAD;
    }

    /** Returns true if this set contains an xmm register. */
    constexpr bool contains(Xmm rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::XMM) ==
             (uint64_t)Mask::XMM;
    }

    /** Returns true if this set contains a ymm register. */
    constexpr bool contains(Ymm rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::YMM) ==
             (uint64_t)Mask::YMM;
    }

    /** Returns true if this set contains an mmx register. */
    constexpr bool contains(Mm rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::MM) ==
             (uint64_t)Mask::MM;
    }

    /** Returns true if this set contains a floating point stack register. */
    constexpr bool contains(St rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::ST) ==
             (uint64_t)Mask::ST;
    }

    /** Returns true if this set contains a segment register. */
    constexpr bool contains(Sreg rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::SREG) ==
             (uint64_t)Mask::SREG;
    }

    /** Returns true if this set contains an environment register. */
    constexpr bool contains(FpuData rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::DATA) ==
             (uint64_t)Mask::DATA;
    }

    /** Returns true if this set contains an environment register. */
    constexpr bool contains(FpuInstruction rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::INSTR) ==
             (uint64_t)Mask::INSTR;
    }

    /** Returns true if this set contains an environment register. */
    constexpr bool contains(FpuOpcode rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::OPCODE) ==
             (uint64_t)Mask::OPCODE;
    }

    /** Returns true if this set contains an environment register. */
    constexpr bool contains(Rip rhs) {
      return ((group2_ >> rhs.val_) & (uint64_t)Mask::RIP) ==
             (uint64_t)Mask::RIP;
    }

    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(Eflags rhs) {
      return ((group3_ >> rhs.index()) & (uint64_t)Mask::EFLAG) ==
             (uint64_t)Mask::EFLAG;
    }

    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(FpuControl rhs) {
      return ((group3_ >> rhs.index()) & (uint64_t)Mask::CONTROL) ==
             (uint64_t)Mask::CONTROL;
    }

    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(FpuStatus rhs) {
      return ((group3_ >> rhs.index()) & (uint64_t)Mask::STATUS) ==
             (uint64_t)Mask::STATUS;
    }

    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(FpuTag rhs) {
      return ((group4_ >> rhs.index()) & (uint64_t)Mask::TAG) ==
             (uint64_t)Mask::TAG;
    }

    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(Mxcsr rhs) {
      return ((group4_ >> rhs.index()) & (uint64_t)Mask::MXCSR) ==
             (uint64_t)Mask::MXCSR;
    }

    /** Returns true if this set contains another set. */
    bool contains(const RegSet& rhs) const {
      return (*this & rhs) == rhs;
    }

		/** Swap. */
		void swap(RegSet& rhs) {
			std::swap(group1_, rhs.group1_);
			std::swap(group2_, rhs.group2_);
			std::swap(group3_, rhs.group3_);
			std::swap(group4_, rhs.group4_);
		}

		/** STL compliant hash function. */
		size_t hash() const {
			return group1_ ^ group2_ ^ group3_ ^ group4_;
		}

  private:
    /** Internal bit mask group 1 (see Mask enum for details). */
    uint64_t group1_;
    /** Internal bit mask group 2 (see Mask enum for details). */
    uint64_t group2_;
    /** Internal bit mask group 3 (see Mask enum for details). */
    uint64_t group3_;
    /** Internal bit mask group 4 (see Mask enum for details). */
    uint64_t group4_;
};

} // namespace x64asm

namespace std {

/** Specialization for use with unordered maps/sets. */
template <>
struct hash<x64asm::RegSet> {
	size_t operator()(const x64asm::RegSet& rs) const {
		return rs.hash();
	}
};

} // namespace std

#endif
