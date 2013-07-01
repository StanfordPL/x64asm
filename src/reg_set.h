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

#ifndef X64ASM_SRC_REG_SET_H
#define X64ASM_SRC_REG_SET_H

#include <functional>

#include "src/alias.h"
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
#include "src/zmm.h"

namespace x64asm {

/** A compact implementation of a bit set for registers. */
class RegSet {
    friend class Instruction;
  private:
    /** Per register type position masks. */
    enum class Mask : uint64_t {
      // Group 1 (general purpose)
      LOW      = 0x0000000000000001,
      HIGH     = 0x0000000000010000,
      WORD     = 0x0000000000010001,
      DOUBLE   = 0x0000000100010001,
      QUAD     = 0x0001000100010001,
      // Group 2 (fpu / sse)
      XMM      = 0x0000000000000001,
      YMM      = 0x0000000000010001,
			ZMM      = 0x0000000100010001,
      MM       = 0x0001000000000000,
      ST       = 0x0100000000000000,
      // Group 3 (env bits) 
      EFLAG    = 0x0000000000000001,
      CONTROL  = 0x0000000100000000,
      STATUS   = 0x0001000000000000,
      // Group 4 (env bits / env regs)
      MXCSR    = 0x0000000000000001,
      TAG      = 0x0000000000010000,
      SREG     = 0x0000000001000000,
      DATA     = 0x0000000100000000,
      INSTR    = 0x0000000200000000,
      OPCODE   = 0x0000000400000000,
      RIP      = 0x0000000800000000,
			// Any Masks
			A_LOW    = 0x000000000000000f,
			A_HIGH   = 0x00000000000f0000,
			A_BYTE   = 0x000000000000fff0, 
			A_WORD   = 0x00000000fff00000, // (this one is tricky -- see below)
			A_DOUBLE = 0x0000ffff00000000,
			A_QUAD   = 0xffff000000000000,
			A_XMM    = 0x000000000000ffff,
			A_YMM    = 0x00000000ffff0000,
			A_ZMM    = 0x0000ffff00000000,
			// All Masks
			LOWS     = 0x000000000000000f,
			HIGHS    = 0x00000000000f0000,
			BYTES    = 0x000000000000fff0,
			WORDS    = 0x00000000ffffffff,
			DOUBLES  = 0x0000ffffffffffff,
			QUADS    = 0xffffffffffffffff,
			XMMS     = 0x000000000000ffff,
			YMMS     = 0x00000000ffffffff,
			ZMMS     = 0x0000ffffffffffff,
      // Top and Bottom
      EMPTY    = 0x0000000000000000,
      UNIV1    = 0xffffffffffffffff,
      UNIV2    = 0xffffffffffffffff,
      UNIV3    = 0xe7ff1a3f003f6fd5,
			UNIV4    = 0x0000000f3fffdfff
    };

    /** Creates a register set from four bit masks. */
    constexpr RegSet(uint64_t g1, uint64_t g2, uint64_t g3, uint64_t g4);
    /** Creates a register set from four position masks. */
    constexpr RegSet(Mask g1, Mask g2, Mask g3, Mask g4);

  public:
    /** Creates an empty register set. */
    constexpr RegSet();

    /** Creates an empty register set. */
    static constexpr RegSet empty();
    /** Creates a full register set. */
    static constexpr RegSet universe();

    /** Set inversion. */
    constexpr RegSet operator~();
    /** Set intersection. */
    constexpr RegSet operator&(const RegSet& rhs);
    /** Set union. */
    constexpr RegSet operator|(const RegSet& rhs);
    /** Set difference. */
    constexpr RegSet operator-(const RegSet& rhs);
    /** Set intersection. */
    RegSet& operator&=(const RegSet& rhs);
    /** Set union. */
    RegSet& operator|=(const RegSet& rhs);
    /** Set difference. */
    RegSet& operator-=(const RegSet& rhs);
    /** Set equality. */
    constexpr bool operator==(const RegSet& rhs);
    /** Set inequality. */
    constexpr bool operator!=(const RegSet& rhs);
    /** Set containment. */
    constexpr bool contains(const RegSet& rhs);

    /** Insert a low register. */
    constexpr RegSet operator+(const Rl& rhs);
    /** Insert a high register. */
    constexpr RegSet operator+(const Rh& rhs);
    /** Insert a byte register. */
    constexpr RegSet operator+(const Rb& rhs);
    /** Insert a word register. */
    constexpr RegSet operator+(const R16& rhs);
    /** Insert a double register. */
    constexpr RegSet operator+(const R32& rhs);
    /** Insert a quad register. */
    constexpr RegSet operator+(const R64& rhs);
    /** Insert an xmm register. */
    constexpr RegSet operator+(const Xmm& rhs);
    /** Insert a ymm register. */
    constexpr RegSet operator+(const Ymm& rhs);
    /** Insert a zmm register. */
    constexpr RegSet operator+(const Zmm& rhs);
    /** Insert an mmx register. */
    constexpr RegSet operator+(const Mm& rhs);
    /** Insert a floating point stack register. */
    constexpr RegSet operator+(const St& rhs);
    /** Insert a segment register. */
    constexpr RegSet operator+(const Sreg& rhs);
    /** Insert an environment register. */
    constexpr RegSet operator+(const FpuData& rhs);
    /** Insert an environment register. */
    constexpr RegSet operator+(const FpuInstruction& rhs);
    /** Insert an environment register. */
    constexpr RegSet operator+(const FpuOpcode& rhs);
    /** Insert an environment register. */
    constexpr RegSet operator+(const Rip& rhs);
    /** Insert environment bits. */
    constexpr RegSet operator+(const Eflags& rhs);
    /** Insert environment bits. */
    constexpr RegSet operator+(const FpuControl& rhs);
    /** Insert environment bits. */
    constexpr RegSet operator+(const FpuStatus& rhs);
    /** Insert environment bits. */
    constexpr RegSet operator+(const FpuTag& rhs);
    /** Insert environment bits. */
    constexpr RegSet operator+(const Mxcsr& rhs);
    /** Insert a memory operand. */
    RegSet operator+(const M& rhs) const;
    /** Insert a moffs operand. */
    RegSet operator+(const Moffs& rhs) const;

    /** Insert a low register. */
    RegSet& operator+=(const Rl& rhs);
    /** Insert a high register. */
    RegSet& operator+=(const Rh& rhs);
    /** Insert a byte register. */
    RegSet& operator+=(const Rb& rhs);
    /** Insert a word register. */
    RegSet& operator+=(const R16& rhs);
    /** Insert a double register. */
    RegSet& operator+=(const R32& rhs);
    /** Insert a quad register. */
    RegSet& operator+=(const R64& rhs);
    /** Insert an xmm register. */
    RegSet& operator+=(const Xmm& rhs);
    /** Insert a ymm register. */
    RegSet& operator+=(const Ymm& rhs);
    /** Insert a zmm register. */
    RegSet& operator+=(const Zmm& rhs);
    /** Insert an mmx register. */
    RegSet& operator+=(const Mm& rhs);
    /** Insert a floating point stack register. */
    RegSet& operator+=(const St& rhs);
    /** Insert a segment register. */
    RegSet& operator+=(const Sreg& rhs);
    /** Insert an environment register. */
    RegSet& operator+=(const FpuData& rhs);
    /** Insert an environment register. */
    RegSet& operator+=(const FpuInstruction& rhs);
    /** Insert an environment register. */
    RegSet& operator+=(const FpuOpcode& rhs);
    /** Insert an environment register. */
    RegSet& operator+=(const Rip& rhs);
    /** Insert environment bits. */
    RegSet& operator+=(const Eflags& rhs);
    /** Insert environment bits. */
    RegSet& operator+=(const FpuControl& rhs);
    /** Insert environment bits. */
    RegSet& operator+=(const FpuStatus& rhs);
    /** Insert environment bits. */
    RegSet& operator+=(const FpuTag& rhs);
    /** Insert environment bits. */
    RegSet& operator+=(const Mxcsr& rhs);
    /** Insert a memory operand. */
    RegSet& operator+=(const M& rhs);
    /** Insert a moffs operand. */
    RegSet& operator+=(const Moffs& rhs);

    /** Returns true if this set contains a low register. */
    constexpr bool contains(const Rl& rhs);
    /** Returns true if this set contains a high register. */
    constexpr bool contains(const Rh& rhs);
    /** Returns true if this set contains a byte register. */
    constexpr bool contains(const Rb& rhs);
    /** Returns true if this set contains a word register. */
    constexpr bool contains(const R16& rhs);
    /** Returns true if this set contains a double register. */
    constexpr bool contains(const R32& rhs);
    /** Returns true if this set contains a quad register. */
    constexpr bool contains(const R64& rhs);
    /** Returns true if this set contains an xmm register. */
    constexpr bool contains(const Xmm& rhs);
    /** Returns true if this set contains a ymm register. */
    constexpr bool contains(const Ymm& rhs);
    /** Returns true if this set contains a zmm register. */
    constexpr bool contains(const Zmm& rhs);
    /** Returns true if this set contains an mmx register. */
    constexpr bool contains(const Mm& rhs);
    /** Returns true if this set contains a floating point stack register. */
    constexpr bool contains(const St& rhs);
    /** Returns true if this set contains a segment register. */
    constexpr bool contains(const Sreg& rhs);
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const FpuData& rhs);
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const FpuInstruction& rhs);
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const FpuOpcode& rhs);
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const Rip& rhs);
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const Eflags& rhs);
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const FpuControl& rhs);
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const FpuStatus& rhs);
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const FpuTag& rhs);
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const Mxcsr& rhs);

		/** Returns true if this set contains any low registers. */
		constexpr bool contains_any_rl();
		/** Returns true if this set contains any high registers. */
		constexpr bool contains_any_rh();
		/** Returns true if this set contains any byte registers. */
		constexpr bool contains_any_rb();
		/** Returns true if this set contains any word registers. */
		constexpr bool contains_any_word();
		/** Returns true if this set contains any double registers. */
		constexpr bool contains_any_double();
		/** Returns true if this set contains any quad registers. */
		constexpr bool contains_any_quad();
		/** Returns true if this set contains any xmm registers. */
		constexpr bool contains_any_xmm();
		/** Returns true if this set contains any ymm registers. */
		constexpr bool contains_any_ymm();
		/** Returns true if this set contains any zmm registers. */
		constexpr bool contains_any_zmm();

		/** Returns true if this set contains all low registers. */
		constexpr bool contains_all_rl();
		/** Returns true if this set contains all high registers. */
		constexpr bool contains_all_rh();
		/** Returns true if this set contains all byte registers. */
		constexpr bool contains_all_rb();
		/** Returns true if this set contains all word registers. */
		constexpr bool contains_all_word();
		/** Returns true if this set contains all double registers. */
		constexpr bool contains_all_double();
		/** Returns true if this set contains all quad registers. */
		constexpr bool contains_all_quad();
		/** Returns true if this set contains all xmm registers. */
		constexpr bool contains_all_xmm();
		/** Returns true if this set contains all ymm registers. */
		constexpr bool contains_all_ymm();
		/** Returns true if this set contains all zmm registers. */
		constexpr bool contains_all_zmm();

		/** STL compliant hash. */
		constexpr size_t hash();
		/** STL compliant swap. */
		void swap(RegSet& rhs);

  private:
    /** Internal bit mask group 1 (see Mask enum for details). */
    uint64_t group1_;
    /** Internal bit mask group 2 (see Mask enum for details). */
    uint64_t group2_;
    /** Internal bit mask group 3 (see Mask enum for details). */
    uint64_t group3_;
    /** Internal bit mask group 4 (see Mask enum for details). */
    uint64_t group4_;

		/** Helper method for inserting elements into a group. */
		constexpr RegSet plus_group1(Mask m, uint64_t val);
		/** Helper method for inserting elements into a group. */
		constexpr RegSet plus_group2(Mask m, uint64_t val);
		/** Helper method for inserting elements into a group. */
		constexpr RegSet plus_group3(Mask m, uint64_t val);
		/** Helper method for inserting elements into a group. */
		constexpr RegSet plus_group4(Mask m, uint64_t val);
		/** Helper method for inserting elements into a group. */
		RegSet& plus_equal(Mask m, uint64_t& group, uint64_t val);
		/** Helper method for checking containment in a group. */
		constexpr bool contains(Mask m, uint64_t group, uint64_t val);
		/** Helper method for checking containment in a group. */
		constexpr bool contains_any(Mask m, uint64_t group);
		/** Helper method for checking containment in a group. */
		constexpr bool contains_all(Mask m, uint64_t group);
};

inline constexpr RegSet::RegSet(uint64_t g1, uint64_t g2, uint64_t g3, 
		uint64_t g4) : 
	group1_ {g1}, group2_ {g2}, group3_ {g3}, group4_ {g4} {
}

inline constexpr RegSet::RegSet(Mask g1, Mask g2, Mask g3, Mask g4) : 
	group1_ {(uint64_t)g1}, group2_ {(uint64_t)g2}, group3_ {(uint64_t)g3}, 
	group4_ {(uint64_t)g4} {
}

inline constexpr RegSet::RegSet() :
	group1_ {(uint64_t)Mask::EMPTY}, group2_ {(uint64_t)Mask::EMPTY},
	group3_ {(uint64_t)Mask::EMPTY}, group4_ {(uint64_t)Mask::EMPTY} {
}

inline constexpr RegSet RegSet::empty() {
	return RegSet {Mask::EMPTY, Mask::EMPTY, Mask::EMPTY, Mask::EMPTY};
}

inline constexpr RegSet RegSet::universe() {
	return RegSet {Mask::UNIV1, Mask::UNIV2, Mask::UNIV3, Mask::UNIV4};
}

inline constexpr RegSet RegSet::operator~() {
	return RegSet {~group1_, ~group2_, ~group3_, ~group4_};
}

inline constexpr RegSet RegSet::operator&(const RegSet& rhs) {
	return RegSet {group1_ & rhs.group1_, group2_ & rhs.group2_,
			group3_ & rhs.group3_, group4_ & rhs.group4_};
}

inline constexpr RegSet RegSet::operator|(const RegSet& rhs) {
	return RegSet {group1_ | rhs.group1_, group2_ | rhs.group2_,
			group3_ | rhs.group3_, group4_ | rhs.group4_};
}

inline constexpr RegSet RegSet::operator-(const RegSet& rhs) {
	return RegSet {group1_& ~rhs.group1_, group2_& ~rhs.group2_,
			group3_& ~rhs.group3_, group4_& ~rhs.group4_};
}

inline RegSet& RegSet::operator&=(const RegSet& rhs) {
	group1_ &= rhs.group1_;
	group2_ &= rhs.group2_;
	group3_ &= rhs.group3_;
	group4_ &= rhs.group4_;
	return *this;
}

inline RegSet& RegSet::operator|=(const RegSet& rhs) {
	group1_ |= rhs.group1_;
	group2_ |= rhs.group2_;
	group3_ |= rhs.group3_;
	group4_ |= rhs.group4_;
	return *this;
}

inline RegSet& RegSet::operator-=(const RegSet& rhs) {
	group1_ &= ~rhs.group1_;
	group2_ &= ~rhs.group2_;
	group3_ &= ~rhs.group3_;
	group4_ &= ~rhs.group4_;
	return *this;
}

inline constexpr bool RegSet::operator==(const RegSet& rhs) {
	return group1_ == rhs.group1_ && group2_ == rhs.group2_ &&
		group3_ == rhs.group3_ && group4_ == rhs.group4_;
}

inline constexpr bool RegSet::operator!=(const RegSet& rhs) {
	return !(*this == rhs);
}

inline constexpr bool RegSet::contains(const RegSet& rhs) {
	return (*this & rhs) == rhs;
}

inline constexpr RegSet RegSet::operator+(const Rl& rhs) {
	return plus_group1(Mask::LOW, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Rh& rhs) {
	return plus_group1(Mask::HIGH, rhs.val_-4);
}

inline constexpr RegSet RegSet::operator+(const Rb& rhs) {
	return plus_group1(Mask::LOW, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const R16& rhs) {
	return plus_group1(Mask::WORD, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const R32& rhs) {
	return plus_group1(Mask::DOUBLE, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const R64& rhs) {
	return plus_group1(Mask::QUAD, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Xmm& rhs) {
	return plus_group2(Mask::XMM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Ymm& rhs) {
	return plus_group2(Mask::YMM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Zmm& rhs) {
	return plus_group2(Mask::ZMM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Mm& rhs) {
	return plus_group2(Mask::MM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const St& rhs) {
	return plus_group2(Mask::ST, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Sreg& rhs) {
	return plus_group4(Mask::SREG, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuData& rhs) {
	return plus_group4(Mask::DATA, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuInstruction& rhs) {
	return plus_group4(Mask::INSTR, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuOpcode& rhs) {
	return plus_group4(Mask::OPCODE, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Rip& rhs) {
	return plus_group4(Mask::RIP, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Eflags& rhs) {
	return plus_group3(Mask::EFLAG, rhs.index_);
}

inline constexpr RegSet RegSet::operator+(const FpuControl& rhs) {
	return plus_group3(Mask::CONTROL, rhs.index_);
}

inline constexpr RegSet RegSet::operator+(const FpuStatus& rhs) {
	return plus_group3(Mask::STATUS, rhs.index_);
}

inline constexpr RegSet RegSet::operator+(const FpuTag& rhs) {
	return plus_group4(Mask::TAG, rhs.index_);
}

inline constexpr RegSet RegSet::operator+(const Mxcsr& rhs) {
	return plus_group4(Mask::MXCSR, rhs.index_);
}

inline RegSet RegSet::operator+(const M& rhs) const {
	auto ret = *this;
	return ret += rhs;
}

inline RegSet RegSet::operator+(const Moffs& rhs) const {
	return rhs.contains_seg() ? plus_group4(Mask::SREG, rhs.get_seg()) : *this;
}

inline RegSet& RegSet::operator+=(const Rl& rhs) {
	return plus_equal(Mask::LOW, group1_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Rh& rhs) {
	return plus_equal(Mask::HIGH, group1_, rhs.val_ - 4);
}

inline RegSet& RegSet::operator+=(const Rb& rhs) {
	return plus_equal(Mask::LOW, group1_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const R16& rhs) {
	return plus_equal(Mask::WORD, group1_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const R32& rhs) {
	return plus_equal(Mask::DOUBLE, group1_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const R64& rhs) {
	return plus_equal(Mask::QUAD, group1_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Xmm& rhs) {
	return plus_equal(Mask::XMM, group2_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Ymm& rhs) {
	return plus_equal(Mask::YMM, group2_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Zmm& rhs) {
	return plus_equal(Mask::ZMM, group2_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Mm& rhs) {
	return plus_equal(Mask::MM, group2_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const St& rhs) {
	return plus_equal(Mask::ST, group2_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Sreg& rhs) {
	return plus_equal(Mask::SREG, group4_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuData& rhs) {
	return plus_equal(Mask::DATA, group4_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuInstruction& rhs) {
	return plus_equal(Mask::INSTR, group4_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuOpcode& rhs) {
	return plus_equal(Mask::OPCODE, group4_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Rip& rhs) {
	return plus_equal(Mask::RIP, group4_, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Eflags& rhs) {
	return plus_equal(Mask::EFLAG, group3_, rhs.index_);
}

inline RegSet& RegSet::operator+=(const FpuControl& rhs) {
	return plus_equal(Mask::CONTROL, group3_, rhs.index_);
}

inline RegSet& RegSet::operator+=(const FpuStatus& rhs) {
	return plus_equal(Mask::STATUS, group3_, rhs.index_);
}

inline RegSet& RegSet::operator+=(const FpuTag& rhs) {
	return plus_equal(Mask::TAG, group4_, rhs.index_);
}

inline RegSet& RegSet::operator+=(const Mxcsr& rhs) {
	return plus_equal(Mask::MXCSR, group4_, rhs.index_);
}

inline RegSet& RegSet::operator+=(const M& rhs) {
  if (rhs.get_addr_or()) {
    if (rhs.contains_base()) {
      *this += Alias::to_double(rhs.get_base());
    }
    if (rhs.contains_index()) {
      *this += Alias::to_double(rhs.get_index());
    }
  } else {
    if (rhs.contains_base()) {
      *this += rhs.get_base();
    }
    if (rhs.contains_index()) {
      *this += rhs.get_index();
    }
  }
  return *this;
}

inline RegSet& RegSet::operator+=(const Moffs& rhs) {
	return rhs.contains_seg() ? plus_equal(Mask::SREG, group4_, rhs.get_seg()) : *this;
}

inline constexpr bool RegSet::contains(const Rl& rhs) {
	return contains(Mask::LOW, group1_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Rh& rhs) {
	return contains(Mask::HIGH, group1_, rhs.val_ - 4);
}

inline constexpr bool RegSet::contains(const Rb& rhs) {
	return contains(Mask::LOW, group1_, rhs.val_);
}

inline constexpr bool RegSet::contains(const R16& rhs) {
	return contains(Mask::WORD, group1_, rhs.val_);
}

inline constexpr bool RegSet::contains(const R32& rhs) {
	return contains(Mask::DOUBLE, group1_, rhs.val_);
}

inline constexpr bool RegSet::contains(const R64& rhs) {
	return contains(Mask::QUAD, group1_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Xmm& rhs) {
	return contains(Mask::XMM, group2_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Ymm& rhs) {
	return contains(Mask::YMM, group2_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Zmm& rhs) {
	return contains(Mask::ZMM, group2_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Mm& rhs) {
	return contains(Mask::MM, group2_, rhs.val_);
}

inline constexpr bool RegSet::contains(const St& rhs) {
	return contains(Mask::ST, group2_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Sreg& rhs) {
	return contains(Mask::SREG, group4_, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuData& rhs) {
	return contains(Mask::DATA, group4_, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuInstruction& rhs) {
	return contains(Mask::INSTR, group4_, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuOpcode& rhs) {
	return contains(Mask::OPCODE, group4_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Rip& rhs) {
	return contains(Mask::RIP, group4_, rhs.val_);
}

inline constexpr bool RegSet::contains(const Eflags& rhs) {
	return contains(Mask::EFLAG, group3_, rhs.index_);
}

inline constexpr bool RegSet::contains(const FpuControl& rhs) {
	return contains(Mask::CONTROL, group3_, rhs.index_);
}

inline constexpr bool RegSet::contains(const FpuStatus& rhs) {
	return contains(Mask::STATUS, group3_, rhs.index_);
}

inline constexpr bool RegSet::contains(const FpuTag& rhs) {
	return contains(Mask::TAG, group4_, rhs.index_);
}

inline constexpr bool RegSet::contains(const Mxcsr& rhs) {
	return contains(Mask::MXCSR, group4_, rhs.index_);
}

constexpr bool RegSet::contains_any_rl() {
	return contains_any(Mask::A_LOW, group1_);
}

constexpr bool RegSet::contains_any_rh() {
	return contains_any(Mask::A_HIGH, group1_);
}

constexpr bool RegSet::contains_any_rb() {
	return contains_any(Mask::A_BYTE, group1_);
}

constexpr bool RegSet::contains_any_word() {
	return contains_any(Mask::A_WORD, group1_) ||
			contains(ax) || contains(bx) || contains(cx) || contains(dx);
}

constexpr bool RegSet::contains_any_double() {
	return contains_any(Mask::A_DOUBLE, group1_);
}

constexpr bool RegSet::contains_any_quad() {
	return contains_any(Mask::A_QUAD, group1_);
}

constexpr bool RegSet::contains_any_xmm() {
	return contains_any(Mask::A_XMM, group2_);
}

constexpr bool RegSet::contains_any_ymm() {
	return contains_any(Mask::A_YMM, group2_);
}

constexpr bool RegSet::contains_any_zmm() {
	return contains_any(Mask::A_ZMM, group2_);
}

constexpr bool RegSet::contains_all_rl() {
	return contains_all(Mask::LOWS, group1_);
}

constexpr bool RegSet::contains_all_rh() {
	return contains_all(Mask::HIGHS, group1_);
}

constexpr bool RegSet::contains_all_rb() {
	return contains_all(Mask::BYTES, group1_);
}

constexpr bool RegSet::contains_all_word() {
	return contains_all(Mask::WORDS, group1_);
}

constexpr bool RegSet::contains_all_double() {
	return contains_all(Mask::DOUBLES, group1_);
}

constexpr bool RegSet::contains_all_quad() {
	return contains_all(Mask::QUADS, group1_);
}

constexpr bool RegSet::contains_all_xmm() {
	return contains_all(Mask::XMMS, group2_);
}

constexpr bool RegSet::contains_all_ymm() {
	return contains_all(Mask::YMMS, group2_);
}

constexpr bool RegSet::contains_all_zmm() {
	return contains_all(Mask::ZMMS, group2_);
}

inline void RegSet::swap(RegSet& rhs) {
	std::swap(group1_, rhs.group1_);
	std::swap(group2_, rhs.group2_);
	std::swap(group3_, rhs.group3_);
	std::swap(group4_, rhs.group4_);
}

inline constexpr size_t RegSet::hash() {
	return group1_ ^ group2_ ^ group3_ ^ group4_;
}

inline constexpr RegSet RegSet::plus_group1(Mask m, uint64_t val) {
	return RegSet{group1_ | ((uint64_t)m << val), group2_, group3_, group4_};
}

inline constexpr RegSet RegSet::plus_group2(Mask m, uint64_t val) {
	return RegSet{group1_, group2_ | ((uint64_t)m << val), group3_, group4_};
}

inline constexpr RegSet RegSet::plus_group3(Mask m, uint64_t val) {
	return RegSet{group1_, group2_, group3_ | ((uint64_t)m << val), group4_};
}

inline constexpr RegSet RegSet::plus_group4(Mask m, uint64_t val) {
	return RegSet{group1_, group2_, group3_, group4_ | ((uint64_t)m << val)};
}

inline RegSet& RegSet::plus_equal(Mask m, uint64_t& group, uint64_t val) {
	group |= ((uint64_t)m << val);
	return *this;
}

inline constexpr bool RegSet::contains(Mask m, uint64_t group, uint64_t val) {
	return ((group >> val) & (uint64_t)m) == (uint64_t)m;
}

inline constexpr bool RegSet::contains_any(Mask m, uint64_t group) {
	return (uint64_t)m & group;
}

inline constexpr bool RegSet::contains_all(Mask m, uint64_t group) {
	return ((uint64_t)m & group) == (uint64_t)m;
}

} // namespace x64asm

namespace std {

/** STL-compliant hash specialization. */
template <>
struct hash<x64asm::RegSet> {
	size_t operator()(const x64asm::RegSet& rs) const {
		return rs.hash();
	}
};

/** STL-compliant swap specialization. */
template <>
void swap(x64asm::RegSet& lhs, x64asm::RegSet& rhs) {
	return lhs.swap(rhs);
}

} // namespace std

#endif
