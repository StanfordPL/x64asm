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

#include <algorithm>
#include <iostream>

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
      LOW     = 0x0000000000000001,
      HIGH    = 0x0000000000010000,
      WORD    = 0x0000000000010001,
      DOUBLE  = 0x0000000100010001,
      QUAD    = 0x0001000100010001,
      // Group 2 (fpu / sse)
      XMM     = 0x0000000000000001,
      YMM     = 0x0000000000010001,
			ZMM     = 0x0000000100010001,
      MM      = 0x0001000000000000,
      ST      = 0x0100000000000000,
      // Group 3 (env bits) 
      EFLAG   = 0x0000000000000001,
      CONTROL = 0x0000000100000000,
      STATUS  = 0x0001000000000000,
      // Group 4 (env bits / env regs)
      MXCSR   = 0x0000000000000001,
      TAG     = 0x0000000000010000,
      SREG    = 0x0000000001000000,
      DATA    = 0x0000000100000000,
      INSTR   = 0x0000000200000000,
      OPCODE  = 0x0000000400000000,
      RIP     = 0x0000000800000000,
      // Top and Bottom
      EMPTY   = 0x0000000000000000,
      UNIV1   = 0xffffffffffffffff,
      UNIV2   = 0xffffffffffffffff,
      UNIV3   = 0xe7ff1a3f003f6fd5,
			UNIV4   = 0x0000000f3fffdfff
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
    RegSet& operator+=(const Ymm& rhs);
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

		/** Helper method for inserting elements into group 1. */
		constexpr RegSet group1_plus(Mask m, uint64_t val);
		/** Helper method for inserting elements into group 2. */
		constexpr RegSet group2_plus(Mask m, uint64_t val);
		/** Helper method for inserting elements into group 3. */
		constexpr RegSet group3_plus(Mask m, uint64_t val);
		/** Helper method for inserting elements into group 4. */
		constexpr RegSet group4_plus(Mask m, uint64_t val);

		/** Helper method for inserting elements into group 1. */
		RegSet& group1_plus_equal(Mask m, uint64_t val);
		/** Helper method for inserting elements into group 2. */
		RegSet& group2_plus_equal(Mask m, uint64_t val);
		/** Helper method for inserting elements into group 3. */
		RegSet& group3_plus_equal(Mask m, uint64_t val);
		/** Helper method for inserting elements into group 4. */
		RegSet& group4_plus_equal(Mask m, uint64_t val);

		/** Helper method for checking containment in group 1. */
		constexpr bool group1_contains(Mask m, uint64_t val);
		/** Helper method for checking containment in group 2. */
		constexpr bool group2_contains(Mask m, uint64_t val);
		/** Helper method for checking containment in group 3. */
		constexpr bool group3_contains(Mask m, uint64_t val);
		/** Helper method for checking containment in group 4. */
		constexpr bool group4_contains(Mask m, uint64_t val);
};

} // namespace x64asm

namespace std {

/** STL-compliant hash specialization. */
template <>
struct hash<x64asm::RegSet> {
	size_t operator()(const x64asm::RegSet& rs) const;
};

/** STL-compliant swap specialization. */
template <>
void swap(x64asm::RegSet& lhs, x64asm::RegSet& rhs);

} // namespace std

namespace x64asm {

inline constexpr RegSet::RegSet(uint64_t g1, uint64_t g2, uint64_t g3, 
		uint64_t g4) : 
	group1_ {g1}, group2_ {g2}, group3_ {g3}, group4_ {g4} {
}

inline constexpr RegSet::RegSet(Mask g1, Mask g2, Mask g3, Mask g4) : 
	group1_ {(uint64_t)g1}, group2_ {(uint64_t)g2}, group3_ {(uint64_t)g3}, 
	group4_ {(uint64_t)g4} {
}

inline constexpr RegSet() :
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
	return group1_plus(Mask::LOW, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Rh& rhs) {
	return group1_plus(Mask::HIGH, rhs.val_ - 4);
}

inline constexpr RegSet RegSet::operator+(const Rb& rhs) {
	return group1_plus(Mask::LOW, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const R16& rhs) {
	return group1_plus(Mask::WORD, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const R32& rhs) {
	return group1_plus(Mask::DOUBLE, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const R64& rhs) {
	return group1_plus(Mask::QUAD, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Xmm& rhs) {
	return group2_plus(Mask::XMM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Ymm& rhs) {
	return group2_plus(Mask::YMM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Zmm& rhs) {
	return group2_plus(Mask::ZMM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Mm& rhs) {
	return group2_plus(Mask::MM, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const St& rhs) {
	return group2_plus(Mask::ST, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Sreg& rhs) {
	return group4_plus(Mask::SREG, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuData& rhs) {
	return group4_plus(Mask::DATA, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuInstruction& rhs) {
	return group4_plus(Mask::INSTR, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuOpcode& rhs) {
	return group4_plus(Mask::OPCODE, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Rip& rhs) {
	return group4_plus(Mask::RIP, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Eflags& rhs) {
	return group3_plus(Mask::EFLAG, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuControl& rhs) {
	return group3_plus(Mask::CONTROL, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuStatus& rhs) {
	return group3_plus(Mask::STATUS, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const FpuTag& rhs) {
	return group4_plus(Mask::TAG, rhs.val_);
}

inline constexpr RegSet RegSet::operator+(const Mxcsr& rhs) {
	return group4_plus(Mask::MXCSR, rhs.val_);
}

inline RegSet RegSet::operator+(const M& rhs) const {
	auto ret = *this;
	return ret += rhs;
}

inline RegSet RegSet::operator+(const Moffs& rhs) const {
	return rhs.contains_seg() ? group4_plus(Mask::SREG, rhs.get_seg()) : *this;
}

inline RegSet& RegSet::operator+=(const Rl& rhs) {
	return group1_plus_equal(Mask::LOW, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Rh& rhs) {
	return group1_plus_equal(Mask::HIGH, rhs.val_ - 4);
}

inline RegSet& RegSet::operator+=(const Rb& rhs) {
	return group1_plus_equal(Mask::LOW, rhs.val_);
}

inline RegSet& RegSet::operator+=(const R16& rhs) {
	return group1_plus_equal(Mask::WORD, rhs.val_);
}

inline RegSet& RegSet::operator+=(const R32& rhs) {
	return group1_plus_equal(Mask::DOUBLE, rhs.val_);
}

inline RegSet& RegSet::operator+=(const R64& rhs) {
	return group1_plus_equal(Mask::QUAD, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Xmm& rhs) {
	return group2_plus_equal(Mask::XMM, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Ymm& rhs) {
	return group2_plus_equal(Mask::YMM, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Zmm& rhs) {
	return group2_plus_equal(Mask::ZMM, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Mm& rhs) {
	return group2_plus_eqaul(Mask::MM, rhs.val_);
}

inline RegSet& RegSet::operator+=(const St& rhs) {
	return group2_plus_eqaul(Mask::ST, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Sreg& rhs) {
	return group4_plus_eqaul(Mask::SREG, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuData& rhs) {
	return group4_plus_eqaul(Mask::DATA, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuInstruction& rhs) {
	return group4_plus_eqaul(Mask::INSTR, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuOpcode& rhs) {
	return group4_plus_eqaul(Mask::OPCODE, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Rip& rhs) {
	return group4_plus_eqaul(Mask::RIP, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Eflags& rhs) {
	return group3_plus_eqaul(Mask::EFLAG, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuControl& rhs) {
	return group3_plus_eqaul(Mask::CONTROL, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuStatus& rhs) {
	return group3_plus_eqaul(Mask::STATUS, rhs.val_);
}

inline RegSet& RegSet::operator+=(const FpuTag& rhs) {
	return group4_plus_eqaul(Mask::TAG, rhs.val_);
}

inline RegSet& RegSet::operator+=(const Mxcsr& rhs) {
	return group4_plus_eqaul(Mask::MXCSR, rhs.val_);
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
	return rhs.contains_seg() ? group4_plus_equal(Mask::SREG, rhs.get_seg()) : *this;
}

inline constexpr bool RegSet::contains(const Rl& rhs) {
	return group1_contains(Mask::LOW, rhs.val_);
}

inline constexpr bool RegSet::contains(const Rh& rhs) {
	return group1_contains(Mask::HIGH, rhs.val_ - 4);
}

inline constexpr bool RegSet::contains(const Rb& rhs) {
	return group1_contains(Mask::LOW, rhs.val_);
}

inline constexpr bool RegSet::contains(const R16& rhs) {
	return group1_contains(Mask::WORD, rhs.val_);
}

inline constexpr bool RegSet::contains(const R32& rhs) {
	return group1_contains(Mask::DOUBLE, rhs.val_);
}

inline constexpr bool RegSet::contains(const R64& rhs) {
	return group1_contains(Mask::QUAD, rhs.val_);
}

inline constexpr bool RegSet::contains(const Xmm& rhs) {
	return group2_contains(Mask::XMM, rhs.val_);
}

inline constexpr bool RegSet::contains(const Ymm& rhs) {
	return group2_contains(Mask::YMM, rhs.val_);
}

inline constexpr bool RegSet::contains(const Zmm& rhs) {
	return group2_contains(Mask::ZMM, rhs.val_);
}

inline constexpr bool RegSet::contains(const Mm& rhs) {
	return group2_contains(Mask::MM, rhs.val_);
}

inline constexpr bool RegSet::contains(const St& rhs) {
	return group2_contains(Mask::ST, rhs.val_);
}

inline constexpr bool RegSet::contains(const Sreg& rhs) {
	return group4_contains(Mask::SREG, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuData& rhs) {
	return group4_contains(Mask::DATA, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuInstruction& rhs) {
	return group4_contains(Mask::INSTR, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuOpcode& rhs) {
	return group4_contains(Mask::OPCODE, rhs.val_);
}

inline constexpr bool RegSet::contains(const Rip& rhs) {
	return group4_contains(Mask::RIP, rhs.val_);
}

inline constexpr bool RegSet::contains(const Eflags& rhs) {
	return group3_contains(Mask::EFLAG, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuControl& rhs) {
	return group3_contains(Mask::CONTROL, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuStatus& rhs) {
	return group3_contains(Mask::STATUS, rhs.val_);
}

inline constexpr bool RegSet::contains(const FpuTag& rhs) {
	return group4_contains(Mask::TAG, rhs.val_);
}

inline constexpr bool RegSet::contains(const Mxcsr& rhs) {
	return group4_contains(Mask::MXCSR, rhs.val_);
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

inline constexpr RegSet RegSet::group1_plus(Mask m, uint64_t val) {
	return RegSet {group1_ | ((uint64_t)m << val), group2_, group3_, group4_};
}

inline constexpr RegSet RegSet::group2_plus(Mask m, uint64_t val) {
	return RegSet {group1_, group2_ | ((uint64_t)m << val), group3_, group4_};
}

inline constexpr RegSet RegSet::group3_plus(Mask m, uint64_t val) {
	return RegSet {group1_, group2_, group3_ | ((uint64_t)m << val), group4_};
}

inline constexpr RegSet RegSet::group4_plus(Mask m, uint64_t val) {
	return RegSet {group1_, group2_, group3_, group4_ | ((uint64_t)m << val)};
}

inline RegSet& RegSet::group1_plus_equal(Mask m, uint64_t val) {
	group1_ |= ((uint64_t)m << val);
	return *this;
}

inline RegSet& RegSet::group2_plus_equal(Mask m, uint64_t val) {
	group2_ |= ((uint64_t)m << val);
	return *this;
}

inline RegSet& RegSet::group3_plus_equal(Mask m, uint64_t val) {
	group3_ |= ((uint64_t)m << val);
	return *this;
}

inline RegSet& RegSet::group4_plus_equal(Mask m, uint64_t val) {
	group4_ |= ((uint64_t)m << val);
	return *this;
}

inline constexpr bool RegSet::group1_contains(Mask m, uint64_t val) {
	return ((group1_ >> val) & m) == m;
}

inline constexpr bool RegSet::group2_contains(Mask m, uint64_t val) {
	return ((group2_ >> val) & m) == m;
}

inline constexpr bool RegSet::group3_contains(Mask m, uint64_t val) {
	return ((group3_ >> val) & m) == m;
}

inline constexpr bool RegSet::group4_contains(Mask m, uint64_t val) {
	return ((group4_ >> val) & m) == m;
}

} // namespace x64asm

namespace std {

template <>
inline size_t hash<x64asm::RegSet>::operator()(const x64asm::RegSet& rs) const {
	return rs.hash();
}

template <>
inline void swap(x64asm::RegSet& lhs, x64asm::RegSet& rhs) {
	lhs.swap(rhs);
}

} // namespace std

#endif
