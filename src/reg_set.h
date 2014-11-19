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
      // Group 1 (general purpose)
      LOW      = 0x0000000000000001,
      HIGH     = 0x0000000000010000,
      WORD     = 0x0000000000010001,
      DOUBLE   = 0x0000000100010001,
      QUAD     = 0x0001000100010001,
      // Group 2 (fpu / sse)
      XMM      = 0x0000000000000001,
      YMM      = 0x0000000000010001,
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
      // All Masks
      LOWS     = 0x000000000000000f,
      HIGHS    = 0x00000000000f0000,
      BYTES    = 0x000000000000fff0,
      WORDS    = 0x00000000ffffffff,
      DOUBLES  = 0x0000ffffffffffff,
      QUADS    = 0xffffffffffffffff,
      XMMS     = 0x000000000000ffff,
      YMMS     = 0x00000000ffffffff,
      // Top and Bottom
      EMPTY    = 0x0000000000000000,
      UNIV1    = 0xffffffffffffffff,
      UNIV2    = 0xffffffffffffffff,
      UNIV3    = 0xe7ff1a3f003f6fd5,
      UNIV4    = 0x0000000f3fffdfff
    };

    /** Creates a register set from four bit masks. */
    constexpr RegSet(uint64_t g1, uint64_t g2, uint64_t g3, uint64_t g4) :
  		group1_(g1), group2_(g2), group3_(g3), group4_(g4) {
		}

    /** Creates a register set from four position masks. */
    constexpr RegSet(Mask g1, Mask g2, Mask g3, Mask g4) :
  		group1_((uint64_t)g1), group2_((uint64_t)g2), group3_((uint64_t)g3), group4_((uint64_t)g4) {
		}

  public:
    /** Creates an empty register set. */
    constexpr RegSet() :
			group1_((uint64_t)Mask::EMPTY), group2_((uint64_t)Mask::EMPTY),
			group3_((uint64_t)Mask::EMPTY), group4_((uint64_t)Mask::EMPTY) {
		}
		/** Copy constructor. */
		RegSet(const RegSet& rhs) {
			group1_ = rhs.group1_;
			group2_ = rhs.group2_;
			group3_ = rhs.group3_;
			group4_ = rhs.group4_;
		}
		/** Move constructor. */
		RegSet(RegSet&& rhs) {
			group1_ = rhs.group1_;
			group2_ = rhs.group2_;
			group3_ = rhs.group3_;
			group4_ = rhs.group4_;
		}
		/** Copy assignment operator. */
		RegSet& operator=(const RegSet& rhs) {
			RegSet(rhs).swap(*this);
			return *this;
		}
		/** Move assignment operator. */
		RegSet& operator=(RegSet&& rhs) {
			RegSet(std::move(rhs)).swap(*this);
			return *this;
		}
    /** Creates an empty register set. */
    static constexpr RegSet empty() {
  		return {Mask::EMPTY, Mask::EMPTY, Mask::EMPTY, Mask::EMPTY};
		}
    /** Creates a register set containing all general purpose registers. */
    static constexpr RegSet all_gps() {
			return {Mask::QUADS, Mask::EMPTY, Mask::EMPTY, Mask::EMPTY};
		}
    /** Creates a register set containing all xmm registers. */
    static constexpr RegSet all_xmms() {
			return {Mask::EMPTY, Mask::XMMS, Mask::EMPTY, Mask::EMPTY};
		}
    /** Creates a register set containing all ymm registers. */
    static constexpr RegSet all_ymms() {
			return {Mask::EMPTY, Mask::YMMS, Mask::EMPTY, Mask::EMPTY};
		}
    /** Creates a register set containing registers that must be preserved
        by the callee. */
    static constexpr RegSet linux_call_preserved() {
			return empty() + 
				Constants::rbx()  + Constants::rbp()  + Constants::rsp()  + 
				Constants::r12()  + Constants::r13()  + Constants::r14()  +
				Constants::r15();
		}
    /** Creates a regster set containing registers that can be changed by 
        the callee at any time. */
    static constexpr RegSet linux_call_scratch() {
			return all_xmms() + 
				Constants::rdi()  + Constants::rsi()  + Constants::rdx()  + 
				Constants::rcx()  + Constants::r8()   + Constants::r9() +
				Constants::r10()  + Constants::r11()  + Constants::rax();
		}
    /** Creates a RegSet containing registers used as parameters. */
    static constexpr RegSet linux_call_parameters() {
			return empty() + 
				Constants::xmm0() + Constants::xmm1() + Constants::xmm2() +
				Constants::xmm3() + Constants::xmm4() + Constants::xmm5() +
				Constants::xmm6() + Constants::xmm7() +
				Constants::rdi()  + Constants::rsi()  + Constants::rdx()  + 
				Constants::rcx()  + Constants::r8()   + Constants::r9() +
				Constants::rax();
		}
    /** Creates a RegSet containing registers returned by linux calls. */
    static constexpr RegSet linux_call_return() {
			return empty() +
				Constants::rax() + Constants::rdx() +
				Constants::xmm0() + Constants::xmm1();
		}

    /** Creates a retister set containing windows caller save reigsters. */
    static constexpr RegSet windows_caller_save() {
			return empty() + 
				Constants::rcx()  + Constants::rdx()  + 
				Constants::r8()   + Constants::r9()   + 
				Constants::xmm0() + Constants::xmm1() + 
				Constants::xmm2() + Constants::xmm3();
		}
    /** Creates a full register set. */
    static constexpr RegSet universe() {
			return {Mask::UNIV1, Mask::UNIV2, Mask::UNIV3, Mask::UNIV4};
		}

    /** Set inversion. */
    constexpr RegSet operator~() {
			return {~group1_, ~group2_, ~group3_, ~group4_};
		}
    /** Set intersection. */
    constexpr RegSet operator&(const RegSet& rhs) {
			return {group1_ & rhs.group1_, group2_ & rhs.group2_,
      	group3_ & rhs.group3_, group4_ & rhs.group4_};
		}
    /** Set union. */
    constexpr RegSet operator|(const RegSet& rhs) {
			return {group1_ | rhs.group1_, group2_ | rhs.group2_,
				group3_ | rhs.group3_, group4_ | rhs.group4_};
		}
    /** Set difference. */
    constexpr RegSet operator-(const RegSet& rhs) {
			return {group1_& ~rhs.group1_, group2_& ~rhs.group2_,
				group3_& ~rhs.group3_, group4_& ~rhs.group4_};
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
    /** Set containment. */
    constexpr bool contains(const RegSet& rhs) {
			return (*this & rhs) == rhs;
		}

    /** Insert a low register. */
    constexpr RegSet operator+(const Rl& rhs) {
			return plus_group1(Mask::LOW, (uint64_t)rhs);
		}
    /** Insert a high register. */
    constexpr RegSet operator+(const Rh& rhs) {
			return plus_group1(Mask::HIGH, (uint64_t)rhs-4);
		}
    /** Insert a byte register. */
    constexpr RegSet operator+(const Rb& rhs) {
			return plus_group1(Mask::LOW, (uint64_t)rhs);
		}
    /** Insert a word register. */
    constexpr RegSet operator+(const R16& rhs) {
			return plus_group1(Mask::WORD, (uint64_t)rhs);
		}
    /** Insert a double register. */
    constexpr RegSet operator+(const R32& rhs) {
			return plus_group1(Mask::DOUBLE, (uint64_t)rhs);
		}
    /** Insert a quad register. */
    constexpr RegSet operator+(const R64& rhs) {
			return plus_group1(Mask::QUAD, (uint64_t)rhs);
		}
    /** Insert an xmm register. */
    constexpr RegSet operator+(const Xmm& rhs) {
			return plus_group2(Mask::XMM, (uint64_t)rhs);
		}
    /** Insert a ymm register. */
    constexpr RegSet operator+(const Ymm& rhs) {
			return plus_group2(Mask::YMM, (uint64_t)rhs);
		}
    /** Insert an mmx register. */
    constexpr RegSet operator+(const Mm& rhs) {
			return plus_group2(Mask::MM, (uint64_t)rhs);
		}
    /** Insert a floating point stack register. */
    constexpr RegSet operator+(const St& rhs) {
			return plus_group2(Mask::ST, (uint64_t)rhs);
		}
    /** Insert a segment register. */
    constexpr RegSet operator+(const Sreg& rhs) {
			return plus_group4(Mask::SREG, (uint64_t)rhs);
		}
    /** Insert an environment register. */
    constexpr RegSet operator+(const FpuData& rhs) {
			return plus_group4(Mask::DATA, 0);
		}
    /** Insert an environment register. */
    constexpr RegSet operator+(const FpuInstruction& rhs) {
			return plus_group4(Mask::INSTR, 0);
		}
    /** Insert an environment register. */
    constexpr RegSet operator+(const FpuOpcode& rhs) {
			return plus_group4(Mask::OPCODE, 0);
		}
    /** Insert an environment register. */
    constexpr RegSet operator+(const Rip& rhs) {
			return plus_group4(Mask::RIP, 0);
		}
    /** Insert environment bits. */
    constexpr RegSet operator+(const Eflags& rhs) {
			return plus_group3(Mask::EFLAG, rhs.index());
		}
    /** Insert environment bits. */
    constexpr RegSet operator+(const FpuControl& rhs) {
			return plus_group3(Mask::CONTROL, rhs.index());
		}
    /** Insert environment bits. */
    constexpr RegSet operator+(const FpuStatus& rhs) {
			return plus_group3(Mask::STATUS, rhs.index());
		}
    /** Insert environment bits. */
    constexpr RegSet operator+(const FpuTag& rhs) {
			return plus_group4(Mask::TAG, rhs.index());
		}
    /** Insert environment bits. */
    constexpr RegSet operator+(const Mxcsr& rhs) {
			return plus_group4(Mask::MXCSR, rhs.index());
		}
    /** Insert a memory operand. */
    template <class T>
    RegSet operator+(const M<T>& rhs) const {
			auto ret = *this;
			return ret += rhs;
		}
    /** Insert a moffs operand. */
    RegSet operator+(const Moffs& rhs) const {
			return rhs.contains_seg() ? plus_group4(Mask::SREG, rhs.get_seg()) : *this;
		}

    /** Insert a low register. */
    RegSet& operator+=(const Rl& rhs) {
			return plus_equal(Mask::LOW, group1_, (uint64_t)rhs);
		}
    /** Insert a high register. */
    RegSet& operator+=(const Rh& rhs) {
			return plus_equal(Mask::HIGH, group1_, (uint64_t)rhs - 4);
		}
    /** Insert a byte register. */
    RegSet& operator+=(const Rb& rhs) {
			return plus_equal(Mask::LOW, group1_, (uint64_t)rhs);
		}
    /** Insert a word register. */
    RegSet& operator+=(const R16& rhs) {
			return plus_equal(Mask::WORD, group1_, (uint64_t)rhs);
		}
    /** Insert a double register. */
    RegSet& operator+=(const R32& rhs) {
			return plus_equal(Mask::DOUBLE, group1_, (uint64_t)rhs);
		}
    /** Insert a quad register. */
    RegSet& operator+=(const R64& rhs) {
			return plus_equal(Mask::QUAD, group1_, (uint64_t)rhs);
		}
    /** Insert an xmm register. */
    RegSet& operator+=(const Xmm& rhs) {
			return plus_equal(Mask::XMM, group2_, (uint64_t)rhs);
		}
    /** Insert a ymm register. */
    RegSet& operator+=(const Ymm& rhs) {
			return plus_equal(Mask::YMM, group2_, (uint64_t)rhs);
		}
    /** Insert an mmx register. */
    RegSet& operator+=(const Mm& rhs) {
			return plus_equal(Mask::MM, group2_, (uint64_t)rhs);
		}
    /** Insert a floating point stack register. */
    RegSet& operator+=(const St& rhs) {
			return plus_equal(Mask::ST, group2_, (uint64_t)rhs);
		}
    /** Insert a segment register. */
    RegSet& operator+=(const Sreg& rhs) {
			return plus_equal(Mask::SREG, group4_, (uint64_t)rhs);
		}
    /** Insert an environment register. */
    RegSet& operator+=(const FpuData& rhs) {
			return plus_equal(Mask::DATA, group4_, 0);
		}
    /** Insert an environment register. */
    RegSet& operator+=(const FpuInstruction& rhs) {
			return plus_equal(Mask::INSTR, group4_, 0);
		}
    /** Insert an environment register. */
    RegSet& operator+=(const FpuOpcode& rhs) {
			return plus_equal(Mask::OPCODE, group4_, 0);
		}
    /** Insert an environment register. */
    RegSet& operator+=(const Rip& rhs) {
			return plus_equal(Mask::RIP, group4_, 0);
		}
    /** Insert environment bits. */
    RegSet& operator+=(const Eflags& rhs) {
			return plus_equal(Mask::EFLAG, group3_, rhs.index());
		}
    /** Insert environment bits. */
    RegSet& operator+=(const FpuControl& rhs) {
			return plus_equal(Mask::CONTROL, group3_, rhs.index());
		}
    /** Insert environment bits. */
    RegSet& operator+=(const FpuStatus& rhs) {
			return plus_equal(Mask::STATUS, group3_, rhs.index());
		}
    /** Insert environment bits. */
    RegSet& operator+=(const FpuTag& rhs) {
			return plus_equal(Mask::TAG, group4_, rhs.index());
		}
    /** Insert environment bits. */
    RegSet& operator+=(const Mxcsr& rhs) {
			return plus_equal(Mask::MXCSR, group4_, rhs.index());
		}
    /** Insert a memory operand. */
    template <class T>
    RegSet& operator+=(const M<T>& rhs) {
      if (rhs.addr_or()) {
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

    /** Insert a moffs operand. */
    RegSet& operator+=(const Moffs& rhs) {
			return rhs.contains_seg() ? plus_equal(Mask::SREG, group4_, rhs.get_seg()) : *this;
		}

    /** Returns true if this set contains a low register. */
    constexpr bool contains(const Rl& rhs) {
			return contains(Mask::LOW, group1_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a high register. */
    constexpr bool contains(const Rh& rhs) {
			return contains(Mask::HIGH, group1_, (uint64_t)rhs - 4);
		}
    /** Returns true if this set contains a byte register. */
    constexpr bool contains(const Rb& rhs) {
			return contains(Mask::LOW, group1_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a word register. */
    constexpr bool contains(const R16& rhs) {
			return contains(Mask::WORD, group1_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a double register. */
    constexpr bool contains(const R32& rhs) {
			return contains(Mask::DOUBLE, group1_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a quad register. */
    constexpr bool contains(const R64& rhs) {
			return contains(Mask::QUAD, group1_, (uint64_t)rhs);
		}
    /** Returns true if this set contains an xmm register. */
    constexpr bool contains(const Xmm& rhs) {
			return contains(Mask::XMM, group2_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a ymm register. */
    constexpr bool contains(const Ymm& rhs) {
			return contains(Mask::YMM, group2_, (uint64_t)rhs);
		}
    /** Returns true if this set contains an mmx register. */
    constexpr bool contains(const Mm& rhs) {
			return contains(Mask::MM, group2_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a floating point stack register. */
    constexpr bool contains(const St& rhs) {
			return contains(Mask::ST, group2_, (uint64_t)rhs);
		}
    /** Returns true if this set contains a segment register. */
    constexpr bool contains(const Sreg& rhs) {
			return contains(Mask::SREG, group4_, (uint64_t)rhs);
		}
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const FpuData& rhs) {
			return contains(Mask::DATA, group4_, 0);
		}
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const FpuInstruction& rhs) {
			return contains(Mask::INSTR, group4_, 0);
		}
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const FpuOpcode& rhs) {
			return contains(Mask::OPCODE, group4_, 0);
		}
    /** Returns true if this set contains an environment register. */
    constexpr bool contains(const Rip& rhs) {
			return contains(Mask::RIP, group4_, 0);
		}
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const Eflags& rhs) {
			return contains(Mask::EFLAG, group3_, rhs.index());
		}
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const FpuControl& rhs) {
			return contains(Mask::CONTROL, group3_, rhs.index());
		}
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const FpuStatus& rhs) {
			return contains(Mask::STATUS, group3_, rhs.index());
		}
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const FpuTag& rhs) {
			return contains(Mask::TAG, group4_, rhs.index());
		}
    /** Returns true if this set contains an environment bit. */
    constexpr bool contains(const Mxcsr& rhs) {
			return contains(Mask::MXCSR, group4_, rhs.index());
		}

    /** Returns true if this set contains any low registers. */
    constexpr bool contains_any_rl() {
			return contains_any(Mask::A_LOW, group1_);
		}
    /** Returns true if this set contains any high registers. */
    constexpr bool contains_any_rh() {
			return contains_any(Mask::A_HIGH, group1_);
		}
    /** Returns true if this set contains any byte registers. */
    constexpr bool contains_any_rb() {
			return contains_any(Mask::A_BYTE, group1_);
		}
    /** Returns true if this set contains any word registers. */
    constexpr bool contains_any_word() {
			return contains_any(Mask::A_WORD, group1_) ||
				contains(ax) || contains(bx) || contains(cx) || contains(dx);
		}
    /** Returns true if this set contains any double registers. */
    constexpr bool contains_any_double() {
			return contains_any(Mask::A_DOUBLE, group1_);
		}
    /** Returns true if this set contains any quad registers. */
    constexpr bool contains_any_quad() {
			return contains_any(Mask::A_QUAD, group1_);
		}
    /** Returns true if this set contains any xmm registers. */
    constexpr bool contains_any_xmm() {
			return contains_any(Mask::A_XMM, group2_);
		}
    /** Returns true if this set contains any ymm registers. */
    constexpr bool contains_any_ymm() {
			return contains_any(Mask::A_YMM, group2_);
		}

    /** Returns true if this set contains all low registers. */
    constexpr bool contains_all_rl() {
			return contains_all(Mask::LOWS, group1_);
		}
    /** Returns true if this set contains all high registers. */
    constexpr bool contains_all_rh() {
			return contains_all(Mask::HIGHS, group1_);
		}
    /** Returns true if this set contains all byte registers. */
    constexpr bool contains_all_rb() {
			return contains_all(Mask::BYTES, group1_);
		}
    /** Returns true if this set contains all word registers. */
    constexpr bool contains_all_word() {
			return contains_all(Mask::WORDS, group1_);
		}
    /** Returns true if this set contains all double registers. */
    constexpr bool contains_all_double() {
			return contains_all(Mask::DOUBLES, group1_);
		}
    /** Returns true if this set contains all quad registers. */
    constexpr bool contains_all_quad() {
			return contains_all(Mask::QUADS, group1_);
		}
    /** Returns true if this set contains all xmm registers. */
    constexpr bool contains_all_xmm() {
			return contains_all(Mask::XMMS, group2_);
		}
    /** Returns true if this set contains all ymm registers. */
    constexpr bool contains_all_ymm() {
			return contains_all(Mask::YMMS, group2_);
		}

    /** STL compliant hash. */
    constexpr size_t hash() {
			return group1_ ^ group2_ ^ group3_ ^ group4_;
		}
    /** STL compliant swap. */
    void swap(RegSet& rhs) {
			std::swap(group1_, rhs.group1_);
			std::swap(group2_, rhs.group2_);
			std::swap(group3_, rhs.group3_);
			std::swap(group4_, rhs.group4_);
		}

		/** Read from an istream using text. */
		std::istream& read_text(std::istream& is);
		/** Write to an ostream using text. */
		std::ostream& write_text(std::ostream& os) const;

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
    constexpr RegSet plus_group1(Mask m, uint64_t val) {
			return {group1_ | ((uint64_t)m << val), group2_, group3_, group4_};
		}
    /** Helper method for inserting elements into a group. */
    constexpr RegSet plus_group2(Mask m, uint64_t val) {
			return {group1_, group2_ | ((uint64_t)m << val), group3_, group4_};
		}
    /** Helper method for inserting elements into a group. */
    constexpr RegSet plus_group3(Mask m, uint64_t val) {
			return {group1_, group2_, group3_ | ((uint64_t)m << val), group4_};
		}
    /** Helper method for inserting elements into a group. */
    constexpr RegSet plus_group4(Mask m, uint64_t val) {
			return {group1_, group2_, group3_, group4_ | ((uint64_t)m << val)};
		}
    /** Helper method for inserting elements into a group. */
    RegSet& plus_equal(Mask m, uint64_t& group, uint64_t val) {
			group |= ((uint64_t)m << val);
			return *this;
		}
    /** Helper method for checking containment in a group. */
    constexpr bool contains(Mask m, uint64_t group, uint64_t val) {
			return ((group >> val) & (uint64_t)m) == (uint64_t)m;
		}
    /** Helper method for checking containment in a group. */
    constexpr bool contains_any(Mask m, uint64_t group) {
			return (uint64_t)m & group;
		}
    /** Helper method for checking containment in a group. */
    constexpr bool contains_all(Mask m, uint64_t group) {
			return ((uint64_t)m & group) == (uint64_t)m;
		}

    /** Iterator over GP registers in a regset */
    class GpIterator {
      friend class RegSet;

      public:
        /** Returns the current GP register we're looking at */
        R operator*() {
          return current_;
        }
        /** Checks for equality of two iterators */
        bool operator==(const GpIterator& other) {
          if (finished_)
            return other.finished_;

          return index_ == other.index_ && size_ == other.size_ && !other.finished_;
        }
        /** Checks for inequality */
        bool operator!=(const GpIterator& other) {
          return !(*this == other);
        }
        /** Advances to the next GP register */
        GpIterator& operator++();

      private:
        /** Tracks the index of the current register */
        size_t index_;
        /** Tracks the size of the current register
          (0 = 64, 1 = 32, 2 = 16, 3 = 8L, 4 = 8H) */
        size_t size_;
        /** The current register */
        R current_;
        /** If we've found all the registers */
        bool finished_;
        /** Our regset */
        const RegSet * const rs_;

        /** Creates iterator for GPs */
        GpIterator(const RegSet* const rs) : rs_(rs), index_(0), size_(-1), current_(rax) {
          ++(*this);
        }
        /** Go to end */
        GpIterator& finish() {
          finished_ = true;
          return *this;
        }
    };

    /** Iterate over largest general-purpose registers */
    GpIterator gp_begin() const {
      return GpIterator(this);
    }
    /** End iterator for the gp registers */
    GpIterator gp_end() const {
      return GpIterator(this).finish();
    }


};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::RegSet> {
  size_t operator()(const x64asm::RegSet& rs) const {
		return rs.hash();
	}
};

/** STL swap overload. */
inline void swap(x64asm::RegSet& lhs, x64asm::RegSet& rhs) {
	lhs.swap(rhs);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::RegSet& rs) {
	return rs.read_text(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::RegSet& rs) {
	return rs.write_text(os);
}

} // namespace std

#endif
