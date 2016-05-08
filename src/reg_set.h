/*
Copyright 2013-2015 Stanford University

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

namespace x64asm {

/** A compact implementation of a bit set for registers. */
class RegSet {
  friend class Instruction;
private:
  /** Per register type position masks. */
  enum class Mask : uint64_t {
    // Group 1 (general purpose)
    BYTE     = 0x0000000000000001,
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
    A_HIGH   = 0x00000000000f0000,
    A_BYTE   = 0x000000000000ffff,
    A_WORD   = 0x00000000fff00000, // (this one is tricky -- see below)
    A_DOUBLE = 0x0000ffff00000000,
    A_QUAD   = 0xffff000000000000,
    A_XMM    = 0x000000000000ffff,
    A_YMM    = 0x00000000ffff0000,
    A_MM     = 0x00ff000000000000,
    // All Masks
    HIGHS    = 0x00000000000f0000,
    BYTES    = 0x000000000000ffff,
    WORDS    = 0x00000000ffffffff,
    DOUBLES  = 0x0000ffffffffffff,
    QUADS    = 0xffffffffffffffff,
    XMMS     = 0x000000000000ffff,
    YMMS     = 0x00000000ffffffff,
    MMS      = 0x00ff000000000000,
    // Top and Bottom
    EMPTY    = 0x0000000000000000,
    UNIV1    = 0xffffffffffffffff,
    UNIV2    = 0xffffffffffffffff,
    UNIV3    = 0xe7ff1a3f003f7fd5,
    UNIV4    = 0x0000000f3fffffff
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
  /** Creates a register set containing all xmm registers. */
  static constexpr RegSet all_mms() {
    return {Mask::EMPTY, Mask::MMS, Mask::EMPTY, Mask::EMPTY};
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
  constexpr RegSet operator~() const {
    return {~group1_, ~group2_, ~group3_, ~group4_};
  }
  /** Set intersection. */
  constexpr RegSet operator&(const RegSet& rhs) const {
    return {group1_ & rhs.group1_, group2_ & rhs.group2_,
            group3_ & rhs.group3_, group4_ & rhs.group4_};
  }
  /** Set union. */
  constexpr RegSet operator|(const RegSet& rhs) const {
    return {group1_ | rhs.group1_, group2_ | rhs.group2_,
            group3_ | rhs.group3_, group4_ | rhs.group4_};
  }
  /** Set difference. */
  constexpr RegSet operator-(const RegSet& rhs) const {
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
  constexpr bool operator==(const RegSet& rhs) const {
    return group1_ == rhs.group1_ && group2_ == rhs.group2_ &&
           group3_ == rhs.group3_ && group4_ == rhs.group4_;
  }
  /** Set inequality. */
  constexpr bool operator!=(const RegSet& rhs) const {
    return !(*this == rhs);
  }
  /** Set containment. */
  constexpr bool contains(const RegSet& rhs) const {
    return (*this & rhs) == rhs;
  }
  /** Set intersection. */
  constexpr bool intersects(const RegSet& rhs) const {
    return (*this - rhs) != *this;
  }

  /** Insert a high register. */
  constexpr RegSet operator+(const Rh& rhs) const {
    return plus_group1(Mask::HIGH, (uint64_t)rhs-4);
  }
  /** Insert a byte register. */
  constexpr RegSet operator+(const R8& rhs) const {
    return plus_group1(Mask::BYTE, (uint64_t)rhs);
  }
  /** Insert a word register. */
  constexpr RegSet operator+(const R16& rhs) const {
    return plus_group1(Mask::WORD, (uint64_t)rhs);
  }
  /** Insert a double register. */
  constexpr RegSet operator+(const R32& rhs) const {
    return plus_group1(Mask::DOUBLE, (uint64_t)rhs);
  }
  /** Insert a quad register. */
  constexpr RegSet operator+(const R64& rhs) const {
    return plus_group1(Mask::QUAD, (uint64_t)rhs);
  }
  /** Insert an xmm register. */
  constexpr RegSet operator+(const Xmm& rhs) const {
    return plus_group2(Mask::XMM, (uint64_t)rhs);
  }
  /** Insert a ymm register. */
  constexpr RegSet operator+(const Ymm& rhs) const {
    return plus_group2(Mask::YMM, (uint64_t)rhs);
  }
  /** Insert an mmx register. */
  constexpr RegSet operator+(const Mm& rhs) const {
    return plus_group2(Mask::MM, (uint64_t)rhs);
  }
  /** Insert a floating point stack register. */
  constexpr RegSet operator+(const St& rhs) const {
    return plus_group2(Mask::ST, (uint64_t)rhs);
  }
  /** Insert a segment register. */
  constexpr RegSet operator+(const Sreg& rhs) const {
    return plus_group4(Mask::SREG, (uint64_t)rhs);
  }
  /** Insert an environment register. */
  constexpr RegSet operator+(const FpuData& rhs) const {
    return plus_group4(Mask::DATA, 0);
  }
  /** Insert an environment register. */
  constexpr RegSet operator+(const FpuInstruction& rhs) const {
    return plus_group4(Mask::INSTR, 0);
  }
  /** Insert an environment register. */
  constexpr RegSet operator+(const FpuOpcode& rhs) const {
    return plus_group4(Mask::OPCODE, 0);
  }
  /** Insert an environment register. */
  constexpr RegSet operator+(const Rip& rhs) const {
    return plus_group4(Mask::RIP, 0);
  }
  /** Insert environment bits. */
  constexpr RegSet operator+(const Eflags& rhs) const {
    return plus_group3(Mask::EFLAG, rhs.index());
  }
  /** Insert environment bits. */
  constexpr RegSet operator+(const FpuControl& rhs) const {
    return plus_group3(Mask::CONTROL, rhs.index());
  }
  /** Insert environment bits. */
  constexpr RegSet operator+(const FpuStatus& rhs) const {
    return plus_group3(Mask::STATUS, rhs.index());
  }
  /** Insert environment bits. */
  constexpr RegSet operator+(const FpuTag& rhs) const {
    return plus_group4(Mask::TAG, rhs.index());
  }
  /** Insert environment bits. */
  constexpr RegSet operator+(const Mxcsr& rhs) const {
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

  /** Insert a high register. */
  RegSet& operator+=(const Rh& rhs) {
    return plus_equal(Mask::HIGH, group1_, (uint64_t)rhs - 4);
  }
  /** Insert a byte register. */
  RegSet& operator+=(const R8& rhs) {
    return plus_equal(Mask::BYTE, group1_, (uint64_t)rhs);
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

  /** Returns true if this set contains a high register. */
  constexpr bool contains(const Rh& rhs) const {
    return contains(Mask::HIGH, group1_, (uint64_t)rhs - 4);
  }
  /** Returns true if this set contains a byte register. */
  constexpr bool contains(const R8& rhs) const {
    return contains(Mask::BYTE, group1_, (uint64_t)rhs);
  }
  /** Returns true if this set contains a word register. */
  constexpr bool contains(const R16& rhs) const {
    return contains(Mask::WORD, group1_, (uint64_t)rhs);
  }
  /** Returns true if this set contains a double register. */
  constexpr bool contains(const R32& rhs) const {
    return contains(Mask::DOUBLE, group1_, (uint64_t)rhs);
  }
  /** Returns true if this set contains a quad register. */
  constexpr bool contains(const R64& rhs) const {
    return contains(Mask::QUAD, group1_, (uint64_t)rhs);
  }
  /** Returns true if this set contains an xmm register. */
  constexpr bool contains(const Xmm& rhs) const {
    return contains(Mask::XMM, group2_, (uint64_t)rhs);
  }
  /** Returns true if this set contains a ymm register. */
  constexpr bool contains(const Ymm& rhs) const {
    return contains(Mask::YMM, group2_, (uint64_t)rhs);
  }
  /** Returns true if this set contains an mmx register. */
  constexpr bool contains(const Mm& rhs) const {
    return contains(Mask::MM, group2_, (uint64_t)rhs);
  }
  /** Returns true if this set contains a floating point stack register. */
  constexpr bool contains(const St& rhs) const {
    return contains(Mask::ST, group2_, (uint64_t)rhs);
  }
  /** Returns true if this set contains a segment register. */
  constexpr bool contains(const Sreg& rhs) const {
    return contains(Mask::SREG, group4_, (uint64_t)rhs);
  }
  /** Returns true if this set contains an environment register. */
  constexpr bool contains(const FpuData& rhs) const {
    return contains(Mask::DATA, group4_, 0);
  }
  /** Returns true if this set contains an environment register. */
  constexpr bool contains(const FpuInstruction& rhs) const {
    return contains(Mask::INSTR, group4_, 0);
  }
  /** Returns true if this set contains an environment register. */
  constexpr bool contains(const FpuOpcode& rhs) const {
    return contains(Mask::OPCODE, group4_, 0);
  }
  /** Returns true if this set contains an environment register. */
  constexpr bool contains(const Rip& rhs) const {
    return contains(Mask::RIP, group4_, 0);
  }
  /** Returns true if this set contains an environment bit. */
  constexpr bool contains(const Eflags& rhs) const {
    return contains(Mask::EFLAG, group3_, rhs.index());
  }
  /** Returns true if this set contains an environment bit. */
  constexpr bool contains(const FpuControl& rhs) const {
    return contains(Mask::CONTROL, group3_, rhs.index());
  }
  /** Returns true if this set contains an environment bit. */
  constexpr bool contains(const FpuStatus& rhs) const {
    return contains(Mask::STATUS, group3_, rhs.index());
  }
  /** Returns true if this set contains an environment bit. */
  constexpr bool contains(const FpuTag& rhs) const {
    return contains(Mask::TAG, group4_, rhs.index());
  }
  /** Returns true if this set contains an environment bit. */
  constexpr bool contains(const Mxcsr& rhs) const {
    return contains(Mask::MXCSR, group4_, rhs.index());
  }

  /** Returns true if this set contains any high registers. */
  constexpr bool contains_any_rh() const {
    return contains_any(Mask::A_HIGH, group1_);
  }
  /** Returns true if this set contains any byte registers. */
  constexpr bool contains_any_r8() const {
    return contains_any(Mask::A_BYTE, group1_);
  }
  /** Returns true if this set contains any word registers. */
  constexpr bool contains_any_word() const {
    return contains_any(Mask::A_WORD, group1_) || 
           contains(Constants::ax()) || contains(Constants::bx()) || 
           contains(Constants::cx()) || contains(Constants::dx());
  }
  /** Returns true if this set contains any double registers. */
  constexpr bool contains_any_double() const {
    return contains_any(Mask::A_DOUBLE, group1_);
  }
  /** Returns true if this set contains any quad registers. */
  constexpr bool contains_any_quad() const {
    return contains_any(Mask::A_QUAD, group1_);
  }
  /** Returns true if this set contains any xmm registers. */
  constexpr bool contains_any_xmm() const {
    return contains_any(Mask::A_XMM, group2_);
  }
  /** Returns true if this set contains any ymm registers. */
  constexpr bool contains_any_ymm() const {
    return contains_any(Mask::A_YMM, group2_);
  }
  /** Returns true if this set contains any mm registers. */
  constexpr bool contains_any_mm() const {
    return contains_any(Mask::A_MM, group2_);
  }

  /** Returns true if this set contains all high registers. */
  constexpr bool contains_all_rh() const {
    return contains_all(Mask::HIGHS, group1_);
  }
  /** Returns true if this set contains all byte registers. */
  constexpr bool contains_all_r8() const {
    return contains_all(Mask::BYTES, group1_);
  }
  /** Returns true if this set contains all word registers. */
  constexpr bool contains_all_word() const {
    return contains_all(Mask::WORDS, group1_);
  }
  /** Returns true if this set contains all double registers. */
  constexpr bool contains_all_double() const {
    return contains_all(Mask::DOUBLES, group1_);
  }
  /** Returns true if this set contains all quad registers. */
  constexpr bool contains_all_quad() const {
    return contains_all(Mask::QUADS, group1_);
  }
  /** Returns true if this set contains all xmm registers. */
  constexpr bool contains_all_xmm() const {
    return contains_all(Mask::XMMS, group2_);
  }
  /** Returns true if this set contains all ymm registers. */
  constexpr bool contains_all_ymm() const {
    return contains_all(Mask::YMMS, group2_);
  }
  /** Returns true if this set contains all mm registers. */
  constexpr bool contains_all_mm() const {
    return contains_all(Mask::XMM, group2_);
  }

  /** Iterator over general purpose registers */
  class gp_iterator {
    friend class RegSet;
  public:
    R operator*() const {
      if (rs_->contains(Constants::r64s()[idx_])) {
        return (R)Constants::r64s()[idx_];
      } else if (rs_->contains(Constants::r32s()[idx_])) {
        return (R)Constants::r32s()[idx_];
      } else if (rs_->contains(Constants::r16s()[idx_])) {
        return (R)Constants::r16s()[idx_];
      } else if (rs_->contains(Constants::r8s()[idx_])) {
        return (R)Constants::r8s()[idx_];
      } else if (idx_ < 4 && rs_->contains(Constants::rhs()[idx_])) {
        return (R)Constants::rhs()[idx_];
      } else {
        assert(false);
        return (R)Constants::rax();
      }
    }
    bool operator==(const gp_iterator& rhs) const {
      return idx_ == rhs.idx_ && rs_ == rhs.rs_;
    }
    bool operator!=(const gp_iterator& rhs) const {
      return !(*this == rhs);
    }
    gp_iterator& operator++() {
      for (; (++idx_ < 16) && !found_any(););
      return *this;
    }

  private:
    const RegSet* rs_;
    size_t idx_;

    gp_iterator(const RegSet* rs) : rs_(rs), idx_(0) {
      if (!found_any()) {
        ++(*this);
      }
    }
    gp_iterator(const RegSet* rs, size_t idx) : rs_(rs), idx_(idx) {
    }
    bool found_any() const {
      return ((uint64_t)Mask::WORD << idx_) & rs_->group1_;
    }
  };

  /** Iterate over largest general-purpose registers */
  gp_iterator gp_begin() const {
    return gp_iterator(this);
  }
  /** End iterator for the gp registers */
  gp_iterator gp_end() const {
    return gp_iterator(this, 16);
  }

  /** Iterator over sse registers */
  class sse_iterator {
    friend class RegSet;
  public:
    Sse operator*() const {
      if (rs_->contains(Constants::ymms()[idx_])) {
        return (Sse)Constants::ymms()[idx_];
      } else if (rs_->contains(Constants::xmms()[idx_])) {
        return (Sse)Constants::xmms()[idx_];
      } else {
        assert(false);
        return (Sse)Constants::xmm0();
      }
    }
    bool operator==(const sse_iterator& rhs) const {
      return idx_ == rhs.idx_ && rs_ == rhs.rs_;
    }
    bool operator!=(const sse_iterator& rhs) const {
      return !(*this == rhs);
    }
    sse_iterator& operator++() {
      for (; (++idx_ < 16) && !found_any(););
      return *this;
    }

  private:
    const RegSet* rs_;
    size_t idx_;

    sse_iterator(const RegSet* rs) : rs_(rs), idx_(0) {
      if (!found_any()) {
        ++(*this);
      }
    }
    sse_iterator(const RegSet* rs, size_t idx) : rs_(rs), idx_(idx) {
    }
    bool found_any() const {
      return ((uint64_t)Mask::XMM << idx_) & rs_->group2_;
    }
  };

  /** Iterates over SSE registers */
  sse_iterator sse_begin() const {
    return sse_iterator(this);
  }
  /** End iterator for the SSE registers */
  sse_iterator sse_end() const {
    return sse_iterator(this, 16);
  }

  /** Iterator over mm registers */
  class mm_iterator {
    friend class RegSet;
  public:
    Mm operator*() const {
      return Constants::mms()[idx_];
    }
    bool operator==(const mm_iterator& rhs) const {
      return idx_ == rhs.idx_ && rs_ == rhs.rs_;
    }
    bool operator!=(const mm_iterator& rhs) const {
      return !(*this == rhs);
    }
    mm_iterator& operator++() {
      for (; (++idx_ < 8) && !rs_->contains(Constants::mms()[idx_]););
      return *this;
    }

  private:
    const RegSet* rs_;
    size_t idx_;

    mm_iterator(const RegSet* rs) : rs_(rs), idx_(0) {
      if (!rs_->contains(Constants::mms()[idx_])) {
        ++(*this);
      }
    }
    mm_iterator(const RegSet* rs, size_t idx) : rs_(rs), idx_(idx) {
    }
  };

  /** Iterates over MM registers */
  mm_iterator mm_begin() const {
    return mm_iterator(this);
  }
  /** End iterator for the MM registers */
  mm_iterator mm_end() const {
    return mm_iterator(this, 8);
  }

  /** Iterator over status flags bits */
  class flags_iterator {
    friend class RegSet;
  public:
    Eflags operator*() const {
      return Constants::eflags()[idx_];
    }
    bool operator==(const flags_iterator& rhs) const {
      return idx_ == rhs.idx_ && rs_ == rhs.rs_;
    }
    bool operator!=(const flags_iterator& rhs) const {
      return !(*this == rhs);
    }
    flags_iterator& operator++() {
      for (; (inc() < 12) && !rs_->contains(Constants::eflags()[idx_]););
      return *this;
    }

  private:
    const RegSet* rs_;
    size_t idx_;

    flags_iterator(const RegSet* rs) : rs_(rs), idx_(0) {
      if (!rs_->contains(Constants::eflags()[idx_])) {
        ++(*this);
      }
    }
    flags_iterator(const RegSet* rs, size_t idx) : rs_(rs), idx_(idx) {
    }
    size_t& inc() {
      switch (idx_) {
        case 0:
        case 2:
        case 4:
          return (idx_ += 2);
        case 7:
          return (idx_ += 4);
        default:
          return (++idx_);
      }
    }
  };

  /** Iterates for the status eflags */
  flags_iterator flags_begin() const {
    return flags_iterator(this);
  }
  /** End iterator for the status eflags */
  flags_iterator flags_end() const {
    return flags_iterator(this, 12);
  }

  /** Iterator over 64-bit registers with any sub-registers set */
  class any_sub_gp_iterator {
    friend class RegSet;
  public:
    R64 operator*() const {
      return Constants::r64s()[idx_];
    }
    bool operator==(const any_sub_gp_iterator& rhs) const {
      return idx_ == rhs.idx_ && rs_ == rhs.rs_;
    }
    bool operator!=(const any_sub_gp_iterator& rhs) const {
      return !(*this == rhs);
    }
    any_sub_gp_iterator& operator++() {
      for (; (++idx_ < 16) && !found_any(););
      return *this;
    }

  private:
    const RegSet* rs_;
    size_t idx_;

    any_sub_gp_iterator(const RegSet* rs) : rs_(rs), idx_(0) {
      if (!found_any()) {
        ++(*this);
      }
    }
    any_sub_gp_iterator(const RegSet* rs, size_t idx) : rs_(rs), idx_(idx) {
    }
    bool found_any() const {
      return ((uint64_t)Mask::WORD << idx_) & rs_->group1_;
    }
  };

  any_sub_gp_iterator any_sub_gp_begin() const {
    return any_sub_gp_iterator(this);
  }
  any_sub_gp_iterator any_sub_gp_end() const {
    return any_sub_gp_iterator(this, 16);
  }

  /** Iterator over ymm registers with any sub-registers set */
  class any_sub_sse_iterator {
    friend class RegSet;
  public:
    Ymm operator*() const {
      return Constants::ymms()[idx_];
    }
    bool operator==(const any_sub_sse_iterator& rhs) const {
      return idx_ == rhs.idx_ && rs_ == rhs.rs_;
    }
    bool operator!=(const any_sub_sse_iterator& rhs) const {
      return !(*this == rhs);
    }
    any_sub_sse_iterator& operator++() {
      for (; (++idx_ < 16) && !found_any(););
      return *this;
    }

  private:
    const RegSet* rs_;
    size_t idx_;

    any_sub_sse_iterator(const RegSet* rs) : rs_(rs), idx_(0) {
      if (!found_any()) {
        ++(*this);
      }
    }
    any_sub_sse_iterator(const RegSet* rs, size_t idx) : rs_(rs), idx_(idx) {
    }
    bool found_any() const {
      return ((uint64_t)Mask::XMM << idx_) & rs_->group2_;
    }
  };

  any_sub_sse_iterator any_sub_sse_begin() const {
    return any_sub_sse_iterator(this);
  }
  any_sub_sse_iterator any_sub_sse_end() const {
    return any_sub_sse_iterator(this, 16);
  }
  /** STL compliant hash. */
  constexpr size_t hash() const {
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
  constexpr RegSet plus_group1(Mask m, uint64_t val) const {
    return {group1_ | ((uint64_t)m << val), group2_, group3_, group4_};
  }
  /** Helper method for inserting elements into a group. */
  constexpr RegSet plus_group2(Mask m, uint64_t val) const {
    return {group1_, group2_ | ((uint64_t)m << val), group3_, group4_};
  }
  /** Helper method for inserting elements into a group. */
  constexpr RegSet plus_group3(Mask m, uint64_t val) const {
    return {group1_, group2_, group3_ | ((uint64_t)m << val), group4_};
  }
  /** Helper method for inserting elements into a group. */
  constexpr RegSet plus_group4(Mask m, uint64_t val) const {
    return {group1_, group2_, group3_, group4_ | ((uint64_t)m << val)};
  }
  /** Helper method for inserting elements into a group. */
  RegSet& plus_equal(Mask m, uint64_t& group, uint64_t val) {
    group |= ((uint64_t)m << val);
    return *this;
  }
  /** Helper method for checking containment in a group. */
  constexpr bool contains(Mask m, uint64_t group, uint64_t val) const {
    return ((group >> val) & (uint64_t)m) == (uint64_t)m;
  }
  /** Helper method for checking containment in a group. */
  constexpr bool contains_any(Mask m, uint64_t group) const {
    return (uint64_t)m & group;
  }
  /** Helper method for checking containment in a group. */
  constexpr bool contains_all(Mask m, uint64_t group) const {
    return ((uint64_t)m & group) == (uint64_t)m;
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
