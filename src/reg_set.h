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
    constexpr RegSet(uint64_t g1, uint64_t g2, uint64_t g3, uint64_t g4);
    /** Creates a register set from four position masks. */
    constexpr RegSet(Mask g1, Mask g2, Mask g3, Mask g4);

  public:
    /** Creates an empty register set. */
    constexpr RegSet();

    /** Creates an empty register set. */
    static constexpr RegSet empty();
    /** Creates a register set containing all general purpose registers. */
    static constexpr RegSet all_gps();
    /** Creates a register set containing all xmm registers. */
    static constexpr RegSet all_xmms();
    /** Creates a register set containing all ymm registers. */
    static constexpr RegSet all_ymms();
    /** Creates a register set containing linux caller save registers. */
    static constexpr RegSet linux_caller_save();
    /** Creates a register set containing linux callee save registers. */
    static constexpr RegSet linux_callee_save();
    /** Creates a register set containing registers that must be preserved
        by the callee. */
    static constexpr RegSet linux_call_preserved();
    /** Creates a regster set containing registers that can be changed by 
        the callee at any time. */
    static constexpr RegSet linux_call_scratch();
    /** Creates a RegSet containing registers used as parameters. */
    static constexpr RegSet linux_call_parameters();
    /** Creates a RegSet containing registers returned by linux calls. */
    static constexpr RegSet linux_call_return();

    /** Creates a retister set containing windows caller save reigsters. */
    static constexpr RegSet windows_caller_save();
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
    constexpr RegSet operator+(const R8& rhs);
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
    RegSet& operator+=(const R8& rhs);
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
    constexpr bool contains(const R8& rhs);
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

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::RegSet> {
  size_t operator()(const x64asm::RegSet& rs) const;
};

/** STL swap overload. */
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

inline constexpr RegSet::RegSet() :
  group1_ {(uint64_t)Mask::EMPTY}, group2_ {(uint64_t)Mask::EMPTY},
  group3_ {(uint64_t)Mask::EMPTY}, group4_ {(uint64_t)Mask::EMPTY} {
}

inline constexpr RegSet RegSet::empty() {
  return RegSet {Mask::EMPTY, Mask::EMPTY, Mask::EMPTY, Mask::EMPTY};
}

inline constexpr RegSet RegSet::all_gps() {
  return RegSet {Mask::QUADS, Mask::EMPTY, Mask::EMPTY, Mask::EMPTY};
}

inline constexpr RegSet RegSet::all_xmms() {
  return RegSet {Mask::EMPTY, Mask::XMMS, Mask::EMPTY, Mask::EMPTY};
}

inline constexpr RegSet RegSet::all_ymms() {
  return RegSet {Mask::EMPTY, Mask::YMMS, Mask::EMPTY, Mask::EMPTY};
}

inline constexpr RegSet RegSet::linux_caller_save() {
  return all_xmms() + 
      Constants::rdi()  + Constants::rsi()  + Constants::rdx()  + 
      Constants::rcx()  + Constants::r8()   + Constants::r9();
}

inline constexpr RegSet RegSet::linux_callee_save() {
  return empty() + 
      Constants::rbx()  + Constants::rbp()  + Constants::rsp()  + 
      Constants::r12()  + Constants::r13()  + Constants::r14()  +
      Constants::r15();
}

inline constexpr RegSet RegSet::linux_call_parameters() {
  return empty() + 
      Constants::xmm0() + Constants::xmm1() + Constants::xmm2() +
      Constants::xmm3() + Constants::xmm4() + Constants::xmm5() +
      Constants::xmm6() + Constants::xmm7() +
      Constants::rdi()  + Constants::rsi()  + Constants::rdx()  + 
      Constants::rcx()  + Constants::r8()   + Constants::r9();
}

inline constexpr RegSet RegSet::linux_call_scratch() {
  return all_xmms() + 
      Constants::rdi()  + Constants::rsi()  + Constants::rdx()  + 
      Constants::rcx()  + Constants::r8()   + Constants::r9() +
      Constants::r10()  + Constants::r11()  + Constants::rax();
}

inline constexpr RegSet RegSet::linux_call_preserved() {
  return empty() + 
      Constants::rbx()  + Constants::rbp()  + Constants::rsp()  + 
      Constants::r12()  + Constants::r13()  + Constants::r14()  +
      Constants::r15();
}

inline constexpr RegSet RegSet::linux_call_return() {
  return empty() +
      Constants::rax() + Constants::rdx() +
      Constants::xmm0() + Constants::xmm1();
}


inline constexpr RegSet RegSet::windows_caller_save() {
  return empty() + 
      Constants::rcx()  + Constants::rdx()  + 
      Constants::r8()   + Constants::r9()   + 
      Constants::xmm0() + Constants::xmm1() + 
      Constants::xmm2() + Constants::xmm3();
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
  return plus_group1(Mask::LOW, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const Rh& rhs) {
  return plus_group1(Mask::HIGH, (uint64_t)rhs-4);
}

inline constexpr RegSet RegSet::operator+(const R8& rhs) {
  return plus_group1(Mask::LOW, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const R16& rhs) {
  return plus_group1(Mask::WORD, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const R32& rhs) {
  return plus_group1(Mask::DOUBLE, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const R64& rhs) {
  return plus_group1(Mask::QUAD, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const Xmm& rhs) {
  return plus_group2(Mask::XMM, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const Ymm& rhs) {
  return plus_group2(Mask::YMM, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const Mm& rhs) {
  return plus_group2(Mask::MM, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const St& rhs) {
  return plus_group2(Mask::ST, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const Sreg& rhs) {
  return plus_group4(Mask::SREG, (uint64_t)rhs);
}

inline constexpr RegSet RegSet::operator+(const FpuData& rhs) {
  return plus_group4(Mask::DATA, 0);
}

inline constexpr RegSet RegSet::operator+(const FpuInstruction& rhs) {
  return plus_group4(Mask::INSTR, 0);
}

inline constexpr RegSet RegSet::operator+(const FpuOpcode& rhs) {
  return plus_group4(Mask::OPCODE, 0);
}

inline constexpr RegSet RegSet::operator+(const Rip& rhs) {
  return plus_group4(Mask::RIP, 0);
}

inline constexpr RegSet RegSet::operator+(const Eflags& rhs) {
  return plus_group3(Mask::EFLAG, rhs.index());
}

inline constexpr RegSet RegSet::operator+(const FpuControl& rhs) {
  return plus_group3(Mask::CONTROL, rhs.index());
}

inline constexpr RegSet RegSet::operator+(const FpuStatus& rhs) {
  return plus_group3(Mask::STATUS, rhs.index());
}

inline constexpr RegSet RegSet::operator+(const FpuTag& rhs) {
  return plus_group4(Mask::TAG, rhs.index());
}

inline constexpr RegSet RegSet::operator+(const Mxcsr& rhs) {
  return plus_group4(Mask::MXCSR, rhs.index());
}

inline RegSet RegSet::operator+(const M& rhs) const {
  auto ret = *this;
  return ret += rhs;
}

inline RegSet RegSet::operator+(const Moffs& rhs) const {
  return rhs.contains_seg() ? plus_group4(Mask::SREG, rhs.get_seg()) : *this;
}

inline RegSet& RegSet::operator+=(const Rl& rhs) {
  return plus_equal(Mask::LOW, group1_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const Rh& rhs) {
  return plus_equal(Mask::HIGH, group1_, (uint64_t)rhs - 4);
}

inline RegSet& RegSet::operator+=(const R8& rhs) {
  return plus_equal(Mask::LOW, group1_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const R16& rhs) {
  return plus_equal(Mask::WORD, group1_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const R32& rhs) {
  return plus_equal(Mask::DOUBLE, group1_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const R64& rhs) {
  return plus_equal(Mask::QUAD, group1_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const Xmm& rhs) {
  return plus_equal(Mask::XMM, group2_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const Ymm& rhs) {
  return plus_equal(Mask::YMM, group2_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const Mm& rhs) {
  return plus_equal(Mask::MM, group2_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const St& rhs) {
  return plus_equal(Mask::ST, group2_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const Sreg& rhs) {
  return plus_equal(Mask::SREG, group4_, (uint64_t)rhs);
}

inline RegSet& RegSet::operator+=(const FpuData& rhs) {
  return plus_equal(Mask::DATA, group4_, 0);
}

inline RegSet& RegSet::operator+=(const FpuInstruction& rhs) {
  return plus_equal(Mask::INSTR, group4_, 0);
}

inline RegSet& RegSet::operator+=(const FpuOpcode& rhs) {
  return plus_equal(Mask::OPCODE, group4_, 0);
}

inline RegSet& RegSet::operator+=(const Rip& rhs) {
  return plus_equal(Mask::RIP, group4_, 0);
}

inline RegSet& RegSet::operator+=(const Eflags& rhs) {
  return plus_equal(Mask::EFLAG, group3_, rhs.index());
}

inline RegSet& RegSet::operator+=(const FpuControl& rhs) {
  return plus_equal(Mask::CONTROL, group3_, rhs.index());
}

inline RegSet& RegSet::operator+=(const FpuStatus& rhs) {
  return plus_equal(Mask::STATUS, group3_, rhs.index());
}

inline RegSet& RegSet::operator+=(const FpuTag& rhs) {
  return plus_equal(Mask::TAG, group4_, rhs.index());
}

inline RegSet& RegSet::operator+=(const Mxcsr& rhs) {
  return plus_equal(Mask::MXCSR, group4_, rhs.index());
}

inline RegSet& RegSet::operator+=(const M& rhs) {
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

inline RegSet& RegSet::operator+=(const Moffs& rhs) {
  return rhs.contains_seg() ? plus_equal(Mask::SREG, group4_, rhs.get_seg()) : *this;
}

inline constexpr bool RegSet::contains(const Rl& rhs) {
  return contains(Mask::LOW, group1_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const Rh& rhs) {
  return contains(Mask::HIGH, group1_, (uint64_t)rhs - 4);
}

inline constexpr bool RegSet::contains(const R8& rhs) {
  return contains(Mask::LOW, group1_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const R16& rhs) {
  return contains(Mask::WORD, group1_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const R32& rhs) {
  return contains(Mask::DOUBLE, group1_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const R64& rhs) {
  return contains(Mask::QUAD, group1_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const Xmm& rhs) {
  return contains(Mask::XMM, group2_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const Ymm& rhs) {
  return contains(Mask::YMM, group2_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const Mm& rhs) {
  return contains(Mask::MM, group2_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const St& rhs) {
  return contains(Mask::ST, group2_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const Sreg& rhs) {
  return contains(Mask::SREG, group4_, (uint64_t)rhs);
}

inline constexpr bool RegSet::contains(const FpuData& rhs) {
  return contains(Mask::DATA, group4_, 0);
}

inline constexpr bool RegSet::contains(const FpuInstruction& rhs) {
  return contains(Mask::INSTR, group4_, 0);
}

inline constexpr bool RegSet::contains(const FpuOpcode& rhs) {
  return contains(Mask::OPCODE, group4_, 0);
}

inline constexpr bool RegSet::contains(const Rip& rhs) {
  return contains(Mask::RIP, group4_, 0);
}

inline constexpr bool RegSet::contains(const Eflags& rhs) {
  return contains(Mask::EFLAG, group3_, rhs.index());
}

inline constexpr bool RegSet::contains(const FpuControl& rhs) {
  return contains(Mask::CONTROL, group3_, rhs.index());
}

inline constexpr bool RegSet::contains(const FpuStatus& rhs) {
  return contains(Mask::STATUS, group3_, rhs.index());
}

inline constexpr bool RegSet::contains(const FpuTag& rhs) {
  return contains(Mask::TAG, group4_, rhs.index());
}

inline constexpr bool RegSet::contains(const Mxcsr& rhs) {
  return contains(Mask::MXCSR, group4_, rhs.index());
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

inline size_t hash<x64asm::RegSet>::operator()(const x64asm::RegSet& rs) const {
  return rs.hash();
}

inline void swap(x64asm::RegSet& lhs, x64asm::RegSet& rhs) {
  lhs.swap(rhs);
}

inline ostream& operator<<(ostream& os, const x64asm::RegSet& rs) {

  class gp_stack {

    public:
      x64asm::R64 r64;
      x64asm::R32 r32;
      x64asm::R16 r16;
      x64asm::R8  r8;

      gp_stack(x64asm::R64 r64_, x64asm::R32 r32_, 
               x64asm::R16 r16_, x64asm::R8 r8_) : 
                r8(r8_), r16(r16_), r32(r32_), r64(r64_) {}
  };

  class sse_stack {

    public:
      x64asm::Xmm xmm;
      x64asm::Ymm ymm;

      sse_stack(x64asm::Xmm xmm_, x64asm::Ymm ymm_) :
        xmm(xmm_), ymm(ymm_) {}
  };

  vector<sse_stack> sse_registers = 
  {
    sse_stack(x64asm::Constants::xmm0(),
              x64asm::Constants::ymm0()),
    sse_stack(x64asm::Constants::xmm1(),
              x64asm::Constants::ymm1()),
    sse_stack(x64asm::Constants::xmm2(),
              x64asm::Constants::ymm2()),
    sse_stack(x64asm::Constants::xmm3(),
              x64asm::Constants::ymm3()),
    sse_stack(x64asm::Constants::xmm4(),
              x64asm::Constants::ymm4()),
    sse_stack(x64asm::Constants::xmm5(),
              x64asm::Constants::ymm5()),
    sse_stack(x64asm::Constants::xmm6(),
              x64asm::Constants::ymm6()),
    sse_stack(x64asm::Constants::xmm7(),
              x64asm::Constants::ymm7()),
    sse_stack(x64asm::Constants::xmm8(),
              x64asm::Constants::ymm8()),
    sse_stack(x64asm::Constants::xmm9(),
              x64asm::Constants::ymm9()),
    sse_stack(x64asm::Constants::xmm10(),
              x64asm::Constants::ymm10()),
    sse_stack(x64asm::Constants::xmm11(),
              x64asm::Constants::ymm11()),
    sse_stack(x64asm::Constants::xmm12(),
              x64asm::Constants::ymm12()),
    sse_stack(x64asm::Constants::xmm13(),
              x64asm::Constants::ymm13()),
    sse_stack(x64asm::Constants::xmm14(),
              x64asm::Constants::ymm14()),
    sse_stack(x64asm::Constants::xmm15(),
              x64asm::Constants::ymm15())

  };

  vector<gp_stack> possible_registers = 
    {
      gp_stack(
        (x64asm::R64)x64asm::Constants::rax(),
        (x64asm::R32)x64asm::Constants::eax(),
        (x64asm::R16)x64asm::Constants::ax(),
        (x64asm::R8)x64asm::Constants::al()
      ),

      gp_stack(
        x64asm::Constants::rbx(),
        x64asm::Constants::ebx(),
        x64asm::Constants::bx(),
        x64asm::Constants::bl()
      ),

      gp_stack(
        x64asm::Constants::rcx(),
        x64asm::Constants::ecx(),
        x64asm::Constants::cx(),
        x64asm::Constants::cl()
      ),

      gp_stack(
        x64asm::Constants::rdx(),
        x64asm::Constants::edx(),
        x64asm::Constants::dx(),
        x64asm::Constants::dl()
      ),

      gp_stack(
        x64asm::Constants::rsi(),
        x64asm::Constants::esi(),
        x64asm::Constants::si(),
        x64asm::Constants::sil()
      ),

      gp_stack(
        x64asm::Constants::rdi(),
        x64asm::Constants::edi(),
        x64asm::Constants::di(),
        x64asm::Constants::dil()
      ),

      gp_stack(
        x64asm::Constants::rbp(),
        x64asm::Constants::ebp(),
        x64asm::Constants::bp(),
        x64asm::Constants::bpl()
      ),

      gp_stack(
        x64asm::Constants::rsp(),
        x64asm::Constants::esp(),
        x64asm::Constants::sp(),
        x64asm::Constants::spl()
      ),

      gp_stack(
        x64asm::Constants::r8(),
        x64asm::Constants::r8d(),
        x64asm::Constants::r8w(),
        x64asm::Constants::r8b()
      ),

      gp_stack(
        x64asm::Constants::r9(),
        x64asm::Constants::r9d(),
        x64asm::Constants::r9w(),
        x64asm::Constants::r9b()
      ),

      gp_stack(
        x64asm::Constants::r10(),
        x64asm::Constants::r10d(),
        x64asm::Constants::r10w(),
        x64asm::Constants::r10b()
      ),

      gp_stack(
        x64asm::Constants::r11(),
        x64asm::Constants::r11d(),
        x64asm::Constants::r11w(),
        x64asm::Constants::r11b()
      ),

      gp_stack(
        x64asm::Constants::r12(),
        x64asm::Constants::r12d(),
        x64asm::Constants::r12w(),
        x64asm::Constants::r12b()
      ),

      gp_stack(
        x64asm::Constants::r13(),
        x64asm::Constants::r13d(),
        x64asm::Constants::r13w(),
        x64asm::Constants::r13b()
      ),

      gp_stack(
        x64asm::Constants::r14(),
        x64asm::Constants::r14d(),
        x64asm::Constants::r14w(),
        x64asm::Constants::r14b()
      ),

      gp_stack(
        x64asm::Constants::r15(),
        x64asm::Constants::r15d(),
        x64asm::Constants::r15w(),
        x64asm::Constants::r15b()
      )
    };

  os << "{";

  for(auto gps : possible_registers) {
    if (rs.contains(gps.r64)) {
      os << " " << gps.r64;
      continue;
    }
    if (rs.contains(gps.r32)) {
      os << " " << gps.r32;
      continue;
    }
    if (rs.contains(gps.r16)) {
      os << " " << gps.r16;
      continue;
    }
    if (rs.contains(gps.r8)) {
      os << " " << gps.r8;
      continue;
    }
  }
  for (auto sses: sse_registers) {
    if (rs.contains(sses.ymm)) {
      os << " " << sses.ymm;
      continue;
    }
    if (rs.contains(sses.xmm)) {
      os << " " << sses.xmm;
      continue;
    }
  }
#ifdef DEBUG_REGSET
  os << " (" << hex << rs.get_group1() << ", " << rs.get_group2();
  os << ", " << rs.get_group3() << ", " << rs.get_group4() << dec << ")";
#endif
  os << " }";
  return os;
}


} // namespace std

#endif
