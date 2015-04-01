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

#ifndef X64ASM_SRC_INSTRUCTION_H
#define X64ASM_SRC_INSTRUCTION_H

#include <array>
#include <cassert>
#include <initializer_list>
#include <iostream>
#include <type_traits>

#include "src/flag_set.h"
#include "src/opcode.h"
#include "src/operand.h"
#include "src/reg_set.h"
#include "src/type.h"
#include "src/type_traits.h"

namespace x64asm {

/** A hardware instruction; operands are stored in intel order with target
    given first. The implementation of this class is similar to the java
    implementation of type erasures. The user has the ability to assign an
    operand of any type to any argument position. However in doing so, the type
    information for that operand is lost. The alternative implementation
    choices were either to define a specific type for each mnemonic (seemed
    bloated) or to store pointers rather than Operand references (this seemed
    preferable, but required the user to manage memory.)
*/
class Instruction {
private:
  /** A read/write/undefined mask for an operand. */
  enum class Property : uint32_t {
    MUST_READ      = 0x00000003,
    MAYBE_READ     = 0x00000001,

    MUST_WRITE_ZX  = 0x00000700,
    MUST_WRITE     = 0x00000300,
    MAYBE_WRITE_ZX = 0x00000500,
    MAYBE_WRITE    = 0x00000100,

    MUST_UNDEF     = 0x00030000,
    MAYBE_UNDEF    = 0x00010000,

    NONE           = 0x00000000,
    ALL            = 0x00030703
  };

  /** A bit set representing per-operand read/write/undefined properties. */
  class Properties {
  private:
    /** Creates a property set using a bit mask. */
    constexpr Properties(uint32_t p) : mask_(p) {}
    /** Creates a property set using three property masks. */
    constexpr Properties(Property r, Property w, Property u) :
      mask_((uint32_t)r | (uint32_t)w | (uint32_t)u) {}

  public:
    /** Creates an empty property set. */
    constexpr Properties() : mask_((uint32_t)Property::NONE) {}

    /** Returns an empty property set. */
    static constexpr Properties none() {
      return Properties();
    }

    /** Inserts a property. */
    constexpr Properties operator+(Property rhs) {
      return Properties(mask_ | (uint32_t)rhs);
    }
    /** Removes a property. */
    constexpr Properties operator-(Property rhs) {
      return Properties(mask_& ~(uint32_t)rhs);
    }

    /** Inserts a property. */
    Properties& operator+=(Property rhs) {
      mask_ |= ((uint32_t)rhs);
      return *this;
    }
    /** Removes a property. */
    Properties& operator-=(Property rhs) {
      mask_ &= ~((uint32_t)rhs);
      return *this;
    }

    /** Checks whether a property is set. */
    constexpr bool contains(Property p) {
      return (mask_ & (uint32_t)p) == (uint32_t)p;
    }

  private:
    /** The underlying property bit mask. */
    uint32_t mask_;
  };

  /** Fix the types of all the operands */
  void fix_operands_type() {
    for(size_t i = 0; i < operands_.size(); ++i) {
      operands_[i].set_type(type(i));
    }
  }

public:
  /** Creates an instruction with no operands. */
  Instruction(Opcode opcode) : opcode_(opcode), operands_{{}} {}
  /** Creates an instruction using initializer list syntax. */
  Instruction(Opcode opcode, const std::initializer_list<Operand>& operands) :
    opcode_(opcode), operands_{{}} {
    assert(operands.size() <= 4);
    std::copy(operands.begin(), operands.end(), operands_.begin());
    fix_operands_type();
  }
  /** Creates an instruction from an stl container of operands. */
  template <typename InItr>
  Instruction(Opcode opcode, InItr begin, InItr end) :
    opcode_(opcode), operands_ {} {
    assert(end - begin <= 4);
    std::copy(begin, end, operands_.begin());
    fix_operands_type();
  }

  /** Returns the current opcode. */
  Opcode get_opcode() const {
    return opcode_;
  }
  /** Sets the current opcode. */
  void set_opcode(Opcode o) {
    opcode_ = o;
    fix_operands_type();
  }

  /** Gets an operand and casts it to a user-specified type. */
  template <typename T>
  typename std::enable_if<is_operand<T>::value, const T&>::type
  get_operand(size_t index) const {
    assert(index < operands_.size());
    return reinterpret_cast<const T&>(operands_[index]);
  }
  /** Sets an operand. */
  void set_operand(size_t index, const Operand& o) {
    assert(index < operands_.size());
    operands_[index] = o;
    fix_operands_type();
  }

  /** Returns the arity of this instruction. */
  size_t arity() const {
    assert((size_t)get_opcode() < arity_.size());
    return arity_[get_opcode()];
  }
  /** Returns the type expected at index for this instruction. */
  Type type(size_t index) const {
    assert((size_t)get_opcode() < type_.size());
    assert(index < type_[get_opcode()].size());
    return type_[get_opcode()][index];
  }

  /** Is this any of the bt family of instructions. */
  bool is_any_bt() const {
    return opcode_ >= BT_M16_IMM8 && opcode_ <= BTS_R64_R64;
  }
  /** Does this instruction invoke a function call. */
  bool is_any_call() const {
    return is_call() || is_syscall();
  }
  /** Returns true if this instruction is a jmp or jmpcc to an indirect pointer. */
  bool is_any_indirect_jump() const {
    return opcode_ >= JMP_FARPTR1616 && opcode_ <= JMP_M64;
  }
  /** Returns true if this instruction causes a control flow jump. */
  bool is_any_jump() const {
    return opcode_ >= JA_REL32 && opcode_ <= JZ_REL8_HINT;
  }
  /** Returns true if this instruction induces loop behavior. */
  bool is_any_loop() const {
    return opcode_ >= LOOP_REL8 && opcode_ <= LOOPNE_REL8;
  }
  /** Returns true if this instruction does not modify machine state. */
  bool is_any_nop() const {
    return is_nop() || is_fnop();
  }
  /** Returns true if this instruction causes control to return. */
  bool is_any_return() const {
    return is_ret() || is_iret() || is_sysret();
  }

  /** Is this a variant of the bt instruction? */
  bool is_bt() const {
    return opcode_ >= BT_M16_IMM8 && opcode_ <= BT_R64_R64;
  }
  /** Is this a variant of the btc instruction? */
  bool is_btc() const {
    return opcode_ >= BTC_M16_IMM8 && opcode_ <= BTC_R64_R64;
  }
  /** Is this a variant of the btr instruction? */
  bool is_btr() const {
    return opcode_ >= BTR_M16_IMM8 && opcode_ <= BTR_R64_R64;
  }
  /** Is this a variant of the bt instruction? */
  bool is_bts() const {
    return opcode_ >= BTS_M16_IMM8 && opcode_ <= BTS_R64_R64;
  }
  /** Is this a variant of the call instruction? */
  bool is_call() const {
    return opcode_ >= CALL_FARPTR1616 && opcode_ <= CALL_LABEL;
  }
  /** Is this a variant of the div instruction? */
  bool is_div() const {
    return opcode_ >= DIV_M16 && opcode_ <= DIV_RL;
  }
  /** Is this a variant of the enter instruction? */
  bool is_enter() const {
    return opcode_ >= ENTER_IMM8_IMM16 && opcode_ <= ENTER_ZERO_IMM16;
  }
  /** Is this a variant of the fnop instruction? */
  bool is_fnop() const {
    return opcode_ == FNOP;
  }
  /** Is this a variant of the idiv instruction? */
  bool is_idiv() const {
    return opcode_ >= IDIV_M16 && opcode_ <= IDIV_RL;
  }
  /** Is this a variant of the iret instruction? */
  bool is_iret() const {
    return opcode_ >= IRET && opcode_ <= IRETQ;
  }
  /** Is this a variant of the jcc (jump conditional) instruction? */
  bool is_jcc() const {
    return opcode_ >= JA_REL32 && opcode_ <= JZ_REL8_HINT && !is_jmp();
  }
  /** Is this a variant of the jmp instruction? */
  bool is_jmp() const {
    return opcode_ >= JMP_FARPTR1616 && opcode_ <= JMP_REL8;
  }
  /** Returns true if this instruction is a label definiton. */
  bool is_label_defn() const {
    return opcode_ == LABEL_DEFN;
  }
  /** Is this a variant of the lea instruction? */
  bool is_lea() const {
    return opcode_ >= LEA_R16_M16 && opcode_ <= LEA_R64_M64;
  }
  /** Is this a variant of the lddqu instruction? */
  bool is_lddqu() const {
    return opcode_ == LDDQU_XMM_M128;
  }
  /** Is this a variant of the leave instruction? */
  bool is_leave() const {
    return opcode_ == LEAVE || opcode_ == LEAVE_PREF66;
  }
  /** Is this a variant of the maskmovdqu instruction? */
  bool is_maskmovdqu() const {
    return opcode_ == MASKMOVDQU_XMM_XMM;
  }
  /** Is this a variant of the movdqu instruction? */
  bool is_movdqu() const {
    return opcode_ >= MOVDQU_M128_XMM && opcode_ <= MOVDQU_XMM_XMM;
  }
  /** Is this a variant of the movupd instruction? */
  bool is_movupd() const {
    return opcode_ >= MOVUPD_M128_XMM && opcode_ <= MOVUPD_XMM_XMM;
  }
  /** Is this a variant of the movups instruction? */
  bool is_movups() const {
    return opcode_ >= MOVUPS_M128_XMM && opcode_ <= MOVUPS_XMM_XMM;
  }
  /** Is this a variant of the nop instruction? */
  bool is_nop() const {
    return opcode_ >= NOP && opcode_ <= NOP_R32;
  }
  /** Is this a variant of the pop instruction? */
  bool is_pop() const {
    return opcode_ >= POP_FS && opcode_ <= POP_R64;
  }
  /** Is this a variant of the popf instruction? */
  bool is_popf() const {
    return opcode_ == POPF || opcode_ == POPFQ;
  }
  /** Is this a variant of the popcnt instruction? */
  bool is_popcnt() const {
    return opcode_ >= POPCNT_R16_M16 && opcode_ <= POPCNT_R64_R64;
  }
  /** Is this a variant of the push instruction? */
  bool is_push() const {
    return opcode_ >= PUSH_FS && opcode_ <= PUSH_R64;
  }
  /** Is this a variant of the pushf instruction? */
  bool is_pushf() const {
    return opcode_ == PUSHF || opcode_ == PUSHFQ;
  }
  /** Is this a variant of the rdrand instruction? */
  bool is_rdrand() const {
    return opcode_ >= RDRAND_R16 && opcode_ <= RDRAND_R64;
  }
  /** Is this a variant of the ret instruction? */
  bool is_ret() const {
    return opcode_ >= RET && opcode_ <= RET_IMM16_FAR;
  }
  /** Is this a variant of the syscall instruction? */
  bool is_syscall() const {
    return opcode_ == SYSCALL;
  }
  /** Is this a variant of the sysenter instruction? */
  bool is_sysenter() const {
    return opcode_ == SYSENTER;
  }
  /** Is this a variant of the sysexit instruction? */
  bool is_sysexit() const {
    return opcode_ == SYSEXIT || opcode_ == SYSEXIT_PREFREXW;
  }
  /** Is this a variant of the sysret instruction? */
  bool is_sysret() const {
    return opcode_ == SYSRET || opcode_ == SYSRET_PREFREXW;
  }
  /** Is this a variant of the vlddqu instruction? */
  bool is_vlddqu() const {
    return opcode_ == VLDDQU_XMM_M128  || opcode_ == VLDDQU_YMM_M256;
  }
  /** Is this a variant of the vmaskmovdqu instruction? */
  bool is_vmaskmovdqu() const {
    return opcode_ == VMASKMOVDQU_XMM_XMM;
  }
  /** Is this a variant of the vmovdqu instruction? */
  bool is_vmovdqu() const {
    return opcode_ >= VMOVDQU_M128_XMM && opcode_ <= VMOVDQU_YMM_YMM;
  }
  /** Is this a variant of the vmovupd instruction? */
  bool is_vmovupd() const {
    return opcode_ >= VMOVUPD_M128_XMM && opcode_ <= VMOVUPD_YMM_YMM;
  }
  /** Is this a variant of the vmovups instruction? */
  bool is_vmovups() const {
    return opcode_ >= VMOVUPS_M128_XMM && opcode_ <= VMOVUPS_YMM_YMM;
  }

  /** Is this an SSE instruction? */
  bool is_sse() const {
    return required_flags().contains(Flag::SSE);
  }
  /** Is this an SSE2 instruction? */
  bool is_sse2() const {
    return required_flags().contains(Flag::SSE2);
  }
  /** Is this an SSSE3 instruction? */
  bool is_ssse3() const {
    return required_flags().contains(Flag::SSSE3);
  }
  /** Is this an SSE4_1 instruction? */
  bool is_sse4_1() const {
    return required_flags().contains(Flag::SSE4_1);
  }
  /** Is this an SSE4_2 instruction? */
  bool is_sse4_2() const {
    return required_flags().contains(Flag::SSE4_2);
  }
  /** Is this an SSE instruction (any version) ? */
  bool is_any_sse() const {
    constexpr auto fs = FlagSet::empty() + Flag::SSE + Flag::SSE2 + Flag::SSSE3 + Flag::SSE4_1 + Flag::SSE4_2;
    return required_flags().intersects(fs);
  }

  /** Is this an AVX 1 instruction? */
  bool is_avx() const {
    return flags_[get_opcode()].contains(Flag::AVX);
  }
  /** Is this an AVX 2 instruction? */
  bool is_avx2() const {
    return flags_[get_opcode()].contains(Flag::AVX2);
  }
  /** Is this any AVX instruction? */
  bool is_any_avx() const {
    constexpr auto fs = FlagSet::empty() + Flag::AVX + Flag::AVX2;
    return required_flags().intersects(fs);
  }

  /** Is this an unaligned instruction? */
  bool is_unaligned() const {
    return is_lddqu() || is_movdqu() || is_movupd() || is_movups() ||
           is_vlddqu() || is_vmovdqu() || is_vmovupd() || is_vmovups();
  }

  /** Does this instruction implicitly dereference memory? @todo missing cases */
  bool is_implicit_memory_dereference() const {
    return is_leave() || is_push() || is_pushf() || is_pop() || is_popf() || is_ret();
  }
  /** Does this instruction explicitly dereference memory? */
  bool is_explicit_memory_dereference() const {
    return mem_index() != -1 && !is_lea();
  }
  /** Does this instruction dereference memory? */
  bool is_memory_dereference() const {
    return is_explicit_memory_dereference() || is_implicit_memory_dereference();
  }
  /** Returns the index of a memory operand if present, -1 otherwise. */
  int mem_index() const {
    assert((size_t)get_opcode() < mem_index_.size());
    return mem_index_[get_opcode()];
  }
  /** Returns true if this instruction must read the operand at index. */
  bool must_read(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MUST_READ);
  }
  /** Returns true if this instruction must read memory. */
  bool must_read_memory() const {
    if(is_explicit_memory_dereference())
      return must_read(mem_index());
    return false;
  }
  /** Returns true if this instruction might read the operand at index. */
  bool maybe_read(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MAYBE_READ);
  }
  /** Returns true if this instruction might read memory. */
  bool maybe_read_memory() const {
    if(is_implicit_memory_dereference())
      return true;
    if(is_explicit_memory_dereference())
      return maybe_read(mem_index());
    return false;
  }
  /** Returns true if this instruction must write the operand at index. */
  bool must_write(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MUST_WRITE);
  }
  /** Returns true if this instruction must write memory. */
  bool must_write_memory() const {
    if(is_explicit_memory_dereference())
      return must_write(mem_index());
    return false;
  }
  /** Returns true if this instruction must write the operand at index and
    zero extend into its parent register.
  */
  bool must_extend(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MUST_WRITE_ZX);
  }
  /** Returns true if this instruction might write the operand at index. */
  bool maybe_write(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MAYBE_WRITE);
  }
  /** Returns true if this instruction might write memory. */
  bool maybe_write_memory() const {
    if(is_implicit_memory_dereference())
      return true;
    if(is_explicit_memory_dereference())
      return maybe_write(mem_index());
    return false;
  }
  /** Returns true if this instruction might write the operand at index and
    zero extend its parent register.
  */
  bool maybe_extend(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MAYBE_WRITE_ZX);
  }
  /** Returns true if this instruction must undefine the operand at index. */
  bool must_undef(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MUST_UNDEF);
  }
  /** Returns true if this instruction must undef memory. */
  bool must_undef_memory() const {
    if(is_explicit_memory_dereference())
      return must_undef(mem_index());
    return false;
  }
  /** Returns true if this instruction might undefine the operand at index. */
  bool maybe_undef(size_t index) const {
    assert((size_t)get_opcode() < properties_.size());
    assert(index < properties_[get_opcode()].size());
    return properties_[get_opcode()][index].contains(Property::MAYBE_UNDEF);
  }
  /** Returns true if this instruction might undef memory. */
  bool maybe_undef_memory() const {
    if(is_implicit_memory_dereference())
      return true;
    if(is_explicit_memory_dereference())
      return maybe_undef(mem_index());
    return false;
  }


  /** Returns the set of registers this instruction must read. */
  RegSet must_read_set() const {
    auto rs = implicit_must_read_set();
    return explicit_must_read_set(rs);
  }
  /** Returns the set of registers this instruction might read. */
  RegSet maybe_read_set() const {
    auto rs = implicit_maybe_read_set();
    return explicit_maybe_read_set(rs);
  }
  /** Returns the set of registers this instruction must write. */
  RegSet must_write_set() const {
    auto rs = implicit_must_write_set();
    return explicit_must_write_set(rs);
  }
  /** Returns the set of registers this instruction might write. */
  RegSet maybe_write_set() const {
    auto rs = implicit_maybe_write_set();
    return explicit_maybe_write_set(rs);
  }
  /** Returns the set of registers this instruction must undefine. */
  RegSet must_undef_set() const {
    auto rs = implicit_must_undef_set();
    return explicit_must_undef_set(rs);
  }
  /** Returns the set of registers this instruction might undefine. */
  RegSet maybe_undef_set() const {
    auto rs = implicit_maybe_undef_set();
    return explicit_maybe_undef_set(rs);
  }

  /** Returns the set of registers this instruction must implicitly read. */
  const RegSet& implicit_must_read_set() const {
    assert((size_t)get_opcode() < implicit_must_read_set_.size());
    return implicit_must_read_set_[get_opcode()];
  }
  /** Returns the set of registers this instruction might implicitly read. */
  const RegSet& implicit_maybe_read_set() const {
    assert((size_t)get_opcode() < implicit_maybe_read_set_.size());
    return implicit_maybe_read_set_[get_opcode()];
  }
  /** Returns the set of registers this instruction must implicitly write. */
  const RegSet& implicit_must_write_set() const {
    assert((size_t)get_opcode() < implicit_must_write_set_.size());
    return implicit_must_write_set_[get_opcode()];
  }
  /** Returns the set of registers this instruction might implicitly write. */
  const RegSet& implicit_maybe_write_set() const {
    assert((size_t)get_opcode() < implicit_maybe_write_set_.size());
    return implicit_maybe_write_set_[get_opcode()];
  }
  /** Returns the set of registers this instruction must implicitly undef. */
  const RegSet& implicit_must_undef_set() const {
    assert((size_t)get_opcode() < implicit_must_undef_set_.size());
    return implicit_must_undef_set_[get_opcode()];
  }
  /** Returns the set of registers this instruction might implicitly undef. */
  const RegSet& implicit_maybe_undef_set() const {
    assert((size_t)get_opcode() < implicit_maybe_undef_set_.size());
    return implicit_maybe_undef_set_[get_opcode()];
  }

  /** Returns the set of registers this instruction must explicitly read. */
  const RegSet& explicit_must_read_set() const {
    auto rs = RegSet::empty();
    return explicit_must_read_set(rs);
  }
  /** Returns the set of registers this instruction might explicitly read. */
  const RegSet& explicit_maybe_read_set() const {
    auto rs = RegSet::empty();
    return explicit_maybe_read_set(rs);
  }
  /** Returns the set of registers this instruction must explicitly write. */
  const RegSet& explicit_must_write_set() const {
    auto rs = RegSet::empty();
    return explicit_must_write_set(rs);
  }
  /** Returns the set of registers this instruction might explicitly write. */
  const RegSet& explicit_maybe_write_set() const {
    auto rs = RegSet::empty();
    return explicit_maybe_write_set(rs);
  }
  /** Returns the set of registers this instruction must explicitly undef. */
  const RegSet& explicit_must_undef_set() const {
    auto rs = RegSet::empty();
    return explicit_must_undef_set(rs);
  }
  /** Returns the set of registers this instruction might explicitly undef. */
  const RegSet& explicit_maybe_undef_set() const {
    auto rs = RegSet::empty();
    return explicit_maybe_undef_set(rs);
  }

  /** Returns the cpu flag set required by this instruction. */
  FlagSet required_flags() const {
    assert((size_t)get_opcode() < flags_.size());
    return flags_[get_opcode()];
  }
  /** Returns whether this instruction is enabled for a flag set. */
  bool enabled(FlagSet fs) const {
    return fs.contains(required_flags());
  }

  /** Returns true if this instruction is well-formed. */
  bool check() const;

  /** Comparison based on on opcode and operands. */
  bool operator<(const Instruction& rhs) const;
  /** Comparison based on on opcode and operands. */
  bool operator==(const Instruction& rhs) const;
  /** Comparison based on on opcode and operands. */
  bool operator!=(const Instruction& rhs) const {
    return !(*this == rhs);
  }

  /** STL-compliant hash. */
  size_t hash() const;
  /** STL-compliant swap. */
  void swap(Instruction& rhs) {
    std::swap(opcode_, rhs.opcode_);
    std::swap(operands_, rhs.operands_);
  }

  /** @todo This method is undefined. */
  std::istream& read_att(std::istream& is) {
    is.setstate(std::ios::failbit);
    return is;
  }
  /** Writes this instruction to an ostream using at&t syntax. */
  std::ostream& write_att(std::ostream& os) const;

  /** @Deprecated. Use is_jcc() */
  bool is_cond_jump() const {
    return is_jcc();
  }
  /** @Deprecated. Use is_any_jump() */
  bool is_jump() const {
    return is_any_jump();
  }
  /** @Deprecated. Use is_any_return() */
  bool is_return() const {
    return is_any_return();
  }
  /** @Deprecated. Use is_jmp() */
  bool is_uncond_jump() const {
    return is_jmp();
  }

private:
  /** Instruction mnemonic. */
  Opcode opcode_;
  /** As many as four operands. */
  std::array<Operand, 4> operands_;

  // Static lookup tables which back the public API of this class.
  static const std::array<size_t, X64ASM_NUM_OPCODES> arity_;
  static const std::array<std::array<Properties, 4>, X64ASM_NUM_OPCODES> properties_;
  static const std::array<std::array<Type, 4>, X64ASM_NUM_OPCODES> type_;
  static const std::array<int, X64ASM_NUM_OPCODES> mem_index_;
  static const std::array<RegSet, X64ASM_NUM_OPCODES> implicit_must_read_set_;
  static const std::array<RegSet, X64ASM_NUM_OPCODES> implicit_maybe_read_set_;
  static const std::array<RegSet, X64ASM_NUM_OPCODES> implicit_must_write_set_;
  static const std::array<RegSet, X64ASM_NUM_OPCODES> implicit_maybe_write_set_;
  static const std::array<RegSet, X64ASM_NUM_OPCODES> implicit_must_undef_set_;
  static const std::array<RegSet, X64ASM_NUM_OPCODES> implicit_maybe_undef_set_;
  static const std::array<FlagSet, X64ASM_NUM_OPCODES> flags_;

  /** Returns the set of operands this instruction must read. */
  RegSet& explicit_must_read_set(RegSet& rs) const;
  /** Returns the set of operands this instruction might read. */
  RegSet& explicit_maybe_read_set(RegSet& rs) const;
  /** Returns the set of operands this instruction must write. */
  RegSet& explicit_must_write_set(RegSet& rs) const;
  /** Returns the set of operands this instruction might write. */
  RegSet& explicit_maybe_write_set(RegSet& rs) const;
  /** Returns the set of operands this instruction must undef. */
  RegSet& explicit_must_undef_set(RegSet& rs) const;
  /** Returns the set of operands this instruction might undef. */
  RegSet& explicit_maybe_undef_set(RegSet& rs) const;

  /** Is this a variant of an XOR instruction with the source and destination
  operands being the same register? */
  bool is_xor_reg_reg() const;
};

} // namespace x64asm

#endif
