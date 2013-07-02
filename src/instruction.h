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
        constexpr Properties(uint32_t p)
          : mask_ {p} {
        }

        /** Creates a property set using three property masks. */
        constexpr Properties(Property r, Property w, Property u)
          : mask_ {(uint32_t)r | (uint32_t)w | (uint32_t)u} {
        }

      public:
        /** Creates an empty property set. */
        constexpr Properties()
          : mask_ {(uint32_t)Property::NONE} {
        }

        /** Returns an empty property set. */
        static constexpr Properties none() {
          return Properties {(uint32_t)Property::NONE};
        }

        /** Inserts a property. */
        constexpr Properties operator+(Property rhs) {
          return Properties {mask_ | (uint32_t)rhs};
        }

        /** Removes a property. */
        constexpr Properties operator-(Property rhs) {
          return Properties {mask_& ~(uint32_t)rhs};
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

  public:
    /** Creates an instruction with no operands. */
    Instruction(Opcode opcode)
      : opcode_ {opcode}, operands_ {} {
    }

    /** Creates an instruction using initializer list syntax. */
    Instruction(Opcode opcode, const std::initializer_list<Operand>& operands)
      : opcode_ {opcode}, operands_ {} {
      assert(operands.size() <= 4);
      std::copy(operands.begin(), operands.end(), operands_.begin());
    }

    /** Creates an instruction from an stl container of operands. */
    template <typename InItr>
    Instruction(Opcode opcode, InItr begin, InItr end)
      : opcode_ {opcode}, operands_ {} {
      assert(end - begin <= 4);
      std::copy(begin, end, operands_.begin());
    }

    /** Returns the current opcode. */
    Opcode get_opcode() const {
      return opcode_;
    }

    /** Sets the current opcode. */
    void set_opcode(Opcode o) {
      opcode_ = o;
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

    /** Returns true if this instruction is a label definiton. */
    bool is_label_defn() const {
      return get_opcode() == Opcode::LABEL_DEFN;
    }

    /** Returns true if this instruction causes control to return from
        a function.
    */
    bool is_return() const {
      assert((size_t)get_opcode() < is_return_.size());
      return is_return_[get_opcode()];
    }

    /** Returns true if this instruction does not modify machine state. */
    bool is_nop() const {
      assert((size_t)get_opcode() < is_nop_.size());
      return is_nop_[get_opcode()];
    }

    /** Returns true if this instruction causes a control jump. */
    bool is_jump() const {
      assert((size_t)get_opcode() < is_jump_.size());
      return is_jump_[get_opcode()];
    }

    /** Returns true if this instruction conditionally causes a control jump. */
    bool is_cond_jump() const {
      assert((size_t)get_opcode() < is_cond_jump_.size());
      return is_cond_jump_[get_opcode()];
    }

    /** Returns true if this instruction unconditionally causes a control
        jump.
    */
    bool is_uncond_jump() const {
      assert((size_t)get_opcode() < is_uncond_jump_.size());
      return is_uncond_jump_[get_opcode()];
    }

    /** Returns true if this instruction must read the operand at index. */
    bool must_read(size_t index) const {
      assert((size_t)get_opcode() < properties_.size());
      assert(index < properties_[get_opcode()].size());
      return properties_[get_opcode()][index].contains(Property::MUST_READ);
    }

    /** Returns true if this instruction might read the operand at index. */
    bool maybe_read(size_t index) const {
      assert((size_t)get_opcode() < properties_.size());
      assert(index < properties_[get_opcode()].size());
      return properties_[get_opcode()][index].contains(Property::MAYBE_READ);
    }

    /** Returns true if this instruction must write the operand at index. */
    bool must_write(size_t index) const {
      assert((size_t)get_opcode() < properties_.size());
      assert(index < properties_[get_opcode()].size());
      return properties_[get_opcode()][index].contains(Property::MUST_WRITE);
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

    /** Returns true if this instruction might undefine the operand at index. */
    bool maybe_undef(size_t index) const {
      assert((size_t)get_opcode() < properties_.size());
      assert(index < properties_[get_opcode()].size());
      return properties_[get_opcode()][index].contains(Property::MAYBE_UNDEF);
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

    /** Returns true if this instruction is well-formed. */
    bool check() const;

    /** Writes this instruction to an ostream using at&t syntax. */
    void write_att(std::ostream& os) const;

  private:
    /** Instruction mnemonic. */
    Opcode opcode_;
    /** As many as four operands. */
    std::array<Operand, 4> operands_;

    // Static lookup tables which back the public API of this class.
    static const std::array<size_t, 3257> arity_;
    static const std::array<std::array<Properties, 4>, 3257> properties_;
    static const std::array<std::array<Type, 4>, 3257> type_;
    static const std::array<bool, 3257> is_return_;
    static const std::array<bool, 3257> is_nop_;
    static const std::array<bool, 3257> is_jump_;
    static const std::array<bool, 3257> is_cond_jump_;
    static const std::array<bool, 3257> is_uncond_jump_;
    static const std::array<RegSet, 3257> implicit_must_read_set_;
    static const std::array<RegSet, 3257> implicit_maybe_read_set_;
    static const std::array<RegSet, 3257> implicit_must_write_set_;
    static const std::array<RegSet, 3257> implicit_maybe_write_set_;
    static const std::array<RegSet, 3257> implicit_must_undef_set_;
    static const std::array<RegSet, 3257> implicit_maybe_undef_set_;

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

    /** Returns the set of operands this instruction must read. */
    RegSet& explicit_must_read_set(RegSet& rs) const ;
    /** Returns the set of operands this instruction might read. */
    RegSet& explicit_maybe_read_set(RegSet& rs) const ;
    /** Returns the set of operands this instruction must write. */
    RegSet& explicit_must_write_set(RegSet& rs) const ;
    /** Returns the set of operands this instruction might write. */
    RegSet& explicit_maybe_write_set(RegSet& rs) const ;
    /** Returns the set of operands this instruction must undef. */
    RegSet& explicit_must_undef_set(RegSet& rs) const ;
    /** Returns the set of operands this instruction might undef. */
    RegSet& explicit_maybe_undef_set(RegSet& rs) const ;
};

} // namespace x64asm

#endif
