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

  public:
    /** Creates an instruction with no operands. */
    Instruction(Opcode opcode) : opcode_(opcode), operands_{{}} {}
    /** Creates an instruction using initializer list syntax. */
    Instruction(Opcode opcode, const std::initializer_list<Operand>& operands) :
				opcode_(opcode), operands_{{}} {
			assert(operands.size() <= 4);
			std::copy(operands.begin(), operands.end(), operands_.begin());
		}
    /** Creates an instruction from an stl container of operands. */
    template <typename InItr>
    Instruction(Opcode opcode, InItr begin, InItr end) :
				opcode_(opcode), operands_ {} {
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

    /** Does this instruction invoke a function call. */
    bool is_any_call() const {
			return is_call() || is_enter() || is_syscall() || is_sysenter();
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
			return is_ret() || is_iret() || is_leave() || is_sysret() || is_sysexit();
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
		/** Is this a variant of the leave instruction? */
		bool is_leave() const {
			return opcode_ == LEAVE || opcode_ == LEAVE_PREF66;
		}
		/** Is this a variant of the nop instruction? */
		bool is_nop() const {
			return opcode_ >= NOP && opcode_ <= NOP_R32;
		}
		/** Is this a variant of the pop instruction? */
		bool is_pop() const {
			return opcode_ >= POP_M16 && opcode_ <= POP_R64;
		}
		/** Is this a variant of the push instruction? */
		bool is_push() const {
			return opcode_ >= PUSH_M16 && opcode_ <= PUSH_R64;
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

		/** Does this instruction implicitly dereference memory? @todo missing cases */
		bool is_implicit_memory_dereference() const {
			return is_push() || is_pop() || is_ret();
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
    static const std::array<size_t, 3801> arity_;
    static const std::array<std::array<Properties, 4>, 3801> properties_;
    static const std::array<std::array<Type, 4>, 3801> type_;
		static const std::array<int, 3801> mem_index_;
    static const std::array<RegSet, 3801> implicit_must_read_set_;
    static const std::array<RegSet, 3801> implicit_maybe_read_set_;
    static const std::array<RegSet, 3801> implicit_must_write_set_;
    static const std::array<RegSet, 3801> implicit_maybe_write_set_;
    static const std::array<RegSet, 3801> implicit_must_undef_set_;
    static const std::array<RegSet, 3801> implicit_maybe_undef_set_;
    static const std::array<FlagSet, 3801> flags_;

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
};

} // namespace x64asm

namespace std {

/** STL hash specialization. */
template <>
struct hash<x64asm::Instruction> {
  size_t operator()(const x64asm::Instruction& i) const {
		return i.hash();
	}
};

/** STL swap overload. */
inline void swap(x64asm::Instruction& lhs, x64asm::Instruction& rhs) {
	lhs.swap(rhs);
}

/** iostream overload. */
inline istream& operator>>(istream& is, x64asm::Instruction& i) {
	return i.read_att(is);
}
/** iostream overload. */
inline ostream& operator<<(ostream& os, const x64asm::Instruction& i) {
	return i.write_att(os);
}

} // namespace std

#endif
