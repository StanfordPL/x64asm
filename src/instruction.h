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

#ifndef X64ASM_SRC_INSTRUCTION_H
#define X64ASM_SRC_INSTRUCTION_H

#include <array>
#include <cassert>
#include <initializer_list>
#include <iostream>
#include <type_traits>

#include "src/opcode.h"
#include "src/operand.h"
#include "src/op_type.h"
#include "src/properties.h"
#include "src/reg_set.h"
#include "src/type_traits.h"

namespace x64asm {

/** A hardware instruction. */
class Instruction {
	public:
		Instruction(Opcode opcode) 
				: opcode_{opcode}, operands_{} { 
		}

		Instruction(Opcode opcode, std::initializer_list<Operand> operands)
				: opcode_{opcode}, operands_{} { 
			assert(operands.size() <= 4);
			std::copy(operands.begin(), operands.end(), operands_.begin());		
		}

		template <typename InItr>
		Instruction(Opcode opcode, InItr begin, InItr end)
				: opcode_{opcode}, operands_{} { 
			assert(end - begin <= 4);		
			std::copy(begin, end, operands_.begin());
		}

		Opcode get_opcode() const {
			return opcode_;
		}

		void set_opcode(Opcode o) {
			opcode_ = o;
		}

		template <typename T>
		typename std::enable_if<is_operand<T>::value, const T&>::type
		get_operand(size_t index) const {
			assert(index < operands_.size());
			return reinterpret_cast<const T&>(operands_[index]);
		}

		void set_operand(size_t index, const Operand& o) {
			assert(index < operands_.size());
			operands_[index] = o;
		}	

		size_t arity() const {
			assert((size_t)get_opcode() < arity_.size());
			return arity_[get_opcode()];
		}

		Properties properties(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index];
		}

		OpType type(size_t index) const {
			assert((size_t)get_opcode() < type_.size());
			assert(index < type_[get_opcode()].size());
			return type_[get_opcode()][index];
		}

		bool is_label_defn() const {
			return get_opcode() == Opcode::LABEL_DEFN;
		}

		bool is_return() const {
			assert((size_t)get_opcode() < is_return_.size());
			return is_return_[get_opcode()];
		}

		bool is_nop() const {
			assert((size_t)get_opcode() < is_nop_.size());
			return is_nop_[get_opcode()];
		}

		bool is_jump() const {
			assert((size_t)get_opcode() < is_jump_.size());
			return is_jump_[get_opcode()];
		}

		bool is_cond_jump() const {
			assert((size_t)get_opcode() < is_cond_jump_.size());
			return is_cond_jump_[get_opcode()];
		}

		bool is_uncond_jump() const {
			assert((size_t)get_opcode() < is_uncond_jump_.size());
			return is_uncond_jump_[get_opcode()];
		}

		const RegSet& implicit_must_read_set() const {
			assert((size_t)get_opcode() < implicit_must_read_set_.size());
			return implicit_must_read_set_[get_opcode()];
		}

		const RegSet& implicit_maybe_read_set() const {
			assert((size_t)get_opcode() < implicit_maybe_read_set_.size());
			return implicit_maybe_read_set_[get_opcode()];
		}

		const RegSet& implicit_must_write_set() const {
			assert((size_t)get_opcode() < implicit_must_write_set_.size());
			return implicit_must_write_set_[get_opcode()];
		}

		const RegSet& implicit_maybe_write_set() const {
			assert((size_t)get_opcode() < implicit_maybe_write_set_.size());
			return implicit_maybe_write_set_[get_opcode()];
		}

		const RegSet& implicit_must_undef_set() const {
			assert((size_t)get_opcode() < implicit_must_undef_set_.size());
			return implicit_must_undef_set_[get_opcode()];
		}

		const RegSet& implicit_maybe_undef_set() const {
			assert((size_t)get_opcode() < implicit_maybe_undef_set_.size());
			return implicit_maybe_undef_set_[get_opcode()];
		}

		RegSet explicit_must_read_set() const ;
		RegSet explicit_maybe_read_set() const ;
		RegSet explicit_must_write_set() const ;
		RegSet explicit_maybe_write_set() const ;
		RegSet explicit_must_undef_set() const ;
		RegSet explicit_maybe_undef_set() const ;

		RegSet must_read_set() const {
			return implicit_must_read_set() | explicit_must_read_set();
		}

		RegSet maybe_read_set() const {
			return implicit_maybe_read_set() | explicit_maybe_read_set();
		}

		RegSet must_write_set() const {
			return implicit_must_write_set() | explicit_must_write_set();
		}

		RegSet maybe_write_set() const {
			return implicit_maybe_write_set() | explicit_maybe_write_set();
		}

		RegSet must_undef_set() const {
			return implicit_must_undef_set() | explicit_must_undef_set();
		}

		RegSet maybe_undef_set() const {
			return implicit_maybe_undef_set() | explicit_maybe_undef_set();
		}

		bool check() const;

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		Opcode opcode_;
		std::array<Operand, 4> operands_;

		static const std::array<size_t, 3257> arity_;
		static const std::array<std::array<Properties, 4>, 3257> properties_;
		static const std::array<std::array<OpType, 4>, 3257> type_;
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
};

} // namespace x64asm

#endif
