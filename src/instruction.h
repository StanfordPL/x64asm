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
#include "src/reg_set.h"
#include "src/type.h"
#include "src/type_traits.h"

namespace x64asm {

/** A hardware instruction. */
class Instruction {
	private:
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

		class Properties {
			private:
				constexpr Properties(uint32_t p) 
					: mask_{p} { 
					}

				constexpr Properties(Property r, Property w, Property u) 
					: mask_{(uint32_t)r | (uint32_t)w | (uint32_t)u} {
					}

			public:
				constexpr Properties() 
					: mask_{(uint32_t)Property::NONE} {
					}	

				// Static Constants
				static constexpr Properties none() {
					return Properties{(uint32_t)Property::NONE};
				}

				// Element Operators
				constexpr Properties operator+(Property rhs) {
					return Properties{(uint32_t)rhs};
				}

				constexpr Properties operator-(Property rhs) {
					return Properties{mask_ & ~(uint32_t)rhs};
				}

				Properties& operator+=(Property rhs) {
					mask_ |= ((uint32_t)rhs);
					return *this;
				}

				Properties& operator-=(Property rhs) {
					mask_ &= ~((uint32_t)rhs);
					return *this;
				}

				// Queries
				constexpr bool contains(Property p) {
					return (mask_ & (uint32_t)p) == (uint32_t)p;
				}

			private:
				uint32_t mask_;
		};

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

		Type type(size_t index) const {
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

		bool must_read(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MUST_READ);
		}

		bool maybe_read(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MAYBE_READ);
		}

		bool must_write(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MUST_WRITE);
		}

		bool must_extend(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MUST_WRITE_ZX);
		}

		bool maybe_write(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MAYBE_WRITE);
		}

		bool maybe_extend(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MAYBE_WRITE_ZX);
		}

		bool must_undef(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MUST_UNDEF);
		}

		bool maybe_undef(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index].contains(Property::MAYBE_UNDEF);
		}

		RegSet must_read_set() const {
			auto rs = implicit_must_read_set();
			return explicit_must_read_set(rs);
		}

		RegSet maybe_read_set() const {
			auto rs = implicit_maybe_read_set();
			return explicit_maybe_read_set(rs);
		}

		RegSet must_write_set() const {
			auto rs = implicit_must_write_set();
			return explicit_must_write_set(rs);
		}

		RegSet maybe_write_set() const {
			auto rs = implicit_maybe_write_set();
			return explicit_maybe_write_set(rs);
		}

		RegSet must_undef_set() const {
			auto rs = implicit_must_undef_set();
			return explicit_must_undef_set(rs);
		}

		RegSet maybe_undef_set() const {
			auto rs = implicit_maybe_undef_set();
			return explicit_maybe_undef_set(rs);
		}

		bool check() const;

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		Opcode opcode_;
		std::array<Operand, 4> operands_;

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

		RegSet& explicit_must_read_set(RegSet& rs) const ;
		RegSet& explicit_maybe_read_set(RegSet& rs) const ;
		RegSet& explicit_must_write_set(RegSet& rs) const ;
		RegSet& explicit_maybe_write_set(RegSet& rs) const ;
		RegSet& explicit_must_undef_set(RegSet& rs) const ;
		RegSet& explicit_maybe_undef_set(RegSet& rs) const ;
};

} // namespace x64asm

#endif
