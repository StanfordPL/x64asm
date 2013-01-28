#ifndef X64ASM_SRC_INSTRUCTION_H
#define X64ASM_SRC_INSTRUCTION_H

#include <cassert>
#include <initializer_list>
#include <iostream>
#include <vector>

#include "src/opcode.h"
#include "src/operand.h"
#include "src/op_set.h"
#include "src/op_type.h"
#include "src/properties.h"

namespace x64asm {

/** A hardware instruction. */
class Instruction {
	public:
		inline Instruction(Opcode opcode) 
				: opcode_(opcode), operands_{0,0,0,0} { 
		}

		inline Instruction(Opcode opcode, 
				               std::initializer_list<const Operand*> operands)
				: opcode_(opcode), operands_{0,0,0,0} {
			assert(operands.size() <= 4);
			std::copy(operands.begin(), operands.end(), operands_.begin());
		}

		template <typename InItr>
		inline Instruction(Opcode opcode, InItr begin, InItr end) 
				: opcode_(opcode), operands_{0,0,0,0} {
			assert(end-begin <= 4);		
			std::copy(begin, end, operands_.begin());
		}

		inline Opcode get_opcode() const {
			return opcode_;
		}

		inline void set_opcode(Opcode o) {
			opcode_ = o;
		}

		inline const Operand* get_operand(size_t index) const {
			assert(index < operands_.size());
			return operands_[index];
		}

		inline void set_operand(const Operand* o, size_t index) {
			assert(index < operands_.size());
			operands_[index] = o;
		}	

		inline size_t arity() const {
			assert((size_t)get_opcode() < arity_.size());
			return arity_[get_opcode()];
		}

		inline Properties properties(size_t index) const {
			assert((size_t)get_opcode() < properties_.size());
			assert(index < properties_[get_opcode()].size());
			return properties_[get_opcode()][index];
		}

		inline OpType type(size_t index) const {
			assert((size_t)get_opcode() < type_.size());
			assert(index < type_[get_opcode()].size());
			return type_[get_opcode()][index];
		}

		inline bool is_label_defn() const {
			return get_opcode() == Opcode::LABEL_DEFN;
		}

		inline bool is_return() const {
			assert((size_t)get_opcode() < is_return_.size());
			return is_return_[get_opcode()];
		}

		inline bool is_nop() const {
			assert((size_t)get_opcode() < is_nop_.size());
			return is_nop_[get_opcode()];
		}

		inline bool is_jump() const {
			assert((size_t)get_opcode() < is_jump_.size());
			return is_jump_[get_opcode()];
		}

		inline bool is_cond_jump() const {
			assert((size_t)get_opcode() < is_cond_jump_.size());
			return is_cond_jump_[get_opcode()];
		}

		inline bool is_uncond_jump() const {
			assert((size_t)get_opcode() < is_uncond_jump_.size());
			return is_uncond_jump_[get_opcode()];
		}

		inline OpSet implicit_must_read_set() const {
			assert((size_t)get_opcode() < implicit_must_read_set_.size());
			return implicit_must_read_set_[get_opcode()];
		}

		inline OpSet implicit_maybe_read_set() const {
			assert((size_t)get_opcode() < implicit_maybe_read_set_.size());
			return implicit_maybe_read_set_[get_opcode()];
		}

		inline OpSet implicit_must_write_set() const {
			assert((size_t)get_opcode() < implicit_must_write_set_.size());
			return implicit_must_write_set_[get_opcode()];
		}

		inline OpSet implicit_maybe_write_set() const {
			assert((size_t)get_opcode() < implicit_maybe_write_set_.size());
			return implicit_maybe_write_set_[get_opcode()];
		}

		inline OpSet implicit_must_undef_set() const {
			assert((size_t)get_opcode() < implicit_must_undef_set_.size());
			return implicit_must_undef_set_[get_opcode()];
		}

		inline OpSet implicit_maybe_undef_set() const {
			assert((size_t)get_opcode() < implicit_maybe_undef_set_.size());
			return implicit_maybe_undef_set_[get_opcode()];
		}

		OpSet explicit_must_read_set() const ;
		OpSet explicit_maybe_read_set() const ;
		OpSet explicit_must_write_set() const ;
		OpSet explicit_maybe_write_set() const ;
		OpSet explicit_must_undef_set() const ;
		OpSet explicit_maybe_undef_set() const ;

		inline OpSet must_read_set() const {
			return implicit_must_read_set() | explicit_must_read_set();
		}

		inline OpSet maybe_read_set() const {
			return implicit_maybe_read_set() | explicit_maybe_read_set();
		}

		inline OpSet must_write_set() const {
			return implicit_must_write_set() | explicit_must_write_set();
		}

		inline OpSet maybe_write_set() const {
			return implicit_maybe_write_set() | explicit_maybe_write_set();
		}

		inline OpSet must_undef_set() const {
			return implicit_must_undef_set() | explicit_must_undef_set();
		}

		inline OpSet maybe_undef_set() const {
			return implicit_maybe_undef_set() | explicit_maybe_undef_set();
		}

		bool check() const;

		void write_att(std::ostream& os) const;
		void write_intel(std::ostream& os) const;

	private:
		Opcode opcode_;
		std::vector<const Operand*> operands_;

		static std::vector<size_t> arity_;
		static std::vector<std::vector<Properties>> properties_;
		static std::vector<std::vector<OpType>> type_;
		static std::vector<bool> is_return_;
		static std::vector<bool> is_nop_;
		static std::vector<bool> is_jump_;
		static std::vector<bool> is_cond_jump_;
		static std::vector<bool> is_uncond_jump_;
		static std::vector<OpSet> implicit_must_read_set_;
		static std::vector<OpSet> implicit_maybe_read_set_;
		static std::vector<OpSet> implicit_must_write_set_;
		static std::vector<OpSet> implicit_maybe_write_set_;
		static std::vector<OpSet> implicit_must_undef_set_;
		static std::vector<OpSet> implicit_maybe_undef_set_;
};

} // namespace x64asm

#endif
