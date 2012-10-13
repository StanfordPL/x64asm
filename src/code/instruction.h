#ifndef X64_SRC_CODE_INSTRUCTION_H
#define X64_SRC_CODE_INSTRUCTION_H

#include <cassert>
#include <initializer_list>
#include <vector>

#include "src/code/addr.h"
#include "src/code/fp_reg.h"
#include "src/code/gp_reg.h"
#include "src/code/imm.h"
#include "src/code/label.h"
#include "src/code/mmx_reg.h"
#include "src/code/offset.h"
#include "src/code/opcode.h"
#include "src/code/operand.h"
#include "src/code/reg_set.h"
#include "src/code/xmm_reg.h"

namespace x64 {

/** Opcode plus operands
*/
class Instruction {
	public:

		inline Instruction() 
				: opcode_(NOP), operands_{{0,0,0}} { 
		}

		inline explicit Instruction(Opcode opcode) 
				: opcode_(opcode), operands_{{0,0,0}} { 
			assert(arity() == 0); 
		}

		inline explicit Instruction(Opcode opcode, 
				                        std::initializer_list<Operand> operands)
				: opcode_(opcode), operands_(operands.begin(), operands.end()) {
		}

		template <typename InItr>
		inline explicit Instruction(Opcode opcode, InItr begin, InItr end) 
				: opcode_(opcode), operands_(begin, end) {
		}

		inline Opcode get_opcode() const { 
			return opcode_; 
		}

		inline Operand get_operand(size_t index) const {
			assert(index < arity());
			return operands_[index];
		}

		inline void set_operand(size_t index, Operand o) {
			assert(index < arity());
			operands_[index] = o;
		}

		bool is_null() const;

		inline size_t arity() const { 
			return opcode_.arity(); 
		}

		inline Type type(size_t index) const { 
			return opcode_.type(index); 
		}

		inline BitWidth width(size_t index) const { 
			return opcode_.width(index); 
		}

		inline Modifier mod(size_t index) const {
			return opcode_.mod(index);
		}

		inline bool is_label_defn() const {
			return opcode_.is_label_defn();
		}

		inline bool is_ret() const {
			return opcode_.is_ret();
		}

		inline bool is_cond_jump() const {
			return opcode_.is_cond_jump();
		}

		inline bool is_uncond_jump() const {
			return opcode_.is_uncond_jump();
		}

		inline bool is_jump() const {
			return opcode_.is_jump();
		}

		inline bool touches_mem() const {
			return opcode_.touches_mem();
		}

		inline Modifier mem_mod() const {
			return opcode_.mem_mod();
		}

		inline bool touches_stack() const {
			const auto mi = opcode_.mem_index();
			if ( mi == 3 )
				return false;
			return ((M)get_operand(mi)).is_stack();
		}

		inline Modifier stack_mod() const {
			assert(touches_stack());
			return mod(opcode_.mem_index());
		}

		inline bool touches_heap() const {
			const auto mi = opcode_.mem_index();
			if ( mi == 3 )
				return false;
			return ((M)get_operand(mi)).is_heap();
		}

		inline Modifier heap_mod() const {
			assert(touches_heap());
			return mod(opcode_.mem_index());
		}

		inline RegSet implicit_read_set() const {
			return opcode_.implicit_read_set();
		}

		inline RegSet implicit_write_set() const {
			return opcode_.implicit_write_set();
		}

		inline RegSet implicit_undef_set() const {
			return opcode_.implicit_undef_set();
		}

		RegSet explicit_read_set() const;

		RegSet explicit_write_set() const;

		inline RegSet read_set() const {
			return implicit_read_set() |= explicit_read_set();
		}

		inline RegSet write_set() const {
			return implicit_write_set() |= explicit_write_set();
		}

		inline RegSet undef_set() const {
			return implicit_undef_set();
		}

	private:
		Opcode opcode_;
		std::vector<Operand> operands_;
};

} // namespace x64

#endif
