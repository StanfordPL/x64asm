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

		inline Addr get_addr(size_t index) const {
			assert(index < arity() && type(index) == ADDR);
			return operands_[index];
		}

		inline FpReg get_fp_reg(size_t index) const {
			assert(index < arity() && 
					   (type(index) == FP_REG || type(index) == ST0));
			return operands_[index];
		}

		inline GpReg get_gp_reg(size_t index) const {
			assert(index < arity() && 
					   (type(index) == GP_REG || 
							type(index) == RAX_ONLY || type(index) == RCX_ONLY));
			return operands_[index];
		}

		inline Imm get_imm(size_t index) const {
			assert(index < arity() && type(index) == IMM);
			return operands_[index];
		}

		inline Label get_label(size_t index) const {
			assert(index < arity() && type(index) == LABEL);
			return operands_[index];
		}

		inline MmxReg get_mmx_reg(size_t index) const {
			assert(index < arity() && type(index) == MMX_REG);
			return operands_[index];
		}

		inline Offset get_offset(size_t index) const {
			assert(index < arity() && type(index) == OFFSET);
			return operands_[index];
		}

		inline XmmReg get_xmm_reg(size_t index) const {
			assert(index < arity() && type(index) == XMM_REG);
			return operands_[index];
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

		// Higher order attributes
		inline bool acceses_stack() const {
			/*
			const auto mo = 0;// TODO -- FIX opcode_.mem_offset();
			if ( mo == 16 )
				return false;

			const auto addr = get_addr(mo);
			const auto b = addr.get_base();
			const auto i = addr.get_index();
			const auto d = addr.get_disp();

			return b == rsp && i.is_null() && (int64_t) d <= 0;
			*/
			return true;
		}

		inline bool accesses_heap() const { 
			/*
			const auto mo = 0; // TODO --- FIX opcode_.mem_offset();
			if ( mo == 16 )
				return false;

			const auto addr = get_addr(mo);
			const auto b = addr.get_base();
			const auto i = addr.get_index();
			const auto d = addr.get_disp();

			return b != rsp || !i.is_null() || (int64_t) d > 0;
			*/
			return true;
		}

	private:
		Opcode opcode_;
		std::vector<Operand> operands_;
};

} // namespace x64

#endif
