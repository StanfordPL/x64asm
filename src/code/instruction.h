#ifndef X64_SRC_CODE_INSTRUCTION_H
#define X64_SRC_CODE_INSTRUCTION_H

#include <algorithm>
#include <array>
#include <cassert>
#include <initializer_list>
#include <iostream>

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

/** Opcode plus appropriate operands (GpReg, XmmReg, Imm, Addr, or Label).
*/
class Instruction {
	public:

		inline Instruction() 
				: opcode_(NOP) { 
		}

		inline explicit Instruction(Opcode opcode) 
				: opcode_(opcode) { 
			assert(arity() == 0); 
		}

		inline Instruction(Opcode opcode, 
				               const std::initializer_list<Operand>& operands) {
			set_all(opcode, operands.begin(), operands.end()); 
		}

		// Get/Set Opcode
		inline Opcode get_opcode() const { 
			return opcode_; 
		}

		inline void set_opcode(Opcode opcode)	{
			assert(check_opcode(opcode));
			opcode_ = opcode;
		}

		// Get/Set Operand (wild west style)
		inline Operand get_operand(size_t index) const {
			assert(index < arity());
			return operands_[index];
		}

		inline void set_operand(size_t index, Operand o) {
			assert(index < arity());
			operands_[index] = o;
			assert(check_operand(index));
		}

		// Get/Set Floating Point Registers
		inline FpReg get_fp_reg(size_t index) const { 
			assert(type(index) == FP_REG || type(index) == ST0_ONLY);
			return operands_[index]; 
		}

		inline void set_fp_reg(size_t index, FpReg r) { 
			assert(type(index) == FP_REG || (type(index) == ST0_ONLY && r == st0));
			operands_[index] = r; 
		}

		// Get/Set General Purpose Registers
		inline GpReg get_gp_reg(size_t index) const { 
			assert(type(index) == GP_REG || type(index) == RCX_ONLY || type(index) == RAX_ONLY);
			return operands_[index]; 
		}

		inline void set_gp_reg(size_t index, GpReg r) { 
			assert(type(index) == GP_REG || (type(index) == RCX_ONLY && r == rcx) || (type(index) == RAX_ONLY && r == rax));
			operands_[index] = r; 
		}

		// Get/Set XMM Registers
		inline XmmReg get_xmm_reg(size_t index) const { 
			assert(type(index) == XMM_REG);
			return operands_[index]; 
		}

		inline void set_xmm_reg(size_t index, XmmReg r) { 
			assert(type(index) == XMM_REG);
			operands_[index] = r; 
		}

		// Get/Set MMX Registers
		inline MmxReg get_mmx_reg(size_t index) const { 
			assert(type(index) == MMX_REG);
			return operands_[index]; 
		}

		inline void set_mmx_reg(size_t index, MmxReg r) { 
			assert(type(index) == MMX_REG);
			operands_[index] = r; 
		}

		// Get/Set offset
		inline Offset get_offset(size_t index) const {
			assert(type(index) == OFFSET);
			return operands_[index];
		}

		inline void set_offset(size_t index, Offset o) {
			assert(type(index) == OFFSET);
			operands_[index] = o;
		}

		// Get/Set Imms
		inline Imm get_imm(size_t index) const { 
			assert(type(index) == IMM);
			return (Imm) operands_[index]; 
		}

		inline void set_imm(size_t index, Imm i) { 
			assert(type(index) == IMM);
			operands_[index] = i; 
		}

		// Get/Set Labels
		inline Label get_label(size_t index) const { 
			assert(type(index) == LABEL);
			return (Label) operands_[index]; 
		}

		inline void set_label(size_t index, Label l) {
			assert(type(index) == LABEL);
			operands_[index] = l; 
		}

		// Get/Set Addrs
		inline Addr get_addr(size_t index) const {
			assert(type(index) == ADDR);
			return operands_[index];
		}

		inline void set_addr(size_t index, Addr a) {
			assert(type(index) == ADDR);
			operands_[index] = a;
		}

		// Set everything!
		template <typename InputIterator>
		inline void set_all(Opcode opcode, InputIterator begin, InputIterator end) {
			assert((size_t) (end - begin) == opcode.arity());
			opcode_ = opcode;
			std::copy(begin, end, operands_.begin());
			assert(check_operands());
		}

		// read / write sets
		RegSet explicit_read_set() const;

		RegSet explicit_write_set() const;

		// Convenience Accessors inherited from current opcode
		inline size_t arity() const { 
			return opcode_.arity(); 
		}

		inline Type type(size_t index) const { 
			return opcode_.type(index); 
		}

		inline BitWidth width(size_t index) const { 
			return opcode_.width(index); 
		}

		inline size_t mem_offset() const { 
			return opcode_.mem_offset(); 
		}

		inline bool accesses_mem() const { 
			return opcode_.accesses_mem(); 
		}

		inline size_t read_offset() const {
			return opcode_.read_offset();
		}

		inline bool writes_reg() const {
			return opcode_.writes_reg();
		}

		inline bool does_implicit_zero_extend() const {
			return opcode_.does_implicit_zero_extend();
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

		inline bool rexw_prefix() const {
			return opcode_.rexw_prefix();
		}

		inline bool mem_size_or() const {
			return opcode_.mem_size_or();
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

		inline bool is_label_defn() const {
			return opcode_.is_label_defn();
		}

		// Higher order attributes
		inline bool acceses_stack() const {
			const auto mo = opcode_.mem_offset();
			if ( mo == 16 )
				return false;

			const auto addr = get_addr(mo);
			const auto b = addr.get_base();
			const auto i = addr.get_index();
			const auto d = addr.get_disp();

			return b == rsp && i.is_null() && (int64_t) d <= 0;
		}

		inline bool accesses_heap() const { 
			const auto mo = opcode_.mem_offset();
			if ( mo == 16 )
				return false;

			const auto addr = get_addr(mo);
			const auto b = addr.get_base();
			const auto i = addr.get_index();
			const auto d = addr.get_disp();

			return b != rsp || !i.is_null() || (int64_t) d > 0;
		}

		inline RegSet read_set() const {
			return implicit_read_set() |= explicit_read_set();
		}

		inline RegSet write_set() const {
			return implicit_write_set() |= explicit_write_set();
		}

		inline RegSet undef_set() const {
			return implicit_undef_set();
		}

		inline void read_att(std::istream& is) {
			is.setstate(std::ios::failbit);
		}

		void write_att(std::ostream& os) const;

	private:

		Opcode opcode_;
		std::array<Operand, 3> operands_;

		bool check_opcode(Opcode o) const;
		bool check_operand(size_t index) const;
		bool check_operands() const;
};

} // namespace x64

#endif
