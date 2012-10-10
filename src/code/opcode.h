#ifndef X64_SRC_CODE_OPCODE_H
#define X64_SRC_CODE_OPCODE_H

#include <array>
#include <algorithm>
#include <cassert>
#include <vector>

#include "src/code/operand.h"
#include "src/code/reg_set.h"

namespace x64 {

/** Opcodes values.
*/	
#include "src/gen/opcode.enum"

/** An extended opcode representation that makes argument types explicit.
*/
class Opcode {
	public:
		inline Opcode() 
				: o_(NOP) { 
		}

		inline Opcode(OpcodeVal o) 
				: o_(o) {
		}

		inline operator Operand() const {
			return o_;
		}

		inline bool is_null() const {
			return o_ >= OPCODE_VAL_NULL;
		}

		__attribute__((pure)) 
		inline size_t arity() const {
			assert(o_ < arity_.size());
			return arity_[o_];
		}

		__attribute__((pure)) 
		inline Type type(size_t index) const {
			assert(o_ < type_.size());
			assert(index < arity());
			return type_[o_][index];
		}

		__attribute__((pure)) 
		inline BitWidth width(size_t index) const {
			assert(o_ < width_.size());
			assert(index < arity());
			return width_[o_][index];
		}

		__attribute__((pure)) 
		inline size_t mem_offset() const {
			assert(o_ < mem_offset_.size());
			return mem_offset_[o_];
		}

		__attribute__((pure)) 
		inline bool accesses_mem() const {
			// NOTE: This is a magic number indicating false
			return mem_offset() != 16;
		}

		__attribute__((pure))
		inline size_t read_offset() const {
			assert(o_ < read_offset_.size());
			return read_offset_[o_];
		}

		__attribute__((pure))
		inline bool writes_reg() const {
			assert(o_ < read_offset_.size());
			return writes_reg_[o_];
		}

		__attribute__((pure))
		inline bool does_implicit_zero_extend() const {
			return writes_reg() && type(0) == GP_REG && width(0) == DOUBLE;
		}

		__attribute__((pure))
		inline bool is_ret() const {
			return o_ == RETQ;
		}

		__attribute__((pure))
		inline bool is_cond_jump() const {
			assert(o_ < is_cond_jump_.size());
			return is_cond_jump_[o_];
		}

		__attribute__((pure))
		inline bool is_uncond_jump() const {
			assert(o_ < is_uncond_jump_.size());
			return is_uncond_jump_[o_];
		}

		__attribute__((pure))
		inline bool is_jump() const {
			assert(o_ < is_jump_.size());
			return is_jump_[o_];
		}

		__attribute__((pure))
		inline bool rexw_prefix() const {
			assert(o_ < rexw_prefix_.size());
			return rexw_prefix_[o_];
		}

		__attribute__((pure))
		inline RegSet implicit_read_set() const {
			assert(o_ < implicit_read_set_.size());
			return implicit_read_set_[o_];
		}

		__attribute__((pure))
		inline RegSet implicit_write_set() const {
			assert(o_ < implicit_write_set_.size());
			return implicit_write_set_[o_];
		}

		__attribute__((pure))
		inline RegSet implicit_undef_set() const {
			assert(o_ < implicit_undef_set_.size());
			return implicit_undef_set_[o_];
		}

		__attribute__((pure))
		inline bool is_label_defn() const {
			return o_ == LABEL_DEFN_64L;
		}

	private:
		Operand o_;

		static std::vector<const char*> opcodes_;
		static std::vector<size_t> arity_;
		static std::vector<std::array<Type, 3>> type_;
		static std::vector<std::array<BitWidth, 3>> width_;
		static std::vector<size_t> mem_offset_;
		static std::vector<size_t> read_offset_;
		static std::vector<bool> writes_reg_;
		static std::vector<bool> is_cond_jump_;
		static std::vector<bool> is_uncond_jump_;
		static std::vector<bool> is_jump_;
		static std::vector<bool> rexw_prefix_;
		static std::vector<RegSet> implicit_read_set_;
		static std::vector<RegSet> implicit_write_set_;
		static std::vector<RegSet> implicit_undef_set_;
};

} // namespace x64

#endif
