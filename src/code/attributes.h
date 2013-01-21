#ifndef X64_SRC_CODE_ATTRIBUTES_H
#define X64_SRC_CODE_ATTRIBUTES_H

#include <cassert>

#include "src/code/instruction.h"
#include "src/code/op_set.h"
#include "src/code/op_type.h"
#include "src/code/opcode.h"
#include "src/code/properties.h"

namespace x64 {

class Attributes {
	public:
		static size_t arity(const Opcode o) {
			return arity_[o];
		}

		static Properties properties(const Opcode o, size_t index) {
			assert(o < properties_.size());
			assert(index < properties_[o].size());
			return properties_[o][index];
		}

		static OpType type(const Opcode o, size_t index) {
			assert(o < type_.size());
			assert(index < type_[o].size());
			return type_[o][index];
		}

		static bool is_label_defn(const Opcode o) {
			return o == LABEL_DEFN;
		}

		static bool is_return(const Opcode o) {
			assert(o < is_return_.size());
			return is_return_[o];
		}

		static bool is_nop(const Opcode o) {
			assert(o < is_nop_.size());
			return is_nop_[o];
		}

		static bool is_jump(const Opcode o) {
			assert(o < is_jump_.size());
			return is_jump_[o];
		}

		static bool is_cond_jump(const Opcode o) {
			assert(o < is_cond_jump_.size());
			return is_cond_jump_[o];
		}

		static bool is_uncond_jump(const Opcode o) {
			assert(o < is_uncond_jump_.size());
			return is_uncond_jump_[o];
		}

		static OpSet implicit_must_read_set(const Opcode o) {
			assert(o < implicit_read_set_.size());
			return implicit_must_read_set_[o];
		}

		static OpSet implicit_maybe_read_set(const Opcode o) {
			assert(o < implicit_read_set_.size());
			return implicit_maybe_read_set_[o];
		}

		static OpSet implicit_must_write_set(const Opcode o) {
			assert(o < implicit_write_set_.size());
			return implicit_must_write_set_[o];
		}

		static OpSet implicit_maybe_write_set(const Opcode o) {
			assert(o < implicit_write_set_.size());
			return implicit_maybe_write_set_[o];
		}

		static OpSet implicit_must_undef_set(const Opcode o) {
			assert(o < implicit_undef_set_.size());
			return implicit_must_undef_set_[o];
		}

		static OpSet implicit_maybe_undef_set(const Opcode o) {
			assert(o < implicit_undef_set_.size());
			return implicit_maybe_undef_set_[o];
		}

	private:
};

} // namespace x64

#endif
