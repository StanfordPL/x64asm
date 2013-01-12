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
			assert(o < arity_.size());
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

		static OpSet implicit_read_set(const Opcode o) {
			assert(o < implicit_read_set_.size());
			return implicit_read_set_[o];
		}

		static OpSet implicit_write_set(const Opcode o) {
			assert(o < implicit_write_set_.size());
			return implicit_write_set_[o];
		}

		static OpSet implicit_def_set(const Opcode o) {
			assert(o < implicit_def_set_.size());
			return implicit_def_set_[o];
		}

		static OpSet implicit_undef_set(const Opcode o) {
			assert(o < implicit_undef_set_.size());
			return implicit_undef_set_[o];
		}

		static inline size_t arity(const Instruction& i) {
			return arity(i.get_opcode());
		}

		static inline Properties properties(const Instruction& i, size_t index) {
			return properties(i.get_opcode(), index);
		}

		static inline OpType type(const Instruction& i, size_t index) {
			return type(i.get_opcode(), index);
		}

		static inline bool is_label_defn(const Instruction& i) {
			return is_label_defn(i.get_opcode());
		}

		static inline bool is_return(const Instruction& i) {
			return is_return(i.get_opcode());
		}

		static inline bool is_nop(const Instruction& i) {
			return is_nop(i.get_opcode());
		}

		static inline bool is_jump(const Instruction& i) {
			return is_jump(i.get_opcode());
		}

		static inline bool is_cond_jump(const Instruction& i) {
			return is_cond_jump(i.get_opcode());
		}

		static inline bool is_uncond_jump(const Instruction& i) {
			return is_uncond_jump(i.get_opcode());
		}

		static inline OpSet implicit_read_set(const Instruction& i) {
			return implicit_read_set(i.get_opcode());
		}

		static inline OpSet implicit_write_set(const Instruction& i) {
			return implicit_write_set(i.get_opcode());
		}

		static inline OpSet implicit_def_set(const Instruction& i) {
			return implicit_def_set(i.get_opcode());
		}

		static inline OpSet implicit_undef_set(const Instruction& i) {
			return implicit_undef_set(i.get_opcode());
		}

		static OpSet explicit_read_set(const Instruction& i);
		static OpSet explicit_write_set(const Instruction& i);
		static OpSet explicit_def_set(const Instruction& i);
		static OpSet explicit_undef_set(const Instruction& i);

		static inline OpSet read_set(const Instruction& i) {
			return implicit_read_set(i) | explicit_read_set(i);
		}

		static inline OpSet write_set(const Instruction& i) {
			return implicit_write_set(i) | explicit_write_set(i);
		}

		static inline OpSet def_set(const Instruction& i) {
			return implicit_def_set(i) | explicit_def_set(i);
		}

		static inline OpSet undef_set(const Instruction& i) {
			return implicit_undef_set(i) | explicit_undef_set(i);
		}

	private:
		static std::vector<size_t> arity_;
		static std::vector<std::vector<Properties>> properties_;
		static std::vector<std::vector<OpType>> type_;
		static std::vector<bool> is_return_;
		static std::vector<bool> is_nop_;
		static std::vector<bool> is_jump_;
		static std::vector<bool> is_cond_jump_;
		static std::vector<bool> is_uncond_jump_;
		static std::vector<OpSet> implicit_read_set_;
		static std::vector<OpSet> implicit_write_set_;
		static std::vector<OpSet> implicit_def_set_;
		static std::vector<OpSet> implicit_undef_set_;
};

} // namespace x64

#endif
