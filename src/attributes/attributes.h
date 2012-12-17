#ifndef X64_SRC_ATTRIBUTES_ATTRIBUTES_H
#define X64_SRC_ATTRIBUTES_ATTRIBUTES_H

#include "src/attributes/op_accessor.h"
#include "src/attributes/op_set.h"
#include "src/attributes/op_type.h"
#include "src/code/instruction.h"
#include "src/code/opcode.h"

namespace x64 {

class Attributes {
	public:
		static size_t arity(const Opcode o);
		static OpAccessor accessor(const Opcode o, size_t index);
		static OpType type(const Opcode o, size_t index);

		static size_t is_label_defn(const Opcode o);
		static size_t is_return(const Opcode o);
		static size_t is_jump(const Opcode o);
		static size_t is_cond_jump(const Opcode o);
		static size_t is_uncond_jump(const Opcode o);

		static OpSet implicit_read_set(const Opcode o);
		static OpSet implicit_write_set(const Opcode o);
		static OpSet implicit_def_set(const Opcode o);
		static OpSet implicit_undef_set(const Opcode o);

		static inline size_t arity(const Instruction& i) {
			return arity(i.get_opcode());
		}

		static inline OpAccessor accessor(const Instruction& i, size_t index) {
			return accessor(i.get_opcode(), index);
		}

		static inline OpType type(const Instruction& i, size_t index) {
			return type(i.get_opcode(), index);
		}

		static inline size_t is_label_defn(const Instruction& i) {
			return is_label_defn(i.get_opcode());
		}

		static inline size_t is_return(const Instruction& i) {
			return is_return(i.get_opcode());
		}

		static inline size_t is_jump(const Instruction& i) {
			return is_jump(i.get_opcode());
		}

		static inline size_t is_cond_jump(const Instruction& i) {
			return is_cond_jump(i.get_opcode());
		}

		static inline size_t is_uncond_jump(const Instruction& i) {
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

};

} // namespace x64

#endif
