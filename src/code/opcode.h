#ifndef X64_SRC_CODE_OPCODE_H
#define X64_SRC_CODE_OPCODE_H

#include <cassert>
#include <cstddef>
#include <vector>

#include "src/code/operand.h"
#include "src/code/reg_set.h"

namespace x64 {

// What's going on here?
// We have 64-bits worth of space for each opcode name.
// That's enough room to fit the answers to most interesting queries.
// This allows us to answer those queries using at most two arithmetic ops.
#define DEF(idx, a, t1, t2, t3, m1, m2, m3, r, j, uj, cj, mi, fr, nw) \
  ((Operand) idx << 50) | \
	((Operand) a   << 48) | \
	((Operand) t3  << 42) | ((Operand) t2 << 36) | ((Operand) t1) << 30 | \
	((Operand) m3  << 27) | ((Operand) m2 << 24) | ((Operand) m1) << 21 | \
	((Operand) r   << 20) | \
	((Operand) j   << 19) | ((Operand) uj << 18) | ((Operand) cj) << 17 | \
	((Operand) mi  << 15) | ((Operand) fr << 13) | ((Operand) nw) << 11

/** Opcodes values.
*/	
#include "src/gen/opcode.enum"

#undef DEF

/** An extended opcode representation that makes argument types explicit.
*/
class Opcode {
	friend class Instruction;

	public:
		inline Opcode() 
				: o_(-1) { 
		}

		inline Opcode(Operand o) 
				: o_(o) {
		}

		inline operator Operand() const {
			return o_;
		}

		inline bool is_null() const {
			return (o_ >> 50) >= domain_.size();
		}

		inline size_t arity() const {
			return (o_ >> 48) & 0x3;
		}

		inline Type type(size_t index) const {
			assert(index < arity());
			return (Type) ((o_ >> (30 + index*6)) & 0x3f);
		}

		inline Modifier modifier(size_t index) const {
			assert(index < arity());
			return (Modifier) ((o_ >> (21 + index*3)) & 0x7);
		}

		inline bool is_label_defn() const {
			return o_ == LABEL_DEFN_64L;
		}

		inline bool is_ret() const {
			return o_ & ((Operand) 0x1 << 20);
		}

		inline bool is_jump() const {
			return o_ & ((Operand) 0x1 << 19);
		}

		inline bool is_cond_jump() const {
			return o_ & ((Operand) 0x1 << 17);
		}

		inline bool is_uncond_jump() const {
			return o_ & ((Operand) 0x1 << 18);
		}

    inline bool touches_mem() const {
			return mem_index() != 3;
		}

		inline Modifier mem_modifier() const {
			assert(touches_mem());
			return modifier(mem_index());
		}

		inline RegSet implicit_read_set() const {
			assert((o_ >> 50) < implicit_read_set_.size());
			return implicit_read_set_[o_ >> 50];
		}

		inline RegSet implicit_write_set() const {
			assert((o_ >> 50) < implicit_write_set_.size());
			return implicit_write_set_[o_ >> 50];
		}

		inline RegSet implicit_undef_set() const {
			assert((o_ >> 50) < implicit_undef_set_.size());
			return implicit_undef_set_[o_ >> 50];
		}

		typedef const std::vector<Opcode>::const_iterator iterator;

		static iterator begin() {
			return domain_.begin();
		}

		static iterator end() {
			return domain_.end();
		}

	private:
		Operand o_;

		static const std::vector<RegSet> implicit_read_set_;
		static const std::vector<RegSet> implicit_write_set_;
		static const std::vector<RegSet> implicit_undef_set_;

		static const std::vector<Opcode> domain_;

		inline size_t mem_index() const {
			return (o_ >> 15) & 0x3;
		}

		inline size_t first_read() const {
			return (o_ >> 13) & 0x3;
		}

		inline size_t num_writes() const {
			return (o_ >> 1) & 0x3;
		}
};

} // namespace x64

#endif
