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
#define DEF(idx, a, t1, t2, t3, w1, w2, w3, m1, m2, m3, r, j, uj, cj, mi, fr, nw) \
  ((Operand) idx << 50) | \
	((Operand) a   << 48) | \
	((Operand) t3  << 44) | ((Operand) t2 << 40) | ((Operand) t1 << 36) | \
	((Operand) w3  << 32) | ((Operand) w2 << 28) | ((Operand) w1 << 24) | \
	((Operand) m3  << 22) | ((Operand) m2 << 20) | ((Operand) m1 << 18) | \
	((Operand) r   << 17) | \
	((Operand) j   << 16) | ((Operand) uj << 15) | ((Operand) cj << 14) | \
	((Operand) mi  << 12) | ((Operand) fr << 10) | ((Operand) nw << 8)

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
				: o_(NOP) { 
		}

		inline Opcode(Operand o) 
				: o_(o) {
		}

		inline operator Operand() const {
			return o_;
		}

		inline bool is_null() const {
			// TODO -- This isn't correct!
			return o_ >= OPCODE_VAL_NULL;
		}

		inline size_t arity() const {
			return (o_ >> 48) & 0x3;
		}

		inline Type type(size_t index) const {
			assert(index < arity());
			return (Type) ((o_ >> (36 + index*4)) & 0x7);
		}

		inline BitWidth width(size_t index) const {
			assert(index < arity());
			return (BitWidth) ((o_ >> (24 + index*4)) & 0x7);
		}

		inline Modifier mod(size_t index) const {
			assert(index < arity());
			return (Modifier) ((o_ >> (18 + index*2)) & 0x3);
		}

		inline bool is_label_defn() const {
			return o_ == LABEL_DEFN_64L;
		}

		inline bool is_ret() const {
			return o_ & ((Operand) 0x1 << 17);
		}

		inline bool is_jump() const {
			return o_ & ((Operand) 0x1 << 16);
		}

		inline bool is_cond_jump() const {
			return o_ & ((Operand) 0x1 << 15);
		}

		inline bool is_uncond_jump() const {
			return o_ & ((Operand) 0x1 << 14);
		}

    inline bool touches_mem() const {
			return mem_index() != 3;
		}

		inline Modifier mem_mod() const {
			assert(touches_mem());
			return mod(mem_index());
		}

		inline RegSet implicit_read_set() const {
			assert(o_ < implicit_read_set_.size());
			return implicit_read_set_[o_ >> 50];
		}

		inline RegSet implicit_write_set() const {
			assert(o_ < implicit_write_set_.size());
			return implicit_write_set_[o_ >> 50];
		}

		inline RegSet implicit_undef_set() const {
			assert(o_ < implicit_undef_set_.size());
			return implicit_undef_set_[o_ >> 50];
		}

	private:
		Operand o_;

		static std::vector<RegSet> implicit_read_set_;
		static std::vector<RegSet> implicit_write_set_;
		static std::vector<RegSet> implicit_undef_set_;

		inline size_t mem_index() const {
			return (o_ >> 12) & 0x3;
		}

		inline size_t first_read() const {
			return (o_ >> 10) & 0x3;
		}

		inline size_t num_writes() const {
			return (o_ >> 8) & 0x3;
		}
};

} // namespace x64

#endif
