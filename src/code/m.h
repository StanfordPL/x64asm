#ifndef X64_SRC_CODE_M_H
#define X64_SRC_CODE_M_H

#include <cassert>

#include "src/code/r.h"
#include "src/code/imm.h"
#include "src/code/operand.h"
#include "src/code/sreg.h"
#include "src/code/scale.h"

namespace x64 {

/** An address in memory.
*/
class M {
	public:
		inline M() {
			set_all(sreg_null, r_null, r_null, scale_null, 0, false); 
		}

		inline M(Operand a) 
				: a_(a) {
		}

		inline M(R64 b) {
			set_all(sreg_null, b, r_null, times_1, 0, false);
		}

		inline M(R64 b, Imm32 d) {
			set_all(sreg_null, b, r_null, times_1, d, false);
		}

		inline M(R64 b, R64 i) {
			set_all(sreg_null, b, i, times_1, 0, false);	
		}

		inline M(R64 b, R64 i, Scale s) {
			set_all(sreg_null, b, i, s, 0, false);
		}

		inline M(R64 b, R64 i, Imm32 d) {
			set_all(sreg_null, b, i, times_1, d, false);
		}

		inline M(R64 b, R64 i, Scale s, Imm32 d) {
			set_all(sreg_null, b, i, s, d, false);
		}

		inline M(R32 b) {
			set_all(sreg_null, b, r_null, times_1, 0, true);
		}

		inline M(R32 b, Imm32 d) {
			set_all(sreg_null, b, r_null, times_1, d, true);
		}

		inline M(R32 b, R32 i) {
			set_all(sreg_null, b, i, times_1, 0, true);	
		}

		inline M(R32 b, R32 i, Scale s) {
			set_all(sreg_null, b, i, s, 0, true);
		}

		inline M(R32 b, R32 i, Imm32 d) {
			set_all(sreg_null, b, i, times_1, d, true);
		}

		inline M(R32 b, R32 i, Scale s, Imm32 d) {
			set_all(sreg_null, b, i, s, d, true);
		}

		inline operator Operand() const {
			return a_;
		}

		inline bool is_null() const {
			return get_base().is_null() || get_scale().is_null() || 
				     get_index() == rsp;			     
		}

		inline Sreg get_seg() const {
			return (Sreg) ((a_ >> 45) & 0x7);
		}

		inline R get_base() const {
			return (R) ((a_ >> 40) & 0x1f);
		}

		inline R get_index() const {
			return (R) ((a_ >> 35) & 0x1f);
		}

		inline Scale get_scale() const {
			return (Scale) ((a_ >> 32)  & 0x7);
		}

		inline Imm32 get_disp() const {
			return (Operand) (a_ & 0xffffffff);
		}

		inline bool get_size_or() const {
			return (a_ >> 48) & 0x1;
		}

		inline bool is_stack() const {
			return get_base() == rsp && get_index().is_null();
		}

		inline bool is_heap() const {
			return !is_stack();
		}

	private:
		// or  seg    base   index  scale  disp
		// [48][47:45][44:40][39:35][34:32][31:0]

		Operand a_;

		inline void set_all(Sreg s, R b, R i, Scale sc, Imm32 d, bool so) {
			a_ = 0x0;
			a_ = ((Operand) (so ? 0x1 : 0x0) << 48) |
					 ((Operand) (s & 0x7) << 45)  | 
					 ((Operand) (b & 0x1f) << 40) | 
					 ((Operand) (i & 0x1f) << 35) | 
		  		 ((Operand) (sc & 0x7) << 32) | 
					 ((Operand) (d & 0xffffffff));
		}
};

class M8 : public M {
	public:
		inline M8() : M() { }
		inline M8(Operand o) : M(o) { }
		inline M8(R64 b) : M(b) { }
		inline M8(R64 b, Imm32 d) : M(b, d) { }
		inline M8(R64 b, R64 i) : M(b, i) { }
		inline M8(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M8(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M8(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M8(R32 b) : M(b) { }
		inline M8(R32 b, Imm32 d) : M(b, d) { }
		inline M8(R32 b, R32 i) : M(b, i) { }
		inline M8(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M8(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M8(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

class M16 : public M {
	public:
		inline M16() : M() { }
		inline M16(Operand o) : M(o) { }
		inline M16(R64 b) : M(b) { }
		inline M16(R64 b, Imm32 d) : M(b, d) { }
		inline M16(R64 b, R64 i) : M(b, i) { }
		inline M16(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M16(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M16(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M16(R32 b) : M(b) { }
		inline M16(R32 b, Imm32 d) : M(b, d) { }
		inline M16(R32 b, R32 i) : M(b, i) { }
		inline M16(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M16(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M16(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

class M32 : public M {
	public:
		inline M32() : M() { }
		inline M32(Operand o) : M(o) { }
		inline M32(R64 b) : M(b) { }
		inline M32(R64 b, Imm32 d) : M(b, d) { }
		inline M32(R64 b, R64 i) : M(b, i) { }
		inline M32(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M32(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M32(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M32(R32 b) : M(b) { }
		inline M32(R32 b, Imm32 d) : M(b, d) { }
		inline M32(R32 b, R32 i) : M(b, i) { }
		inline M32(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M32(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M32(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

class M64 : public M {
	public:
		inline M64() : M() { }
		inline M64(Operand o) : M(o) { }
		inline M64(R64 b) : M(b) { }
		inline M64(R64 b, Imm32 d) : M(b, d) { }
		inline M64(R64 b, R64 i) : M(b, i) { }
		inline M64(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M64(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M64(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M64(R32 b) : M(b) { }
		inline M64(R32 b, Imm32 d) : M(b, d) { }
		inline M64(R32 b, R32 i) : M(b, i) { }
		inline M64(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M64(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M64(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

class M80 : public M {
	public:
		inline M80() : M() { }
		inline M80(Operand o) : M(o) { }
		inline M80(R64 b) : M(b) { }
		inline M80(R64 b, Imm32 d) : M(b, d) { }
		inline M80(R64 b, R64 i) : M(b, i) { }
		inline M80(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M80(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M80(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M80(R32 b) : M(b) { }
		inline M80(R32 b, Imm32 d) : M(b, d) { }
		inline M80(R32 b, R32 i) : M(b, i) { }
		inline M80(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M80(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M80(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

class M128 : public M {
	public:
		inline M128() : M() { }
		inline M128(Operand o) : M(o) { }
		inline M128(R64 b) : M(b) { }
		inline M128(R64 b, Imm32 d) : M(b, d) { }
		inline M128(R64 b, R64 i) : M(b, i) { }
		inline M128(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M128(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M128(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M128(R32 b) : M(b) { }
		inline M128(R32 b, Imm32 d) : M(b, d) { }
		inline M128(R32 b, R32 i) : M(b, i) { }
		inline M128(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M128(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M128(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

class M256 : public M {
	public:
		inline M256() : M() { }
		inline M256(Operand o) : M(o) { }
		inline M256(R64 b) : M(b) { }
		inline M256(R64 b, Imm32 d) : M(b, d) { }
		inline M256(R64 b, R64 i) : M(b, i) { }
		inline M256(R64 b, R64 i, Scale s) : M(b, i, s) { }
		inline M256(R64 b, R64 i, Imm32 d) : M(b, i, d) { }
		inline M256(R64 b, R64 i, Scale s, Imm32 d) : M(b, i, s, d) { }
		inline M256(R32 b) : M(b) { }
		inline M256(R32 b, Imm32 d) : M(b, d) { }
		inline M256(R32 b, R32 i) : M(b, i) { }
		inline M256(R32 b, R32 i, Scale s) : M(b, i, s) { }
		inline M256(R32 b, R32 i, Imm32 d) : M(b, i, d) { }
		inline M256(R32 b, R32 i, Scale s, Imm32 d) : M(b, i, s, d) { }
};

} // namespace x64

#endif
