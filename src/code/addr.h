#ifndef X64_SRC_CODE_ADDR_H
#define X64_SRC_CODE_ADDR_H

#include <cassert>

#include "src/code/gp_reg.h"
#include "src/code/imm.h"
#include "src/code/operand.h"
#include "src/code/seg_reg.h"
#include "src/code/scale.h"

namespace x64 {

/** An address in memory.
*/
class Addr {
	public:
		inline Addr() {
			set_all(seg_null, gp_null, gp_null, scale_null, 0, true); 
		}

		inline Addr(Operand a) 
				: a_(a) {
		}

		inline Addr(GpReg b, bool size_or = false) {
			set_all(seg_null, b, gp_null, times_1, 0, size_or);
		}

		inline Addr(GpReg b, Imm d, bool size_or = false) {
			set_all(seg_null, b, gp_null, times_1, d, size_or);
		}

		inline Addr(GpReg b, GpReg i, bool size_or = false) {
			set_all(seg_null, b, i, times_1, 0, size_or);	
		}

		inline Addr(GpReg b, GpReg i, Scale s, bool size_or = false) {
			set_all(seg_null, b, i, s, 0, size_or);
		}

		inline Addr(GpReg b, GpReg i, Imm d, bool size_or = false) {
			set_all(seg_null, b, i, times_1, d, size_or);
		}

		inline Addr(GpReg b, GpReg i, Scale s, Imm d, bool size_or = false) {
			set_all(seg_null, b, i, s, d, size_or);
		}

		inline operator Operand() const {
			return a_;
		}

		inline bool is_null() const {
			return get_base().is_null() || get_scale().is_null() || 
				     get_index() == rsp;			     
		}

		inline SegReg get_seg() const {
			return (SegReg) ((a_ >> 45) & 0x7);
		}

		inline void set_seg(SegReg s) {
			a_ &= ~((Operand) 0x3 << 45);
			a_ |= s << 45;
		}

		inline GpReg get_base() const {
			return (GpReg) ((a_ >> 40) & 0x1f);
		}

		inline void set_base(GpReg b) {
			a_ &= ~((Operand) 0x7f << 40);
			a_ |= b << 40;
		}

		inline GpReg get_index() const {
			return (GpReg) ((a_ >> 35) & 0x1f);
		}

		inline void set_index(GpReg i) {
			a_ &= ~((Operand) 0x7f << 35);
			a_ |= i << 35;
		}

		inline Scale get_scale() const {
			return (Scale) ((a_ >> 32)  & 0x7);
		}

		inline void set_scale(Scale s) {
			a_ &= ~((Operand) 0x7 << 32);
			a_ |= s << 32;
		}

		inline Imm get_disp() const {
			return (Operand) (a_ & 0xffffffff);
		}

		inline void set_disp(Imm d) {
			a_ &= ~((Operand) 0xffffffff);
			a_ |= d;
		}

		inline bool get_size_or() const {
			return (a_ >> 48) & 0x1;
		}

		inline void set_size_or(bool sor) {
			if ( sor )
				a_ |= ((Operand) 0x1 << 48);
			else
				a_ &= ~((Operand) 0x1 << 48);
		}

		inline BitWidth get_reg_width() const {
			return get_size_or() ? DOUBLE : QUAD;
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

		inline void set_all(SegReg s, GpReg b, GpReg i, Scale sc, Imm d, bool so) {
			a_ = 0x0;
			a_ = ((Operand) (so ? 0x1 : 0x0) << 48) |
					 ((Operand) (s & 0x7) << 45)  | 
					 ((Operand) (b & 0x1f) << 40) | 
					 ((Operand) (i & 0x1f) << 35) | 
		  		 ((Operand) (sc & 0x7) << 32) | 
					 ((Operand) (d & 0xffffffff));
		}
};

} // namespace x64

#endif
